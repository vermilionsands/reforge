;; partially adapted from original clojure gen-class
(ns vermilionsands.reforge.core
  (:require [clojure.spec.alpha :as s])
  (:import [clojure.asm Opcodes ClassReader ClassWriter ClassVisitor Type MethodVisitor]
           [clojure.asm.commons GeneratorAdapter Method]
           [clojure.lang Compiler$HostExpr DynamicClassLoader Var IFn]))

(def the-class        #'clojure.core/the-class)
(def add-annotations  #'clojure.core/add-annotations)
(def asm-type         #'clojure.core/asm-type)

(defn- var-name
  [s]
  (Compiler/munge (str s "__var")))

(defn- to-type
  [^Class c]
  (Type/getType c))

(defn- to-types
  [cs]
  (into-array Type (map to-type cs)))

(def ifn-type (to-type IFn))
(def obj-type (to-type Object))
(def var-type (to-type Var))
(def ex-type  (to-type UnsupportedOperationException))

(defn class-type [s] (Type/getObjectType (.replace s "." "/")))

(def key->opcode
  {:public    Opcodes/ACC_PUBLIC
   :protected Opcodes/ACC_PROTECTED
   :private   Opcodes/ACC_PRIVATE
   :static    Opcodes/ACC_STATIC
   :abstract  Opcodes/ACC_ABSTRACT
   :final     Opcodes/ACC_FINAL})

(defn sum-opcodes
  ([modifiers]
   (sum-opcodes 0 modifiers))
  ([default modifiers]
   (int (transduce (map #(key->opcode % 0)) + default modifiers))))

(defn accept [^ClassReader ^ClassVisitor cr visitor & [flags]]
  (.accept cr visitor (or flags 0)))

(defn accept-and-get [^ClassReader cr ^ClassWriter cw ^ClassVisitor visitor & [flags]]
  (accept cr visitor flags)
  (.toByteArray ^ClassWriter cw))

(defn- classes->desc [return-class param-classes]
  (Type/getMethodDescriptor
    (asm-type return-class)
    (into-array Type (map asm-type param-classes))))

(defn- emit-get-var
  [^GeneratorAdapter gen class-type x]
  (let [false-label (.newLabel gen)
        end-label   (.newLabel gen)]
    (.getStatic gen class-type (var-name x) var-type)
    (.dup gen)
    (.invokeVirtual gen var-type (Method/getMethod "boolean isBound()"))
    (.ifZCmp gen GeneratorAdapter/EQ false-label)
    (.invokeVirtual gen var-type (Method/getMethod "Object get()"))
    (.goTo gen end-label)
    (.mark gen false-label)
    (.pop gen)
    (.visitInsn gen Opcodes/ACONST_NULL)
    (.mark gen end-label)))

(defn- emit-unsupported
  [^GeneratorAdapter gen ^Method m]
  (.throwException gen ex-type (str (.getName m) " not defined?")))

(defn- emit-forwarding-var [^ClassVisitor cv v]
  (let [access (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL Opcodes/ACC_STATIC)
        name   (var-name v)
        desc   (.getDescriptor var-type)]
    (.visitField cv access name desc nil nil)))

(defn- emit-forwarding-method
  [cv class-name method-name return-class param-classes modifiers else-gen]
  (let [class-type        (Type/getObjectType (.replace class-name "." "/"))
        param-metas       (map meta param-classes)
        param-classes     (map the-class param-classes)
        return-class      (the-class return-class)
        param-types       (to-types param-classes)
        return-type ^Type (to-type return-class)

        access            (sum-opcodes modifiers)
        as-static?        ((set modifiers) :static)

        method            (Method. method-name return-type param-types)
        gen               (GeneratorAdapter. access method nil nil cv)

        [found-label else-label end-label] (repeatedly 3 #(.newLabel gen))]

    ;; emit var
    (emit-forwarding-var cv method-name)

    ;; rest of logic from gen-class
    ;; right now no support for overrides (would it be needed anyway?)
    (add-annotations gen (meta name))
    (dotimes [i (count param-metas)]
      (add-annotations gen (nth param-metas i) i))

    (.visitCode gen)
    (if (> (count param-classes) 18)
      (else-gen gen method)
      (do
        (emit-get-var gen class-type method-name)
        (.dup gen)
        (.ifNull gen else-label)
        (.checkCast gen ifn-type)
        (when-not as-static?
          (.loadThis gen))

        ;box args
        (dotimes [i (count param-types)]
          (.loadArg gen i)
          (Compiler$HostExpr/emitBoxReturn nil gen (nth param-classes i)))

        ;call fn
        (.invokeInterface gen ifn-type
          (Method. "invoke" obj-type
                   (to-types
                     (replicate (+ (count param-types) (if as-static? 0 1)) Object))))

        ;unbox return
        (.unbox gen return-type)
        (when (= (.getSort return-type) Type/VOID)
          (.pop gen))
        (.goTo gen end-label)

        ;else call supplied alternative generator
        (.mark gen else-label)
        (.pop gen)
        (else-gen gen method)
        (.mark gen end-label)))
    (.returnValue gen)
    (.endMethod gen)))

(defn class-modifier [modifiers]
  (fn [cv]
    (let [access (sum-opcodes Opcodes/ACC_SUPER modifiers)]
      (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visit [ver _ name signature super-name interfaces]
          (.visit cv ver access name signature super-name interfaces))))))

(defn method-modifier [method-name return-class param-classes modifiers]
  (let [expected (classes->desc return-class param-classes)]
    (fn [cv]
      (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visitMethod [access name desc signature exceptions]
          (if (and (= method-name name) (= expected desc))
            (.visitMethod cv (sum-opcodes modifiers) name desc signature exceptions)
            (.visitMethod cv access name desc signature exceptions)))))))

(defn method-adder [class-name impl-package-name prefix method-name return-class param-classes modifiers]
  (let [class-type (class-type class-name)
        static-block-visitor
        (fn [mv]
          (proxy [MethodVisitor] [Opcodes/ASM4 mv]
            (visitCode []
              (.visitCode mv)
              (.visitLdcInsn mv impl-package-name)
              (.visitLdcInsn mv (str prefix method-name))
              (let [m (Method/getMethod "clojure.lang.Var internPrivate(String,String)")]
                (.visitMethodInsn mv Opcodes/INVOKESTATIC (.getInternalName var-type) (.getName m) (.getDescriptor m)))
              (.visitFieldInsn mv Opcodes/PUTSTATIC (.getInternalName class-type) (var-name method-name) (.getDescriptor var-type)))))
             ;check if we need visitMaxs here
        visited? (volatile! false)]

   (fn [cv]
     (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visitMethod [access name desc signature exceptions]
          (let [mv (.visitMethod cv access name desc signature exceptions)]
            (if (and (= "<clinit>" name) (not @visited?))
              (do
                (vreset! visited? true)
                (static-block-visitor mv))
              mv)))

        (visitEnd []
          (when-not @visited?
            (vreset! visited? true)
            (let [mv (.visitMethod cv Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)
                  mv (static-block-visitor mv)]
              (.visitCode mv)
              (.visitInsn mv Opcodes/RETURN)
              (.visitMaxs mv 0 0)
              (.visitEnd mv)))
          (emit-forwarding-method cv class-name method-name return-class param-classes modifiers emit-unsupported)
          (.visitEnd cv))))))

(defn reload [class-name bytecode]
  (when *compile-files*
    (Compiler/writeClassFile class-name bytecode))
  (.defineClass ^DynamicClassLoader (deref Compiler/LOADER) class-name bytecode nil))

;; simplify
{:class
 {:name 'someName
  :modifiers [:public :nonfinal]
  :extends 'someThing
  :implements [{:name 'someInterface :op :add}
               {:name 'another       :op :remove}]}
 :methods [{}]}

{:name            'SomeName
 :class-modifiers [:public :nonfinal]
 :extends         'Object
 :implements      []
 :methods         []}

(defn reforge-class [opts-map]
  ;;add validation
  (let [{:keys [class-name class-modifiers extends implements methods]} opts-map
        class-name (str class-name)
        short-name (last (clojure.string/split class-name #"\."))
        impl-package-name (str *ns*)
        cr (ClassReader. ^String class-name)
        cv (ClassWriter. cr ClassWriter/COMPUTE_MAXS)

        class-modifier
        (when-not (empty? class-modifiers) (class-modifier class-modifiers))

        method-modifiers
        (for [{:keys [name return-class param-classes method-modifiers op]} methods
              :when (= :modify op)]
          (method-modifier (str name) return-class param-classes method-modifiers))

        method-adders
        (for [{:keys [name return-class param-classes method-modifiers op]} methods
              :when (= :add op)]
          (method-adder class-name impl-package-name "" (str name) return-class param-classes method-modifiers))

        visitor-generators (remove nil? (flatten [class-modifier method-modifiers method-adders]))

        updated-bytecode
        (reduce
          (fn [bytes proxy-gen]
            (let [cr (ClassReader. ^bytes bytes)
                  cv (ClassWriter. cr ClassWriter/COMPUTE_MAXS)]
              (accept-and-get cr cv (proxy-gen cv))))
          (accept-and-get cr cv cv)
          visitor-generators)]

    (reload class-name updated-bytecode)
    class-name))

(s/def ::arrow
  (s/cat :key #{:-} :meta any?))

(s/def ::name
  (s/cat :name symbol? :name-meta (s/? ::arrow)))

(s/def ::arg-vector
  (s/and
    vector?
    (s/* ::name)))

(s/def ::method
  (s/cat
    :name symbol?
    :name-meta (s/? ::arrow)
    :args (s/? ::arg-vector)
    :impl (s/? any?)))

(s/def ::opts-or-specs
  (s/or
    :implements #(or (class? %) (symbol? %))
    :opt        (s/cat :opt-key keyword? :opt-val any?)
    :method     ::method))

(s/def ::modify-type
  (s/cat :class     ::name
         :fields    (s/? ::arg-vector)
         :opts+spec (s/* ::opts-or-specs)))

(defn merge-meta [arrow meta]
  (merge
    (reduce
      (fn [acc x]
        (conj acc (if (map? x) x {x true})))
      {}
      (if (or (nil? arrow) (coll? arrow)) arrow [arrow]))
    meta))

(defn modifiers [m]
  (->> m
    (filter (fn [[k v]] (and (keyword? k) (true? v))))
    (mapv first)))

(defn parse-method [m]
  (let [{:keys [name name-meta args impl]} m
        method-meta (merge-meta (:meta name-meta) (meta name))
        method-modifiers (modifiers method-meta)
        op (-> method-meta
               (select-keys [:add :remove :modify])
               (#(filter (comp true? second) %))
               ffirst)
        return-class (or (:tag method-meta) Object)
        param-classes (mapv (fn [{:keys [name name-meta]}]
                              (or (:tag (merge-meta (:meta name-meta) (meta name))) Object))
                            args)]
    {:name name
     :method-modifiers method-modifiers
     :op op
     :impl impl
     :return-class return-class
     :param-classes param-classes}))

(defn merge-opts [opts]
  (loop [acc {:methods []} [[k v] & rest :as opts] opts]
    (if (empty? opts)
      acc
      (recur
        (condp = k
          :method (update acc :methods conj (parse-method v))
          acc)
        rest))))

(defmacro modify-type [& class-spec]
  (let [{:keys [class fields opts+spec]} (s/conform ::modify-type class-spec)
        {:keys [methods]} (merge-opts opts+spec)
        class-name (:name class)
        class-meta (merge-meta (-> class :name-meta :meta) (meta class-name))
        class-modifiers (modifiers class-meta)]
    `(do
       (reforge-class
         {:class-name      '~class-name
          :class-modifiers '~class-modifiers
          :methods         '~methods})
       (import ~class-name)
       ~class-name)))