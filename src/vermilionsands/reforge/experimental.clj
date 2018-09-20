;; partially adapted from original clojure gen-class
(ns vermilionsands.reforge.experimental
  (:require [clojure.spec.alpha :as s])
  (:import [clojure.asm Opcodes ClassReader ClassWriter ClassVisitor Type MethodVisitor]
           [clojure.asm.commons GeneratorAdapter Method]
           [clojure.lang Compiler$HostExpr DynamicClassLoader Var IFn RT]))

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

(defn- replace-dot->slash [s]
  (.replace s "." "/"))

(defn class-type [s] (Type/getObjectType (replace-dot->slash s)))

(def key->opcode
  {:public    Opcodes/ACC_PUBLIC
   :protected Opcodes/ACC_PROTECTED
   :private   Opcodes/ACC_PRIVATE
   :static    Opcodes/ACC_STATIC
   :abstract  Opcodes/ACC_ABSTRACT
   :final     Opcodes/ACC_FINAL})

(defn- sum-opcodes
  ([modifiers]
   (sum-opcodes 0 modifiers))
  ([default modifiers]
   (int (transduce (map #(key->opcode % 0)) + default (set modifiers)))))

(defn- sum-class-opcodes
  [modifiers]
  (let [xs (set (conj modifiers Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER))]
    (if (contains? xs :nonfinal)
      (sum-opcodes (disj xs :final))
      (sum-opcodes (conj xs :final)))))

(defn- accept [^ClassReader ^ClassVisitor cr visitor & [flags]]
  (.accept cr visitor (or flags 0)))

(defn- accept-and-get [^ClassReader cr ^ClassWriter cw ^ClassVisitor visitor & [flags]]
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

        access            (sum-opcodes (if (empty? modifiers) [Opcodes/ACC_PUBLIC] modifiers))
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
    (let [access (sum-class-opcodes modifiers)]
      (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visit [ver _ name signature super-name interfaces]
          (.visit cv ver access name signature super-name interfaces))))))

(defn method-modifier [method-name return-class param-classes flags]
  (let [expected (classes->desc return-class param-classes)]
    (fn [cv]
      (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visitMethod [access name desc signature exceptions]
          (if (and (= method-name name) (= expected desc))
            (.visitMethod cv (sum-opcodes flags) name desc signature exceptions)
            (.visitMethod cv access name desc signature exceptions)))))))

;; todo - pass only impl-package + impl-name, remove prefix, method-name
(defn method-adder [class-name impl-package-name prefix impl method-name return-class param-classes flags]
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
          (emit-forwarding-method cv class-name method-name return-class param-classes flags emit-unsupported)
          (.visitEnd cv))))))

(defn reload [class-name bytecode]
  (when *compile-files*
    (Compiler/writeClassFile (replace-dot->slash class-name) bytecode))
  (.defineClass ^DynamicClassLoader (RT/makeClassLoader) class-name bytecode nil))

(defn reforge-class [opts-map]
  ;;add validation
  (let [{:keys [class-name class-flags extends implements methods]} opts-map
        class-name (str class-name)
        short-name (last (clojure.string/split class-name #"\."))
        impl-package-name (str *ns*)
        cr (ClassReader. ^String class-name)
        cv (ClassWriter. cr ClassWriter/COMPUTE_MAXS)

        class-modifier
        (when-not (empty? class-flags) (class-modifier class-flags))

        method-modifiers
        (for [{:keys [name return-class param-classes flags op]} methods
              :when (= :modify op)]
          (method-modifier (str name) return-class param-classes flags))

        method-adders
        (for [{:keys [name return-class param-classes flags op prefix impl]} methods
              :when (= :add op)]
          (method-adder class-name impl-package-name prefix impl (str name) return-class param-classes flags))

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

;(s/def ::opts-or-specs
;  (s/or
;    :implements #(or (class? %) (symbol? %))
;    :opt        (s/cat :opt-key keyword? :opt-val any?)
;    :method     ::method))

(s/def ::method
  (s/cat
    :name-sym  symbol?
    :op        keyword?
    :flags     (s/? set?)
    :args      vector?
    :impl      (s/? any?)))

(s/def ::opts-or-specs
  (s/or
    :method ::method))

(s/def ::modify-type
  (s/cat
    :name-sym      symbol?
    :flags         (s/? set?)
    :opts-or-specs (s/* ::opts-or-specs)))

(defn parse-method [m]
  (let [{:keys [name-sym op flags args impl]} m]
    {:name          name
     :op            op
     :flags         (set (filter key flags))
     :return-class  (or (first (filter symbol? flags)) Object)
     :param-classes (map #(if (= '_ %) Object %) args)
     :impl          impl}))

(defmacro modify-type [& class-spec]
  (let [{:keys [name-sym flags opts+spec]} (s/conform ::modify-type class-spec)
        opts+spec-map (group-by first opts+spec)
        class-name (.getCanonicalName ^Class (resolve name-sym))
        methods (map parse-method (:method opts+spec-map))]
    `(let []
       (reforge-class
         {:class-name  ~class-name
          :class-flags ~flags
          :methods     ~methods})
       (import ~(symbol class-name))
       ~class-name)))