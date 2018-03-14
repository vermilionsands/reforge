;; partially adapted from original clojure gen-class
(ns vermilionsands.reforge.core
  (:refer-clojure :exclude [deftype])
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
   (int (transduce (map key->opcode) + default modifiers))))

(defn accept [^ClassReader ^ClassVisitor cr visitor & [flags]]
  (.accept cr visitor (or flags 0)))

(defn accept-and-get [^ClassReader cr ^ClassWriter cw ^ClassVisitor visitor & [flags]]
  (accept cr visitor flags)
  (.toByteArray ^ClassWriter cw))

(defn- classes->desc [return-class param-classes]
  (Type/getMethodDescriptor
    (asm-type return-class)
    (into-array Type (map asm-type param-classes))))

(defn add-forwarding-var-to-static-block [^ClassReader cr ^ClassWriter cv class-type impl-package-name prefix v]
  (let [static-block-visitor
        (fn [mv]
          (proxy [MethodVisitor] [Opcodes/ASM4 mv]
            (visitCode []
              (.visitCode mv)
              ;; generate calls to intern
              (.visitLdcInsn mv impl-package-name)
              (.visitLdcInsn mv (str prefix v))
              (let [m (Method/getMethod "clojure.lang.Var internPrivate(String,String)")]
                (.visitMethodInsn mv Opcodes/INVOKESTATIC (.getInternalName var-type) (.getName m) (.getDescriptor m)))
              (.visitFieldInsn mv Opcodes/PUTSTATIC (.getInternalName class-type) (var-name v) (.getDescriptor var-type)))))
            ;(visitMaxs [max-stack max-locals] ???)
        visited? (volatile! false)

        static-init-visitor
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
            (.visitEnd cv)))]
    (accept cr static-init-visitor)))

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
  [cv class-name method-name param-classes return-class modifiers else-gen]
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

    ;; emit var, would also need static init to point to a correct fn
    (emit-forwarding-var cv method-name)

    ;; rest of logic from gen-class
    ;; right now now support for overrides (would it be needed anyway?)
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

(defn method-added [method-name return-class param-classes modifiers])


(defn reload [class-name bytecode]
  (when *compile-files*
    (Compiler/writeClassFile class-name bytecode))
  (.defineClass ^DynamicClassLoader (deref Compiler/LOADER) class-name bytecode nil))

(defn reforge-class [{:keys [name class-access methods-modify methods-add]}]
  ;;add validation
  (let [qualified-name (str name)
        class-name (last (clojure.string/split qualified-name #"\."))
        cr (ClassReader. ^String qualified-name)
        cv (ClassWriter. cr ClassWriter/COMPUTE_MAXS)
        class-modifier
        (when class-access (class-modifier class-access))

        method-modifiers
        (for [[method-name [return-class param-classes] access-keys] methods-modify]
          (method-modifier (str method-name) return-class param-classes access-keys))

        method-adders
        (for [[method-name [return-class param-classes] access-keys] methods-add]
          ;; add implementation
          (method-adder (str method-name) return-class param-classes access-keys))

        visitor-generators (remove nil? (flatten [class-modifier method-modifiers method-adders]))

        updated-bytecode
        (reduce
          (fn [bytes proxy-gen]
            (let [cr (ClassReader. ^bytes bytes)
                  cv (ClassWriter. cr ClassWriter/COMPUTE_MAXS)]
              (accept-and-get cr (proxy-gen cv) cv)))
          (accept-and-get cr cv cv)
          visitor-generators)]

    ;; fix package name, ns etc.
    ;(when method
    ;  (doseq [[mname [rclass pclasses] modifiers] method]
    ;    (emit-forwarding-method cv class-name mname pclasses rclass modifiers emit-unsupported)
    ;    (add-forwarding-var-to-static-block cr cv (Type/getObjectType (.replace class-name "." "/")) (str (ns-name *ns*)) "-" mname))

    ;; import sucks right now
    (reload qualified-name updated-bytecode)
    name))

;(let [gname name
;      [interfaces methods opts] (parse-opts+specs opts+specs)
;      ns-part (namespace-munge *ns*)
;      classname (symbol (str ns-part "." gname))
;      hinted-fields fields
;      fields (vec (map #(with-meta % nil) fields))
;      [field-args over] (split-at 20 fields)
;  `(let []
;     ~(emit-deftype* name gname (vec hinted-fields) (vec interfaces) methods opts)
;     (import ~classname)
;     ~(build-positional-factory gname classname fields)
;     ~classname)

(defmacro reforge [& opts]
  (let [opts-map (apply hash-map opts)
        class-name (:name opts-map)]
    `(do
       ~(reforge-class opts-map)
       (import ~class-name)
       ~class-name)))