(ns vermilionsands.reforge.core
  (:refer-clojure :exclude [deftype])
  (:import [org.objectweb.asm ClassVisitor Opcodes ClassReader ClassWriter MethodVisitor Type]
           [clojure.lang DynamicClassLoader RT]))

(def clojure-asm-type #'clojure.core/asm-type)

;; repackage clojure Type into objectweb Type
;; maybe switch to clojure asm to simplify things
(defn asm-type [c]
  (Type/getType ^String (.getDescriptor ^clojure.asm.Type (clojure-asm-type c))))

;(defmacro deftype
;  [name fields & opts+specs]
;  (binding [*compile-files* true]
;    `(clojure.core/deftype ~name ~fields ~@opts+specs)))

(def key->modifier
  {:public    Opcodes/ACC_PUBLIC
   :protected Opcodes/ACC_PROTECTED
   :private   Opcodes/ACC_PRIVATE
   :static    Opcodes/ACC_STATIC
   :abstract  Opcodes/ACC_ABSTRACT
   :final     Opcodes/ACC_FINAL})

(defn modify-class-access [^ClassReader cr ^ClassWriter cv modifiers]
  (let [access (int (reduce #(+ %1 (key->modifier %2)) Opcodes/ACC_SUPER modifiers))]
    (.accept cr
      (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visit [ver _ name sig sname ifaces]
          (.visit cv ver access name sig sname ifaces)))
      0)))

(defn- method-types->desc [[ret-class arg-classes]]
  (Type/getMethodDescriptor
    (asm-type ret-class)
    (into-array Type (map asm-type arg-classes))))

(defn modify-method-access [^ClassReader cr ^ClassWriter cv mname types modifiers]
  (let [access (int (reduce #(+ %1 (key->modifier %2)) 0 modifiers))
        mdesc (when types (method-types->desc types))]
    (.accept cr
      (proxy [ClassVisitor] [Opcodes/ASM4 cv]
        (visitMethod [_ name desc signature exceptions]
          (when (and (= mname name) (or (nil? mdesc) (= mdesc mname)))
            (.visitMethod cv access name desc signature exceptions))))
      0)))

(defn reload [cname bytecode]
  (when *compile-files*
    (Compiler/writeClassFile cname bytecode))
  (.defineClass ^DynamicClassLoader (deref Compiler/LOADER) cname bytecode nil))

(defn reforge [name & opts]
  (let [{:keys [class-access method-access]} (apply hash-map opts)
        ^String cname (str name)
        cr (ClassReader. cname)
        cv (ClassWriter. cr 0)]
    (when class-access
      (modify-class-access cr cv class-access))
    (when method-access
      (doseq [[mname types modifiers] method-access]
        (modify-method-access cr cv mname types modifiers)))
    (let [c (reload cname (.toByteArray cv))]
      (list 'clojure.core/import* (symbol cname))
      c)))