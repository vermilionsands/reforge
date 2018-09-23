(ns vermilionsands.reforge
  (:require [clojure.java.io :as io])
  (:import [clojure.asm ClassReader ClassWriter Opcodes ClassVisitor Type MethodVisitor]
           [clojure.lang RT DynamicClassLoader Compiler Compiler$C Compiler$ObjExpr Namespace]
           [java.io ByteArrayInputStream InputStream ByteArrayOutputStream]
           [java.lang.reflect Field]))

(defn- ^bytes eval-deftype
  "Custom eval for `deftype*` calls. Returns bytecode as byte array
  for further transformations."
  [form]
  (let [expanded-form (macroexpand form)
        expr (Compiler/analyze Compiler$C/EVAL expanded-form)
        bytecode (.getDeclaredField Compiler$ObjExpr "bytecode")]
    (.setAccessible bytecode true)
    (.get bytecode expr)))

(def emit-deftype* #'clojure.core/emit-deftype*)
(def build-positional-factory #'clojure.core/build-positional-factory)

(defn- replace-dot->slash [s]
  (.replace s "." "/"))

(defn reload [class-name bytecode]
  (when *compile-files*
    (Compiler/writeClassFile (replace-dot->slash class-name) bytecode))
  (.defineClass ^DynamicClassLoader (RT/makeClassLoader) class-name bytecode nil))

(defn ^bytes class->bytecode [class-name]
  (with-open [is (ClassLoader/getSystemResourceAsStream
                   (str (clojure.string/replace class-name #"\." "/") ".class"))
              os (ByteArrayOutputStream.)]
    (io/copy is os)
    (.toByteArray os)))

(defn- ^Class resolve-classname [sym]
  (let [cname (name sym)]
    (try
      (if (> (.indexOf cname ".") 0)
        (RT/classForName cname)
        (let [c (.getMapping ^Namespace *ns* sym)]
          (if (class? c)
            c
            (RT/classForName cname))))
      (catch Exception _ Object))))

(defn resolve-hint [hint]
  (cond
    (symbol? hint)
    (-> hint resolve-classname Type/getType)

    (instance? Class hint)
    (Type/getType ^Class hint)

    ;; todo
    ;; handle Strings and specials here

    :else
    (Type/getType ^Class Object)))

(defn- hint->desc [hint]
  (.getDescriptor (resolve-hint hint)))

(defn- hint->internal-name [hint]
  (.getInternalName (resolve-hint hint)))

(defn- class-reader [^bytes bytecode]
  (with-open [is (ByteArrayInputStream. bytecode)]
    (ClassReader. ^InputStream is)))

(defn- ^bytes accept-and-get [^ClassReader cr ^ClassWriter cw ^ClassVisitor cv]
  (.accept cr cv 0)
  (.toByteArray cw))

(defn- modify-fields [bytecode fields]
  (let [fields-set (into #{} (map name fields))
        field->hint-mapping (reduce
                              #(assoc %1 (name %2) (:tag (meta %2)))
                              {} fields)
        ;;  todo -> extract this up
        cr (class-reader bytecode)
        cw (ClassWriter. cr ClassWriter/COMPUTE_MAXS)
        ;;
        field-call-replacer
        (fn [mv]
          (proxy [MethodVisitor] [Opcodes/ASM4 mv]
            (visitFieldInsn [opcode owner fname desc]
              (if (and (= opcode Opcodes/PUTFIELD)
                       (fields-set fname)
                       (not= (hint->desc (field->hint-mapping fname))
                             desc))
                (do
                  ;; instead of doing checkcast
                  ;; ctor could be changed
                  ;; or do this as well!
                  (.visitTypeInsn mv Opcodes/CHECKCAST (hint->internal-name (field->hint-mapping fname)))
                  (.visitFieldInsn mv opcode owner fname (hint->desc (field->hint-mapping fname))))
                (.visitFieldInsn mv opcode owner fname desc)))))

        fv (proxy [ClassVisitor] [Opcodes/ASM4 cw]
             (visitField [access fname desc sig value]
               (if (fields-set fname)
                 (let [new-desc (hint->desc (field->hint-mapping fname))]
                   (.visitField cw Opcodes/ACC_PUBLIC fname new-desc sig value))
                 (.visitField cw access fname desc sig value)))

             (visitMethod [access mname desc sig exceptions]
               (let [v (.visitMethod cw access mname desc sig exceptions)]
                 (field-call-replacer v))))]
    ;; need second call here or move changing method to another call
    (accept-and-get cr cw fv)))

(defn- add-no-args-ctor [bytecode]
  (let [cr (class-reader bytecode)
        cw (ClassWriter. cr ClassWriter/COMPUTE_MAXS)
        present? (volatile! false)
        mv (proxy [ClassVisitor] [Opcodes/ASM4 cw]
             (visitMethod [access mname desc sig exceptions]
               (when (and (= "<init>" mname)
                          (= "()V" desc))
                 (vreset! present? true))
               (.visitMethod cw access mname desc sig exceptions))
             (visitEnd []
               (when-not @present?
                 (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
                   (.visitCode mv)
                   (.visitVarInsn mv Opcodes/ALOAD 0)
                   (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
                   (.visitInsn mv Opcodes/RETURN)
                   (.visitMaxs mv 1 1)
                   (.visitEnd mv)))
               (.visitEnd cw)))]
    (accept-and-get cr cw mv)))

(defn modify-bytecode
  ([class-name]
   (let [bytecode (class->bytecode class-name)
         fields   (mapv
                    (fn [^Field f] (symbol (.getName f)))
                    (.getFields (Class/forName class-name)))]
     (modify-bytecode class-name bytecode fields)))
  ([class-name bytecode fields]
   (let [modified-bytecode
         (-> bytecode
              (add-no-args-ctor)
              (modify-fields fields))]
     (reload class-name modified-bytecode))))

;; todo -> merge with other positional factory
(defn- build-no-args-factory [cname class-name]
  (let [fn-name (symbol (str 'nil-> cname))
        docstring (str "Positional factory function for class " class-name ".")]
    `(defn ~fn-name ~docstring [] (new ~class-name))))

(defmacro as-data [cname]
  (let [ns-part (namespace-munge *ns*)
        class-name (symbol (str ns-part "." cname))]
    `(do
       ;; todo -> check if needed
       ~(modify-bytecode (name class-name))
       ~(build-no-args-factory cname class-name)
       (import ~class-name)
       ~class-name)))

(defmacro defdata
  "A variation over `deftype`.

  Defines a class with name `cname` and `fields`.
  Fields would be mutable and additional no-arg ctor
  would be added. "
  [cname fields]
  (let [fields-count (count fields)
        ns-part (namespace-munge *ns*)
        class-name (symbol (str ns-part "." cname))]
    `(do
       ~(if (pos? fields-count)
          (modify-bytecode
            (name class-name)
            (eval-deftype
              (list 'deftype* cname class-name fields :implements ['clojure.lang.IType]))
            fields)
              ; alternative: (first (next (next (macroexpand '(deftype ~name ~fields)))))))
          ; replace with plain deftype call
          (emit-deftype* cname class-name fields [] [] []))
       ~(build-positional-factory cname class-name fields)
       ~(when (pos? fields-count)
          (build-no-args-factory cname class-name))
       (import ~class-name)
       ~class-name)))