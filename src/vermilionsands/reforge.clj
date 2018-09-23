(ns vermilionsands.reforge
  (:require [clojure.java.io :as io])
  (:import [clojure.asm ClassReader ClassWriter Opcodes ClassVisitor]
           [clojure.lang RT DynamicClassLoader Compiler Compiler$C Compiler$ObjExpr]
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

(defn- make-fields-mutable [bytecode fields]
  (let [fields-set (into #{} (map name fields))
        cr (with-open [is (ByteArrayInputStream. bytecode)]
             (ClassReader. ^InputStream is))
        cw (ClassWriter. cr ClassWriter/COMPUTE_MAXS)
        fv (proxy [ClassVisitor] [Opcodes/ASM4 cw]
             (visitField [access fname desc sig value]
               (if (fields-set fname)
                 (.visitField cw Opcodes/ACC_PUBLIC fname desc sig value)
                 (.visitField cw access fname desc sig value)))
             (visitEnd []
               (.visitEnd cw)))]
    (.accept cr fv 0)
    (.toByteArray cw)))

(defn- add-no-args-ctor [bytecode]
  (let [cr (with-open [is (ByteArrayInputStream. bytecode)]
             (ClassReader. ^InputStream is))
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
    (.accept cr mv 0)
    (.toByteArray cw)))

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
              (make-fields-mutable fields))]
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
       ~(modify-bytecode (name class-name))
       ~(build-no-args-factory cname class-name)
       (import ~class-name)
       ~class-name)))

(defmacro defdata [cname fields]
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