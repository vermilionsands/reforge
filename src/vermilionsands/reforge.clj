(ns vermilionsands.reforge
  (:require [clojure.java.io :as io])
  (:import [clojure.asm ClassReader ClassWriter Opcodes]
           [clojure.lang RT DynamicClassLoader Compiler Compiler$C Compiler$ObjExpr]
           [java.io ByteArrayInputStream InputStream ByteArrayOutputStream]))

(defn ^bytes eval-deftype
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

(defn class-valid? [class-name]
  (and
    (Class/forName class-name)
    (nil?
      (try
        (.getConstructor (Class/forName class-name) (into-array Class []))
        (catch NoSuchMethodException _ nil)))))

(defn ^bytes class->byte-array [class-name]
  (with-open [is (ClassLoader/getSystemResourceAsStream
                   (str (clojure.string/replace class-name #"\." "/") ".class"))
              os (ByteArrayOutputStream.)]
    (io/copy is os)
    (.toByteArray os)))

(defn- add-no-args-ctor
  ([class-name]
   (when (class-valid? class-name)
     (add-no-args-ctor class-name (class->byte-array class-name))))
  ([class-name bytecode]
   (let [cr (with-open [is (ByteArrayInputStream. bytecode)]
              (ClassReader. ^InputStream is))
         cw (ClassWriter. cr ClassWriter/COMPUTE_MAXS)
         mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
     (.visitCode mv)
     (.visitVarInsn mv Opcodes/ALOAD 0)
     (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
     (.visitInsn mv Opcodes/RETURN)
     (.visitMaxs mv 1 1)
     (.visitEnd mv)

     (.accept cr cw 0)

     ;; todo -> return bytecode, reload somewhere elese
     (let [updated-bytecode (.toByteArray cw)]
       (reload class-name updated-bytecode)))))

;; todo -> merge with other positional factory
(defn build-no-args-factory [cname class-name]
  (let [fn-name (symbol (str 'nil-> cname))
        docstring (str "Positional factory function for class " class-name ".")]
    `(defn ~fn-name ~docstring [] (new ~class-name))))

(defmacro as-data [cname]
  (let [ns-part (namespace-munge *ns*)
        class-name (symbol (str ns-part "." cname))]
    `(do
       ~(add-no-args-ctor (name class-name))
       ~(build-no-args-factory cname class-name)
       (import ~class-name)
       ~class-name)))

(defmacro defdata [cname fields]
  (let [fields-count (count fields)
        ns-part (namespace-munge *ns*)
        class-name (symbol (str ns-part "." cname))]
    `(do
       ~(if (pos? fields-count)
          (add-no-args-ctor
            (name class-name)
            (eval-deftype
              (list 'deftype* cname class-name fields :implements ['clojure.lang.IType])))
              ; alternative: (first (next (next (macroexpand '(deftype ~name ~fields)))))))
          ; replace with plain deftype call
          (emit-deftype* cname class-name fields [] [] []))
       ~(build-positional-factory cname class-name fields)
       ~(when (pos? fields-count)
          (build-no-args-factory cname class-name))
       (import ~class-name)
       ~class-name)))