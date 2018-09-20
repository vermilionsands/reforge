(ns vermilionsands.reforge
  (:import (clojure.lang RT DynamicClassLoader)
           (clojure.asm ClassReader ClassWriter Opcodes)))

;; 1. OK defdata
;; 2. OK deftype + noarg ctor
;; 3. OK positionial factories
;; 4. all mutable public field
;; 5. change types internally

(def emit-deftype* #'clojure.core/emit-deftype*)
(def build-positional-factory #'clojure.core/build-positional-factory)

(defn- replace-dot->slash [s]
  (.replace s "." "/"))

(defn reload [class-name bytecode]
  (when *compile-files*
    (Compiler/writeClassFile (replace-dot->slash class-name) bytecode))
  (.defineClass ^DynamicClassLoader (RT/makeClassLoader) class-name bytecode nil))

(defn add-no-args-ctor [class-name]
  (when (and (Class/forName class-name)
             (nil?
               (try
                 (.getConstructor (Class/forName class-name) (into-array Class []))
                 (catch NoSuchMethodException _ nil))))
    (let [cr (ClassReader. ^String class-name)
          cw (ClassWriter. cr ClassWriter/COMPUTE_MAXS)
          mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 1 1)
      (.visitEnd mv)

      (.accept cr cw 0)
      (let [updated-bytecode (.toByteArray cw)]
        (reload class-name updated-bytecode)))))

;; todo -> catch old function
;; todo -> replace with defn
(defmacro build-no-args-factory [name class-name]
  (let [fn-name   (symbol (str 'nil-> name))
        docstring (str "Positional factory function for class " class-name ".")]
    `(defn ~fn-name ~docstring [] (new ~class-name))))

;; workaround, though this works
(defmacro as-data [name]
  (let [ns-part (namespace-munge *ns*)
        class-name (symbol (str ns-part "." name))]
    `(do
       ~(add-no-args-ctor (clojure.core/name class-name))
       (build-no-args-factory ~name ~class-name)

       (import ~class-name)
       ~class-name)))

;; todo
;; maybe replace classloader for the first deftype calls?
;; or split this into two calls - one for deftype, one for modification
;; otherwise we can;t get the bytecode to manipulate
(defmacro defdata [name fields]
  (let [fields-count (count fields)
        ns-part (namespace-munge *ns*)
        class-name (symbol (str ns-part "." name))]
    ;; todo -> force deftype to output class to disk?
    ;; or capture bytes from deftype for later reworking
    ;; (that would be tricky...)
    `(do
       ~(emit-deftype* name name fields [] [] [])
       ~(build-positional-factory name class-name fields)
       ~(when (pos? fields-count)
          ~(add-no-args-ctor (clojure.core/name class-name))
          (build-no-args-factory ~name ~class-name))
          ;(add-no-args-ctor (.getCanonicalName ~class-name)))
          ;;(add-no-args-ctor (clojure.core/name class-name)))
         ;; would have to happen in add-no-arg-ctor
         ;; (defn ~no-arg-ctor-name [] (new ~class-name)))

       (import ~class-name)
       ~class-name)))