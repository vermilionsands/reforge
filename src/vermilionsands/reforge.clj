(ns vermilionsands.reforge
  (:import (clojure.lang RT DynamicClassLoader)
           (clojure.asm ClassReader ClassWriter Opcodes)))

;; 1. OK defdata
;; 2. OK deftype + noarg ctor
;; 3. all mutable public field
;; 4. change types internally
;; 5. positionial factories

(def emit-deftype* #'clojure.core/emit-deftype*)

(def build-positional-factory #'clojure.core/build-positional-factory)

(defn- replace-dot->slash [s]
  (.replace s "." "/"))

(defn reload [class-name bytecode]
  (when *compile-files*
    (Compiler/writeClassFile (replace-dot->slash class-name) bytecode))
  (.defineClass ^DynamicClassLoader (RT/makeClassLoader) class-name bytecode nil))

(defn add-no-args-ctor [class-name]
  (println "Noarg ctor")
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

(defmacro build-no-args-factory [name class-name]
  (println "Def no args ctor")
  (let [fn-name (symbol (str '-> name "0"))
        docstring (str "Positional factory function for class " class-name ".")]
    (println "Ctor name: " fn-name)
    `(defn ~fn-name ~docstring [] (new ~class-name))))

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
       (when (pos? ~fields-count)
         ;; would get called two times...
         (add-no-args-ctor (.getCanonicalName ~class-name)))
       (import ~class-name)
       ~class-name)))