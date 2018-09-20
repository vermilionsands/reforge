(ns vermilionsands.reforge.core-test
  (:require [clojure.test :refer [is deftest]]
            [vermilionsands.reforge.experimental :as r])
  (:import [java.lang.reflect Modifier]))

;(defprotocol Fizz)
;  (fizz [this n]))

;(deftype FinalType [x])

;(deftype NonFinalType [x]
;  Fizz
;  (fizz [_ n] (if (zero? (mod n 3)) "fizz" n)))

;(r/modify-type FinalType #{:public})

;(r/modify-type NonFinalType #{:public :nonfinal}
;  (fizz :modify #{:public :synchronized :final} [_]))

;(defn final-class? [c]
;  (Modifier/isFinal (.getModifiers c)))

;(deftest make-type-non-final-test
;  (is (true?  (final-class? FinalType)))
;  (is (false? (final-class? NonFinalType))))

;(deftest modify-method-access-test
;  (.getDeclaredMethod NonFinalType)
;  (is false))


;(r/modify-type 'SampleType :- [:public :nonfinal]
;  (foo       :- [:mod :public :final :synchronized])
;  (bar       :- [:add! String] [_ x :- String y :- String])
;  (staticBar :- [:add! String] [x :- String y] custom-impl))

;(defn reforge-method []
;  (binding [*compile-files* true
;            *compile-path* "target/"]
;    (r/reforge-class {:name           'types.TestType
;                      :class-access   [:public]
;                      :methods-modify [['foo [Object [Object]] [:public :final]]]]))

;(defn reforge-add-method [])
;  (binding [*compile-files* true]))
;            *compile-path* "target/"]))
;    (r/reforge-class {:name 'types.TestType :methods-add [["zoom" [String [String]] [:public]]]})))