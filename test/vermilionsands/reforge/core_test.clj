(ns vermilionsands.reforge.core-test
  (:require [clojure.test :refer [is deftest]]
            [vermilionsands.reforge.core :as r])
  (:import [java.lang.reflect Modifier]))

(deftype FinalType [x])

(deftype NonFinalType [x])

(r/modify-type
  vermilionsands.reforge.core_test.FinalType :- [:public])

(r/modify-type
  vermilionsands.reforge.core_test.NonFinalType :- [:public :nonfinal])

(defn final-class? [c]
  (Modifier/isFinal (.getModifiers c)))

(deftest make-type-non-final
  (is (true?  (final-class? vermilionsands.reforge.core_test.FinalType)))
  (is (false? (final-class? vermilionsands.reforge.core_test.NonFinalType))))

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