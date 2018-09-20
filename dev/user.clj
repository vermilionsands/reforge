(ns user
  (:require [vermilionsands.reforge.experimental :as r]
            [types :as t])
  (:import [types TestType]
           [java.lang.reflect Modifier]))

(deftype CustomType [x])

(defn is-final? [c]
  (Modifier/isFinal (.getModifiers c)))

(defn list-methods [c & [s]]
  (let [xs (map str (vec (.getMethods c)))
        ys (if s (filter #(.contains % s) xs) xs)]
    (doseq [m ys]
      (println m))))

(defn reload! []
  (require 'user :reload-all))

;(defn extend-access []
;  (binding [*compile-files* true
;            *compile-path* "target/"
;    (r/modify-type types.TestType :- [:public])))

;(defn extend-method []
;  (binding [*compile-files* true
;            *compile-path* "target/"
;    (r/modify-type types.TestType :- [:public]
;      (^Object foo :- [:modify :public :final] [_ :- Object])))

;(defn extend-add-method []
;  (binding [*compile-files* true
;            *compile-path* "target/"
;    (r/modify-type types.TestType
;      (fizz :- [:add :public :final] [x :- Integer])
;      (buzz :- [:add :public] [x] custom-buzz)))
      ;(buzz :- [:add] [x] custom-buzz))))