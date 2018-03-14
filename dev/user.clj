(ns user
  (:require [vermilionsands.reforge.core :as r]
            [types :as t])
  (:import [types TestType]
           [java.lang.reflect Modifier]))

(defn is-final? [c]
  (Modifier/isFinal (.getModifiers c)))

(defn list-methods [c & [s]]
  (let [xs (map str (vec (.getMethods c)))
        ys (if s (filter #(.contains % s) xs) xs)]
    (doseq [m ys]
      (println m))))

(defn reload! []
  (require 'user :reload))

(defn reforge-add-method []
  (binding [*compile-files* true
            *compile-path* "target/"]
    (r/reforge-class {:name 'types.TestType :method-add [["zoom" [String [String]] [:public]]]})))

(defn reforge-method []
  (binding [*compile-files* true]
           *compile-path* "target/"
    (r/reforge-class {:name 'types.TestType
                      :class-access [:public]
                      :methods-modify [['foo [Object [Object]] [:public :final]]]})))

(defn reforge-access []
  (binding [*compile-files* true
            *compile-path* "target/"]
    (r/reforge-class {:name 'types.TestType :class-access [:public]})))
