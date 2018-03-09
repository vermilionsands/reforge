(ns user
  (:require [vermilionsands.reforge.core :as r]
            [types :as t])
  (:import [types TestType]
           [java.lang.reflect Modifier]))

(defn is-final? [c]
  (Modifier/isFinal (.getModifiers c)))

(defn list-methods [c & [s]]
  (let [xs (map str (vec (.getMethods c)))]
    (if s
      (filter #(.contains % s) xs)
      xs)))

(defn reload! []
  (require 'vermilionsands.reforge.core :reload))

(defn reforge-method []
  (binding [*compile-files* true
            *compile-path* "target/"]
    (r/reforge 'types.TestType :method [["zoom" [String [String]] [:public]]])))

(defn reforge-access []
  (binding [*compile-files* true
            *compile-path* "target/"]
    (r/reforge 'types.TestType :class-access [:public])))
