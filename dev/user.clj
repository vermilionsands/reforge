(ns user
  (:require [vermilionsands.reforge.core :as r]
            [types :as t])
  (:import [java.lang.reflect Modifier]))

(defn is-final? [c]
  (Modifier/isFinal (.getModifiers c)))

(defn list-methods [c & [s]]
  (let [xs (map str (vec (.getMethods c)))]
    (if s
      (filter #(.contains % s) xs)
      xs)))

(defn reload! []
  (require 'vermilionsands.reforge.core :reload))