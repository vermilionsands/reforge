(ns vermilionsands.reforge.reforge-test
  (:require [clojure.test :refer [deftest is testing]]
            [vermilionsands.reforge :as reforge])
  ;; -> this should not be required
  (:gen-class))

;(reforge/defdata Point [x y])
(deftype Point [x y])
(reforge/as-data Point)

(deftest defdata-basic-test
  (let [x (->Point 1 2)]
    (is (some? x))
    (is (= 1 (.x x)))
    (is (= 2 (.y x)))))

(deftest no-arg-ctor-test
  (let [x (Point.)]
    (is (some? x))
    (is (= nil (.x x)))
    (is (= nil (.y x)))))

(deftest instances-type-match-test
  (let [x (nil->Point)
        y (->Point 1 2)]
     (is (= (.getClass x)
            (.getClass y)))))

(deftest factory-test
  (testing "no-arg factory fn"
    (let [x (nil->Point)]
      (is (some? x))
      (is (= nil (.x x)))
      (is (= nil (.y x))))))

;(deftest fields-are-mutable-test
;  (let [x (->TestPoint 1 2)]
;    (set! (.x x) 2)
;    (set! (.y x) 2)
;    (is (= 2 (.x x)))
;    (is (= 3 (.y x)))))

