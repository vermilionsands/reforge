(ns vermilionsands.reforge-test
  (:require [clojure.test :refer [deftest is testing]]
            [vermilionsands.reforge :as reforge]))

(reforge/defdata Point [x y])

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

(deftest fields-are-mutable-test
  (let [x (nil->Point)]
    (set! (.x x) 1)
    (set! (.y x) 2)
    (is (= 1 (.x x)))
    (is (= 2 (.y x)))))