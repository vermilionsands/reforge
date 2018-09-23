(ns vermilionsands.reforge-aot-test
  "A mirror copy of `vermilionsands.reforge-test,
   but with AOT."
  (:require [clojure.test :refer [deftest is testing]]
            [vermilionsands.reforge :as reforge]))

;; this requires AOT compilation to generate Point.class file
(deftype Point [x y])
(reforge/as-data Point)
(reforge/defdata AnotherPoint [x y])

(deftest defdata-basic-test
  (testing "as-data basic test"
    (let [x (->Point 1 2)]
      (is (some? x))
      (is (= 1 (.x x)))
      (is (= 2 (.y x)))))
  (testing "defdata basic test"
    (let [x (->AnotherPoint 1 2)]
      (is (some? x))
      (is (= 1 (.x x)))
      (is (= 2 (.y x))))))

(deftest no-arg-ctor-test
  (testing "as-data no-arg ctor test"
    (let [x (Point.)]
      (is (some? x))
      (is (= nil (.x x)))
      (is (= nil (.y x)))))
  (testing "defdata no-arg ctor test"
    (let [x (AnotherPoint.)]
      (is (some? x))
      (is (= nil (.x x)))
      (is (= nil (.y x))))))

(deftest instances-type-match-test
  (testing "as-data classes match test"
    (let [x (nil->Point)
          y (->Point 1 2)]
      (is (= (.getClass x)
             (.getClass y)))))
  (testing "def-data classes match test"
    (let [x (nil->AnotherPoint)
          y (->AnotherPoint 1 2)]
      (is (= (.getClass x)
             (.getClass y))))))

(deftest factory-test
  (testing "as-data no-arg factory fn"
    (let [x (nil->Point)]
      (is (some? x))
      (is (= nil (.x x)))
      (is (= nil (.y x)))))
  (testing "defdata no-arg factory fn"
    (let [x (nil->AnotherPoint)]
      (is (some? x))
      (is (= nil (.x x)))
      (is (= nil (.y x))))))

(deftest fields-are-mutable-test
  (let [x (->Point 1 2)]
    (set! (.x x) 2)
    (set! (.y x) 3)
    (is (= 2 (.x x)))
    (is (= 3 (.y x)))))

