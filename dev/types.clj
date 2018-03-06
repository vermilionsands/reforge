(ns types)

(definterface TestInterface
  (foo [x]))

(clojure.core/deftype TestType [x y]
  TestInterface
  (foo [_ z] z))
