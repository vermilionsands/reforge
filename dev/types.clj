(ns types)

(definterface TestInterface
  (foo [x]))

(clojure.core/deftype TestType [x y]
  TestInterface
  (foo [_ z] z))

(gen-class
  :name types.TestGenclass
  :prefix g-
  :methods [[foo [] void]
            [bar [String] void]])