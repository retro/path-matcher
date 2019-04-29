(ns path-matcher.core-test
  (:require [clojure.test :refer :all]
            [path-matcher.core :as pm]))

(deftest matches?
  (is (pm/matches? [:foo :bar] [:foo :bar]))
  (is (pm/matches? [:foo :bar] [:*]))
  (is (pm/matches? [:foo :bar] [:foo :* :* :bar]))
  (is (pm/matches? [:foo :bar] [:* :foo :bar]))
  (is (pm/matches? [:foo :bar :baz] [:foo :bar :*]))
  (is (pm/matches? [:foo :bar :baz :qux] [:foo :* :qux]))
  (is (pm/matches? [:foo :bar :baz :qux] [:foo :* :baz :*]))
  (is (pm/matches? [:foo :bar :baz :qux] [:* :qux]))
  (is (pm/matches? [:foo :bar :baz :qux] [:* :baz :qux]))
  (is (pm/matches? [:foo :bar :baz :qux :foo1 :bar1 :baz1 :qux1] [:* :bar :* :foo1 :bar1 :* :qux1]))
  (is (not (pm/matches? [:foo :bar :baz :qux] [:foo :* :baz :qux :foo1])))
  (is (not (pm/matches? [:foo :bar :baz :qux] [:* :baz]))))

(deftest get-matching
  (is (= [1 2 3 4 5]
         (pm/get-matching
          {[:*] 5
           [:foo :bar :baz :qux :foo1 :bar1 :baz1 :qux1] 1
           [:foo :bar :* :qux1] 3
           [:foo :bar :* :foo1 :bar1 :baz1 :qux1] 2
           [:foo :* :bar1 :* :qux1] 4
           [:bar] 99
           [:test] 999
           [:foo :baz :*] 9999}
          [:foo :bar :baz :qux :foo1 :bar1 :baz1 :qux1]))))
