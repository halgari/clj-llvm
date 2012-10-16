(ns clj-llvm.core-test
  (:use clojure.test
        clj-llvm.core))


(deftest test-func
  (let [f (native-fn '{:name ret-x
                      :op :fn
                      :args [{:name x :type :int-32}]
                      :ret-type :int-32
                      :body [{:op :ret
                              :value x}]})]
    (is (= (f 4) 4))))
