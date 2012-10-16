(ns clj-llvm.llvmc-test
  (:use clojure.test
        clj-llvm.llvmc))


(deftest test-get-function
  (is (get-function 'c/printf)))

(binding [*lib* 'c]
  (defnative Integer c/atoi))

(deftest test-defnative
  (is (= (atoi "42") 42)))

(deftest test-llvm-startup
  (is (LLVMLinkInJIT)))

(deftest test-llvm-module
  (is (LLVMModuleCreateWithName "42_mod")))

(deftest test-add-function
  (let [args (to-pointers (LLVMInt32Type))]
    (is (LLVMAddFunction (LLVMModuleCreateWithName "42_mod")
                         "42_func"
                         (LLVMFunctionType (LLVMInt32Type)
                                           args
                                           1
                                           0)))))
