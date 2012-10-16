(ns clj-llvm.llvmc
  (:import (com.sun.jna Native Pointer)))

(def ^:dynamic *lib* 'LLVM-3.1)

(defn get-function [s]
  `(com.sun.jna.Function/getFunction ~(name *lib*) ~(name s)))

(defn debug [s]
  (println s)
  s)

(defmacro defnative
  [return-type function-symbol]
  `(let [func# ~(get-function function-symbol)]
     (defn ~(symbol (name function-symbol))
       [& args#]
       (.invoke func# ~return-type (to-array args#)))))

(def LLVMCCallConv 0)
(def LLVMFastCallConv 8)
(def LLVMColdCallConv 9)
(def LLVMX86StdcallCallConv 64)
(def LLVMX86FastcallCallConv 65)
(defnative Integer LLVMSetFunctionCallConv)

(defnative Pointer LLVMAppendBasicBlock)
(defnative Pointer LLVMCreateBuilder)

(defnative Pointer LLVMGetParam)

(defnative Integer LLVMLinkInJIT)
'(defnative Integer LLVMInitializeNativeTarget)

(defnative Pointer LLVMModuleCreateWithName)

(defnative Pointer LLVMInt32Type)
(defnative Pointer LLVMFunctionType)

(defnative Pointer LLVMAddFunction)

(defnative Integer LLVMPositionBuilderAtEnd)

(defnative Boolean LLVMVerifyModule)

(def LLVMAbortProcessAction 0)
(def LLVMPrintMessageAction 1)
(def LLVMReturnStatusAction 2)

(defnative Pointer LLVMCreateModuleProviderForExistingModule)

(defnative Integer LLVMDisposeMessage)
(defnative Integer LLVMCreateJITCompiler)
(defnative Integer LLVMCreateInterpreterForModule)
(defnative Pointer LLVMCreatePassManager)
(defnative Pointer LLVMGetExecutionEngineTargetData)
(defnative Integer LLVMAddTargetData)
(defnative Integer LLVMRunPassManager)
(defnative Integer LLVMDumpModule)
(defnative Integer LLVMDisposePassManager)
(defnative Integer LLVMDisposeExecutionEngine)
(defnative Integer LLVMBuildRet)

(defnative Integer LLVMLinkInJIT)
(defnative Integer LLVMLinkInInterpreter)
(defnative Integer LLVMInitializeX86Target)
(defnative Integer LLVMInitializeX86TargetInfo)
(defnative Integer LLVMInitializeX86TargetMC)
(defnative Pointer LLVMRunFunction)
(defnative Boolean LLVMFindFunction)
(defnative Pointer LLVMCreateGenericValueOfInt)
(defnative Integer LLVMGenericValueToInt)



(defn to-pointers [& args]
  (let [arr (make-array Pointer (count args))]
    (loop [a args
           c 0]
      (if a
        (do (aset arr c (first a))
            (recur (next a) (inc c)))
        arr))))
