(ns clj-llvm.core
  (:use [clj-llvm.llvmc])
  (:import (com.sun.jna Native Pointer Memory)))

(LLVMLinkInJIT)
(LLVMLinkInInterpreter)
(LLVMInitializeX86TargetInfo)
(LLVMInitializeX86Target)
(LLVMInitializeX86TargetMC)

(def type-maps
  {:int-32 (LLVMInt32Type)})

(defn new-pointer []
  (let [p (Memory. Pointer/SIZE)]
   (.clear p)
    p))

(defn & [ptr]
  (.getPointer ptr 0))

(defn get-type [t]
  (get type-maps t))

(defn make-args [args]
  (apply to-pointers (map (comp get-type :type) args)))

(defmulti -emit (fn [ast env] (:op ast)))

(defmethod -emit :ret
  [{:keys [value]} {:keys [builder locals]}]
  (cond
   (symbol? value) (LLVMBuildRet builder (get locals value))
      ))

(defmethod -emit :fn
  [{:keys [name args body ret-type]} env]
  (let [nargs (make-args args)
        f-type (LLVMFunctionType (get-type ret-type)
                                 nargs
                                 (count args)
                                 0)
        fnc (LLVMAddFunction (:module env)
                               (clojure.core/name name)
                               f-type)
        _ (LLVMSetFunctionCallConv fnc LLVMCCallConv)
        mm (loop [args args
                  mp {}
                  idx 0]
             (if args
               (recur (next args)
                      (assoc mp
                        (:name (first args))
                        (LLVMGetParam fnc idx))
                      (inc idx))
               mp))
        nenv (update-in mm [:locals] merge mm)
        blk (LLVMAppendBasicBlock fnc "")
        nenv (assoc nenv :block blk)
        nenv (assoc nenv :builder (LLVMCreateBuilder))]
    (LLVMPositionBuilderAtEnd (:builder nenv) blk)
    (doall (for [x body]
             (-emit x nenv)))
    fnc))



(defn native-fn [ast]
  (let [error (new-pointer)
        module (LLVMModuleCreateWithName (name (gensym "module")))
        fnc (-emit ast {:module module})]
    (LLVMVerifyModule module LLVMAbortProcessAction (& error))
    (LLVMDisposeMessage error)
    (let [provider (LLVMCreateModuleProviderForExistingModule module)
          _ (assert provider)
          error (new-pointer)
          engine (new-pointer)
          ]
      (if (not= (LLVMCreateJITCompiler
                 engine
                 provider
                 2
                 error)
                0)
        (assert false (.getString error 0 false))
        (let [_ (assert (& engine))
              pass (LLVMCreatePassManager)
                                        ;_
              ;data (LLVMGetExecutionEngineTargetData engine)
              ;_ (assert data)
              ;_
              ;
              #_(LLVMAddTargetData data
              ;                   pass
              ;
                                 )
              _ (LLVMRunPassManager pass module)
              _ (LLVMDumpModule module)]
          #_(LLVMDisposePassManager pass)
          #_(LLVMDisposeExecutionEngine engine)
          (reify
            clojure.lang.IFn
            (invoke [this x]
              (let [p (to-pointers (LLVMCreateGenericValueOfInt (get-type :int-32)
                                                                x
                                                                1))

                    ex_res (LLVMRunFunction (& engine) fnc 1 p)]
                (LLVMGenericValueToInt ex_res 0)))))))))
