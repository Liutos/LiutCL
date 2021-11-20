(in-package #:cl-user)

(defpackage #:com.liutos.liutcl.interpreter.test
  (:use #:cl
        #:fiveam)
  (:import-from #:com.liutos.liutcl.interpreter
                #:<core-num>
                #:<core-plus>
                #:<value-num>
                #:interpret
                #:value-equal-p))
