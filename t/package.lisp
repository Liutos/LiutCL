(in-package #:cl-user)

(defpackage #:com.liutos.liutcl.interpreter.test
  (:use #:cl
        #:fiveam)
  (:import-from #:com.liutos.liutcl.interpreter
                #:<binding>
                #:<core-app>
                #:<core-id>
                #:<core-lambda>
                #:<core-num>
                #:<core-plus>
                #:<value-fun>
                #:<value-num>
                #:extend-env
                #:interpret
                #:make-empty-env
                #:value-equal-p
                #:wrong-type))
