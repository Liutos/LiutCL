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
                #:make-empty-env
                #:make-empty-store
                #:put-store
                #:value-equal-p
                #:wrong-type))
