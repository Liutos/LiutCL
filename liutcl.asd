(in-package #:cl-user)

(defpackage #:system-liutcl
  (:use #:cl #:asdf))

(in-package #:system-liutcl)

(defsystem #:liutcl
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on ()
  :pathname "src/"
  :components
  ((:file "interpreter")))

(defsystem #:liutcl/test
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on (#:fiveam
               #:liutcl)
  :pathname "t/"
  :components
  ((:file "interpreter" :depends-on ("package"))
   (:file "package"))
  :perform (test-op (o c)
                    (let ((names '(:interpret
                                   :load-source-file
                                   :arithmetic
                                   :macro
                                   :progn
                                   :assignment
                                   :mutual-recursion)))
                      (dolist (name names)
                        (uiop:symbol-call :fiveam :run! (uiop:find-symbol* name :com.liutos.liutcl.interpreter.test))))))
