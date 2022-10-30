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
  ((:file "core-syntax"
          :depends-on ("package"))
   (:file "interpreter"
          :depends-on ("core-syntax" "package" "value"))
   (:file "package")
   (:file "value"
          :depends-on ("package"))))

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
                    (let ((names '(:dynamic-scope
                                   :interpret
                                   :let-form
                                   :load-source-file
                                   :arithmetic
                                   :macro
                                   :palindromic
                                   :progn
                                   :assignment
                                   :mutual-recursion
                                   :start-repl)))
                      (dolist (name names)
                        (uiop:symbol-call :fiveam :run! (uiop:find-symbol* name :com.liutos.liutcl.interpreter.test))))))
