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
  ((:file "env" :depends-on ("package"))
   (:file "interpreter" :depends-on ("env" "package" "reader" "stdlib"))
   (:file "package")
   (:file "reader" :depends-on ("package"))
   (:file "stdlib" :depends-on ("package"))
   (:file "typing" :depends-on ("package"))))

(defsystem #:liutcl/test
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on (#:fiveam
               #:liutcl)
  :pathname "t/"
  :components
  ((:file "interpreter" :depends-on ("package"))
   (:file "package")
   (:file "typing" :depends-on ("package")))
  :perform (test-op (o c)
                    (uiop:symbol-call :com.liutos.liutcl.interpreter :init-top-level-venv)
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :interpret :com.liutos.liutcl.interpreter.test))
                    (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :check-233-type :com.liutos.liutcl.interpreter.test))))
