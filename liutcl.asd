(in-package #:cl-user)

(defpackage #:system-liutcl
  (:use #:cl #:asdf))

(in-package #:system-liutcl)

(defsystem #:liutcl
  :author "Liutos <mat.liutos@gmail.com>"
  :version "0.1.0"
  :depends-on ()
  :components
  ((:module "src"
    :components
    ((:file "interpreter")))))
