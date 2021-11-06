(in-package #:com.liutos.liutcl.interpreter.test)

(defun read-and-check-type (source-code)
  (check-type source-code string)
  (com.liutos.liutcl.interpreter::check-233-type
   (com.liutos.liutcl.interpreter::read-source-code-from-string source-code)
   com.liutos.liutcl.interpreter::*testing-tenv*))

(test check-233-type
  "测试233-lisp的静态类型检查机制。"
  (is (equal (read-and-check-type "233") com.liutos.liutcl.interpreter::*233-type-integer*))
  (is (equal (read-and-check-type "(+ 1 (+ 2 3))") com.liutos.liutcl.interpreter::*233-type-integer*))
  (is (equal (read-and-check-type "(- 1 (- 2 3))") com.liutos.liutcl.interpreter::*233-type-integer*))
  (is (equal (read-and-check-type "(* 1 (* 2 3))") com.liutos.liutcl.interpreter::*233-type-integer*))
  (is (equal (read-and-check-type "(/ 1 (/ 2 3))") com.liutos.liutcl.interpreter::*233-type-integer*))
  (is (equal (read-and-check-type "foo") com.liutos.liutcl.interpreter::*233-type-integer*))
  (is (equal (read-and-check-type "(let a = 1 (+ a 1))") com.liutos.liutcl.interpreter::*233-type-integer*)))
