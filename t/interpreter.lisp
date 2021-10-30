(in-package #:cl-user)

(defpackage #:com.liutos.liutcl.interpreter.test
  (:use #:cl
        #:com.liutos.liutcl.interpreter
        #:fiveam))

(in-package #:com.liutos.liutcl.interpreter.test)

(defun read-and-interpret (source-code)
  (com.liutos.liutcl.interpreter::interpret
   (com.liutos.liutcl.interpreter::read-source-code-from-string source-code)
   com.liutos.liutcl.interpreter::*testing-venv*))

(defun hash-table-equal (ht1 ht2)
  "比较两个哈希表是否相等。"
  (check-type ht1 hash-table)
  (check-type ht2 hash-table)
  (unless (= (hash-table-count ht1)
             (hash-table-count ht2))
    (return-from hash-table-equal nil))

  (maphash #'(lambda (k v)
               (unless (equal v (gethash k ht2))
                 (return-from hash-table-equal nil)))
           ht1)
  t)

(test interpret
  "Test the COM.LIUTOS.LIUTCL.INTERPRETER::INTERPRET."
  (is (equal (read-and-interpret "123") 123))
  (is (equal (read-and-interpret "(+ 1 (+ 2 3))") 6))
  (is (equal (read-and-interpret "(- 1 (- 2 3))") 2))
  (is (equal (read-and-interpret "(* 1 (* 2 3))") 6))
  (is (equal (read-and-interpret "(/ 1 (/ 2 3))") 3/2))
  (is (equal (read-and-interpret "foo") 233))
  (is (equal (read-and-interpret "(let a = 1 b = 2 (+ a b))") 3))
  (is (equal (read-and-interpret "(let a = 1 (+ a 1) b = 2 (+ a b) c = 3 (+ a (+ b c)))") 6))
  (is (equal (read-and-interpret "(defun 1+ (x) (+ x 1)) (1+ 2)") 3))
  (is (equal (read-and-interpret "(defun early (x) (return (+ x 2)) (+ x 1)) (early 3)") 5))
  (is (equal (read-and-interpret "(if t 1 2)") 1))
  (is (equal (read-and-interpret "(if nil 1 2)") 2))
  (is (equal (read-and-interpret "(let a = 1 (setf a (+ a 1)) a)") 2))
  (is (equal (read-and-interpret "(let a = 0 sum = 0 (for (< a 6) (setf sum (+ sum a)) (setf a (+ a 1))) sum)") 15))
  (is (equal (read-and-interpret "(let a = 0 sum = 0 (for (< a 6) (if (= a 2) (break) (setf sum (+ sum a))) (setf a (+ a 1))) sum)") 1))
  (is (equal (read-and-interpret "(let i = 0 sum = 0 (for (< i 6) (if (= i 3) (progn (setf i (+ i 1)) (continue)) (print \"Hi\")) (setf sum (+ sum i)) (setf i (+ i 1))) sum)") 12))
  (is (equal (read-and-interpret "\"Hello, world!\\n\"") (format nil "Hello, world!~%")))
  (is (hash-table-equal (read-and-interpret "(dict \"a\" 1)")
                        (let ((ht (make-hash-table :test #'equal)))
                          (setf (gethash "a" ht) 1)
                          ht))))

