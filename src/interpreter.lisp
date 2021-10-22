(in-package #:cl)

(defpackage #:com.liutos.liutcl.interpreter
  (:use #:cl)
  (:export #:load-source-file))

(in-package #:com.liutos.liutcl.interpreter)

(defgeneric extend (venv sym val)
  (:documentation "扩展值环境。"))
(defgeneric lookup (venv sym)
  (:documentation "在值环境中查找标识符的值。"))

(defun interpret (expr venv)
  (when (symbolp expr)
    (return-from interpret (lookup venv expr)))

  (when (atom expr)
    (return-from interpret expr))

  (let ((func (first expr)))
    (ecase func
      ;; TODO: 将加减乘除法运算都实现为函数，减少函数 interpret 的体积。
      (*
       (assert (= (length expr) 3))
       (let ((lhs (interpret (second expr) venv))
             (rhs (interpret (third expr) venv)))
         (* lhs rhs)))
      (+
       (assert (= (length expr) 3))
       (let ((lhs (interpret (second expr) venv))
             (rhs (interpret (third expr) venv)))
         (+ lhs rhs)))
      (-
       (assert (= (length expr) 3))
       (let ((lhs (interpret (second expr) venv))
             (rhs (interpret (third expr) venv)))
         (- lhs rhs)))
      (/
       (assert (= (length expr) 3))
       (let ((lhs (interpret (second expr) venv))
             (rhs (interpret (third expr) venv)))
         (/ lhs rhs)))
      (progn
        (let (last)
          (dolist (e (rest expr))
            (setf last (interpret e venv)))
          last))
      (print
       (assert (= (length expr) 2))
       (let* ((arg (second expr))
              (obj (interpret arg venv)))
         (assert (typep obj 'string))
         (format t "~A" obj))))))

(defun load-source-file (filespec)
  (check-type filespec string)
  (with-open-file (s filespec)
    (interpret (read-source-code s))))

(defun read-source-code (stream)
  (let ((*readtable* (copy-readtable nil)))
    (let ((eof (gensym))
          (exprs '()))
      (loop
        (let ((expr (read stream nil eof)))
          (when (eq expr eof)
            (return-from read-source-code (cons 'progn (nreverse exprs))))

          (push expr exprs))))))

(defun read-source-code-from-string (str)
  (check-type str string)
  (with-input-from-string (s str)
    (read-source-code s)))

(defmethod extend ((venv list) (sym symbol) val)
  (cons (cons sym val) venv))
(defmethod lookup ((venv list) (sym symbol))
  (cdr (assoc sym venv)))

(defparameter *testing-venv*
  (extend '() 'foo 233))

(defun test-interpret (source-code expected)
  (check-type source-code string)
  (let ((output (interpret (read-source-code-from-string source-code) *testing-venv*)))
    (if (equal output expected)
        (format t "测试通过：~A => ~A~%" source-code output)
        (format t "测试失败：~A => ~A != ~A~%" source-code output expected))))

(defun run-test-cases ()
  (test-interpret "123" 123)
  (test-interpret "(+ 1 (+ 2 3))" 6)
  (test-interpret "(- 1 (- 2 3))" 2)
  (test-interpret "(* 1 (* 2 3))" 6)
  (test-interpret "(/ 1 (/ 2 3))" 3/2)
  (test-interpret "foo" 233))
