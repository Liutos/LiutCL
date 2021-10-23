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
    (case func
      (let (interpret-let expr venv))
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
         (format t "~A" obj)))
      (t
       (let* ((args (rest expr))
              (vals (mapcar #'(lambda (arg)
                                (interpret arg venv))
                            args))
              (func (interpret func venv)))
         (unless (functionp func)
           (error "仅支持调用 CL 的原生函数：~A" func))

         (apply func vals))))))

(defun interpret-let (expr venv)
  (check-type expr cons)
  (assert (eq (first expr) 'let))
  (multiple-value-bind (bindings body)
      (transform-let expr)
    (let ((extended venv))
      (dolist (binding bindings)
        (destructuring-bind (var . val)
            binding
          (setf extended (extend extended var (interpret val venv)))))
      (interpret body extended))))

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
  (let ((bindings (list (list '* #'*)
                        (list '+ #'+)
                        (list '- #'-)
                        (list '/ #'/)))
        (venv '()))
    (dolist (binding bindings)
      (destructuring-bind (name func)
          binding
        (setf venv (extend venv name func))))
    venv))

(defun test-interpret (source-code expected)
  (check-type source-code string)
  (let ((output (interpret (read-source-code-from-string source-code) *testing-venv*)))
    (if (equal output expected)
        (format t "测试通过：~A => ~A~%" source-code output)
        (format t "测试失败：~A => ~A != ~A~%" source-code output expected))))

(defun transform-let (expr)
  "将一个LET语法分解为一系列的绑定和一个PROGN语法。"
  (check-type expr cons)
  ;; TODO: 这里用一个类似while循环的语法来代替递归应当是更可读的。
  (labels ((aux (bindings forms stack)
             (unless forms
               (return-from aux
                 (values bindings `(progn ,@(nreverse stack)))))

             (let ((next (first forms)))
               (cond ((eq next '=)
                      (assert (> (length stack) 0))
                      (let ((var (pop stack))
                            (val (second forms)))
                        (if (> (length stack) 0)
                            (return-from aux
                              (values bindings
                                      `(progn ,@(nreverse stack)
                                              (let ,var = ,val
                                                ,@(rest (rest forms))))))
                            (aux (cons (cons var val) bindings)
                                 (rest (rest forms))
                                 stack))))
                     (t
                      (aux bindings (rest forms) (push next stack)))))))
    (aux '() (rest expr) '())))

(defun run-test-cases ()
  (test-interpret "123" 123)
  (test-interpret "(+ 1 (+ 2 3))" 6)
  (test-interpret "(- 1 (- 2 3))" 2)
  (test-interpret "(* 1 (* 2 3))" 6)
  (test-interpret "(/ 1 (/ 2 3))" 3/2)
  (test-interpret "foo" 233)
  (test-interpret "(let a = 1 b = 2 (+ a b))" 3)
  (test-interpret "(let a = 1 (+ a 1) b = 2 (+ a b) c = 3 (+ a (+ b c)))" 6))
