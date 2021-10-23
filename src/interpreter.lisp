(in-package #:cl)

(defpackage #:com.liutos.liutcl.interpreter
  (:use #:cl)
  (:export #:load-source-file))

(in-package #:com.liutos.liutcl.interpreter)

(defgeneric extend (venv sym val)
  (:documentation "扩展值环境。"))
(defgeneric lookup (venv sym)
  (:documentation "在值环境中查找标识符的值。"))

;;; 定义233-lisp中的函数对象需要满足的接口。
(defgeneric get-body (func)
  (:documentation "获取函数体的表达式列表。"))
(defgeneric get-parameters (func)
  (:documentation "获取函数的参数列表。"))

(defclass <func> ()
  ((body
    :documentation "函数体的表达式列表，它们被包裹在一个隐式的LET语法中。"
    :initarg :body)
   (params
    :documentation "参数列表。"
    :initarg :params)))

(defun make-func (body params)
  (check-type body list)
  (check-type params list)
  (make-instance '<func> :body body :params params))

(defmethod get-body ((func <func>))
  (slot-value func 'body))
(defmethod get-parameters ((func <func>))
  (slot-value func 'params))

(defun interpret (expr venv)
  (when (symbolp expr)
    (return-from interpret (lookup venv expr)))

  (when (atom expr)
    (return-from interpret expr))

  (let ((func (first expr)))
    (case func
      (defun (interpret-defun expr venv))
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
         (when (functionp func)
           (return-from interpret
             (apply func vals)))

         (let* ((body (get-body func))
                (params (get-parameters func))
                (new-venv
                  (reduce #'(lambda (acc e)
                              (destructuring-bind (var . val)
                                  e
                                (extend acc var val)))
                          (mapcar #'(lambda (param expr)
                                      (cons param
                                            (interpret expr venv)))
                                  params (rest expr))
                          :initial-value venv)))
           (interpret `(let ,@body) new-venv)))))))

(defparameter *top-level-venv*
  '()
  "全局顶层值环境。")

(defun interpret-defun (expr venv)
  (declare (ignorable venv))            ; TODO: venv应当是影响这个函数是否为一个闭包的关键，之后再处理。
  (check-type expr cons)
  (assert (eq (first expr) 'defun))
  (let* ((body (rest (rest (rest expr))))
         (name (second expr))
         (params (third expr))
         (func (make-func body params)))
    (push (cons name func) *top-level-venv*)
    func))

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
  (or (cdr (assoc sym venv))
      (cdr (assoc sym *top-level-venv*))))

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
  (test-interpret "(let a = 1 (+ a 1) b = 2 (+ a b) c = 3 (+ a (+ b c)))" 6)
  (test-interpret "(defun 1+ (x) (+ x 1)) (1+ 2)" 3))
