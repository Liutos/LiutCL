(in-package #:com.liutos.liutcl.interpreter)

(defparameter *233-type-integer*
  'integer
  "233-lisp中的整数类型。")

(defparameter *testing-tenv*
  (let ((tenv '()))
    (setf tenv (extend tenv 'foo *233-type-integer*))
    tenv)
  "单元测试专用的类型环境。")

(defun check-233-type (expr tenv)
  (when (integerp expr)
    (return-from check-233-type *233-type-integer*))

  (when (symbolp expr)
    (let ((type (lookup tenv expr)))
      (unless type
        (error "未定义的变量~A" expr))

      (return-from check-233-type type)))

  (assert (consp expr))
  (let ((func (first expr)))
    (when (eq func 'progn)
      (let (last)
        (dolist (e (rest expr))
          (setf last (check-233-type e tenv)))
        (return-from check-233-type last)))

    (when (member func '(+ - * /))
      (destructuring-bind (lhs rhs)
          (rest expr)
        (let ((lhs-type (check-233-type lhs tenv))
              (rhs-type (check-233-type rhs tenv)))
          (unless (equal lhs-type *233-type-integer*)
            (error "要求类型~A，但表达式~A的类型为~A" *233-type-integer* lhs lhs-type))

          (unless (equal rhs-type *233-type-integer*)
            (error "要求类型~A，但表达式~A的类型为~A" *233-type-integer* rhs rhs-type))

          (return-from check-233-type *233-type-integer*)))))
  (error "未知类型的表达式：~A" expr))
