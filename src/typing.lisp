(in-package #:com.liutos.liutcl.interpreter)

(defparameter *233-type-integer*
  'integer
  "233-lisp中的整数类型。")

(defun check-233-type (expr)
  (when (integerp expr)
    (return-from check-233-type *233-type-integer*))

  (assert (consp expr))
  (let ((func (first expr)))
    (when (eq func 'progn)
      (let (last)
        (dolist (e (rest expr))
          (setf last (check-233-type e)))
        (return-from check-233-type last)))

    (when (member func '(+ - * /))
      (destructuring-bind (lhs rhs)
          (rest expr)
        (let ((lhs-type (check-233-type lhs))
              (rhs-type (check-233-type rhs)))
          (unless (equal lhs-type *233-type-integer*)
            (error "要求类型~A，但表达式~A的类型为~A" *233-type-integer* lhs lhs-type))

          (unless (equal rhs-type *233-type-integer*)
            (error "要求类型~A，但表达式~A的类型为~A" *233-type-integer* rhs rhs-type))

          (return-from check-233-type *233-type-integer*)))))
  (error "未知类型的表达式：~A" expr))
