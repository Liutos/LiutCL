(in-package #:com.liutos.liutcl.interpreter)

(defparameter *233-type-integer*
  'integer
  "233-lisp中的整数类型。")

(defparameter *testing-tenv*
  (let ((tenv '()))
    (setf tenv (extend tenv 'foo *233-type-integer*))
    tenv)
  "单元测试专用的类型环境。")

(defun is-type-var-p (type)
  "检查类型是否为一个类型变量。"
  (and (symbolp type)
       (string= (symbol-name type) "?" :end1 1)))

(defun check-is-expr-type-p (expr type)
  "检查表达式EXPR是否为类型TYPE。

第一个返回值表示是否通过了检查，第二个返回值表示其中未确定的类型变量的值。"
  (cond ((and (integerp expr)
              (eq type *233-type-integer*))
         t)
        ((and (integerp expr) (is-type-var-p type))
         (values t (list (cons type *233-type-integer*))))
        ((eq (first expr) 'progn)
         (let (last)
           (dolist (e (rest expr))
             (let ((var (gensym "?")))
               (multiple-value-bind (pass var-types)
                   (check-is-expr-type-p e var)
                 (unless pass
                   (return-from check-is-expr-type-p nil)) ; TODO: 可以考虑将返回值改为抛出异常，并说明这里的类型为什么不行。
                 (setf last (cdr (assoc var var-types))))))
           (eq last type)))
        ((member (first expr) '(+ - * /))
         (let ((lhs (second expr))
               (rhs (third expr)))
           (multiple-value-bind (pass var-types)
               (check-is-expr-type-p lhs *233-type-integer*)
             (declare (ignorable var-types))
             (unless pass
               (return-from check-is-expr-type-p nil)))
           (multiple-value-bind (pass var-types) ; TODO: 这里是否可以造一个类似Monad的东西来简化这种写法？
               (check-is-expr-type-p rhs *233-type-integer*)
             (declare (ignorable var-types))
             (unless pass
               (return-from check-is-expr-type-p nil)))
           (if (is-type-var-p type)
               (values t (list (cons type *233-type-integer*)))
               (eq type *233-type-integer*))))
        (t
         (error "未知类型的表达式：~A" expr))))

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

          (return-from check-233-type *233-type-integer*))))

    (when (eq func 'let)
      (multiple-value-bind (bindings body)
          (transform-let expr)
        (let ((ntenv tenv))
          (dolist (binding bindings)
            (destructuring-bind (var . val)
                binding
              (setf ntenv (extend ntenv var (check-233-type val ntenv)))))
          (return-from check-233-type (check-233-type body ntenv)))))) ; TODO: 总是要写return-from太麻烦了，想办法简化一下。
  (error "未知类型的表达式：~A" expr))
