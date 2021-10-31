(in-package #:com.liutos.liutcl.interpreter)

(defgeneric extend (venv sym val)
  (:documentation "扩展值环境。"))
(defgeneric lookup (venv sym)
  (:documentation "在值环境中查找标识符的值。"))
(defgeneric modify (venv sym val)
  (:documentation "修改值环境中已有的一个变量的值。"))

;;; 基于列表的实现。
(defmethod extend ((venv list) (sym symbol) val)
  (cons (cons sym val) venv))
(defmethod lookup ((venv list) (sym symbol))
  (or (cdr (assoc sym venv))
      (cdr (assoc sym *top-level-venv*))))
(defmethod modify ((venv list) (sym symbol) val)
  (let ((binding (assoc sym venv)))
    (unless binding
      (setf binding (assoc sym *top-level-venv*)))
    (unless binding
      (error "变量未定义：~A" sym))

    (setf (cdr binding) val)))
