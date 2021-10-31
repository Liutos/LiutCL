(in-package #:com.liutos.liutcl.interpreter)

;;; 下列代码来自：https://ambrevar.xyz/modern-common-lisp/index.html?utm_source=pocket_mylist#orgf68c5ba
;;; 可以让 SBCL 允许用 step 单步调试
(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; 定义233-lisp中的函数对象需要满足的接口。
(defgeneric get-body (func)
  (:documentation "获取函数体的表达式列表。"))
(defgeneric get-parameters (func)
  (:documentation "获取函数的参数列表。"))
(defgeneric get-tag (func)
  (:documentation "获取函数存储RETURN POINT的标号。"))

(defclass <func> ()
  ((body
    :documentation "函数体的表达式列表，它们被包裹在一个隐式的LET语法中。"
    :initarg :body)
   (params
    :documentation "参数列表。"
    :initarg :params)
   (tag
    :documentation "用于捕捉RETURN语句的标号。"
    :initarg :tag)))

(defun make-func (body params tag)
  (check-type body list)
  (check-type params list)
  (check-type tag symbol)
  (make-instance '<func> :body body :params params :tag tag))

(defmethod get-body ((func <func>))
  (slot-value func 'body))
(defmethod get-parameters ((func <func>))
  (slot-value func 'params))
(defmethod get-tag ((func <func>))
  (slot-value func 'tag))

(defparameter *symbol-return-from* '#:return-from)

(defun interpret (expr venv)
  (when (member expr '(nil t))
    (return-from interpret expr))

  (when (symbolp expr)
    (let ((val (lookup venv expr)))
      (unless val
        (error "变量~A未定义" expr))

      (return-from interpret val)))

  (when (atom expr)
    (return-from interpret expr))

  (let ((func (first expr)))
    (when (eq func *symbol-return-from*)
      (let ((form (third expr))
            (tag (second expr)))
        (throw tag (interpret form venv))))

    (case func
      (break (interpret-break expr venv))
      (continue (interpret-continue expr venv))
      (defun (interpret-defun expr venv))
      (for (interpret-for expr venv))
      (if (interpret-if expr venv))
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
      (setf (interpret-setf expr venv))
      (t
       (let* ((args (rest expr))
              (vals (mapcar #'(lambda (arg)
                                (interpret arg venv))
                            args))
              (func (interpret func venv)))
         (when (functionp func)
           (return-from interpret
             (apply func vals)))
         (when (hash-table-p func)
           (return-from interpret
             (interpret-access func (first vals))))

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
           (catch (get-tag func)
             (interpret `(let ,@body) new-venv))))))))

(defparameter *top-level-venv*
  '()
  "全局顶层值环境。")

(defun init-top-level-venv ()
  (let ((venv '()))
    (setf venv (extend venv '< #'<))
    (setf venv (extend venv '+ #'+))
    (setf venv (extend venv 'dict #'make-dict))
    (setf *top-level-venv* venv)))

(defparameter *symbol-throw-tag-for-break* '#:tag-for-break
  "用于由BREAK抛出，被FOR循环捕捉的符号。")

(defun interpret-break (expr venv)
  (declare (ignorable expr venv))
  (assert (eq (first expr) 'break))
  (throw *symbol-throw-tag-for-break* nil))

(defparameter *symbol-throw-tag-for-continue* '#:tag-for-continue)

(defun interpret-continue (expr venv)
  (declare (ignorable expr venv))
  (throw *symbol-throw-tag-for-continue* nil))

(defun interpret-defun (expr venv)
  (declare (ignorable venv))            ; TODO: venv应当是影响这个函数是否为一个闭包的关键，之后再处理。
  (check-type expr cons)
  (assert (eq (first expr) 'defun))
  (let* ((body (rest (rest (rest expr))))
         (name (second expr))
         (params (third expr))
         (tag (gensym))
         (func (make-func (rewrite-defun-return body tag) params tag)))
    (push (cons name func) *top-level-venv*)
    func))

(defun interpret-for (expr venv)
  (assert (eq (first expr) 'for))
  (destructuring-bind (test . body)
      (rest expr)
    (catch *symbol-throw-tag-for-break*
      (loop
        (unless (interpret test venv)
          (return-from interpret-for))

        (catch *symbol-throw-tag-for-continue*
          (interpret `(progn ,@body) venv))))))

(defun interpret-access (container indicator)
  "对访问字典对象的表达式求值。"
  (check-type container hash-table)
  (check-type indicator (or number string))
  (gethash indicator container))

(defun interpret-if (expr venv)
  (assert (eq (first expr) 'if))
  (destructuring-bind (test then else)
      (rest expr)
    (if (interpret test venv)
        (interpret then venv)
        (interpret else venv))))

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

(defun interpret-setf (expr venv)
  (assert (eq (first expr) 'setf))
  (destructuring-bind (var val)
      (rest expr)
    (let ((val (interpret val venv)))
      (cond ((symbolp var)
             (modify venv var val))
            ((consp var)
             (let ((container (interpret (first var) venv))
                   (indicator (interpret (second var) venv)))
               (assert (hash-table-p container))
               (setf (gethash indicator container) val))))
      val)))

(defun load-source-file (filespec)
  (check-type filespec string)
  (init-top-level-venv)
  (with-open-file (s filespec)
    (let* ((seq (read-source-code s))
           (filtered (remove-if #'(lambda (expr)
                                    (or (not (consp expr))
                                        (not (eq (first expr) 'defun))))
                                seq)))
      (unless (= (length filtered)
                 (1- (length seq)))
        (error "源文件 ~A 中含有 DEFUN 之外的顶层表达式" filespec))
      (interpret (cons 'progn filtered) nil)
      (interpret '(main) nil))))

(defun rewrite-defun-return (body tag)
  "重写DEFUN的函数体部分，将RETURN替换为RETURN-FROM，塞入一个标号。"
  (check-type tag symbol)
  (assert (null (symbol-package tag)))
  (when (atom body)
    (return-from rewrite-defun-return body))

  (if (eq (first body) 'return)
      `(,*symbol-return-from* ,tag ,@(rest body))
      (mapcar #'(lambda (expr)
                  (rewrite-defun-return expr tag))
              body)))

(defparameter *testing-venv*
  (let ((bindings (list (list '* #'*)
                        (list '+ #'+)
                        (list '- #'-)
                        (list '/ #'/)
                        (list 'foo 233)
                        (list '< #'<)
                        (list '= #'=)))
        (venv '()))
    (dolist (binding bindings)
      (destructuring-bind (name func)
          binding
        (setf venv (extend venv name func))))
    venv))

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
