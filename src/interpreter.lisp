(in-package #:cl)

(defpackage #:com.liutos.liutcl.interpreter
  (:use #:cl)
  (:export #:load-source-file))

(in-package #:com.liutos.liutcl.interpreter)

;;; 下列代码来自：https://ambrevar.xyz/modern-common-lisp/index.html?utm_source=pocket_mylist#orgf68c5ba
;;; 可以让 SBCL 允许用 step 单步调试
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defgeneric extend (venv sym val)
  (:documentation "扩展值环境。"))
(defgeneric lookup (venv sym)
  (:documentation "在值环境中查找标识符的值。"))
(defgeneric modify (venv sym val)
  (:documentation "修改值环境中已有的一个变量的值。"))

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
      (modify venv var val)
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

(defun |"-reader| (stream char)
  "读取字符串并识别其中的C语言风格的转义序列。"
  (declare (ignorable char))
  (let ((cs '()))
    (loop
      (let ((c (read-char stream)))
        (cond ((char= c #\")
               (return-from |"-reader|
                 (coerce (nreverse cs) 'string)))
              ((char= c #\\)
               (let ((c (read-char stream)))
                 (ecase c
                   (#\n (push #\Newline cs))
                   (#\t (push #\Tab cs)))))
              (t (push c cs)))))))

(defun read-source-code (stream)
  (let ((*package* (find-package '#:com.liutos.liutcl.interpreter))
        (*readtable* (copy-readtable nil)))
    (set-macro-character #\" '|"-reader|)
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
