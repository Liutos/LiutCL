(in-package #:com.liutos.liutcl.interpreter)

;;; 下列代码来自：https://ambrevar.xyz/modern-common-lisp/index.html?utm_source=pocket_mylist#orgf68c5ba
;;; 可以让 SBCL 允许用 step 单步调试
(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; 语法相关 begin
(defclass <core> ()
  ()
  (:documentation "核心语言中各种语法结构的基类。"))

(defmacro define-core-variant (name slots &rest options)
  "定义一个<CORE>的子类并在INITIALIZE-INSTANCE :AFTER中检查参数类型。"
  (check-type name symbol)
  (let* ((specs (mapcar #'cdr slots))
         (initargs (mapcar #'(lambda (spec) (intern (symbol-name (getf spec :initarg)))) specs))
         (slot-names (mapcar #'car slots))
         (slot-types (mapcar #'(lambda (spec) (getf spec :type)) specs))
         (assertions
           (mapcar #'(lambda (name type)
                       (when type
                         `(unless (typep ,name ',type)
                            (error ,(format nil ":~A必须为~A类型，但传入了~~A" name type) ,name))))
                   slot-names slot-types)))
    `(progn
       (defclass ,name (<core>)
         ,slots
         ,@options)

       (defmethod initialize-instance :after ((instance ,name) &rest initargs &key ,@initargs &allow-other-keys)
         (declare (ignorable initargs instance ,@initargs))
         ,@assertions))))
;;; <core-app> begin
(defclass <core-app> (<core>)
  ((fun
    :initarg :fun
    :type <core>)
   (args
    :documentation "实参表达式组成的列表。"
    :initarg :args))
  (:documentation "语言核心中表示函数调用的语法结构。"))

(defmethod initialize-instance :after ((instance <core-app>) &rest initargs &key args fun &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (typep fun '<core>)
    (error ":FUN必须为一个语法结构，但传入了~S" fun))
  (unless (or (null args) (and (listp args) (typep (first args) '<core>)))
    (error ":ARGS必须为空，或者是一个语法结构列表，但传入了~S" args)))

(defmethod print-object ((object <core-app>) stream)
  "将函数调用的 AST 打印为 S 表达式。"
  (print-unreadable-object (object stream)
    (with-slots (args fun) object
      (format stream "(~A ~{~A~^ ~})" fun args))))
;;; <core-app> end

;;; defun 语法相关 begin
(define-core-variant <core-defun>
    ((body
      :documentation "函数体代码。"
      :initarg :body
      :type <core>)
     (name
      :documentation "函数名"
      :initarg :name
      :type symbol)
     (parameters
      :documentation "参数列表"
      :initarg :parameters))
  (:documentation "表示定义全局函数的语法。"))
;;; defun 语法相关 end

;;; <core-id> begin
(defclass <core-id> (<core>)
  ((s
    :initarg :s
    :reader core-id-s
    :type symbol))
  (:documentation "语言核心中表示标识符的语法结构。"))

(defmethod initialize-instance :after ((instance <core-id>) &rest initargs &key s &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (symbolp s)
    (error ":S必须为一个符号，但传入了~S" s)))

(defmethod print-object ((object <core-id>) stream)
  "打印出标识符的名称。"
  (print-unreadable-object (object stream)
    (princ (core-id-s object) stream)))
;;; <core-id> end

;;; <core-lambda> begin
(defclass <core-lambda> (<core>)
  ((body
    :initarg :body
    :type <core>)
   (par
    :initarg :par
    :type symbol))
  (:documentation "语言核心中表示匿名函数定义的语法结构。"))

(defmethod initialize-instance :after ((instance <core-lambda>) &rest initargs &key body par &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (typep body '<core>)
    (error ":BODY必须为一个语法结构，但传入了~S" body))
  (unless (symbolp par)
    (error ":PAR必须为一个符号，但传入了~S" par)))
;;; <core-lambda> end

;;; <core-num> begin
(defclass <core-num> (<core>)
  ((n
    :initarg :n
    :reader core-num-n
    :type integer))
  (:documentation "核心语言中表示数值字面量的语法结构。"))

(defmethod initialize-instance :after ((instance <core-num>) &rest initargs &key n &allow-other-keys)
  "检查传入的N是否为整数类型。"
  (declare (ignorable initargs instance))
  (unless (integerp n)
    (error ":N必须为一个整数，但传入了~S" n)))

(defmethod print-object ((object <core-num>) stream)
  (print-unreadable-object (object stream)
    (with-slots (n) object
      (princ n stream))))
;;; <core-num> end

;;; <core-plus> begin
(defclass <core-plus> (<core>)
  ((l
    :initarg :l
    :type <core>)
   (r
    :initarg :r
    :type <core>))
  (:documentation "核心语言中表示加法运算的语法结构。"))

(defmethod initialize-instance :after ((instance <core-plus>) &rest initargs &key l r &allow-other-keys)
  "检查传入的左右操作数是否为语法结构类型。"
  (declare (ignorable initargs instance))
  (unless (typep l '<core>)
    (error ":L必须为一个<CORE>类型，但传入了~S" l))
  (unless (typep r '<core>)
    (error ":R必须为一个<CORE>类型，但传入了~S" r)))
;;; <core-plus> end

;;; call/cc 语法相关 begin
(define-core-variant <core-call/cc>
    ((body
      :documentation "在能够访问到当前续延的环境下求值的表达式。"
      :initarg :body
      :type <core>)
     (var
      :documentation "用于捕捉当前续延的变量名。"
      :initarg :var
      :type <core-id>)))
;;; call/cc 语法相关 end

;;; 布尔类型字面量语法 begin
(define-core-variant <core-bool>
    ((id
      :documentation "表示布尔类型字面量的标识符。"
      :initarg :id
      :type symbol)))
;;; 布尔类型字面量语法 end

;;; if 语法相关 begin
(define-core-variant <core-if>
    ((test
         :documentation "首先被求值的测试表达式。"
       :initarg :test
       :type <core>)
     (then
      :documentation "测试表达式成立时执行的代码。"
      :initarg :then
      :type <core>)
     (else
      :documentation "测试表达式不成立时的代码。"
      :initarg :else
      :type <core>))
  (:documentation "if 语法的语法结构。"))

(defmethod print-object ((object <core-if>) stream)
  (print-unreadable-object (object stream)
    (with-slots (else test then) object
      (format stream "(if ~A ~A ~A)" test then else))))
;;; if 语法相关 end

;;; 顺序求值语法相关 begin
(define-core-variant <core-progn>
    ((forms
      :documentation "要顺序求值的一系列表达式。"
      :initarg :forms
      :reader core-progn-forms
      :type list))
  (:documentation "progn 语句的抽象语法树。"))
;;; 顺序求值语法相关 end

;;; 赋值语法相关 begin
(define-core-variant <core-setf>
    ((val-expr
      :documentation "右值表达式。"
      :initarg :val-expr
      :reader core-setf-val-expr
      :type <core>)
     (var
      :documentation "被赋值的变量的名字。"
      :initarg :var
      :reader core-setf-var
      :type symbol))
  (:documentation "赋值语句。"))
;;; 赋值语法相关 end

;;; 相互递归的函数定义语法 begin
(define-core-variant <core-labels>
    ((definitions
      :documentation "一系列相互递归的函数的定义。"
      :initarg :definitions)))

(defmethod initialize-instance :after ((instance <core-labels>) &rest initargs &key definitions &allow-other-keys)
  (declare (ignorable initargs instance))
  (check-type definitions list)
  (dolist (definition definitions)
    (check-type definition <core-defun>)))
;;; 相互递归的函数定义语法 end

;;; 动态作用域变量定义语法 begin
(define-core-variant <core-defvar>
    ((val
      :documentation "变量初值的表达式。"
      :initarg :val
      :type <core>)
     (var
      :documentation "动态作用域变量名。"
      :initarg :var
      :type symbol))
  (:documentation "定义动态作用域变量。"))
;;; 动态作用域变量定义语法 end
;;; 语法相关 end
