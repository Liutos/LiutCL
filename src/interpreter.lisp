(in-package #:cl)

(defpackage #:com.liutos.liutcl.interpreter
  (:use #:cl))

(in-package #:com.liutos.liutcl.interpreter)

;;; 下列代码来自：https://ambrevar.xyz/modern-common-lisp/index.html?utm_source=pocket_mylist#orgf68c5ba
;;; 可以让 SBCL 允许用 step 单步调试
(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; 语法相关 begin
(defclass <core> ()
  ()
  (:documentation "核心语言中各种语法结构的基类。"))

;;; <core-id> begin
(defclass <core-id> (<core>)
  ((s
    :initarg :s
    :type symbol))
  (:documentation "语言核心中表示标识符的语法结构。"))

(defmethod initialize-instance :after ((instance <core-id>) &rest initargs &key s &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (symbolp s)
    (error ":S必须为一个符号，但传入了~S" s)))
;;; <core-id> end

;;; <core-num> begin
(defclass <core-num> (<core>)
  ((n
    :initarg :n
    :type integer))
  (:documentation "核心语言中表示数值字面量的语法结构。"))

(defmethod initialize-instance :after ((instance <core-num>) &rest initargs &key n &allow-other-keys)
  "检查传入的N是否为整数类型。"
  (declare (ignorable initargs instance))
  (unless (integerp n)
    (error ":N必须为一个整数，但传入了~S" n)))
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
;;; 语法相关 end

;;; 语言值类型 begin
(defclass <value> ()
  ()
  (:documentation "被实现语言中的类型的基类。"))

(defgeneric value-equal-p (x y)
  (:documentation "比较两个<VALUE>类的实例对象X和Y是否相等。")
  (:method ((x <value>) (y <value>))
    "通常情况下，任意的两个值对象是不相等的。"
    (declare (ignorable x y))
    nil))

;;; <value-num> begin
(defclass <value-num> (<value>)
  ((n
    :initarg :n
    :reader value-num-n))
  (:documentation "被实现语言中的数值类型。"))

(defmethod initialize-instance :after ((instance <value-num>) &rest initargs &key n &allow-other-keys)
  (declare (ignorable instance initargs))
  (unless (integerp n)
    (error ":N必须为一个整数，但传入了~S" n)))

(defmethod value-equal-p ((x <value-num>) (y <value-num>))
  (= (value-num-n x) (value-num-n y)))
;;; <value-num> end
;;; 语言值类型 end

;;; 环境相关 begin
;;; <binding> begin
(defclass <binding> ()
  ((name
    :initarg :name
    :reader binding-name
    :type symbol)
   (val
    :initarg :val
    :reader binding-val
    :type <value>))
  (:documentation "将名称与值对象联系起来。"))

(defmethod initialize-instance :after ((instance <binding>) &rest initargs &key name val &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (symbolp name)
    (error ":NAME必须为一个符号，但传入了~S" name))
  (unless (typep val '<value>)
    (error ":VAL必须为<VALUE>及其子类的实例，但传入了~S" val)))
;;; <binding> end

;;; env begin
(deftype env ()
  `list)

(defun extend-env (binding env)
  "返回一个新的环境，它在旧环境ENV的基础上，增加了一个绑定BINDING。"
  (declare (type <binding> binding))
  (declare (type env env))
  (cons binding env))

(defun lookup-env (name env)
  "在环境ENV中查找与名称NAME关联的值并返回。"
  (declare (symbol name))
  (declare (type env env))
  (cond ((null env)
         (error "找不到标识符~S的定义" name))
        (t
         (let ((binding (first env)))
           (cond ((eq (binding-name binding) name)
                  (binding-val binding))
                 (t
                  (lookup-env name (rest env))))))))

(defun make-empty-env ()
  '())
;;; env end
;;; 环境相关 end

(defun interpret (ast env)
  "解释执行抽象语法树AST，返回代码的执行结果。"
  (declare (type <core> ast))
  (declare (type env env))
  (etypecase ast
    (<core-id>
     (with-slots (s) ast
       (lookup-env s env)))
    (<core-num>
     (with-slots (n) ast
       (make-instance '<value-num> :n n)))
    (<core-plus>
     (with-slots (l r) ast
       (make-instance '<value-num>
                      :n (+ (value-num-n (interpret l env)) (value-num-n (interpret r env))))))))
