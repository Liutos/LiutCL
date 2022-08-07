(in-package #:com.liutos.liutcl.interpreter)

;;; 下列代码来自：https://ambrevar.xyz/modern-common-lisp/index.html?utm_source=pocket_mylist#orgf68c5ba
;;; 可以让 SBCL 允许用 step 单步调试
(declaim (optimize (speed 0) (space 0) (debug 3)))

;;; 语言值类型 begin
(defclass <value> ()
  ()
  (:documentation "被实现语言中的类型的基类。"))

(defgeneric value-equal-p (x y)
  (:documentation "比较两个<VALUE>类的实例对象X和Y是否相等。")
  (:method ((x <value>) (y <value>))
    "通常情况下，任意的两个值对象是不相等的。"
    (eq x y)))
;;; <value-fun> begin
(defclass <value-fun> (<value>)
  ((args
    :documentation "由形参组成的列表。"
    :initarg :args)
   (body
    :documentation "表示函数逻辑的语法结构。"
    :initarg :body
    :type <core>)
   (env
    :accessor value-fun-env
    :documentation "函数对象被创建时的环境。"
    :initarg :env
    :initform (error "参数 :ENV 不能为空。") ; 一种模拟字段必填效果的方法。
    :type env))
  (:documentation "被实现语言中的函数类型。"))

(defmethod initialize-instance :after ((instance <value-fun>) &rest initargs &key args body env &allow-other-keys)
  (declare (ignorable instance initargs))
  (unless (and (listp args) (symbolp (first args)))
    (error ":ARGS必须为一个符号列表，但传入了~S" args))
  (unless (typep body '<core>)
    (error ":BODY必须为一个语法结构，但传入了~S" body))
  (unless (typep env 'env)
    (error ":ENV必须为一个环境，但传入了~S" env)))
;;; <value-fun> end

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

(defmethod print-object ((v <value-num>) stream)
  (print-unreadable-object (v stream :type t)
    (princ (value-num-n v) stream)))

(defmethod value-equal-p ((x <value-num>) (y <value-num>))
  (= (value-num-n x) (value-num-n y)))
;;; <value-num> end

;;; 续延值类型 begin
(defclass <value-cont> (<value>)
  ((cont
    :documentation "一个续延对象。"
    :initarg :cont
    :reader value-cont-cont))
  (:documentation "被实现语言中的续延类型。"))

(defmethod initialize-instance :after ((instance <value-cont>) &rest initargs &key cont &allow-other-keys)
  (declare (ignorable instance initargs))
  (unless (typep cont '<cont>)
    (error ":CONT 必须为一个续延，但传入了 ~S" cont)))
;;; 续延值类型 end

;;; 布尔类型 begin
(defclass <value-bool> (<value>)
  ((val
    :documentation "布尔值"
    :initarg :val
    :reader value-bool-val))
  (:documentation "被实现语言中的布尔类型。"))

(defmethod value-equal-p ((x <value-bool>) (y <value-bool>))
  (eq (value-bool-val x) (value-bool-val y)))

(defmethod print-object ((object <value-bool>) stream)
  (print-unreadable-object (object stream :type t)
    (princ (if (value-bool-val object) "true" "false") stream)))
;;; 布尔类型 end

;;; 原生函数类型 begin
(defclass <value-primitive> (<value>)
  ((f
    :documentation "一个 CL 函数对象。"
    :initarg :f
    :reader value-primitive-f
    :type function))
  (:documentation "用 CL 函数实现的、目标语言中的函数类型。"))

(defmethod value-equal-p ((x <value-primitive>) (y <value-primitive>))
  (eq (value-primitive-f x) (value-primitive-f y)))
;;; 原生函数类型 end

;;; 字符串类型 begin
(defclass <value-str> (<value>)
  ((content
    :documentation "字符串的底层字符数据。"
    :initarg :content
    :reader value-str-content
    :type string))
  (:documentation "233-lisp 中的字符串类型。"))

(defmethod value-equal-p ((x <value-str>) (y <value-str>))
  (string= (value-str-content x) (value-str-content y)))
;;; 字符串类型 end
;;; 语言值类型 end
