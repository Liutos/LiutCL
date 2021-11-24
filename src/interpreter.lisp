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

;;; <core-app> begin
(defclass <core-app> (<core>)
  ((fun
    :initarg :fun
    :type <core>)
   (arg
    :documentation "实参表达式。"
    :initarg :arg
    :type <core>))
  (:documentation "语言核心中表示函数调用的语法结构。"))

(defmethod initialize-instance :after ((instance <core-app>) &rest initargs &key fun arg &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (typep fun '<core>)
    (error ":FUN必须为一个语法结构，但传入了~S" fun))
  (unless (typep arg '<core>)
    (error ":ARG必须为一个语法结构，但传入了~S" arg)))
;;; <core-app> end

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
    (eq x y)))
;;; <value-fun> begin
(defclass <value-fun> (<value>)
  ((arg
    :documentation "当前唯一的形参的名字。"
    :initarg :arg
    :type symbol)
   (body
    :documentation "表示函数逻辑的语法结构。"
    :initarg :body
    :type <core>)
   (env
    :documentation "函数对象被创建时的环境。"
    :initarg :env
    :type env))
  (:documentation "被实现语言中的函数类型。"))

(defmethod initialize-instance :after ((instance <value-fun>) &rest initargs &key arg body env &allow-other-keys)
  (declare (ignorable instance initargs))
  (unless (symbolp arg)
    (error ":ARG必须为一个符号，但传入了~S" arg))
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

(defmethod value-equal-p ((x <value-num>) (y <value-num>))
  (= (value-num-n x) (value-num-n y)))
;;; <value-num> end
;;; 语言值类型 end

;;; 环境相关 begin
;;; <binding> begin
(defclass <binding> ()
  ((location
    :initarg :location
    :reader binding-location
    :type integer)
   (name
    :initarg :name
    :reader binding-name
    :type symbol))
  (:documentation "将名称与对象的存储位置联系起来。"))

(defmethod initialize-instance :after ((instance <binding>) &rest initargs &key location name &allow-other-keys)
  (declare (ignorable initargs instance))
  (unless (integerp location)
    (error ":LOCATION必须为一个整数，但传入了~S" location))
  (unless (symbolp name)
    (error ":NAME必须为一个符号，但传入了~S" name)))
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
                  (binding-location binding))
                 (t
                  (lookup-env name (rest env))))))))

(defun make-empty-env ()
  '())
;;; env end
;;; 环境相关 end

;;; 类型错误 begin
(define-condition wrong-type ()
  ((actual
    :documentation "实际传入的错误类型"
    :initarg :actual)
   (expected
    :documentation "函数所期望的正确类型"
    :initarg :expected))
  (:report (lambda (condition stream)
             (with-slots (actual expected) condition
               (format stream "期望的类型为~S，但传入了~S。" actual expected)))))
;;; 类型错误 end

;;; 原生函数 begin
(defun num+ (l r)
  "将两个<VALUE-NUM>类型的值相加，返回一个新的<VALUE-NUM>类型的值。"
  (unless (typep l '<value-num>)
    (error 'wrong-type :actual (type-of l) :expected '<value-num>))
  (unless (typep r '<value-num>)
    (error 'wrong-type :actual (type-of r) :expected '<value-num>))
  (make-instance '<value-num>
                 :n (+ (value-num-n l) (value-num-n r))))
;;; 原生函数 end

;;; 存储相关 begin
(deftype store ()
  `hash-table)

(defun fetch-store (store location)
  (declare (type store store))
  (declare (integer location))
  (multiple-value-bind (v foundp)
      (gethash location store)
    (unless foundp
      (error "位置~D上没有值" location))

    v))

(defun make-empty-store ()
  (make-hash-table))

(defparameter *next-location* 0
  "下一个可用的位置。")

(defun put-store (store new-value)
  (declare (type store store))
  (let ((location *next-location*))
    (setf (gethash location store) new-value)
    (incf *next-location*)
    location))
;;; 存储相关 end

(defun interpret (ast env store)
  "解释执行抽象语法树AST，返回代码的执行结果。"
  (declare (type <core> ast))
  (declare (type env env))
  (declare (type store store))
  (etypecase ast
    (<core-app>
     (with-slots (fun arg) ast
       (let ((arg-val (interpret arg env store))
             (fun-val (interpret fun env store)))
         (with-slots (arg body) fun-val
           (interpret body
                      (extend-env (make-instance '<binding>
                                                 :location (put-store store arg-val)
                                                 :name arg)
                                  env)
                      store)))))
    (<core-id>
     (with-slots (s) ast
       (fetch-store store (lookup-env s env))))
    (<core-lambda>
     (with-slots (body par) ast
       (make-instance '<value-fun> :arg par :body body :env env)))
    (<core-num>
     (with-slots (n) ast
       (make-instance '<value-num> :n n)))
    (<core-plus>
     (with-slots (l r) ast
       (num+ (interpret l env store) (interpret r env store))))))
