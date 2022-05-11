(in-package #:cl)

(defpackage #:com.liutos.liutcl.interpreter
  (:export #:load-source-file)
  (:use #:cl))

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
                       `(unless (typep ,name ',type)
                          (error ,(format nil ":~A必须为~A类型，但传入了~~A" name type) ,name)))
                   slot-names slot-types)))
    `(progn
       (defclass ,name (<core>)
         ,slots
         ,@options)

       (defmethod initialize-instance :after ((instance ,name) &rest initargs &key ,@initargs &allow-other-keys)
         (declare (ignorable initargs instance))
         ,@assertions))))
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
    :reader core-id-s
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

;;; (print ...) 语法相关 begin
(define-core-variant <core-print>
    ((arg
      :documentation "要打印的数字。"
      :initarg :arg
      :type <core>)))
;;; (print ...) 语法相关 end

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
  ((args
    :documentation "由形参组成的列表。"
    :initarg :args)
   (body
    :documentation "表示函数逻辑的语法结构。"
    :initarg :body
    :type <core>)
   (env
    :documentation "函数对象被创建时的环境。"
    :initarg :env
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
;;; 布尔类型 end
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

;;; 续延相关 begin
(deftype continuation ()
  '<cont>)

(defclass <cont> ()
  ()
  (:documentation "表示续延的抽象类。"))

;;; TODO: 与前文的 define-core-variant 合并。
(defmacro define-cont-variant (name slots &rest options)
  "定义一个<CONT>的子类并在INITIALIZE-INSTANCE :AFTER中检查参数类型。"
  (check-type name symbol)
  (let* ((specs (mapcar #'cdr slots))
         (initargs (mapcar #'(lambda (spec) (intern (symbol-name (getf spec :initarg)))) specs))
         (slot-names (mapcar #'car slots))
         (slot-types (mapcar #'(lambda (spec) (getf spec :type)) specs))
         (assertions
           (mapcar #'(lambda (name type)
                       `(unless (typep ,name ',type)
                          (error ,(format nil ":~A必须为~A类型，但传入了~~A" name type) ,name)))
                   slot-names slot-types)))
    `(progn
       (defclass ,name (<cont>)
         ,slots
         ,@options)

       (defmethod initialize-instance :after ((instance ,name) &rest initargs &key ,@initargs &allow-other-keys)
         (declare (ignorable initargs instance))
         ,@assertions))))

(define-cont-variant <arg-cont>
  ((fun
    :initarg :fun
    :type <core>)
   (env
    :initarg :env
    :type env)
   (store
    :initarg :store
    :type store)
   (cont
    :initarg :cont
    :type <cont>))
  (:documentation "表示求值了实参之后要做的计算的续延。"))

(define-cont-variant <end-cont>
    ()
  (:documentation "表示没有更多的计算的续延。"))

(define-cont-variant <fun-cont>
    ((arg-val
      :initarg :arg-val
      :type <value>)
     (env
      :initarg :env
      :type env)
     (store
      :initarg :store
      :type store)
     (cont
      :initarg :cont
      :type <cont>))
  (:documentation "表示求值了函数位置的值之后要做的计算的续延。"))

(define-cont-variant <lhs-cont>
    ((r
      :initarg :r
      :type <core>)
     (env
      :initarg :env
      :type env)
     (store
      :initarg :store
      :type store)
     (cont
      :initarg :cont
      :type <cont>))
  (:documentation "表示求值了加法运算的左操作数后要做的计算的续延。"))

(define-cont-variant <print-cont>
    ((cont
      :initarg :cont
      :type <cont>))
  (:documentation "表示求值了 print 语法的参数后要做的动作的续延。"))

(define-cont-variant <rhs-cont>
    ((lhs
      :initarg :lhs
      :type <value>)
     (cont
      :initarg :cont
      :type <cont>))
  (:documentation "表示求值了加法运算的右操作数后要做的计算的续延。"))

(defun apply-continuation (cont v)
  "将值 V 传给续延 CONT。"
  (declare (type (or continuation <cont>) cont))
  (declare (type <value> v))
  (etypecase cont
    (<arg-cont>
     (with-slots (cont fun env store) cont
       (interpret/k fun env store
                    (make-fun-cont v env store cont))))
    (<end-cont>
     v)
    (<fun-cont>
     (with-slots (arg-val cont env store) cont
       (etypecase v
         (<value-cont>
          (with-slots (cont) v
            (apply-continuation cont arg-val)))
         (<value-fun>
          (with-slots (args body) v
            (let* ((location (put-store store arg-val))
                   (binding (make-instance '<binding>
                                           :location location
                                           :name (first args))))
              (interpret/k body
                           (extend-env binding env)
                           store
                           cont)))))))
    (<lhs-cont>
     (with-slots (cont env r store) cont
       (interpret/k r env store (make-rhs-cont v cont))))
    (<print-cont>
     (with-slots (cont) cont
       (etypecase v
         (<value-bool>
          (format t "~A~%" (if (value-bool-val v) "true" "false")))
         (<value-num>
          (format t "~D~%" (value-num-n v))))
       (apply-continuation cont v)))
    (<rhs-cont>
     (with-slots (cont lhs) cont
       (apply-continuation cont (num+ lhs v))))
    (function
     (funcall cont v))))

(defun make-arg-cont (fun env store cont)
  "创建一个表示求值了实参之后要做的计算的续延。"
  (make-instance '<arg-cont>
                 :cont cont
                 :env env
                 :fun fun
                 :store store))

(defun make-end-cont ()
  (make-instance '<end-cont>))

(defun make-fun-cont (arg-val env store cont)
  "创建一个表示求值了函数位置的值之后要做的计算的续延。"
  (make-instance '<fun-cont>
                 :arg-val arg-val
                 :cont cont
                 :env env
                 :store store))

(defun make-lhs-cont (r env store cont)
  "创建一个表示求值了加法运算的左操作数后要做的计算的续延。"
  (make-instance '<lhs-cont> :cont cont :env env :r r :store store))

(defun make-print-cont (cont)
  (make-instance '<print-cont> :cont cont))

(defun make-rhs-cont (lhs cont)
  "创建一个表示求值了加法运算的右操作数后要做的计算的续延。"
  (make-instance '<rhs-cont> :cont cont :lhs lhs))
;;; 续延相关 end

(defun interpret/k (ast env store cont)
  "CPS版本的interpret解释器，其中CONT表示“接下来的运算”。"
  (declare (type <core> ast))
  (declare (type env env))
  (declare (type store store))
  (declare (type continuation cont))
  (etypecase ast
    (<core-app>
     (with-slots (fun arg) ast
       (interpret/k arg env store (make-arg-cont fun env store cont))))
    (<core-bool>
     (with-slots (id) ast
       (let (bv rv)                    ; rv 表示要传入给续延的值，bv 表示根据字面量映射出来的 nil 或 t。
         (assert (member id '(false true) :test 'string=))
         (setf bv (if (string= id 'false) nil t))
         (setf rv (make-instance '<value-bool> :val bv))
         (apply-continuation cont rv)))) ; 常量的行为都是直接应用当前续延。
    (<core-call/cc>
     (with-slots (body var) ast
       ;; 新建一个环境，在这个环境中，将当前续延绑定到变量 VAR 上，然后求值表达式 BODY，并将结果传递给当前续延。
       (let* ((binding (make-instance '<binding>
                                      :location (put-store store (make-instance '<value-cont>
                                                                                :cont cont))
                                      :name (core-id-s var)))
              (env (extend-env binding env)))
         (interpret/k body env store cont))))
    (<core-id>
     (with-slots (s) ast
       (apply-continuation cont (fetch-store store (lookup-env s env)))))
    (<core-lambda>
     (with-slots (body par) ast
       (apply-continuation cont (make-instance '<value-fun> :args (list par) :body body :env env))))
    (<core-num>
     (with-slots (n) ast
       (apply-continuation cont (make-instance '<value-num> :n n))))
    (<core-plus>
     (with-slots (l r) ast
       (interpret/k l env store (make-lhs-cont r env store cont))))
    (<core-print>
     (with-slots (arg) ast
       (interpret/k arg env store (make-print-cont cont))))))

;;; 具体语法相关 begin
(defun parse-concrete-syntax (expr)
  "解析作为具体语法的S表达式 EXPR，返回对象的抽象语法 <CORE> 类的实例对象。"
  (cond ((and (listp expr) (eq (first expr) 'lambda))
         (destructuring-bind (_ parameters . body)
             expr
           (declare (ignorable _))      ; TODO: CL 一处值得改进的地方，即无法用下划线来便捷地表达“不使用的变量”这一意图。
           (assert (= (length parameters) 1) nil "仅支持一个参数：~S" parameters)
           (assert (= (length body) 1) nil "仅支持一个表达式：~S" body)
           (make-instance '<core-lambda>
                          :body (parse-concrete-syntax (first body))
                          :par (first parameters))))
        ((and (listp expr) (eq (first expr) '+))
         (destructuring-bind (_ lhs rhs)
             expr
           (declare (ignorable _))
           (make-instance '<core-plus>
                          :l (parse-concrete-syntax lhs)
                          :r (parse-concrete-syntax rhs))))
        ((and (listp expr) (symbolp (first expr)) (string= (first expr) 'call/cc))
         (destructuring-bind (_ vars body)
             expr
           (declare (ignorable _))
           (make-instance '<core-call/cc>
                          :body (parse-concrete-syntax body)
                          :var (make-instance '<core-id> :s (first vars)))))
        ((and (listp expr) (eq (first expr) 'print))
         (destructuring-bind (_ arg)
             expr
           (declare (ignorable _))
           (make-instance '<core-print>
                          :arg (parse-concrete-syntax arg))))
        ((listp expr)
         (destructuring-bind (fun arg)
             expr
           (make-instance '<core-app>
                          :arg (parse-concrete-syntax arg)
                          :fun (parse-concrete-syntax fun))))
        ((and (symbolp expr) (member expr '(false true) :test #'string=)) ; 由于标识符可能属于其它 package，因此这里用 string= 来比较。
         (make-instance '<core-bool> :id expr))
        ((symbolp expr)
         (make-instance '<core-id> :s expr))
        ((integerp expr)
         (make-instance '<core-num> :n expr))
        (t
         (error "不支持的具体语法：~S" expr))))
;;; 具体语法相关 end

;;; 运行脚本相关 begin
(defun load-source-file (filespec)
  "读取文件 FILESPEC 中的代码并从上到下顺序执行。"
  (with-open-file (s filespec)
    (let ((env (make-empty-env))
          (expr (read s))
          (store (make-empty-store)))
      (interpret/k (parse-concrete-syntax expr)
                   env
                   store
                   (make-end-cont)))))
;;; 运行脚本相关 end
