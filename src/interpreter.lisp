(in-package #:com.liutos.liutcl.interpreter)

;;; 下列代码来自：https://ambrevar.xyz/modern-common-lisp/index.html?utm_source=pocket_mylist#orgf68c5ba
;;; 可以让 SBCL 允许用 step 单步调试
(declaim (optimize (speed 0) (space 0) (debug 3)))

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
  (dolist (binding env)
    (when (string= (binding-name binding) name) ; 为了可以比较不同 package 中的符号 main，这里使用 string= 而不是 eq。
      (return-from lookup-env (binding-location binding))))
  (error "找不到标识符~S的定义" name))

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

(defparameter *next-location* 0         ; TODO: 修复 *next-location* 会不断变大的问题。
  "下一个可用的位置。")

(defun put-store (store new-value)
  (declare (type store store))
  (let ((location *next-location*))
    (setf (gethash location store) new-value)
    (incf *next-location*)
    location))

(defun update-store (store location new-value)
  "将存储 STORE 中一个已有的位置 LOCATION 上的值替换为 NEW-VALUE。"
  (check-type store store)
  (check-type location integer)
  (check-type new-value <value>)
  (let ((foundp (nth-value 1 (gethash location store))))
    (assert foundp nil "位置 ~D 必须已经在内存 ~A 中被定义了才行" location store)
    (setf (gethash location store) new-value)))
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
                       (when type
                         `(unless (typep ,name ',type)
                            (error ,(format nil ":~A必须为~A类型，但传入了~~A" name type) ,name))))
                   slot-names slot-types)))
    `(progn
       (defclass ,name (<cont>)
         ,slots
         ,@options)

       (defmethod initialize-instance :after ((instance ,name) &rest initargs &key ,@initargs &allow-other-keys)
         (declare (ignorable initargs instance ,@initargs))
         ,@assertions))))

(define-cont-variant <arg2-cont>
    ((denv
      :documentation "求值参数之前的动态作用域环境。"
      :initarg :denv
      :type env)
     (fun
      :documentation "求值完所有参数表达式后要求的函数位置上的表达式。"
      :initarg :fun
      :type <core>)
     (env
      :initarg :env
      :type env)
     (rest-args
      :documentation "剩余待求值的参数表达式列表。"
      :initarg :rest-args)
     (store
      :initarg :store
      :type store)
     (cont
      :initarg :cont
      :type <cont>)
     (vs
      :documentation "已被求值的表达式的结果组成的列表，用于最终绑定到函数体的环境上。"
      :initarg :vs))
  (:documentation "代表求值了函数的第一个参数之后接着要做的事情。"))

(define-cont-variant <end-cont>
    ()
  (:documentation "表示没有更多的计算的续延。"))

(define-cont-variant <fun-cont>
    ((args
      :documentation "全部求值后的参数列表。"
      :initarg :args)
     (denv
      :documentation "求值参数之前的动态作用域环境。"
      :initarg :denv
      :type env)
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

(define-cont-variant <test-cont>
    ((denv
      :documentation "求值参数之前的动态作用域环境。"
      :initarg :denv
      :type env)
     (saved-cont
      :initarg :saved-cont
      :type <cont>)
     (else
      :initarg :else
      :type <core>)
     (env
      :initarg :env
      :type env)
     (store
      :initarg :store
      :type store)
     (then
      :initarg :then
      :type <core>))
  (:documentation "表示求值了 if 语句的测试表达式之后要做的计算。"))

(define-cont-variant <lhs-cont>
    ((r
      :initarg :r
      :type <core>)
     (denv
      :documentation "求值参数之前的动态作用域环境。"
      :initarg :denv
      :type env)
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

(define-cont-variant <progn-cont>
    ((denv
      :documentation "求值参数之前的动态作用域环境。"
      :initarg :denv
      :type env)
     (forms
      :documentation "剩余要求值的表达式列表。"
      :initarg :forms
      :type list)
     (saved-cont
      :documentation "求值完所有表达式之后，要将最后一个表达式的值传入的续延。"
      :initarg :saved-cont
      :type <cont>)
     (saved-env
      :documentation "接下来求值 FORMS 中的表达式时要用到的环境。"
      :initarg :saved-env
      :type env)
     (store
      :documentation "全局内存对象的引用。"
      :initarg :store
      :type store))
  (:documentation "表示求值了 progn 语句的第一个表达式之后要做的计算。"))

(define-cont-variant <rhs-cont>
    ((lhs
      :initarg :lhs
      :type <value>)
     (cont
      :initarg :cont
      :type <cont>))
  (:documentation "表示求值了加法运算的右操作数后要做的计算的续延。"))

(define-cont-variant <setf-cont>
    ((location
      :documentation "右值表达式计算后要写入的内存位置。"
      :initarg :location
      :type integer)
     (saved-cont
      :documentation "赋值结束后要进行的计算。"
      :initarg :saved-cont
      :type <cont>)
     (store
      :documentation "全局内存对象的引用。"
      :initarg :store
      :type store)
     (var
      :documentation "要赋值的变量名。"
      :initarg :var
      :type symbol))
  (:documentation "表示计算完右值表达式后要进行的计算。"))

(defun apply-continuation (cont v)
  "将值 V 传给续延 CONT。"
  (declare (type (or continuation <cont>) cont))
  (declare (type <value> v))
  (etypecase cont
    (<arg2-cont>
     (with-slots (cont fun env rest-args store vs denv) cont
       ;; 如果还有剩余的参数表达式，就将当前获得的值“压栈”，并继续求值下一个表达式。
       ;; 否则，代表所有参数都求值完毕，可以求值函数位置上的表达式了。
       (cond ((null rest-args)
              (lambda ()
                (interpret/k fun env store
                             (make-fun-cont (cons v vs) env store cont denv) denv)))
             (t
              (let ((next-arg (first rest-args))
                    (rest-args (rest rest-args)))
                (lambda ()
                  (interpret/k next-arg
                               env
                               store
                               (make-arg2-cont fun
                                               env
                                               rest-args
                                               store
                                               cont
                                               (cons v vs)
                                               denv)
                               denv)))))))
    (<end-cont>
     (lambda () v))
    (<fun-cont>
     (with-slots (args cont denv env store) cont
       (etypecase v
         (<value-cont>
          (with-slots (cont) v
            (assert (= (length args) 1) nil "续延只能有一个参数")
            (lambda ()
              (apply-continuation cont (first args)))))
         (<value-fun>
          (let ((body (slot-value v 'body))
                (names (slot-value v 'args))
                ;; 这里不能用当前的环境来扩展，不然就变成动态作用域了。要基于函数被定义时的环境来添上形参。
                (new-env (value-fun-env v))
                (new-denv denv))
            (assert (= (length names) (length args)) nil "形参和实参的数量必须相等：~D != ~D" (length names) (length args))
            ;; TODO: 这里用 mapcar 只为了副作用怪怪的。
            (mapcar #'(lambda (name arg-val)
                        (let* ((location (put-store store arg-val))
                               (binding (make-instance '<binding>
                                                       :location location
                                                       :name name)))
                          (if (is-dynamic name)
                              (setf new-denv (extend-env binding new-denv))
                              (setf new-env (extend-env binding new-env)))))
                    names (nreverse args)) ; args 是用 cons 构造的，次序与形参列表相反。
            (lambda ()
              (interpret/k body
                           new-env
                           store
                           cont
                           new-denv))))
         (<value-primitive>
          (apply (value-primitive-f v) `(,@(nreverse args) ,cont))))))
    (<lhs-cont>
     (with-slots (cont denv env r store) cont
       (lambda ()
         (interpret/k r env store (make-rhs-cont v cont) denv))))
    (<print-cont>
     (with-slots (cont) cont
       (etypecase v
         (<value-bool>
          (format t "~A~%" (if (value-bool-val v) "true" "false")))
         (<value-num>
          (format t "~D~%" (value-num-n v))))
       (lambda ()
         (apply-continuation cont v))))
    (<progn-cont>
     (with-slots (denv forms saved-cont saved-env store) cont
       (cond ((null forms)
              (apply-continuation saved-cont v))
             (t
              ;; 如果 v 不是最后一个表达式的值，那么是不会使用的。
              (interpret/k (first forms) saved-env store
                           (make-instance '<progn-cont>
                                          :denv denv
                                          :forms (rest forms)
                                          :saved-cont saved-cont
                                          :saved-env saved-env
                                          :store store)
                           denv)))))
    (<rhs-cont>
     (with-slots (cont lhs) cont
       (lambda ()
         (apply-continuation cont (num+ lhs v)))))
    (<setf-cont>
     (with-slots (location saved-cont store var) cont
       (update-store store location v)
       (lambda ()
         (apply-continuation saved-cont v))))
    (<test-cont>
     (with-slots (denv else env saved-cont store then) cont
       (lambda ()
         (interpret/k (if (value-bool-val v) then else) env store saved-cont denv))))))

(defun make-arg2-cont (fun env rest-args store cont vs denv)
  (make-instance '<arg2-cont>
                 :cont cont
                 :denv denv
                 :env env
                 :fun fun
                 :rest-args rest-args
                 :store store
                 :vs vs))

(defun make-end-cont ()
  (make-instance '<end-cont>))

(defun make-fun-cont (args env store cont denv)
  "创建一个表示求值了函数位置的值之后要做的计算的续延。"
  (make-instance '<fun-cont>
                 :args args
                 :cont cont
                 :denv denv
                 :env env
                 :store store))

(defun make-lhs-cont (r env store cont denv)
  "创建一个表示求值了加法运算的左操作数后要做的计算的续延。"
  (make-instance '<lhs-cont> :cont cont :denv denv :env env :r r :store store))

(defun make-print-cont (cont)
  (make-instance '<print-cont> :cont cont))

(defun make-rhs-cont (lhs cont)
  "创建一个表示求值了加法运算的右操作数后要做的计算的续延。"
  (make-instance '<rhs-cont> :cont cont :lhs lhs))
;;; 续延相关 end

(defun is-dynamic (var)
  "判定一个变量是否为动态作用域的。"
  (check-type var symbol)
  (and *dynamic-variables*
       (nth-value 1 (gethash var *dynamic-variables*))))

(defun interpret/k (ast env store cont denv)
  "CPS版本的interpret解释器，其中CONT表示“接下来的运算”。

参数 DENV 表示存储着动态作用域变量的值的环境。"
  (declare (type <core> ast))
  (declare (type env env))
  (declare (type store store))
  (declare (type continuation cont))
  (etypecase ast
    (<core-app>
     (with-slots (args fun) ast
       (cond ((= (length args) 0)
              ;; 既然没有参数要求值，便可以直接求值函数位置的表达式并准备调用了。
              (lambda ()
                (interpret/k fun env store
                             (make-fun-cont nil env store cont denv) denv)))
             (t
              (let ((next-arg (first args))    ; 第一个要被求值的参数表达式。
                    (rest-args (rest args)))   ; 剩下的待求值的参数表达式组成的列表。
                (lambda ()
                  (interpret/k next-arg env store (make-arg2-cont fun env rest-args store cont nil denv) denv)))))))
    (<core-bool>
     (with-slots (id) ast
       (let (bv rv)                    ; rv 表示要传入给续延的值，bv 表示根据字面量映射出来的 nil 或 t。
         (assert (member id '(false true) :test 'string=))
         (setf bv (if (string= id 'false) nil t))
         (setf rv (make-instance '<value-bool> :val bv))
         (lambda ()
           (apply-continuation cont rv))))) ; 常量的行为都是直接应用当前续延。
    (<core-call/cc>
     (with-slots (body var) ast
       ;; 新建一个环境，在这个环境中，将当前续延绑定到变量 VAR 上，然后求值表达式 BODY，并将结果传递给当前续延。
       (let* ((binding (make-instance '<binding>
                                      :location (put-store store (make-instance '<value-cont>
                                                                                :cont cont))
                                      :name (core-id-s var)))
              (env (extend-env binding env)))
         (lambda ()
           (interpret/k body env store cont denv)))))
    (<core-defun>
     (error "<CORE-DEFUN> 不允许出现在运行时"))
    (<core-id>
     (with-slots (s) ast
       (lambda ()
         (let ((location (if (is-dynamic s)
                             (lookup-env s denv)
                             (lookup-env s env))))
           (apply-continuation cont (fetch-store store location))))))
    (<core-if>
     (with-slots (else test then) ast
       (lambda ()
         (interpret/k test env store
                      (make-instance '<test-cont>
                                     :denv denv
                                     :else else
                                     :env env
                                     :saved-cont cont
                                     :store store
                                     :then then)
                      denv))))
    (<core-lambda>
     (with-slots (body par) ast
       (lambda ()
         (apply-continuation cont (make-instance '<value-fun> :args (list par) :body body :env env)))))
    (<core-num>
     (with-slots (n) ast
       (lambda ()
         (apply-continuation cont (make-instance '<value-num> :n n)))))
    (<core-plus>
     (with-slots (l r) ast
       (lambda ()
         (interpret/k l env store (make-lhs-cont r env store cont denv) denv))))
    (<core-print>
     (with-slots (arg) ast
       (lambda ()
         (interpret/k arg env store (make-print-cont cont) denv))))
    (<core-progn>
     (with-slots (forms) ast
       (assert (> (length forms) 0))
       (lambda ()
         (interpret/k (first forms) env store
                      (make-instance '<progn-cont>
                                     :denv denv
                                     :forms (rest forms)
                                     :saved-cont cont
                                     :saved-env env
                                     :store store)
                      denv))))
    (<core-setf>
     (let* ((val-expr (core-setf-val-expr ast))
            (var (core-setf-var ast))
            (location (lookup-env var env)))
       (lambda ()
         (interpret/k val-expr env store
                      (make-instance '<setf-cont>
                                     :location location
                                     :saved-cont cont
                                     :store store
                                     :var var)
                      denv))))))

;;; 具体语法相关 begin
(defun expand-or-to-if (expr)
  "将 OR 语句 EXPR 替换为等价的 IF 语句。"
  (assert (eq (first expr) 'or))
  (let ((body (rest expr)))
    (when (null body)
      (return-from expand-or-to-if nil))

    (let ((forms (rest body))         ; or 之后第二个元素开始的所有表达式组成的列表。
          (test (first body))          ; or 之后的第一个表达式。
          (val-var (gensym)))
      (if (null forms)
          test
          `((lambda (,val-var)          ; 目标语言尚未支持 LET 语句，用 LAMBDA 代替。
              (if ,val-var
                  ,val-var
                  ,(expand-or-to-if (cons 'or forms))))
            ,test)))))

(defun expand-cond-to-if (expr)
  "将 COND 语句 EXPR 替换为等价的 IF 语句。"
  (assert (eq (first expr) 'cond))
  (let ((forms (rest expr)))
    (when (null forms)
      (return-from expand-cond-to-if nil))

    (let ((form (first forms))
          (forms (rest forms)))
      (destructuring-bind (test then) form
        (cond ((null forms)
               `(if ,test
                    ,then
                    0))                 ; TODO: 这里用一个更合适的值替换掉 0 作为返回值。
              (t
               `(if ,test
                    ,then
                    ,(expand-cond-to-if (cons 'cond forms)))))))))

(defun expand-let-to-lambda (expr)
  "将 LET 语句视为 LAMBDA 语法的宏进行展开。"
  (destructuring-bind (bindings &rest body) (rest expr)
    ;; 对 rest 部分做解构，是因为 first 部分固定为符号 let。
    (let ((vals (mapcar #'cadr bindings))
          (vars (mapcar #'car bindings)))
      `((lambda ,vars ,@body) ,@vals))))

(defun is-x-list (expr x)
  "检查 EXPR 是否为一个以 X 为 car 的列表。"
  (and (listp expr) (eq (first expr) x)))

(defun parse-defun-syntax (expr)
  "解析一个 defun 语法的函数名、参数列表，以及函数体部分，生成一个 <core-defun> 类的实例对象。"
  (destructuring-bind (name parameters &rest body) expr
    (make-instance '<core-defun>
                   ;; 不管三七二十一，用一个 call/cc 来定义函数 return，来支持提前返回。
                   :body (make-instance '<core-call/cc>
                                        :body (make-instance '<core-progn>
                                                             :forms (mapcar #'parse-concrete-syntax body))
                                        :var (make-instance '<core-id> :s 'return))
                   :name name
                   :parameters parameters)))

(defun parse-defvar-syntax (expr)
  "解析一个 defvar 语法的变量名和初值表达式，生成一个 <core-defvar> 类的实例对象。"
  (destructuring-bind (var val) (rest expr)
    (assert (symbolp var) nil "变量名必须为一个符号，实际为：~S" var)
    (let ((s (symbol-name var)))
      (when (or (char/= (char s 0) #\*)
                (char/= (char s (1- (length s))) #\*))
        (error "变量名必须以星号包裹，实际为：~A" s)))

    (unless (atom val)
      (error "初值必须为一个原子类型，实际为：~S" val))

    (make-instance '<core-defvar>
                   :val (parse-concrete-syntax val)
                   :var var)))

(defun parse-concrete-syntax (expr)
  "解析作为具体语法的S表达式 EXPR，返回对象的抽象语法 <CORE> 类的实例对象。"
  (cond ((and (listp expr) (eq (first expr) 'lambda))
         (destructuring-bind (_ parameters . body)
             expr
           (declare (ignorable _))      ; TODO: CL 一处值得改进的地方，即无法用下划线来便捷地表达“不使用的变量”这一意图。
           (assert (= (length parameters) 1) nil "仅支持一个参数：~S" parameters)
           (assert (> (length body) 0) nil "至少要包含一个表达式：~S" body)
           (make-instance '<core-lambda>
                          :body (make-instance '<core-progn> :forms (mapcar #'parse-concrete-syntax body))
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
        ((and (listp expr) (eq (first expr) 'if))
         (destructuring-bind (_ test then else)
             expr
           (declare (ignorable _))
           (make-instance '<core-if>
                          :else (parse-concrete-syntax else)
                          :test (parse-concrete-syntax test)
                          :then (parse-concrete-syntax then))))
        ((and (listp expr) (eq (first expr) 'defun))
         (parse-defun-syntax (rest expr)))
        ((and (listp expr) (eq (first expr) 'let))
         ;; 将每一个绑定的 car 部分提取为形参列表，将 cadr 部分提取为实参列表。
         (parse-concrete-syntax (expand-let-to-lambda expr)))
        ((and (listp expr) (eq (first expr) 'or))
         ;; 将 OR 语句作为宏来实现，展开为上面已经支持的 IF 语句。
         (let ((expanded (expand-or-to-if expr)))
           (parse-concrete-syntax expanded)))
        ((and (listp expr) (eq (first expr) 'cond))
         (parse-concrete-syntax (expand-cond-to-if expr)))
        ((and (listp expr) (eq (first expr) 'setf))
         (destructuring-bind (_ var val-expr) expr
           (declare (ignorable _))
           (assert (symbolp var) nil "暂时仅支持给变量赋值：~A" var)
           (make-instance '<core-setf>
                          :val-expr (parse-concrete-syntax val-expr)
                          :var var)))
        ((and (listp expr) (eq (first expr) 'labels))
         (destructuring-bind (_ &rest forms) expr
           (declare (ignorable _))
           (make-instance '<core-labels>
                          :definitions (mapcar #'parse-defun-syntax forms))))
        ((is-x-list expr 'defvar)
         (parse-defvar-syntax expr))
        ;; 特殊操作符都需要在此之前进行判断。
        ((listp expr)
         (destructuring-bind (fun . args)
             expr
           (make-instance '<core-app>
                          :args (mapcar #'parse-concrete-syntax args)
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

;;; 内置函数相关 begin
(defun make-233-arithmetic-wrapper (f)
  "实现一个 CL 中的函数 F 的包装函数。"
  (check-type f function)
  (lambda (&rest args)
    (let* ((ns (butlast args))
           (k (first (last args)))
           (rv (apply f (mapcar #'value-num-n ns))))
      (lambda ()
        (apply-continuation k
                            (etypecase rv
                              (boolean (make-instance '<value-bool> :val rv))
                              (number (make-instance '<value-num> :n rv))
                              (string (make-instance '<value-str> :content rv))))))))

(defun %233-check-is-string-equal (s1 s2 k)
  "检查两个 233-lisp 中的字符串是否相同。"
  (check-type s1 <value-str>)
  (check-type s2 <value-str>)
  (check-type k <cont>)
  (apply-continuation k (make-instance '<value-bool>
                                       :val (string= (value-str-content s1) (value-str-content s2)))))

(defun %233-reverse-string (str k)      ; TODO: 暂时没想好怎么给 233-lisp 的内置函数起名字，先用 %233- 作为统一前缀。
  "前后倒置一个 233-lisp 中的字符串。"
  (check-type str <value-str>)
  (check-type k <cont>)
  (let ((new-s (make-instance '<value-str>
                              :content (reverse (value-str-content str)))))
    (apply-continuation k new-s)))

(defun make-prelude-env (store)
  "创建一个仅在空环境的基础上添加了 233-lisp 内置函数的环境。"
  (check-type store store)
  (let ((built-ins (list (cons '= (make-233-arithmetic-wrapper #'=))
                         (cons '> (make-233-arithmetic-wrapper #'>))
                         (cons 'mod (make-233-arithmetic-wrapper #'mod))
                         (cons '>= (make-233-arithmetic-wrapper #'>=))
                         (cons 'evenp (make-233-arithmetic-wrapper #'evenp))
                         (cons '<= (make-233-arithmetic-wrapper #'<=))
                         (cons '/ (make-233-arithmetic-wrapper #'/))
                         (cons '- (make-233-arithmetic-wrapper #'-))
                         (cons 'itoa (make-233-arithmetic-wrapper #'(lambda (n)
                                                                      (format nil "~D" n))))
                         (cons 'reverse #'%233-reverse-string)
                         (cons 'string= #'%233-check-is-string-equal)))
        (env (make-empty-env)))
    (dolist (pair built-ins)
      (destructuring-bind (name . fun) pair
        (let* ((fun (make-instance '<value-primitive> :f fun))
               (location (put-store store fun))
               (binding (make-instance '<binding> :location location :name name)))
          (setf env (extend-env binding env)))))
    env))
;;; 内置函数相关 end

;;; 解决调用栈耗尽相关 begin
(defun trampoline (bounce)
  "模仿《EOPL》第 5.2 章节实现的函数，驱动解释器返回的无参匿名函数持续执行，直到返回最终结果。"
  ;; 参考资料：https://eli.thegreenplace.net/2017/on-recursion-continuations-and-trampolines/
  (let ((v bounce))
    (loop
      (if (functionp v)
          (setf v (funcall v))
          (return-from trampoline v)))))
;;; 解决调用栈耗尽相关 end

;;; 运行脚本相关 begin
(defun read-expressions (stream)
  "从文件流 STREAM 中读取出所有的表达式并组成一个列表。"
  (let ((eof (gensym))
        (exprs '()))
    (loop
      (let ((expr (read stream nil eof)))
        (when (eq expr eof)
          (return-from read-expressions (nreverse exprs)))

        (push expr exprs)))))

(defvar *dynamic-variables* nil
  "记录着当前解释器中属于动态作用域的变量的哈希表。")

(defun add-definition-to-env (expr env denv store)
  "使表达式 EXPR 的定义在相应的环境 ENV 或 DENV 中生效。

STORE 为 ENV 和 DENV 共同使用的存储容器。"
  (let ((ast (parse-concrete-syntax expr)))
    (assert (or (typep ast '<core-defun>)
                (typep ast '<core-defvar>)
                (typep ast '<core-labels>)) nil "顶层语句只能是 DEFUN 或 LABELS：~A" ast)
    (etypecase ast
      (<core-defun>
       (with-slots (body name parameters) ast
         (let* ((fun (make-instance '<value-fun>
                                    :args parameters
                                    :body body
                                    :env env))
                (location (put-store store fun))
                (binding (make-instance '<binding>
                                        :location location
                                        :name name)))
           (setf env (extend-env binding env))
           ;; 为了可以在递归函数的词法环境中找到自身函数名的定义，必须在闭包中保存被自身扩展后的词法环境。
           (setf (value-fun-env fun) env))))
      (<core-defvar>
       (with-slots (val var) ast
         (let* ((v (etypecase val
                     ;; TODO: 暂时只处理数值字面量。
                     (<core-num> (make-instance '<value-num>
                                                :n (core-num-n val)))))
                (location (put-store store v))
                (binding (make-instance '<binding>
                                        :location location
                                        :name var)))
           (setf denv (extend-env binding denv))
           (setf (gethash var *dynamic-variables*) t))))
      (<core-labels>
       (with-slots (definitions) ast
         (let* ((fns (mapcar #'(lambda (definition)
                                 (with-slots (body parameters) definition
                                   (make-instance '<value-fun>
                                                  :args parameters
                                                  :body body
                                                  :env env)))
                             definitions))
                (names (mapcar #'(lambda (definition)
                                   (slot-value definition 'name))
                               definitions)))
           (mapcar #'(lambda (fn name)
                       (let* ((location (put-store store fn))
                              (binding (make-instance '<binding>
                                                      :location location
                                                      :name name)))
                         (setf env (extend-env binding env))))
                   fns names)
           (dolist (fn fns)
             (setf (value-fun-env fn) env)))))))
  (values env denv))

(defun load-source-file (stream)
  "读取文件流 STREAM 中的代码并调用其中定义的 MAIN 函数。"
  (assert (input-stream-p stream) nil "STREAM 必须为一个输入流")
  (let* ((*dynamic-variables* (make-hash-table))
         (store (make-empty-store))
         (prelude (make-prelude-env store))
         (denv (make-empty-env))
         (env prelude)
         (exprs (read-expressions stream)))
    (dolist (expr exprs)
      (setf (values env denv)
            (add-definition-to-env expr env denv store)))
    (trampoline
     (interpret/k (parse-concrete-syntax '(main))
                  env
                  store
                  (make-end-cont)
                  denv))))
;;; 运行脚本相关 end
