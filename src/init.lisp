;;; 内置函数相关
(in-package #:com.liutos.liutcl.interpreter)

(defun make-233-arithmetic-wrapper (f)
  "实现一个 CL 中的函数 F 的包装函数。"
  (check-type f function)
  (lambda (&rest args)
    (let* ((ns (butlast args))
           (k (first (last args))))
      (dolist (n ns)
        (unless (typep n '<value-num>)
          (error 'wrong-type :actual (type-of n) :expected '<value-num>)))

      (let ((rv (apply f (mapcar #'value-num-n ns))))
        (lambda ()
          (apply-continuation k
                              (etypecase rv
                                (boolean (make-instance '<value-bool> :val rv))
                                (number (make-instance '<value-num> :n rv))
                                (string (make-instance '<value-str> :content rv)))))))))

(defun %233-check-is-string-equal (s1 s2 k)
  "检查两个 233-lisp 中的字符串是否相同。"
  (check-type s1 <value-str>)
  (check-type s2 <value-str>)
  (check-type k <cont>)
  (apply-continuation k (make-instance '<value-bool>
                                       :val (string= (value-str-content s1) (value-str-content s2)))))

(defun %233-print (v k)
  "将值 V 打印到标准输出流中。"
  (check-type v <value>)
  (check-type k <cont>)
  (etypecase v
    (<value-bool>
     (format t "~A~%" (if (value-bool-val v) "true" "false")))
    (<value-num>
     (format t "~D~%" (value-num-n v)))
    (<value-str>
     (format t "~A~%" (value-str-content v))))
  (apply-continuation k v))

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
                         (cons '+ (make-233-arithmetic-wrapper #'+))
                         (cons '< (make-233-arithmetic-wrapper #'<))
                         (cons '* (make-233-arithmetic-wrapper #'*))
                         (cons 'itoa (make-233-arithmetic-wrapper #'(lambda (n)
                                                                      (format nil "~D" n))))
                         (cons 'print #'%233-print)
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
