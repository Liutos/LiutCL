(in-package #:com.liutos.liutcl.interpreter.test)

(defun interpret (expr env &optional store)
  "为了避免修改太多代码，对COM.LIUTOS.LIUTCL.INTERPRETER::INTERPRET做一层封装。"
  (let ((store (or store (make-empty-store))))
    (trampoline
     (com.liutos.liutcl.interpreter::interpret/k expr env store (com.liutos.liutcl.interpreter::make-end-cont)
                                                 (make-empty-env)))))

(defun interpret-concrete (expr env &optional store)
  (interpret (com.liutos.liutcl.interpreter::parse-concrete-syntax expr)
             env
             store))

(test interpret
  "测试interpret函数。"
  (is (value-equal-p (make-instance '<value-num> :n 233) (interpret-concrete 233 (make-empty-env))))
  (is (value-equal-p (make-instance '<value-num> :n 6) (interpret-concrete (+ 1 (+ 2 3)) (make-empty-env))))
  (let* ((env (make-empty-env))
         (store (make-empty-store))
         (binding (make-instance '<binding>
                                 :location (put-store store (make-instance '<value-num> :n 666))
                                 :name 'foo)))
    (is (value-equal-p
         (make-instance '<value-num> :n 666)
         (interpret-concrete 'foo
                             (extend-env binding env)
                             store))))
  (let* ((env (make-empty-env))
         (store (make-empty-store))
         (fun (make-instance '<value-fun>
                             :args '(x)
                             :body (make-instance '<core-plus>
                                                  :l (make-instance '<core-id> :s 'x)
                                                  :r (make-instance '<core-num> :n 1))
                             :env env))
         (binding (make-instance '<binding>
                                 :location (put-store store fun)
                                 :name 'add1)))
    (is (value-equal-p
         (make-instance '<value-num> :n 233)
         (interpret-concrete '(add1 232)
                             (extend-env binding env)
                             store))))
  (let* ((env (make-empty-env))
         (store (make-empty-store))
         (binding (make-instance '<binding>
                                 :location (put-store store (make-instance '<value-fun>
                                                                           :args '(x)
                                                                           :body (make-instance '<core-id> :s 'x)
                                                                           :env env))
                                 :name 'add1)))
    (signals wrong-type
      (interpret-concrete '(+ 232 add1)
                          (extend-env binding env)
                          store)))
  (is (value-equal-p
       (make-instance '<value-num> :n 233)
       (interpret-concrete '((lambda (x) (+ x 1)) 232)
                           (make-empty-env))))
  (is (value-equal-p
         (make-instance '<value-num> :n 3)
         (interpret-concrete '((lambda (x) ((lambda (y) (+ x y)) 2)) 1)
                             (make-empty-env))))
  (is (string=
       (concatenate 'string "233" (list #\Newline))
       (with-output-to-string (*standard-output*)
         (interpret-concrete '(print 233) (make-empty-env)))))
  (is (value-equal-p
       (make-instance '<value-num> :n 233)
       (interpret-concrete '(call/cc (k) (+ 1 (k 233))) (make-empty-env))))
  (is (value-equal-p
       (make-instance '<value-bool> :val nil)
       (interpret-concrete 'false (make-empty-env))))
  (is (string=
       (concatenate 'string "false" (list #\Newline))
       (with-output-to-string (*standard-output*)
         (interpret-concrete '(print false) (make-empty-env)))))
  (is (value-equal-p
       (make-instance '<value-bool> :val t)
       (let ((store (make-empty-store)))
         (interpret-concrete '(> 2 1) (make-prelude-env store) store))))
  (is (value-equal-p
       (make-instance '<value-bool> :val t)
       (let ((store (make-empty-store)))
         (interpret-concrete '(= 3 (+ 1 2)) (make-prelude-env store) store))))
  (is (value-equal-p
       (make-instance '<value-num> :n 2)
       (interpret-concrete '(if true 2 1) (make-empty-env))))
  (is (value-equal-p
       (make-instance '<value-num> :n 1)
       (interpret-concrete '(if false 2 1) (make-empty-env)))))

(test load-source-file
  "测试 load-source-file 函数。"
  (is (string=
       (concatenate 'string "233" (list #\Newline))
       (with-output-to-string (*standard-output*)
         (with-input-from-string (s "(defun main () (print 233))")
           (com.liutos.liutcl.interpreter:load-source-file s))))))

(test arithmetic
  "测试算术运算函数。"
  (is (value-equal-p
       (make-instance '<value-num> :n 1)
       (let ((store (make-empty-store)))
         (interpret-concrete '(mod 10 3) (make-prelude-env store) store)))))

(test macro
  "测试语言内置的宏。"
  (is (value-equal-p
       (make-instance '<value-bool> :val nil)
       (let ((store (make-empty-store)))
         (interpret-concrete '(or (> 1 2) (> 2 3)) (make-prelude-env store) store))))
  (is (value-equal-p
       (make-instance '<value-bool> :val t)
       (let ((store (make-empty-store)))
         (interpret-concrete '(or (> 2 1) (> 2 3)) (make-prelude-env store) store))))
  (is (value-equal-p
       (make-instance '<value-num> :n 233)
       (let ((store (make-empty-store)))
         (interpret-concrete '(cond ((> 1 2) 666) ((> 2 1) 233)) (make-prelude-env store) store)))))

(test progn
  "测试函数体内多个语句顺序求值的特性。"
  (let* (v
         (out (with-output-to-string (*standard-output*)
                (let ((store (make-empty-store)))
                  (setf v (interpret-concrete '((lambda (x) (print x) 1) 233) (make-prelude-env store) store))))))
    (is (string=
         (concatenate 'string "233" (list #\Newline))
         out))
    (is (value-equal-p
         (make-instance '<value-num> :n 1)
         v))))

(test assignment
  "测试语言的赋值特性。"
  (is (value-equal-p
       (make-instance '<value-num> :n 233)
       (let ((store (make-empty-store)))
         (interpret-concrete '((lambda (x) (setf x 233) x) 1) (make-prelude-env store) store)))))

(test mutual-recursion
  "测试定义相互递归的函数。"
  (is (string=
       (concatenate 'string "true" (list #\Newline) "false" (list #\Newline))
       (with-output-to-string (*standard-output*)
         (with-input-from-string (s "(labels
    (my-evenp (n)
              (if (= n 0)
                  true
                  (if (= n 1)
                      false
                      (my-oddp (- n 1)))))
  (my-oddp (n)
           (if (= n 0)
               false
               (if (= n 1)
                   true
                   (my-evenp (- n 1))))))

(defun main ()
  (print (my-evenp 2))
  (print (my-evenp 3)))
")
           (com.liutos.liutcl.interpreter:load-source-file s))))))

(test let-form
  "测试 LET 语法定义局部变量。"
  (is (value-equal-p
       (make-instance '<value-num> :n 233)
       (let ((store (make-empty-store)))
         (interpret-concrete '(let ((x 1))
                               (setf x 233)
                               x)
                             (make-prelude-env store) store)))))

(test dynamic-scope
  "测试动态作用域特性。"
  (is (string=
       (format nil "233~%")
       (with-output-to-string (*standard-output*)
         (with-input-from-string (s "(defvar *x* 1)

(defun foo ()
  (print *x*))

(defun main ()
  (let ((*x* 233))
    (foo)))")
           (com.liutos.liutcl.interpreter:load-source-file s))))))

(test palindromic
  "测试一个判断回文数的函数。"
  (is (string=
       (format nil "true~%false~%")
       (with-output-to-string (*standard-output*)
         (with-input-from-string (s "(defun palindromicp (number)
  (let ((str (itoa number)))
    (string= str (reverse str))))

(defun main ()
  (print (palindromicp 232))
  (print (palindromicp 233)))")
           (com.liutos.liutcl.interpreter:load-source-file s))))))

(test start-repl
  "测试 REPL 运作是否正常。"
  (is (string=
       (format nil "233-USER> 233~%233-USER> ")
       (with-output-to-string (*standard-output*)
         (with-input-from-string (*standard-input* "(print 233)")
           (com.liutos.liutcl.interpreter:start-repl))))))
