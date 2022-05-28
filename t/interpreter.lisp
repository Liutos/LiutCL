(in-package #:com.liutos.liutcl.interpreter.test)

(defun interpret (expr env &optional store)
  "为了避免修改太多代码，对COM.LIUTOS.LIUTCL.INTERPRETER::INTERPRET做一层封装。"
  (let ((store (or store (make-empty-store))))
    (com.liutos.liutcl.interpreter::interpret/k expr env store (com.liutos.liutcl.interpreter::make-end-cont))))

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
         (binding (make-instance '<binding>
                                 :location (put-store store (make-instance '<value-fun>
                                                                           :args '(x)
                                                                           :body (make-instance '<core-plus>
                                                                                                :l (make-instance '<core-id> :s 'x)
                                                                                                :r (make-instance '<core-num> :n 1))))
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
                                                                           :body (make-instance '<core-id> :s 'x)))
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
         (interpret-concrete '(or (> 2 1) (> 2 3)) (make-prelude-env store) store)))))
