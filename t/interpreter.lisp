(in-package #:com.liutos.liutcl.interpreter.test)

(defun interpret (expr env)
  "为了避免修改太多代码，对COM.LIUTOS.LIUTCL.INTERPRETER::INTERPRET做一层封装。"
  (com.liutos.liutcl.interpreter::interpret expr env (com.liutos.liutcl.interpreter::make-empty-store)))

(test interpret
  "测试interpret函数。"
  (is (value-equal-p (make-instance '<value-num> :n 233) (interpret (make-instance '<core-num> :n 233) (make-empty-env))))
  (is (value-equal-p (make-instance '<value-num> :n 6) (interpret (make-instance '<core-plus>
                                                                                 :l (make-instance '<core-num> :n 1)
                                                                                 :r (make-instance '<core-plus>
                                                                                                   :l (make-instance '<core-num> :n 2)
                                                                                                   :r (make-instance '<core-num> :n 3))) (make-empty-env))))
  (let ((env (make-empty-env))
        (binding (make-instance '<binding>
                                :name 'foo
                                :val (make-instance '<value-num> :n 666))))
    (is (value-equal-p
         (make-instance '<value-num> :n 666)
         (interpret (make-instance '<core-id> :s 'foo)
                    (extend-env binding env)))))
  (let ((env (make-empty-env))
        (binding (make-instance '<binding>
                                :name 'add1
                                :val (make-instance '<value-fun>
                                                    :arg 'x
                                                    :body (make-instance '<core-plus>
                                                                         :l (make-instance '<core-id> :s 'x)
                                                                         :r (make-instance '<core-num> :n 1))))))
    (is (value-equal-p
         (make-instance '<value-num> :n 233)
         (interpret (make-instance '<core-app>
                                   :arg (make-instance '<core-num> :n 232)
                                   :fun (make-instance '<core-id> :s 'add1))
                    (extend-env binding env)))))
  (let ((env (make-empty-env))
        (binding (make-instance '<binding>
                                :name 'add1
                                :val (make-instance '<value-fun>
                                                    :arg 'x
                                                    :body (make-instance '<core-id> :s 'x)))))
    (signals wrong-type
      (interpret (make-instance '<core-plus>
                                :l (make-instance '<core-num> :n 232)
                                :r (make-instance '<core-id> :s 'add1))
                 (extend-env binding env))))
  (is (value-equal-p
       (make-instance '<value-num> :n 233)
       (interpret (make-instance '<core-app>
                                 :arg (make-instance '<core-num> :n 232)
                                 :fun (make-instance '<core-lambda>
                                                     :body (make-instance '<core-plus>
                                                                          :l (make-instance '<core-id> :s 'x)
                                                                          :r (make-instance '<core-num> :n 1))
                                                     :par 'x))
                  (make-empty-env))))
  (let* ((inner (make-instance '<core-lambda>
                               :body (make-instance '<core-plus>
                                                    :l (make-instance '<core-id> :s 'x)
                                                    :r (make-instance '<core-id> :s 'y))
                               :par 'y))
         (outer (make-instance '<core-lambda>
                               :body (make-instance '<core-app>
                                                    :arg (make-instance '<core-num> :n 2)
                                                    :fun inner)
                               :par 'x)))
    (is (value-equal-p
         (make-instance '<value-num> :n 3)
         (interpret (make-instance '<core-app> :arg (make-instance '<core-num> :n 1) :fun outer)
                    (make-empty-env))))))
