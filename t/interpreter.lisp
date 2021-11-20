(in-package #:com.liutos.liutcl.interpreter.test)

(test interpret
  "测试interpret函数。"
  (is (value-equal-p (make-instance '<value-num> :n 233) (interpret (make-instance '<core-num> :n 233))))
  (is (value-equal-p (make-instance '<value-num> :n 6) (interpret (make-instance '<core-plus>
                                                                                 :l (make-instance '<core-num> :n 1)
                                                                                 :r (make-instance '<core-plus>
                                                                                                   :l (make-instance '<core-num> :n 2)
                                                                                                   :r (make-instance '<core-num> :n 3)))))))

