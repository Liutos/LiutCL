(in-package #:com.liutos.liutcl.interpreter)

(defun transform-let (expr)
  "将一个LET语法分解为一系列的绑定和一个PROGN语法。"
  (check-type expr cons)
  ;; TODO: 这里用一个类似while循环的语法来代替递归应当是更可读的。
  (labels ((aux (bindings forms stack)
             (unless forms
               (return-from aux
                 (values bindings `(progn ,@(nreverse stack)))))

             (let ((next (first forms)))
               (cond ((eq next '=)
                      (assert (> (length stack) 0))
                      (let ((var (pop stack))
                            (val (second forms)))
                        (if (> (length stack) 0)
                            (return-from aux
                              (values bindings
                                      `(progn ,@(nreverse stack)
                                              (let ,var = ,val
                                                ,@(rest (rest forms))))))
                            (aux (cons (cons var val) bindings)
                                 (rest (rest forms))
                                 stack))))
                     (t
                      (aux bindings (rest forms) (push next stack)))))))
    (aux '() (rest expr) '())))
