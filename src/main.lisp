(in-package #:com.liutos.liutcl.interpreter)

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

(defun add-definition-to-env (ast env denv store)
  "使经过解析的表达式 AST 的定义在相应的环境 ENV 或 DENV 中生效。

STORE 为 ENV 和 DENV 共同使用的存储容器。"
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
           (setf (value-fun-env fn) env))))))
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
            (add-definition-to-env (parse-concrete-syntax expr) env denv store)))
    (trampoline
     (interpret/k (parse-concrete-syntax '(main))
                  env
                  store
                  (make-end-cont)
                  denv))))

(defun start-repl ()
  "启动一个 read-evaluate-print 的循环。"
  (let* ((eof (gensym))
         (*dynamic-variables* (make-hash-table))
         (store (make-empty-store))
         (prelude (make-prelude-env store))
         (denv (make-empty-env))
         (env prelude))
    (loop
      (format t "233-USER> ")
      (finish-output)
      (let ((expr (read *standard-input* nil eof)))
        (when (eql expr eof)
          (return-from start-repl))

        (let ((ast (parse-concrete-syntax expr)))
          (if (or (typep ast '<core-defun>)
                  (typep ast '<core-defvar>)
                  (typep ast '<core-labels>))
              (setf (values env denv)
                    (add-definition-to-env ast env denv store))
              (trampoline
               (interpret/k ast
                            env
                            store
                            (make-print-cont
                             (make-end-cont))
                            denv))))))))
;;; 运行脚本相关 end
