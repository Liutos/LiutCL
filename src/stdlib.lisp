(in-package #:com.liutos.liutcl.interpreter)

(defun make-dict (&rest key-values)
  "创建一个233-lisp中的字典对象。"
  (unless (evenp (length key-values))
    (error "参数列表的长度必须为偶数"))

  (let ((ht (make-hash-table :test #'equal)))
    (do ((i 0 (+ i 2)))
        ((= i (length key-values)) ht)
      (let ((key (nth i key-values))
            (val (nth (1+ i) key-values)))
        (setf (gethash key ht) val)))))
