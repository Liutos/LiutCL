(in-package #:com.liutos.liutcl.interpreter)

(defun |"-reader| (stream char)
  "读取字符串并识别其中的C语言风格的转义序列。"
  (declare (ignorable char))
  (let ((cs '()))
    (loop
      (let ((c (read-char stream)))
        (cond ((char= c #\")
               (return-from |"-reader|
                 (coerce (nreverse cs) 'string)))
              ((char= c #\\)
               (let ((c (read-char stream)))
                 (ecase c
                   (#\n (push #\Newline cs))
                   (#\t (push #\Tab cs)))))
              (t (push c cs)))))))

(defun read-source-code (stream)
  (let ((*package* (find-package '#:com.liutos.liutcl.interpreter))
        (*readtable* (copy-readtable nil)))
    (set-macro-character #\" '|"-reader|)
    (let ((eof (gensym))
          (exprs '()))
      (loop
        (let ((expr (read stream nil eof)))
          (when (eq expr eof)
            (return-from read-source-code (cons 'progn (nreverse exprs))))

          (push expr exprs))))))

(defun read-source-code-from-string (str)
  (check-type str string)
  (with-input-from-string (s str)
    (read-source-code s)))
