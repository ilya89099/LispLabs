(defun process-text(text)
    (let ((result NIL) (cur-char #\0))
    (dolist (str text) (setq result (append result (list (copy-seq str)))))
    (multiple-value-bind (text-pos word-pos)
        (loop for i upfrom 0 to (- (length text) 1)
            do
            (if (position #\+ (nth i text))
                (return (values i (position #\+ (nth i text))))))
        (if text-pos
            (loop for i upfrom 0 to text-pos
                do
                (loop for j upfrom 0 to (if (= i text-pos) word-pos (- (length (nth i result)) 1))
                    do
                    (setq cur-char (aref (nth i result) j))
                    (if (and (char>= cur-char #\0) (char<= cur-char #\9))
                        (setf (aref (nth i result) j) #\-))))))   
    result)
)


(process-text '("abc1234567890abc" "abc123+123abc" "abc1234567890abc"))