(defun spiral-matrix(n)
    (let ((matrix (make-array (list n n)))
          (layers (+ 1 (floor (/ (- n 1) 2))))
          (current-value 1)
          (last-index 0))
    
    (loop for i upfrom 0 to (- layers 1)
        do
        (setq last-index (- n (+ i 1)))
        (loop for s1 upfrom i to last-index
            do
            (setf (aref matrix i s1) current-value)
            (setq current-value (+ 1 current-value))
        )
        (loop for r1 upfrom (+ i 1) to last-index
            do
            (setf (aref matrix r1 last-index) current-value)
            (setq current-value (+ 1 current-value))
        )
        (loop for s2 downfrom (- last-index 1) to i
            do
            (setf (aref matrix last-index s2) current-value)
            (setq current-value (+ 1 current-value))
        )
        (loop for r2 downfrom (- last-index 1) to (+ i 1)
            do
            (setf (aref matrix r2 i) current-value)
            (setq current-value (+ 1 current-value))
        )       
    )
    matrix)
)

(print (spiral-matrix 3))

