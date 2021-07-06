(defun f-iter (a b c count) 
  (if (= count 0)
      c
      (f-iter b c (+ a b c) (- count 1))
  )
)

(defun f(n)
  (if (< n 3)
      n
      (f-iter (- n (floor n))
              (+ 1 (- n (floor n)))
              (+ 2 (- n (floor n))) 
              (- (floor n) 2)
      )
  )
) 
