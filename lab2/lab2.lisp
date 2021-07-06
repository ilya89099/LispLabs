(defun test (input-list)
        (multiple-value-bind (odd-list even-list) (test-impl '() '() input-list)
          (combine-lists odd-list even-list '())
        )
)

(defun test-impl (odd-list even-list input-list) 
  (if (null input-list)
      (values (reverse odd-list) (reverse even-list))
      (if (oddp (first input-list))
          (test-impl (cons (first input-list) odd-list) even-list (rest input-list))
          (test-impl odd-list (cons (first input-list) even-list) (rest input-list))
      )
   ) 
)

(defun combine-lists (odd-list even-list result)
  (if (or (null odd-list) (null even-list))
      result
      (combine-lists (rest odd-list) 
                     (rest even-list) 
                     (append result (list (first even-list) (first odd-list)))
      )
  )
)


(test '(1))