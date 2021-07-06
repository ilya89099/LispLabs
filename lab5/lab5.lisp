(defun make-term(&key order coeff)
    (list order coeff))

(defun order(term) (first term))
(defun coeff(term) (second term))

(defclass polynom ()
 ((var-symbol :initarg :var :reader var)
  ;; Разреженный список термов в порядке убывания степени
  (term-list :initarg :terms :reader terms)))

(defgeneric der-polynom(p) (:documentation "Находит производную полинома"))

(defmethod der-polynom ((p polynom))
    (let ((result-term-list NIL))
        (dolist (term (terms p))
            (if (/= (order term) 0)
            (setf result-term-list (append result-term-list (list (make-term :order (- (order term) 1) :coeff (* (coeff term) (order term))))))))
        (make-instance 'polynom :var (var p) :terms result-term-list)))

(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defmethod print-object ((p polynom) stream)
  (format stream "[МЧ (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
          (var p)
          (mapcar (lambda (term)
                    (list (zerop1 (coeff term))
                          (minusp1 (coeff term))
                          (if (minusp1 (coeff term))
                              (abs (coeff term))
                              (coeff term))
                          (order term)
                          (var p)
                          (order term)))
                  (terms p))))


(setq p1 (make-instance 'polynom
          :var 'x
          :terms (list (make-term :order 4 :coeff 2)
                       (make-term :order 2 :coeff 5)
                       (make-term :order 1 :coeff 3.3)
                       (make-term :order 0 :coeff -7))))

(setq p1 (der-polynom p1))

(setq p1 (der-polynom p1))
(setq p1 (der-polynom p1))
(setq p1 (der-polynom p1))
(setq p1 (der-polynom p1))