(load "./common.lisp")

;;;===================================================================
(defmacro mywhen (pred &rest body)
  `(if ,pred
     (progn ,@body)))
;(print (macroexpand-1 '(mywhen t
			       ;(print "mywhen t1")
			       ;5)))
;;;===================================================================
(defmacro myunless (pred &rest body)
  `(if (not ,pred)
     (progn ,@body)))
;(print (macroexpand-1 '(myunless nil
				  ;(print "myunless t1")
				  ;8)))

;;;===================================================================
(defmacro mycond (&rest body)
  (if (null body)
    nil
    `(if ,(car (car body))
       (progn ,@(cdr (car body)))
       (mycond ,@(cdr body)))))

;(print (macroexpand-1 (mycond ((> 5 5) 1)
			      ;((> 5 4) 2)
			      ;((> 5 7) 3)
			      ;(t 100))))
;;;===================================================================
(defun mynot (n)
  (cond ((null n) t)
	(t nil)))
;;;===================================================================
(defmacro myand (&body body)
  `(cond
     ,@(loop for f in body collect `((not ,f) nil))))
;(print (macroexpand-1 '(myand t nil (format t "bbb~%"))))
;(print (macroexpand-1 '(and t nil (format t "aaa~%"))))
;;;===================================================================
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
(defun next-prime (number)
  (loop for n from number when (primep n) return n))
;(do ((p (next-prime 0) (next-prime (1+ p))))
  ;((> p 19))
  ;(format t "~d " p))
(defmacro do-primes ((p s e) &body body)
  (let ((ending-value (gensym)))
    `(do ((,p (next-prime ,s) (next-prime (1+ ,p)))
	  (,ending-value ,e))
       ((> ,p ,ending-value))
       ,@ body)))
;(format t "~a~%" (macroexpand-1 '(do-primes (p 0 19)
	   ;(format t "~d " p))))
;(do-primes (p 0 19)
	   ;(format t "~d " p))
;;;===================================================================
(deftest test-mymacro ()
	 (check (= (mywhen t
			   (print "mywhen t1")
			   5) 5)
		(= (myunless nil
			     (print "myunless t1")
			     8) 8)
		(mynot ())
		(not (mynot 'a))
		(not (myand t nil (format t "bbb~%")))
		(not (myand t nil (format t "aaa~%")))
		(not (myand t t (format t "aaa~%")))
		(myand t t t)
		(= (next-prime 6) 7)))
(format t "~a~%" (test-mymacro))


;;;===================================================================
(defmacro withgensyms ((prefix &body syms))
  `(let ,@(mapcar #'(lambda (s) (list (list s '(gensym)))) syms)))
(print (macroexpand-1 '(withgensyms ("" a b c d))))
(print (macroexpand-1 '(with-gensyms ("ando" a b))))
