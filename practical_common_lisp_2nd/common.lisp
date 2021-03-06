;;;test framework start
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;(defmacro with-gensyms (syms &body body)
  ;`(let ,(mapcar #'(lambda (s) (list s '(gensym))) syms)
     ;,@body))

(defmacro combine-results (&body forms)
  (with-gensyms ("" result)
		`(let ((,result t))
		   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
		   ,result)))
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(deftest test-+ ()
  (check (= (+ 1 2) 3)
	 (= (+ 1 2 3) 6)
	 (= (+ -1 -3) -4)))
(deftest test-* ()
  (check (= (* 2 2) 4)
	 (= (* 3 5) 15)))
(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
;(format t "~%")
;(format t "~a~%" (test-arithmetic))
;;;test framework end
