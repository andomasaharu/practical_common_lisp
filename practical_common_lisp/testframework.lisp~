;test framework

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~&~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms ("" result)
		`(let ((,result t))
		   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
		   ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (cons ',name *test-name*)))
       (format t "~&~%~A Start====================================================================" *test-name*)
       ,@body)))

;;; deftestの使い方
;(deftest testsample ()
	 ;(check
	   ;(= (+ 1 2 3) 6)
	   ;(= (+ -1 -3 -4) -7)))

;(format t "~A~%" (testsample))

