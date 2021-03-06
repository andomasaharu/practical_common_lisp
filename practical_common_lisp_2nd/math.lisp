(load "./common.lisp")

(defun series1 (n r limit)
  (cond ((= n limit) r)
	(t (series1 (1+ n) (+ r (/ 1 n)) limit))))
(format t "~f~%" (series1 1 0 99999))


(defun series2 (limit)
  (let ((r 0))
    (dotimes (x limit x)
      (setf r (+ r (/ 1 (1+ x)))))
    r))

;(deftest test-series ()
; (check (> (series2 100) 1)))
;test-series)
(format t "~f~%" (series2 99999))
