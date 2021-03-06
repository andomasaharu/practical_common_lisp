(defvar *db* nil)

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun add-record (cd) (push cd *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun prompt-for-cd ()
  (add-record 
    (make-cd
      (prompt-read "Title")
      (prompt-read "Artist")
      (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
      (y-or-n-p "Ripped [y/n]: "))))

(defun add-cds ()
  (loop (add-record (prompt-for-cd)
		    (if (not (y-or-n-p "Another? [y/n]: ")) (return)))))

(defun save-db (filename)
       (with-open-file (out filename
			    :direction :output
			    :if-exists :supersede)
	 (with-standard-io-syntax
	   (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;(format t "~A~%" (add-record (make-cd "Roses" "Kathy Mattea" 7 t)))
;(format t "~A~%" (add-record (make-cd "Fly" "Dixie Chicks" 8 t)))
;(format t "~A~%" (add-record (make-cd "Home" "Dixie Chicks" 9 t)))

;(add-cds)

;(save-db "/home/ando/lisp/practical_common_lisp/my-cds.db")
;(save-db "my-cds.db")

(load-db "/home/ando/lisp/practical_common_lisp/my-cds.db")

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
	collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	  #'(lambda (row)
	      (when (funcall selector-fn row)
		(if title (setf (getf row :title) title))
		(if artist (setf (getf row :artist) artist))
		(if rating (setf (getf row :rating) rating))
		(if ripped-p (setf (getf row :ripped) ripped)))
	      row)
	  *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(format t "~A~%" (select (artist-selector "van halen")))
;(format t "~A~%" (select (where :artist "van halen")))
;(format t "~A~%" (select (where :title "Home")))
;(format t "~A~%" (select (where :artist "van halen")))
;(format t "~A~%" (select (where :rating 88)))
;(format t "~A~%" (select (where :ripped t)))
;(update (where :title "Home") :title "Home sweet home")
;(delete-rows (where :artist "van halen"))

(dump-db)

(load "~ando/lisp/practical_common_lisp/testframework.lisp")

(deftest test-+ ()
	 (check
	   (= (+ 1 2) 3)
	   (= (+ 1 2 3) 6)
	   (= (+ -1 -3) -4)))

(format t "~A~%" (test-+))

(deftest test-arithmetic ()
	 (combine-results
	   (test-+)))

(format t "~A~%" (test-arithmetic))

(defmacro assoc-s (s alist)
  `(assoc ,s ,alist :test #'string=))
(format t "~A~%" (assoc-s "a" '(("a" . 1) ("b" . 2) ("c" . 3))))

(defmacro enum-line (filename callback)
  (with-gensyms ("" in w)
		`(let ((,in (open ,filename)))
		   (do ((,w (read-line ,in nil) (read-line ,in nil)))
		     ((not ,w))
		     (funcall ,callback ,w))
		   (close ,in))))

(defun enum-linef (filename callback)
  (with-open-file (in filename)
    (do ((w (read-line in nil) (read-line in nil)))
      ((not w))
      (funcall callback w))))

(format t "~A~%" (macroexpand-1 '(enum-line "name.txt" #'(lambda (w) (format t "~A~%" w)))))
(princ (macroexpand-1 '(enum-line "name.txt" #'(lambda (w) (format t "~A~%" w)))))
(prin1 (macroexpand-1 '(enum-line "name.txt" #'(lambda (w) (format t "~A~%" w)))))
(enum-linef "name.txt" #'(lambda (w) (format t "~A~%" w)))

;銀行のサンプルプログラム、オブジェクト指向のサンプル start
(defvar *account-numbers* 0)
(defvar *minimum-balance* 99999999999)

(defgeneric withdraw (account amount)
	    (:documentation "amountで指定された額の口座から引き落とす
			    現在の残高がamountより少なかったらエラーを通知する"))

(defclass bank-account ()
  ((customer-name
     :initarg :customer-name
     :initform (error "Must supply a customer name.")
     :accessor customer-name)
   (balance
     :initarg :balance
     :initform 0
     :reader balance)
   (account-number
     :initform (incf *account-numbers*))
   account-type))

(defclass checking-account (bank-account) ())

(defclass savings-account (bank-account) ())

(defmethod initialize-instance :after ((account bank-account) &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100))))
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000) :silver)
	    (t :bronze)))))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account Overdrawn."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))

(defparameter *account*
  (make-instance 'bank-account :customer-name "John Doe" :balance 1000))

(defparameter *account*
  (make-instance
    'bank-account
    :customer-name "John Lennon"
    :balance 100000
    :opening-bonus-percentage 5))

;(setf (slot-value *account* 'customer-name) "John Doe")
;(setf (slot-value *account* 'balance) 1000)

;(pprint (slot-value *account* 'balance))
(assess-low-balance-penalty *account*)
(muriyari-with-accessoros *account*)
(pprint (balance *account*))
;(setf (customer-name *account*) "John Paul")
;(pprint (slot-value *account* 'customer-name))
(pprint (customer-name *account*))
(pprint (slot-value *account* 'account-number))
(pprint (slot-value *account* 'account-type))
;銀行のサンプルプログラム、オブジェクト指向のサンプル end
