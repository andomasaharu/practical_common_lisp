(defun hello-world ()
  (format t "hello world"))

(defmacro fmt (fn)
  `(format t "~A~%" ,fn))

(defun formatdb(db)
  (format t "~{~{~a:~10t~a~%~}~%~}" db))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))
(add-record (make-cd "let the music do the talking" "the joe perry project" 9 t))
(add-record (make-cd "exile on main st" "the rolling stones" 8 t))
(add-record (make-cd "music from another dimension!" "aerosmith" 7 t))
(add-record (make-cd "get a grip" "aerosmith" 20 t))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
	(prompt-read "Title")
	(prompt-read "Artist")
	(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
	(y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
		(if (not (y-or-n-p "Another? [y/n]: ")) (return))))
;(add-cds)

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
;(save-db "cdrecords.txt")
;(formatdb *db*)

(defun select (selectp)
  (remove-if-not selectp  *db*))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun update(selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
		(mapcar
		  #'(lambda(row)
			  (when (funcall selector-fn row)
				(if title (setf (getf row :title) title))
				(if artist (setf (getf row :artist) artist))
				(if rating (setf (getf row :rating) rating))
				(if ripped-p (setf (getf row :ripped) ripped)))
			  row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;(fmt (macroexpand-1 '(where :artist "aerosmith")))
;(fmt (macroexpand-1 '(where :rating 7)))
(formatdb (select (where :artist "aerosmith")))
;(update (where :artist "aerosmith") :artist "aero")
;(update (where :artist "the rolling stones") :artist "stones")
;(formatdb *db*)
;(formatdb (select (where0 :artist "aerosmith")))
;(formatdb (select (where0 :title "get a grip")))
;(formatdb (select (where0 :rating 7)))
