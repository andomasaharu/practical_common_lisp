(defvar *db* nil)


(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
(load-db "my-cds.db")

(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd) (equal (getf cd :artist) artist)) *db*))
(format t "~a~%" (select-by-artist "aerosmith"))


(defun select (k v)
  (remove-if-not #'(lambda (cd) (string= (getf cd :artist) v)) *db*))
(format t "~a~%" (select "a" "home"))
;(defun add-record (cd) (push cd *db*))


(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))


;(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
;(add-record (make-cd "Home" "Dixie Chicks" 9 t))


(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
  ;(dolist (cd *db*)
    ;(format t "~{~a:~10t~a~%~}~%" cd)))


(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]: ")))


(defun add-cds ()
  (when (y-or-n-p "Add cds? [y/n]: ")
    (loop (add-record (prompt-for-cd))
	  (if (not (y-or-n-p "Another? [y/n]: ")) (return)))))
(add-cds)


(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))
(save-db "./my-cds.db")

(dump-db)
