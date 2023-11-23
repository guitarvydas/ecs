(defun main ()
       ;; initialize player at (0,0) with face "pensive"
  ;; initialize rock at (10,10)
  (let ((player (make-instance 'Playing-Character 
			       :pos (make-instance 'Movable :pos (make-instance 'ECS-Position :x 0 :y 0))
			       :face (make-instance 'Facial-expression :expression "pensive"))))
    (let ((rock (make-instance 'Stationary-item
			       :pos (make-instance 'ECS-Position :x 10 :y 10))))
      ;; initially
      (let ((all-entities (list player rock)))
	(print-all-entities all-entities)
	
	;; make 1 move
	(system-increment-position all-entities 1 1)
	(system-change-facial-expression all-entities "smiling")

        (terpri)

	(print-all-entities all-entities))))
  (values))


(defun print-all-entities (all-entities)
  (mapc #'(lambda (e)
	    (format *standard-output* "~a" e))
	all-entities))
