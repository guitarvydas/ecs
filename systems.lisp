;; Systems
(defun system-increment-position (all-entities delta-x delta-y)
  (mapc #'(lambda (e)
	    (when (slot-exists-p e 'pos)
	      (proc-increment (pos e) delta-x delta-y)))
	    all-entities))

(defun system-change-facial-expression (all-entities facial-thingy)
  (mapc #'(lambda (e)
	    (when (slot-exists-p e 'face)
	      (proc-change-face (face e) facial-thingy)))
	all-entities))
  


;;; methods (procs)


;; incrementing position, if Movable
(defmethod proc-increment ((p Movable) dx dy)
  ;; modify x and y in some way
  (setf (x (pos p)) (+ (x (pos p)) dx))
  (setf (y (pos p)) (+ (y (pos p)) dy)))

(defmethod proc-increment ((p T) dx dy)
  ;; else, don't do anything
  (declare (ignore p dx dy))
  )

;; change the facial expression, if the Entity includes a Facial-expression
(defmethod proc-change-face ((f Facial-expression) new-face)
  (setf (expression f) new-face))
