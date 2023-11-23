;; Entities
(defclass Entity ()
  ()
  )

(defclass Playing-Character (Entity)
  ((pos :type 'Movable :accessor pos :initarg :pos)
   (face :type 'Facial-expression :accessor face :initarg :face))
  )

(defclass Stationary-Item (Entity)
  ((pos :type 'ECS-Position :accessor pos :initarg :pos))
  )

(defmethod print-object ((self Playing-Character) stream)
  (format stream "Playing-Character")
  (print-slot-values self stream)
  (format stream "~%"))

(defun print-slot-values (e stream)
  (let ((slot-names (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots (class-of e)))))
    (mapc #'(lambda (slot-name)
              (print-slot-value e slot-name stream))
          slot-names)))

(defun print-slot-value (e slot-name stream)
  (let ((val (slot-value e slot-name)))
    (format stream " [~a ~a]" slot-name val)))

(defmethod print-object ((self Stationary-Item) stream)
  (format stream "Stationary-Item")
  (print-slot-values self stream)
  (format stream "~%"))

