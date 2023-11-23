;; Components
(defclass ECS-Position ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y))
  )

(defclass Movable ()
  ((pos :accessor pos :type ECS-Position :initarg :pos))
  )

(defclass Stationary ()
  ((pos :accessor pos :type ECS-Position :initarg :pos))
  )

(defclass Facial-expression ()
  ((expression :accessor expression :initarg :expression))
  )


(defmethod print-object ((self ECS-Position) stream)
  (format stream "(~a,~a)" (x self) (y self)))

(defmethod print-object ((self Movable) stream)
  (format stream "<Movable ~a>" (print-object (pos self) nil)))

(defmethod print-object ((self Stationary) stream)
  (format stream "<Stationary ~a>" (print-object (pos self) nil)))

(defmethod print-object ((self Facial-expression) stream)
  (format stream "~a" (expression self)))
