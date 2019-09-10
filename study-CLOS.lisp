;; CLOS ;;;

;; Define a Box class, with three slots length, breadth, and height
(defclass Box ()
  (length
   breadth
   height))

;; Above is useless, you need accessors (getters and setters)
(defclass box ()
  ((length :accessor box-length)
   (breadth :accessor box-breadth)
   (height :accessor box-height)))

;; Creating instance of a class
;; The generic function make-instance creates and returns a new instance of a class
(defparameter my-box (make-instance 'box))

;; Getting and setting the fields (reading and writing)
(setf (box-length my-box) 10)
(setf (box-breadth my-box) 5)
(setf (box-height my-box) 3)

(defun print-box (box)
  (format t "Length of the box: ~d~%" (box-length box))
  (format t "Breadth of the box: ~d~%" (box-breadth box))
  (format t "Height of the box: ~d~%" (box-height box))
  (format t "Volume of the box: ~d~%" (volume box)))

(print-box my-box) ; see in repl

;; Defining a class method
(defmethod volume ((object box))
  (* (box-length object) (box-breadth object) (box-height object)))

;; Inheritance
;; Creating a subclass of box
(defclass wooden-box (box) ; this specifies the superclass
  ((price :accessor box-price)))

(defparameter my-wooden-box (make-instance 'wooden-box))

(setf (box-length my-wooden-box) 10)
(setf (box-breadth my-wooden-box) 5)
(setf (box-height my-wooden-box) 3)
(setf (box-price my-wooden-box) 15)

(format t "Price of the box: ~d~%" (box-price my-wooden-box)) ; see repl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Another exmaple

;; Class definition
(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))

;; Instance creation
(defparameter person1 (make-instance 'person :name "salvi"))

(name person1)

(lisper person1)
(setf (lisper person1) t)

(defclass child (person)
  ((can-walk-p
    :accessor can-walk-p
    :initform t)))

(can-walk-p (make-instance 'child))

(class-of person1)
(type-of person1)

;; A class inherits by default from the class t and from standard-object

;; It is a good practice to define a constructor
(defun make-person (name &key lisper)
  (make-instance 'person :name name :lisper lisper))

(defparameter person2 (make-person "angy"))

;; A little aclaration ;;;;;;;;;;;;;;;;;;
;;
(defun foo (&key a b c) (list a b c))
(foo)
(foo :a 2 :b 'w)
(foo :a 1 :b 'x :c 2)
(foo :a :b :c)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass point ()
  (x y z))

(defparameter my-point (make-instance 'point))
(inspect my-point)

;; The function to access any slot anytime is (slot-value <object> <slot-name>)
(setf (slot-value my-point 'x) 3)
(setf (slot-value my-point 'y) -4)
(setf (slot-value my-point 'z) 7)

(defun print-point (point)
  (format t "x-axis position: ~d~%" (slot-value point 'x))
  (format t "y-axis position: ~d~%" (slot-value point 'y))
  (format t "z-axis position: ~d~%" (slot-value point 'z)))

(print-point my-point)

;; Getters and setters
(type-of #'name)

;; Pretty printing
(defmethod print-object ((obj person) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((name name)
                         (lisper lisper))
            obj
          (format stream "~a, lisper: ~a" name lisper))))

person1
person2

;; Methods
(defmethod greet (obj)
  (format t "Are you a person? You are a ~a.~&" (type-of obj)))

(greet :anything)
(greet person1)

(defmethod greet ((obj person))
  (format t "Hello ~a !~&" (name obj)))

(greet person1)
