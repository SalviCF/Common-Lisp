

(mapcar #'(lambda (person job) (list person 'gets job)) '(joe angy sebas) '(job1 job2 job3))
(mapcar #'+ '(1 2 3) '(10 20 30 40))

(defparameter g #'(lambda (x) (+ x 2)))
(funcall g 12)
(apply g '(12))

(find-if #'oddp '(2 3 4 5 6) :from-end t)
(find-if #'oddp '(2 3 4 5 6))

(reduce #'cons '(1 2 3 4))
(reduce #'cons '(1 2 3 4) :from-end t)

(cons 'c (cons 'd 'e))

(defparameter x 4)

(defun f ()
  "Function to test parent lexical context of variables."
  (+ 2 x))

(f)

;; Taking a function as argument

(defun name (f)
  "Applyng f to the name."
  (funcall f '(Salvador Carrillo Fuentes)))

(name #'length)
(name #'reverse)
(name #'first)
(name #'(lambda (x) (cons 'Don x)))

;; Returning a function

(defun adder (x y)
  "Takes two numbers a returns it's sum."
  (+ x y))

(defun super-adder (d)
  "Takes a number and returns a function."
  #'(lambda (n) (+ n d)))

(defparameter sumando (super-adder 3))

(funcall sumando 5)
