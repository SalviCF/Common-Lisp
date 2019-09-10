
(defun greet ()
  (format t "Hi, my name is Salvi"))

;; Special formatting directives (begin with ~)
(format t "Time flies~%like an arrow") ; move to new line
(format t "Fruit:~%~%apple~%~%banana") ; inserts a blank line

;; ~& moves to a new line
(defun salvi ()
  (format t "~&Salvi has a little cat.")
  (format t "~&Its eyes are big and blue.")
  (format t "~&And she sleeps with Salvi.")
  (format t "~&Every single night."))

;; To insert a Lisp object
(format t "From ~S to ~S in ~S minutes!"
        'boston '(new york) 55)

(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))

;; Printing without using escape characters
(defun test (x)
  (format t "~&With escape characters: ~S" x)
  (format t "~&Wothout escape characters: ~A" x))

; (test "hola mundo")

;; Printing values of a list delimited with commas
(defparameter lista '(cat dog bird fish))

(loop for cons on lista
   do (format t "~a" (car cons))
   when (cdr cons) do (format t ", "))

(format t "~{~a~^, ~}" lista)

;; Printing floating point numbers
(format t "~$" pi) ; two decimal precision by default
(format t "~5$" pi) ; changing decimal precision

; v causes FORMAT to consume one format argument and use its value for the prefix parameter
(format t "~v$" 4 pi) ; shows pi with 4 decimals
