;; Numbers, T and NIL evaluate to themselves

;; Defun is a special kind of function, called macro function

(defun cube (n)
  (* (* n n) n))

(cube 3)

(defun pythago (x y)
  (sqrt (+ (* x x) (* y y))))

(pythago 3 4)

(defun milles-per-gallon (init-odo fin-odo gal-consu)
  (/ (- fin-odo init-odo) gal-consu))

(milles-per-gallon 100 200 15)

;; Symbols vs variables: a symbol evaluates to the value of the variable it refers to

(defparameter salvi 2)
(defparameter angy 2)

(equal salvi angy) ; evaluate the values
(equal 'salvi 'angy) ; comparing the symbols themselves

;; A quoted object evaluates ti the object itself, without the quote

(+ (length '(1 foo 2 moo))
   (third '(1 foo 2 moo)))

(defun longer-than (xs ys)
  (> (length xs) (length ys)))

(longer-than '(a b c d) '(a b y))

(defun add-length (xs)
  (cons (length xs) xs))

(add-length '(moo goo gai pan))
(add-length (add-length '(a b c)))

