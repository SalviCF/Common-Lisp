(defparameter *big* 100)

(defparameter *small* 1)

(defun guess ()
  (ash (+ *big* *small*) -1))

;; (defun smaller ()
;;   (setf *big* (1- (guess)))
;;   (guess))

(defun smaller ()
  (setf *big* (1- (guess)))
  (guess))

(defun bigger ()
  (setf *small* (1+ (guess)))
  (guess))

(defun reload ()
  (setf *big* 100)
  (setf *small* 1)
  (guess))

(let ((x 3)
      (y 5))
  (+ x y))

(flet ((f (n) (+ n 10))
       (g (n) (- n 5)))
  (f (g 10)))

(labels ((f (n)
	   (+ n 10))
	 (g (n)
	   (+ (f n) 3)))
  (g 6))

(defun factorial (n)
  (if (<= n 0) 1 (* n (factorial (- n 1)))))

(factorial 10)

(eq 'foo 'fOo)

(quote foo)

(eq 'foo (quote foo))

(princ "he said: \"hola !!\"") ; see in repl

(cons 1 (cons 2 (cons 3 ())))

'(1 2 3)

(equal '(1 2 3) (cons 1 (cons 2 (cons 3 ()))))

(eq '(1 2 3) (cons 1 (cons 2 (cons 3 ()))))

'(1 cat 2)

(cons 1 (cons 'cat (cons 2 ())))

(cons 'cats 2)

(cons 'cat nil)
(cons 'cat 'nil)
(cons 'cat ())
(cons 'cat '())
(cons 'cat '(dog snake))

(car '(a b c)) ; head
(cdr '(a b c)) ; tail

(car (cdr '(a b c))) ; head of the tail
(cdr (car '((x y z) b c))) ; tail of the head

(cadr '(a b c))

(list 'a 'b 'c)
'(a b c)
(cons 'a (cons 'b (cons 'c ())))
(cons 'a (cons 'b (cons 'c ())))

(cons '(a b c) (cons '(e f g) ()))

(cons (cons 'a ()) (cons (cons 'b ()) ()))

(cons (cons 'brain (cons 'surgery ()))  (cons 'never (cons (cons'say (cons 'oops ())) ())))

(defun add1 (n)
    (+ 1 n))

(add1 3)

(defun add2 (n)
  (add1 (add1 n)))

(add2 3)

(defun twop (n)
  (eq n 2))

(twop 2)

(defun sub2 (n)
  (- n 2))

(sub2 6)

(defun twopp (n)
  (zerop (sub2 n)))

(twopp 2)

(defun half1 (n)
  (ash n -1))

(half1 10)

(defun half2 (n)
  (/ n 2.0))

(half2 10)

(defun gt9 (n)
  (> n 9))

(gt9 10)

(defun neg (n)
  (- 0 n))

(neg -5)

(defun even (n)
  (not (oddp n)))

(even 4)

(defun xor (a b)
  (equal a (not b)))

(xor nil t)

(/ 4) ; reciprocal

(- 4)

'((blue sky) ((green grass) (brown earth)))

(list (list 'blue 'sky) (list 'green 'grass) (list 'brown 'earth))

(cons (cons 'a (cons 'b ()))
      (cons (cons 'c (cons 'd ()))
	    (cons (cons 'e (cons 'f ())) ())))

(cons 'please
      (cons (cons 'be (cons 'my ()))
	    (cons 'valentine ())))

(cons (cons 'bows (cons 'arrows ()))
      (cons (cons 'flowers (cons 'chocolate ())) ()))

(length '(a b c))
(length '(a (b c) c))

(length ())
(length nil)

(first '(a b c d))
(second '(a b c d))
(third '(a b c d))
(rest '(a b c d)) ; tail

(defun my-third (l)
  (first (rest (rest l))))

(my-third '(a b c d))

(defun my-third2 (l)
  (second (rest l)))

(my-third2 '(a b c d))

(cdr '(hola))
(car '(hola))

(car '((hola)))
(cdr '((hola)))

(defparameter my-list (cons (cons (cons 'phone (cons 'home ())) ()) ()))

(car my-list)
(cdr my-list)

(cons 'a
      (cons (cons 'toll ())
	    (cons (cons (cons 'call ()) ()) ())))

(cdr '(a b)) ; is (b), not b

(cdr (cons (cons 'blue (cons 'cube ()))
	    (cons (cons 'red (cons 'pyramid ())) ())))

(caadr (cons (cons 'blue (cons 'cube ()))
	     (cons (cons 'red (cons 'pyramid ())) ())))


(cons (cons nil nil) nil)

(list 'foo)
(list (list 'foo))

(list 'zort nil)

(defun is-a (a b)
  (list a 'is 'a b))

(is-a 'salvi 'nerd)

(defun what (l)
  (cons 'what (rest l)))

(what '(take a nap))

(defun 2y2 (x y z w)
  (list (list x y) (list z w)))

(2y2 'a 'b 'c 'd)

(defun duo-cons (e1 e2 lista)
  (cons e1 (cons e2 lista)))

(duo-cons 'patrick 'seymour '(marvin))

(defun two-deeper (in)
  (list (list in)))

(two-deeper 'salvi)
(two-deeper '(salvi))

(caaadr '(((good)) ((night))))

(listp '(a b))
(listp 'a)
(listp nil)
(consp '(a b))
(consp nil)

(atom 12) ; atom is opposite to consp
(atom nil)
(atom 'a)
(atom '(a))

(null nil)
(null '(a))

(defun unary-add1 (u)
  (cons 'x u))

(unary-add1 nil)
(unary-add1 '(x))
(unary-add1 '(x x))

(defun unary-greaterp (u1 u2)
  (> (length u1) (length u2)))

(unary-greaterp '(x x x) '(x x x x))

(cons 'a (cons 'b 'c))

(cons (cons 'a 'b) (cons (cons 'c 'd) ()))

(last '(a b c)) 

;; Circular lists with sharp-equal notation

(defun circular (items)
  (setf (cdr (last items)) items)
  items)

(setf *print-circle* t)

(circular (list 1 2 3))
