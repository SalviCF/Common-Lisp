(defun square (n)
  (* n n))

(square 5)

;; Symbols

(eq 'foo 'fOo) ; it's seen as the same symbol because is not case sensitive


;; Numbers

(+ 1 1.0) ; is casting to a floating number


(expt 2 3) ; third power of 2

(expt 53 53)

(/ 4 6) ; returns a rational number

(/ 4.0 6) ; this returns a floating point number

(/ 4 6.0) ; also returns a floating point number


;; Strings

(princ "hello world")

(format nil "He said \"Hello!\"") ; it works in the repl


;; Code mode VS Data mode

(expt 2 3) ; code mode

(expt 2 (+ 1 2)) ; also code mode 

; Quoting: this isn't a command, it's just a chunk of data for my program
'(expt 2 3) ; data mode, won't be evaluated (quoting)


;;; Lists in Lisp

'(1 2 3) ; is made of cons cells (nodes of a linked list)

;; List functions

(cons 1 2)

(cons 'a (cons 'b '())) ; cons is like ':' in Haskell

(cons 'a (cons 'b (cons 'c '()))) ;

; this is a cos cells, not a list (see the point)
(cons 'cat 'paw) ; linking the symbol cat and the symbol paw

(cons 'cat NIL)
(cons 'cat nil)
(cons 'cat 'nil) ; all equivalents because is NIL

(cons 'cat ())

(cons 'a (cons 'b ()))

(cons 'snake '(cat dog))

(cons 2 '(3 5))

(cons 'snake (cons 'cat (cons 'dog ())))

;; car and cdr functions

(car '(snake cat dog)) ; takes the thing inside the 1st slot, like head function in haskell

(cdr '(snake cat dog)) ; takes the thing inside the 2st slot, it discards the 1st node
; it's like tail function in haskell

(cadr '(snake cat dog)) ; first applies car and then applies cdr
(car (cdr '(snake cat dog))) ; equivalent to the previous one, returns the 2nd item

;; The list function

(list 'snake 'cat 'dog) ; builts the list all at one (creates all the cons cells)

;; All of these all equivalents

(cons 'snake (cons 'cat (cons 'dog ()))) ; 1
'(snake cat dog) ; 2
(list 'snake 'cat 'dog) ; 3

;; Nested lists

'(cat (duck bat) ant) ; this list has three items (the second one is a list)
(length '(cat (duck bat) ant))

'((peas carrots tomatoes) (snake cat dog)) ; list of two lists

(car '((peas carrots tomatoes) (snake cat dog))) ; pulling out the 1st 

(cdr '((peas carrots tomatoes) (snake cat dog))) ; pulling out the tail (2nd list here)

(cdr (car '((peas carrots tomatoes) (snake cat dog)))) ; pulls tail of 1st list (head)

(car (cdr '((peas carrots tomatoes) (snake cat dog)))) ; pulls head of the 2nd list (tail) 

(car '(snake cat dog)) ; yeah ok but the next is not the same
(car '((snake cat dog)))

; 1st applies car and then cdr
; car == head so it returns (peas carrots tomatoes) 
; cdr == tail so it returns (carrots tomatoes)

(car '((peas carrots tomatoes) (snake cat dog) (book pencil pen)))
(cdr (car '((peas carrots tomatoes) (snake cat dog) (book pencil pen))))

(cdar '((peas carrots tomatoes) (snake cat dog) (book pencil pen)))

;; We can also use cddr caddr and cadadr

; cddr == cdr + cdr

(cddr '((peas carrots tomatoes) (snake cat dog) duck))
; the tail of the tail...after 1st cdr we have ((snake cat dog) duck)
; after the 2nd cdr we have (duck)

; caddr == 1st cdr 2nd cdr 3rd car, so it gives me DUCK
(car (cddr '((peas carrots tomatoes) (snake cat dog) duck)))
(caddr '((peas carrots tomatoes) (snake cat dog) duck))

; cddar == 1st car 2nd cdr 3rd cdr
(cddar '((peas carrots tomatoes) (snake cat dog) duck)) ; (tomatoes)

; cadadr == 1st cdr 2nd car 3rd cdr 4th car

(cadadr '((peas carrots tomatoes) (snake cat dog) duck)) ; CAT







; end buffer
