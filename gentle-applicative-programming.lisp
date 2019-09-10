;;; FUNCALL ;;;

(cons 'a 'b)
(funcall #' cons 'a 'b) ; applies cons with these arguments

;; We use #' to quote a function in Common Lisp

;;; MAPCAR ;;;

(mapcar #'1+ '(1 2 3 4 5)) ; applies the function 1+ to all elements

;;; TABLES & MAPCAR ;;;

(defparameter words
  '((uno one)
    (dos two)
    
    (tres three)))

(mapcar #'first words)
(mapcar #'second words)
(mapcar #'reverse words)

(assoc 'uno words)

(defun translate (x)
  (second (assoc x words)))

(translate 'dos)
(mapcar #'translate '(tres uno dos tres))

;;; Exercises ;;;

(defun add1 (n)
  (+ n 1))

(mapcar #'add1 '(1 3 5 7 9))

(defvar *daily-planet* '((olsen jimmy 123 cub-reporter)
			 (kent clark 089 reporter)
			 (lane lois 951 reporter)
			 (white perry 355 editor)))

(mapcar #'third *daily-planet*)

(mapcar #'zerop '(2 0 3 4 0 -5 -6))

(defun g-t-5 (n)
  (> n 5))

(mapcar #'g-t-5 '(3 7 2 6 8))

;;; LAMBDA EXPRESSIONS ;;;

(mapcar #'(lambda (n) (> n 5)) '(2 0 3 4 0 -5 6))

;;; Exercises ;;;

(funcall #'(lambda (n) (- n 7)) 8)
(funcall #'(lambda (in) (or (equal in t) (equal in nil))) 34)

;;; FIND-IF ;;;
(find-if #'oddp '(2 4 6 7 8 9))
(find-if #'(lambda (n) (> n 3)) '(2 4 6 7 8 9))

;;; WRITING ASSOC USING FIND-IF
words
(assoc 'uno words)
(defun my-assoc (key table)
  (find-if #'(lambda (entry)
	       (equal (first entry) key))
	   table))

(my-assoc 'uno words)

;;; Exercises ;;;
(defun roughly-equal (xs k)
  (find-if #'(lambda (n)
	       (and (< k (+ n 10)) (> k (- n 10))))
	   xs))

(roughly-equal '(20 12 100 105) 96)

(defun find-nested (xs)
  (find-if #'(lambda (e)
	       (not (equal e nil)))
	   xs))

(find-nested '(() () (1 2) ()))

;;; Exercises ;;;

(defvar *note-table* '((c 1)
		       (c-sharp 2)
		       (d 3)
		       (d-sharp 4)
		       (e 5)
		       (f 6)
		       (f-sharp 7)
		       (g 8)
		       (g-sharp 9)
		       (a 10)
		       (a-sharp 11)
		       (b 12)))

*note-table*

(defun numbers (notes)
  (mapcar #'(lambda (note)
	      (second (assoc note *note-table*)))
	  notes))

(numbers '(e d c d e e e))

(defun notes (numbers)
  (let ((number-table (mapcar #'reverse *note-table*)))
    (mapcar #' (lambda (number)
		 (second (assoc number number-table)))
	       numbers)))

(notes '(5 3 1 3 5 5 5))

(defun raise (n numbers)
  (mapcar #'(lambda (e)
	      (+ e n))
	  numbers))

(raise 5 '(5 3 1 3 5 5 5))

(defun normalize (numbers)
  (mapcar #'(lambda (n)
	      (cond ((> n 12) (- n 12))
		    ((< n 1) (+ n 12))
		    (t n)))
	  numbers))

(normalize '(6 10 13))
(normalize '(14 -2 3))

(defun transpose (n song)
  (let* ((numbers (numbers song))
	 (r-num (raise n numbers)))
    (notes (normalize r-num))))

(transpose 5 '(e d c d e e e))
(transpose 11 '(e d c d e e e))
(transpose 12 '(e d c d e e e))
(transpose -1 '(e d c d e e e))

;;; REMOVE-IF & REMOVE-IF-NOT

(remove-if #'numberp '(2 for 1 sale))
(remove-if #'oddp '(1 2 3 4 5))

(remove-if #'(lambda (n)
	       (not (plusp n)))
	   '(2 -3 0 6 -9))

(remove-if-not #'plusp '(2 -3 0 6 -9))

(remove-if-not #'(lambda (n)
		   (> n 3))
	       '(2 4 6 8 4 2 1))

(remove-if-not #'symbolp '(3 apples 4 pears and 2 little plums))

(defun count-zeros (ns)
  (length (remove-if-not #'zerop ns)))

(count-zeros '(1 2 3 0 0 2 0 0 6 0))

;;; Exercises ;;;

(defun 1-5 (ns)
  (remove-if-not #'(lambda (n)
		     (and (> n 1) (< n 5)))
		 ns))

(1-5 '(1 2 3 6 7 8 9))

(defun count-the (sentence)
  (length (remove-if-not #'(lambda (word)
			     (equal word 'the))
			 sentence)))

(count-the '(the dragon was in the mountain with the others))

(defun count-2list (lss)
  (remove-if-not #'(lambda (ls)
		     (equal (length ls) 2))
		 lss))

(count-2list '((1 2) (1 2 3) (a b) (a b c d)))

(defun my-intersec (x y)
  (remove-if-not #'(lambda (e)
		 (member e y))
		 x))

(my-intersec '(1 2 3 4) '(1 4 5 7))
(intersection '(1 2 3 4) '(1 4 5 7))

(defun my-union (x y)
  (append y (remove-if #'(lambda (e)
			 (member e y))
		     x)))

(my-union '(1 2 3 4) '(1 4 5 7))
(union '(1 2 3 4) '(1 4 5 7))

;;; Exercises ;;;

(defun rank (card)
  (first card))

(defun suit (card)
  (second card))

(rank '(2 clubs))
(suit '(2 clubs))

(defvar *my-hand* '((3 hearts)
		    (5 clubs)
		    (2 diamonds)
		    (4 diamonds)
		    (ace spades)))

(defun count-suit (suit hand)
  (length (remove-if-not #'(lambda (card)
			     (equal (suit card) suit))
			 hand)))

(count-suit 'diamonds *my-hand*)

(defvar *colors* '((clubs black)
		   (diamonds red)
		   (hearts red)
		   (spades black)))

(defun color-of (card)
  (second (assoc (suit card) *colors*)))

(color-of '(2 clubs))
(color-of '(6 hearts))

(defun first-red (hand)
  (find-if #'(lambda (card)
	       (equal (color-of card) 'red))
	   hand))

(first-red *my-hand*)

(defun black-cards (hand)
  (remove-if-not #'(lambda (card)
		     (equal (color-of card) 'black))
		 hand))

(black-cards *my-hand*)

(defun what-ranks (my-suit hand)
  (mapcar #'first (remove-if-not #'(lambda (card)
				     (equal (suit card) my-suit))
				 hand)))

(what-ranks 'diamonds *my-hand*)
(what-ranks 'spades *my-hand*)

(defvar *all-ranks* '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (c1 c2)
  (let ((r1 (rank c1))
	(r2 (rank c2)))
    (cond ((and (numberp r1) (numberp r2)) (> r1 r2))
	  ((and (symbolp r1) (symbolp r2)) (if ((lambda (x y) (member y (member x *all-ranks*))) r1 r2) nil t))
	  ((numberp r1) nil)
	  (t t))))

(higher-rank-p '(queen hearts) '(ace diamonds))

(defun high-card (hand)
  (let ((ranks (mapcar #'first hand))) ; ranks of the hand
    ;; check if is in ranks. extract first non-nil list. extract the first element. extract the associated card
    (assoc
     (first
      (find-if #'(lambda (e) (not (equal e nil)))
	       (mapcar #'(lambda (e) (member e ranks))
		       '(ace king queen jack 10 9 8 7 6 5 4 3 2))))
     hand)))

(defparameter *other-hand* '((queen hearts)
		      (2 clubs)
		      (jack diamonds)
		      (king diamonds)
		      (5 spades)))

*other-hand*

(high-card *other-hand*)

;;; THE REDUCE OPERATOR ;;;

(reduce #'+ '(1 2 3))
(reduce #'append '((one uno) (two dos) (three tres)))

;; Exercises ;;

(reduce #'union '((a b c) (c d a) (f b d) (g)))

(reduce #'(lambda (x y)
	    (if (numberp x)
		(+ x (length y))
		(+ (length x) (length y))))
	'((a b c) (c d a) (f b d) (g)))

(reduce #'+ nil) ; 0 neutral element in + operation
(reduce #'* nil) ; 1 neutral element in * operation

;;; EVERY ;;;

(every #'numberp '(1 2 3 4))
(every #'numberp '(1 2 3 4 a))
(every #'(lambda (e) (> e 0)) '(1 2 3 4 5))
(every #'oddp nil)
(every #'> '(10 20 30 40) '(1 2 3 4)) ; 10>1 && 20>2 && ...

;; Exercises

; T si todos impares
(defun all-odd (xs)
  (every #'oddp xs))

(all-odd '(1 3 5 7 9))
(all-odd '(1 3 5 7 2))

; T si todos pares
(defun none-odd (xs)
  (every #'evenp xs))

(none-odd '(2 4 6 8))
(none-odd '(1 2 4 6 8))

; T si algun par. any-even
(defun not-all-odd (xs)
  (not (all-odd xs)))

(not-all-odd '(2 1 3 5 7))
(not-all-odd '(1 3 5 7))

; T si algun impar. any-odd
(defun not-none-odd (xs)
  (not (none-odd xs)))

(not-none-odd '(1 2 4 6 8))
(not-none-odd '(2 4 6 8))

words
(mapcar #'(lambda (x y) (push x y)) '(un deux trois) words)
