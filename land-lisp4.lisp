;; Conditions

(if nil
    'true
    'false)

(if ()
    'true
    'false)

(if '()
    'true
    'false)

(if '(3)
    'true
    'false)

; So, if the list is empty -> false, if not empty -> true
; Is easy detect if a list is empty

;; Length of a list using recursion
(defun my-length (xs)
  (if xs
      (1+ (my-length (cdr xs)))
      0))

(my-length '())
(my-length '(1 2 3 4 5))

;; Example of execution:
;; (my-length '(1 2 3)) =>
;; (1+ (my-length (cdr '(1 2 3)))) =>
;; (1+ (my-length '(2 3))) =>
;; (1+ (1+ (my-length (cdr '(2 3))))) =>
;; (1+ (1+ (my-length '(3)))) =>
;; (1+ (1+ (1+ (my-length (cdr '(3)))))) =>
;; (1+ (1+ (1+ (my-length '())))) =>
;; (1+ (1+ (1+ 0))) =>
;; 3

; Tail-recursive version
(defun my-length-tr (xs &optional (acc 0))
  (if xs
      (my-length-tr (cdr xs) (1+ acc))
      acc))

(my-length-tr '(1 2 3 4) 0)
(my-length-tr '(1 2 3 4))

(defun factorial-tr (n &optional (acc 1))
  (if (<= n 1)
      acc
      (factorial-tr (1- n) (* acc n))))

(factorial-tr 0)
(factorial-tr 3)
(factorial-tr 12)

;; nil == 'nil == () == '()
;; todo lo que no sea equivalente a nil se evalua a TRUE

(if (= (+ 1 2) 3)
    'yes
    'no)

(if (= (+ 1 2) 4)
    'yes
    'no)

(if '()
    'the-list-has-stuff
    'the-list-is-empty)

(if '(z)
    'the-list-has-stuff
    'the-list-is-empty)

(if (oddp 5)
    'odd-number
    'even-number)

;; Evaluating more than 1 instruction in the branch

(defun test-odd (n)
  (if (oddp n)
    (progn (princ "oh yes") (princ "odd number"))
    (progn (princ "oh no") (princ "even number"))))

(if t 'true (/ 1 0)) ; else is never evaluated

(defvar *n-was-odd* nil)

*n-was-odd*

(if (oddp 5)
    (progn (setf *n-was-odd* t) 'odd-number)
    'even-number)

;; when and unless (implicit progn)
(defvar *n-is-odd* nil)
(when (oddp 5) ; cuando se cumpla que ...
  (setf *n-is-odd* t)
  'odd-number)

; when: all expr are evaluated if condition true

*n-is-odd*

(unless (oddp 4) ; si no se cumple que ...
  (setf *n-is-odd* nil)
  'even-number)

; unless: all expr are evaluated if cond false

;; The command COND (also implicit progn)
(defvar *arch-enemy* nil)

(defun pudding-eater (person)
  (cond ((eq person 'alien) (setf *arch-enemy* 'alien)
	                    '(maldito alien...))
	((eq person 'jon) (setf *arch-enemy* 'jon)
	                  '(maldito jon))
	(t                (setf *arch-enemy* 'stranger)
	                  '(a stranger ate my pudding))))

(pudding-eater 'jon)
*arch-enemy*
(pudding-eater 'peter)

; It in the fisrt match (checked from top down)
; If does not enter in previous, enters in last (default)

;; Branching with case

(defun cake-eater (person)
  (case person
    ((alien)	(setf *arch-enemy* 'alien)
           	'(maldito alien...))

    ((jon)	(setf *arch-enemy* 'jon)
         	'(maldito jon))

    (otherwise	(setf *arch-enemy* 'stranger)
	     	'(a stranger ate my cake))))

; C-q <tab> to insert tab
; Usually only for branching on symbol values

;; Tricks with conditions

(and (oddp 3) (oddp 5) (oddp 7))
(or (oddp 4) (oddp 5) (oddp 8))

(defparameter *is-even* nil)

(or (oddp 4) (setf *is-even* t))
; como (oddp 4) da nil, tengo que evaluar lo otro...
; y pongo que es even

(or (oddp 5) (setf *is-even* t))
; como (oddp 5) da t, no evalúo lo otro
; *is-even* se queda como nil

*is-even*

; Es decir, se usa evaluation en cortocircuito
; Si en una lista se encuentra un t, no eval el resto

(defparameter *are-even* nil)
*are-even*
(and (evenp 4) (evenp 8) (setf *are-even* t))

; Three versions

; Version 1: verbose
(defparameter *file-modified* nil)

(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))

; Version 2: not very clear
(and *file-modified* (ask-user-about-saving) (save-file))

; Version 3: compromise between the previous (clearer)
(defun ask-user-about-saving ())
(defun save-file ())

(if (and *file-modified*
         (ask-user-about-saving))
    (save-file))

;; Return more than just the truth

(member 1 '(4 6 1 8))
; see (describe 'member)
; de paso devuelve algo ya que todo lo !nil es t

(if (member 1 '(4 6 1 8))
    'one-is-in-the-list
    'one-is-not-in-the-list)

(find-if #'oddp '(2 4 5 6))
; find-if takes a function and check if any true
; returns the first match

(if (find-if #'oddp '(2 4 5 6))
    'odd-number-found
    'odd-number-missing)

;; Any expression preceded by #'
;; (sharpsign followed by single-quote),
;; as in #'expression, is treated by the Lisp reader as
;; an abbreviation for and parsed identically
;; to the expression (function expression).
;; See function. For example,

;; (apply #'+ l) ==  (apply (function +) l)

(null nil)
(find-if #'null '(1 4 nil 5)) ; debe retornar t

;; eq, equal and more

; 1. Use eq to compare symbols
; 2. Use equal for everything else

(defparameter *fruit* 'apple)

(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))

;;comparing symbols
(equal 'apple 'apple)

;;comparing lists
(equal (list 1 2 3) (list 1 2 3))

;;Identical lists created in different ways still compare as the same

(equal '(1 2 3) (cons 1 (cons 2 (cons 3))))

;;comparing integers

(equal 5 5)

;;comparing floating point numbers

(equal 2.5 2.5)

;;comparing strings

(equal "foo" "foo")

;;comparing characters

(equal #\a #\a)
(princ #\a)

;; eql is similar to eq but alse handles numbers and chars

; https://stackoverflow.com/questions/547436/whats-the-difference-between-eq-eql-equal-and-equalp-in-common-lisp

;; eqlp similar to equal but strings capitalized and
; integers against floating point
(equal 'foo 'fOo)
(equalp 2 2.0)

(= 2 2.0)
(string-equal "hola" "hola")
(char-equal #\a #\a)
