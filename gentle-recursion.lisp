;;;; Chapter 8: Recursion

(defun any-odd (numbers)
  "Recursive function that takes a list of numbers and returns T if any is odd."
  (cond ((null numbers) nil)
	((oddp (first numbers)) t)
	(t (any-odd (rest numbers)))))

; testing
(any-odd '(2 4 6 8)) 

; testing
(trace any-odd) 

(defun fact (n)
  "Takes a number and returns it's factorial."
  (cond ((zerop n) 1)
	(t (* n (fact (1- n)))))) ; recurse infinitely with a negative argument...

; testing
(fact 5) 

(defun fact-tr (n &optional (acc 1))
  "Tail recursive version of the fact function."
  (cond ((zerop n) acc)
	(t (fact-tr (1- n) (* acc n)))))

; testing
(fact-tr 5) 

(defun count-slices (loaf)
  "Recursive version of the length function."
  (cond ((null loaf) 0)
	(t (1+ (count-slices (rest loaf))))))

; testing
(count-slices '(1 2 3 4))

(defun count-slices-tr (loaf &optional (acc 0))
  "Tail-recursive version of the count-slices function."
  (cond ((null loaf) acc)
	(t (count-slices-tr (rest loaf) (1+ acc)))))

; testing
(count-slices-tr '(1 2 3 4))

;;; Recursion exercises

(defun laugh (n)
  "Takes a number and returns a list of that many HAs."
  (cond ((zerop n) nil)
	(t (cons 'ha (laugh (1- n))))))

; testing
(laugh 3)

(defun add-up (numbers)
  "Sum all the numbers in the list."
  (cond ((null numbers) 0)
	(t (+ (first numbers) (add-up (rest numbers))))))

; testing
(add-up '(2 3 7))

(defun alloddp (numbers)
  "Returns T if all the numbers in a list are odd."
  (cond ((null numbers) nil)
	((= (length numbers) 1) (oddp (first numbers)))
	((or (evenp (first numbers)) (null numbers)) nil) 
	(t (alloddp (rest numbers)))))

; testing
(alloddp '(3 7 2))
(alloddp '())
(trace alloddp)

(defun rec-member (element list)
  "Recursive version of the function member."
  (cond ((null list) nil)
	((equal (first list) element) list)
	(t (rec-member element (rest list)))))

; testing
(rec-member 'a '(b d a c e))
(rec-member 'a ())
(rec-member 'a '(a))
(member 'a '(a))

(defun rec-assoc (element list)
  "Recursive version of the function assoc."
  (cond ((null list) nil)
	((equal (caar list) element) (car list))
	(t (rec-assoc element (rest list)))))

; testing
(assoc 'a '((a 1) (b 2) (c 3)))
(rec-assoc 'b '((a 1) (b 2) (c 3)))

(defun rec-nth (idx list)
  "Recursive version of the nth function."
  (cond ((null list) nil)
	((= idx 0) (first list))
	(t (rec-nth (1- idx) (rest list)))))

; testing 
(nth 1 '(a b c d e))
(nth 3 '(2))
(rec-nth 3 '(a b c))

(defun add1 (n)
  (1+ n))

(defun sub1 (n)
  (1- n))

(defun rec-plus (x y)
  "Recursive version of +."
  (cond ((zerop y) x)
	(t (rec-plus (add1 x) (sub1 y)))))

;; testing
(rec-plus 27 15)

;; Exercises

(defun fib (n)
  "Compute the nth Fibonacci number."
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib (- n 1)) (fib (- n 2))))))

;; testing
(fib 0)
(fib 11)

(defun fib-2 (n &optional (acc1 0) (acc2 1))
  "Tail recursive version of the fib function."
  (cond ((zerop n) acc1)
	(t (fib-2 (1- n) acc2 (+ acc1 acc2)))))

;; testing
(fib-2 0)
(fib-2 19)

(defun any-7-p (x)
  "Searches a list for the number seven."
  (cond ((equal (first x) 7) t)
	(t (any-7-p (rest x))))) ; if number seven is not in the list, the function will recurse infinitely

;; testing
; (fact -1) ; recurses infinitely

(defun find-first-odd (numbers)
  "Returns the first odd number in a list."
  (cond ((null numbers) nil)
	((oddp (first numbers)) (first numbers))
	(t (find-first-odd (rest numbers)))))

;; testing
(find-first-odd '(12 2 4))

(defun find-first-atom (element)
  "Returns the first atom in a list."
  (cond ((symbolp element) element)
	(t (find-first-atom (first element)))))

;; testing
(find-first-atom '(oh ah eh))
(find-first-atom '((((a f)) i) r))
(find-first-atom 'fred)

(defun last-element (cells)
  "Returns the last element of a list."
  (cond ((symbolp (cdr cells)) (car cells))
	(t (last-element (rest cells)))))

;; testing
(last-element '(oh ah eh))
(last-element '((((a f)) i) r))

(defun add-nums (n)
  "Adds up the numbers n, n-1 and n-2, down to 0."
  (cond ((zerop n) 0)
	(t (+ n (add-nums (1- n))))))

;; testing
(add-nums 5)

(defun add-nums-tr (n &optional (acc n))
  "Tail recursive version of the add-nums function."
  (cond ((zerop n) acc)
	(t (add-nums-tr (1- n) (+ acc (1- n))))))


;; testing
(add-nums-tr 5)

(defun all-equal (list)
  "Returns T  if all elements in a list are equal."
  (cond ((< (length list) 2) t)
	(t (and (equal (first list) (second list)) (all-equal (rest list))))))

;; testing
(all-equal '(a))
(all-equal '(i i i i i))
(all-equal '(s i i i))

;; Variations of the basic templates (exercises)

(defun count-down (n)
  "Counts down from n using list consing recursion."
  (cond ((zerop n) nil)
	(t (cons n (count-down (1- n))))))

;; testing
(count-down 5)

(defun count-down-fact (n)
  "Applicative version of the fact function using count-down."
  (reduce #'* (count-down n)))

;; testing
(count-down-fact 5)

(defun count-down-2 (n)
  "Does the same as count-down but reaching zero."
  (cond ((zerop n) (cons 0 nil))
	(t (cons n (count-down-2 (1- n))))))

;; testing
(count-down-2 5)
(cons 0 nil)

(defun count-down-3 (n)
  "Does the same as count-down but reaching zero."
  (cond ((= n -1) nil)
	(t (cons n (count-down-3 (1- n))))))

;; testing
(count-down-3 5)

(defun my-nth (n x)
  "Version of the function nth."
  (cond ((or (zerop n) (null x)) (first x))
	(t (my-nth (1- n) (rest x)))))

;; testing
(my-nth 2 '(a b c d e))

(defun my-member (element list)
  "Recursive version of the member function."
  (cond ((or (equal element (first list)) (null list)) list)
	(t (my-member element (rest list)))))

;; testing
(my-member 'a '(b f h a e c b))
(my-member 'a nil)
(my-member 'a '(b c g e))

(defun my-assoc (element list)
  "Another recursive version of the function assoc."
  (cond ((or (equal element (caar list)) (null list)) (first list))
	(t (my-assoc element (rest list)))))

;; testing
(my-assoc 'c '((a 1) (b 2) (c 3)))

(defun compare-lengths (xs ys)
  "Compare the lengths of the two lists that takes as input usgin recursion."
  (cond ((null xs) (if (null ys) 'same-length 'second-is-longer))
	((null ys) 'first-is-longer)
	(t (compare-lengths (rest xs) (rest ys)))))

;; testing
(compare-lengths nil nil)

(defun sum-numeric-elements (xs)
  "Adds up the numbers of a list, ignoring the non-numbers."
  (cond ((null xs) 0)
	((numberp (first xs)) (+ (first xs)
				 (sum-numeric-elements (rest xs))))
	(t (sum-numeric-elements (rest xs)))))

;; testing
(sum-numeric-elements '(3 bears 3 bowls 1 owls))

(defun my-remove (x xs)
  "Recursive version of the remove function-."
  (cond ((null xs) nil)
	((equal x (first xs)) (my-remove x (rest xs)))
	(t (cons (first xs) (my-remove x (rest xs))))))

;; testing
(my-remove 'a nil)
(my-remove 'a '(b f a t r))
(my-remove 'b '(b g d t s b w))

(defun my-intersection (xs ys)
  "Recursive version of the intersection function."
  (cond ((or (null xs) (null ys)) nil)
	((member (first xs) ys) (cons (first xs)
				      (my-intersection (rest xs) ys)))
	(t (my-intersection (rest xs) ys))))

;; testing
(intersection '(a b c d) '(b c e f))
(intersection nil '(a b))
(my-intersection nil '(a b))
(my-intersection '(a b c d) '(a d e f))
(my-intersection '(a a b c d e d) '(b a h j k))
(intersection '(a a b c d e d) '(b a h j k))

(defun my-set-difference (xs ys)
  "Recursive version of the set difference function."
  (cond ((null xs) nil)
	((null ys) xs)
	((not (member (first xs) ys)) (cons (first xs)
					    (my-set-difference (rest xs) ys)))
	(t (my-set-difference (rest xs) ys))))

;; testing
(set-difference '(a b c d) '(b d e t))
(set-difference nil '(a c))
(set-difference '(a b c) nil)
(my-set-difference '(a b c) nil)
(my-set-difference nil '(a b c))
(my-set-difference '(a b c d) '(b d e t))

(defun count-odd (xs)
  "Counts the number of odd elements using conditional augmentation recursion."
  (cond ((null xs) 0)
	((oddp (first xs)) (1+ (count-odd (rest xs))))
	(t (count-odd (rest xs)))))

;; testing
(count-odd '(1 2 3 4 5))

(defun count-odd-2 (xs)
  "Does the same as count-odd but using regular augmenting recursion."
  (cond ((null xs) 0)
	(t (+ (if (oddp (first xs)) 1 0) (count-odd-2 (rest xs))))))

;; testing
(count-odd-2 '(1 2 3 4 5))
(count-odd nil)

(defun fib-naive (n)
  "Recursive naive version for compute the nth Fibonacci number."
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib-naive (- n 1))
	      (fib-naive (- n 2))))))

;; testing
(fib-naive 20)

(defun combine (x y)
  "Returns the sum of it's two inputs."
  (+ x y))

(defun fib-naive-2 (n)
  "Recursive naive version for compute the nth Fibonacci number."
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (combine (fib-naive (- n 1))
		    (fib-naive (- n 2))))))

;; Trees and car/cdr recursion

(defun atoms-to-q (xs)
  "Takes a tree and returns a new tree in which every non-NIL atom is the symbol q."
  (cond ((null xs) nil)
	((atom xs) 'q)
	(t (cons (atoms-to-q (car xs))
		 (atoms-to-q (cdr xs))))))

;; testing
(atoms-to-q '(a . b))
(atoms-to-q '(a (b c) e))

(defun count-atoms (tree)
  "Returns the number of atoms in a tree."
  (cond ((null tree) 1)
	((atom tree) 1)
	(t (+ (count-atoms (car tree))
	      (count-atoms (cdr tree))))))

;; testing
(count-atoms '(a (b) c))

(defun count-cons (tree)
  "Returns the  number of cons cells."
  (cond ((null tree) 0)
	((atom tree) 0)
	(t (+ 1 (+ (count-cons (car tree)) (count-cons (cdr tree)))))))

;; testing
(count-cons '(foo))
(count-cons '(foo bar))
(count-cons '((foo)))
(count-cons 'fred)
(count-cons '(((gold . and)) (th . 3) bears))

(defun sum-tree (tree)
  "Returns the sum of all the numbers in the tree."
  (cond ((or (null tree) (symbolp tree)) 0)
	((numberp tree) tree)
	(t (+ (sum-tree (car tree))
	      (sum-tree (cdr tree))))))

;; testing
(sum-tree '((3 bears) (3 bowls) (1 cup)))

(defun my-subst (new old tree)
  "Recursive version of the subst function."
  (cond ((null tree) nil)
	((atom (first tree)) (if (equal old (first tree)) (cons new (my-subst new old (cdr tree)))
			                             (cons (first tree) (my-subst new old (cdr tree)))))
	(t (cons (my-subst new old (car tree)) (my-subst new old (cdr tree))))))


;; Testing
(subst "two" 2 '(1 (1 2) (1 2 3) (1 2 3 4)))
(subst "two" 2 nil)
(my-subst "two" 2 '(1 (1 2) (1 2 3) (1 2 3 4)))
(cdr '(1 (1 2) (1 2 3) (1 2 3 4)))

(defun flatten (xs)
  "Returns all the elements of an arbitrary nested list in a single-level list."
  (cond ((null xs) nil)
	((atom (first xs)) (cons (first xs) (flatten (cdr xs))))
	(t (append (flatten (car xs))
       		   (flatten (cdr xs))))))

;; testing
(flatten '((a b r) (a) (c a d (a)) ((b) r a))) 

(defun tree-depth (tree)
  "Returns the maximum depth of a binary tree."
  (cond ((or (null tree) (symbolp tree)) 0)
	((atom (first tree)) (1+ (tree-depth (cdr tree))))
	(t (1+ (max (tree-depth (car tree))
		    (tree-depth (cdr tree)))))))

;; testing
(tree-depth nil)
(tree-depth '(a . b))
(tree-depth '((a b c d)))
(tree-depth '((a . b) . (c . d)))
(cons (cons 'a 'b) (cons (cons 'c 'd) nil))

(defun paren-depth (xs)
  "Returns the maximum depth of nested parenthesis in a list."
  (cond ((null xs) 1)
	((atom (first xs)) (paren-depth (cdr xs)))
	(t (max (1+ (paren-depth (car xs)))
		(paren-depth (cdr xs))))))

;; testing
(paren-depth nil)
(paren-depth '(a b c))
(paren-depth '(a b ((c) d) e))
(paren-depth '((c)))

(defun count-up (n)
  "Counts from one up to n."
  (cond ((= 0 n) nil)
	(t (append (count-up (1- n)) (list n)))))

;; testing
(count-up 5)

(defun make-loaf (n)
  "Returns a loaf of size n."
  (if (= n 0)
      nil
      (cons 'x (make-loaf (1- n)))))

;; testing
(make-loaf 4)

(defun bury (item n)
  "Buries an item under n levels of parentheses."
  (if (= n 0)
      item
      (list (bury item (1- n)))))

;; testing
(bury 'fred 5)

(defun pairings (xs ys)
  "Pairs the elements of two lists of equal length."
  (cond ((null xs) nil)
	(t (cons (list (first xs) (first ys)) (pairings (rest xs) (rest ys))))))

;; testing
(pairings '(a b c) '(1 2 3))

(defun sublists (xs)
  "Returns the sucesive sublists of a list."
  (cond ((null xs) nil)
	(t (cons xs (sublists (rest xs))))))

;; testing
(sublists '(fee fie foe))

(defun my-reverse (xs)
  "Recursive version of the reverse function."
  (cond ((null xs) nil)
	(t (append (my-reverse (rest xs)) (list (first xs))))))

;; testing
(my-reverse '(1 2 3 4))

(defun my-union (xs ys)
  "Recursive version of the union function."
  (cond ((null xs) ys)
	((null ys) xs)
	((not (member (first xs) ys)) (cons (first xs)
				      (my-union (rest xs) ys)))
	(t (my-union (rest xs) ys))))

;; testing
(my-union '(1 2 3) '(3 4 5))
(my-union nil '(1 2 3 4))

(defun largest-even (xs)
  "Returns the largest even number in a list of nonnegative numbers."
  (cond ((null xs) 0)
	((numberp xs) (if (evenp xs) xs 0))
	(t (max (largest-even (car xs)) (largest-even (cdr xs))))))

;; testing
(largest-even '(3 7 1))
(evenp '(1 2 3))
(car '(3))
(cdr '(3))

(defun huge (n &optional (cont 0))
  "Raises a number to it's own power."
  (cond ((or (= n 0) (= cont n)) 1)
	(t (* n (huge n (1+ cont))))))

;; testing
(huge 2)
(huge 4)

(defun every-other (xs)
  "Returns every other element of a list."
  (cond ((null xs) nil)
	(t (cons (first xs) (every-other (cddr xs))))))

;; testing
(every-other '(a b c d e f g))
(every-other '(i came i swa i conquered))

(defun left-half (xs)
  "Returns the first n/2 elements of a list of length n."
  (left-half-helper xs (/ (length xs) 2) 0))

(defun left-half-helper (xs half cont)
  (cond ((null xs) nil)
	((>= cont half) nil)
	(t (cons (first xs) (left-half-helper (rest xs) half (1+ cont))))))

;; testing
(round (/ (length '(1 2 3)) 2.0))
(left-half '(a b c d e))
(left-half '(1 2 3 4 5 6 7 8))

(defun merge-lists (xs ys)
  "Takes two lists of numbers, each in increasing order 
   and returns a list that is a merger of it's input, in order."
  (cond ((null xs) ys)
	((null ys) xs)
	((= (first xs) (min (first xs) (first ys))) (cons (first xs) (merge-lists (rest xs) ys)))
	(t (cons (first ys) (merge-lists xs (rest ys))))))

;; testing
(merge-lists '(1 2 6 8 10 12) '(2 3 5 9 13))


;; The debugger
;; https://malisper.me/debugging-lisp-part-1-recompilation/
;; https://lispcookbook.github.io/cl-cookbook/debugging.html#the-interactive-debugger
(defun fact-debug (n)
  (cond ((zerop n) (break "N is zero"))
	(t (* n (fact-debug (1- n))))))

;; testing
(fact-debug 5)
(step (fact-debug 5))

;; keyboard exercise

;; To indent several lines with TAB: 1. C-x r t ... 2. C-q TAB
(defvar *family*
	'((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))

;; a
(defun father (person)
  "Returns the father of a person."
  (second (assoc person *family*)))

(defun mother (person)
  "Returns the motther of a person."
  (third (assoc person *family*)))

(defun parents (person)
  "Returns the parents of a person."
  (remove nil (cdr (assoc person *family*))))

(defun children (person)
  "Returns the children of a person."
  (cond ((null person) nil)
	(t (mapcar #'first (remove-if-not #'(lambda (element) (equal person (father (car element)))) *family*)))))

(defun children-2 (person)
  "Returns the children of a person."
  (remove nil (mapcar #'(lambda (element) (when (equal person (father (car element))) (car element))) *family*)))

(defun children-3 (person)
  "Returns the children of a person."
  (let ((children nil))
    (mapcar #'(lambda (element) (when (equal person (father (car element))) (push (car element) children))) *family*)
    children))

;; testing
(father 'suzanne)
(mother 'yvette)
(parents 'suzanne)
(parents 'frederick)
(children 'arthur)
(children-2 'arthur)
(children-3 'arthur)
	
