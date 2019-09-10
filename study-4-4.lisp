(defun contains-article-p (sent)
  (or (member 'the sent)
      (member 'a sent)
      (member'an sent)))

(defun right-side (xs)
  (cdr (member '-vs- xs)))

(right-side '(large red shiny -vs- small shiny red four-sided pyramid))

(reverse '(large red shiny -vs- small shiny red four-sided pyramid))

(defun left-side (xs)
 (reverse (right-side (reverse xs))))

(left-side '(large red shiny -vs- small shiny red four-sided pyramid))

(defun count-common (xs)
  (length (intersection (left-side xs) (right-side xs))))

(count-common '(large red shiny -vs- small shiny red four-sided pyramid))

(defun compare (xs)
  (list (count-common xs) 'common 'features))

(compare '(small red metal cube -vs- red plastic small cube))

(defparameter *plist* '(:nombre salvi :apellido1 carrillo :apellido2 fuentes))
(getf *list* :nombre)

;;; Lists as tables
(setf words '((one un) (two deux) (three trois)))

;; Assoc
(assoc 'three words) ; similar to plist but also returns the keyword
(assoc 'six words)

;; But we can make a table behave like a plist like this:
(defparameter plist-words '(:one uno :two dos :three tres))
(defparameter table-words '((one uno) (two dos) (three tres)))

(getf plist-words :one)
(nth 1 (assoc 'one table-words))
(cadr (assoc 'one table-words))
(second (assoc 'one table-words))

(set-exclusive-or '(a b c) '(a b d))

(remove-duplicates '(a b c a c b))
(length table-words)

(defparameter text '(b a n a n a))

(remove 'a text)
(remove 'a text :count 2) ; keyword arguments
(remove 'a text :count 1 :from-end t)

(defparameter cards '((3 clubs) (5 diamonds) (ace spades)))
(member '(5 diamonds) cards)
(member '(5 diamonds) cards :test #'equal)
(second cards)

;;; Arrays, Hash tables & Property

;; Array creation
(defparameter my-vector '#(tuning violin 440 a)) ; an array is not a chain, is a contiguous block

(aref my-vector 0) ; equivalent to nth woth lists
(setf (aref my-vector 0) 'my)
my-vector

(find-if #'numberp my-vector) ; returns the value that makes true the predicate
(reverse my-vector)

;; Array creation with make-array
(make-array 5 :initial-element 3)
(make-array 5 :initial-contents '(a e i o u))
(make-array 5)

;; Strings as vectors
(length "coco")
(reverse "coco")
(aref "coco" 0)

(type-of 'f)
(type-of #\d)
(defparameter animal "coco")
(setf (aref animal 1) #\u)
animal

;; Hash table
;; Same functionality as an association list

(defparameter ht (make-hash-table))
ht
(type-of ht)
(setf (gethash 'jon ht) 
      '(attorney (16 mapple drive)))

(gethash 'jon ht)

(setf (gethash 'salvi ht)
      '(hola que tal))

(gethash 'salvi ht)
(setf (gethash 'value-nil ht)
      nil)

(gethash 'value-nil ht)
(gethash 'no-encontrado ht)

(describe ht)
ht

;; Property lists
(setf (get 'fred 'sex) 'male)
(setf (get 'fred 'age) '23)
(setf (get 'fred 'siblings) '(george wanda))
(describe 'fred)

(get 'fred 'age)
(get 'clara 'siblings 'unknown)
(get 'clara 'siblings)

(incf (get 'fred 'age))

(symbol-plist 'fred)
(remprop 'fred 'age)

(defun addprop (sym elem prop)
  (pushnew elem (get sym prop)))

(defun record-meeting (x  y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)

(symbol-plist 'jaime)
(record-meeting 'jaime 'daenerys)
(symbol-plist 'daenerys)
(record-meeting 'jaime 'brienne)

;; Some exercises

(setf (get 'alpha 'fooprop) '(a b c d e))
(symbol-plist 'alpha)

(defun subprop (sym elem prop)
  (delete elem (get sym prop))) ; delete is destructive, remove is not

(subprop 'alpha 'd 'fooprop)

(defun forget-meeting (x y)
  (subprop x y 'has-met)
  (subprop y x 'has-met))

(forget-meeting 'jaime 'daenerys)


(subprop 'daenerys 'jaime 'has-met)