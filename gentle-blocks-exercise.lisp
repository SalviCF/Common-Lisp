;;; Keyboard exercise

(defparameter *database*
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

;; a

(defun match-element (x y)
  "Takes two symbols and returns T if they are equal or the second is a question mark."
  (or (equal x y) (equal y '?)))

;; b

(defun match-triple (assertion pattern)
  "Takes an assertion and a pattern and returns T if the assertion matches the pattern."
  (every #'(lambda (b) (equal b t)) (mapcar #'match-element assertion pattern)))

;; c

(defun fetch (pattern)
  "Takes a pattern and returns all assertions in the database that match the pattern."
  (remove-if-not #'(lambda (assertion) (match-triple assertion pattern)) *database*))

;; d

(fetch '(b4 shape ?))
(fetch '(? shape brick))
(fetch '(b2 ? b3))
(fetch '(? color ?))
(fetch '(b4 ? ?))

;; e

(defun pattern-generator-color (block-name)
  "Takes a block name and returns a pattern asking the color of the block."
  (list block-name 'color '?))

;; f

(defun supporters (block-name)
  "Takes a block and returns a list of the blocks that support it."
  (mapcar #'third (fetch (list block-name 'supported-by '?))))

;; g

(defun supp-cube (block-name)
  "Takes a block and returns T if that block is supported by a cube."
  (supporters block-name))

;; h


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a
(match-element 'red 'red)n
(match-element 'red '?)
(match-element 'red 'blue)

;; b
(mapcar #'equal '(1 2 3) '(1 2 4)) ; comparing element-wise with equal
(mapcar #'match-element '(b2 color red) '(b2 color ?))
(match-triple '(b2 color red) '(b2 color ?))
(match-triple '(b2 color red) '(b2 color green))

;; c
(fetch '(b2 color ?))
(fetch '(? supports b1))

;; e
(pattern-generator-color 'b3)

;; f
(supporters 'b1)

;; g
(supp-cube 'b4)


