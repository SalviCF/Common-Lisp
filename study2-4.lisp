((lambda (x y) (+ x y)) 2 3)
(+ 2 3) ; is the same

(eval '(+ 2 2))

(eval '(list * 9 6))

(eval (list '* 9 6))

(eval '(list '* 9 6))

(eval (eval '(list '* 9 6)))

;; The way of quoting functions
; using #' instead of only '
(apply #'+ '(2 3))

; apply takes a symbol function and a list
(cons 'as '(u like))
(apply #'cons '(as (u like)))

(eval (list 'cons t nil))
(apply #'cons '(t nil))
(eval nil)
(eval (list 'eval nil))
(eval (eval nil))

(defun even-odd (n)
  (if (oddp n)
      (list n 'is 'odd)
      (list n 'is 'even)))

(even-odd 4)

(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
	((< x y) 'first-is-smaller)
	(t 'first-is-bigger)))

(compare 4 4)

(defun first-zero (xs)
  (cond ((equal 0 (nth 0 xs)) 'first)
	((equal 0 (nth 1 xs)) 'second)
	((equal 0 (nth 2 xs)) 'third)
	(t 'none)))

(first-zero '(1 2 3))
(first-zero '(0 2 3))
(first-zero '(1 0 3))
(first-zero '(1 2 0))

(defun cycle (n)
  (cond ((< n 99) (+ n 1))
	(t 1)))

(cycle 21)
(cycle 99)
(cycle -3)

(defun constrain-cond (x min max)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(constrain-cond 3 -50 50)
(constrain-cond 92 -50 50)
(constrain-cond 3 10 20)

(defun constrain-ifs (x min max)
  (if (< x min)
      min
      (if (> x max)
	  max
	  x)))

(constrain-ifs 3 -50 50)
(constrain-ifs 92 -50 50)
(constrain-ifs 3 10 20)
