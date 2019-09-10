;; LOOP

(defun collecting-numbers (n)
  (do ((nums nil)
       (i 1 (1+ i)))
      ((> i n) (reverse nums))
    (push i nums)))

(collecting-numbers 6)

(loop for i from 1 to 10 collect i) ; so expressive

(loop for i from 1 to 10 summing (expt i 2)) ; su, the first 10 squares

(loop for x across "the black cat with hat" counting (find x "aeiou")) ; vowels of a string

;; computes the 9 nth Fibonacci number
(loop for i below 8
   and a = 0 then b
   and b = 1 then (+ b a)
   finally (return a))

(loop for i upto 10 collect i)
(loop for i from 0 downto -10 collect i)
(loop for number in '(2 4 6 8) collect number)
(loop for number in (list 10 20 30 40) by #'cddr collect number)
(loop for number on (list 10 20 30 40) collect number)

(cons 10 (cons 20 (cons 30 (cons 40 nil))))

(loop for x across "abcd" collect x)

;; initial x is 0. Step for x in next iteration: value of y
(loop repeat 5
   for x = 0 then y
   for y = 1 then (+ x y)
   collect y)

;; To evaluate before the variables get their updated values
(loop repeat 5
      for x = 0 then y
      and y = 1 then (+ x y)
   collect y)


;; Summary information about numbers
(defparameter *random* (list 2 3 65 3 5 7 56 7 45 234 8 5 4521 9 86))

(loop for i in *random*
   counting (evenp i) into evens
   counting (oddp i) into odds
   summing i into total
   maximizing i into max
   minimizing i into min
   finally (return (list min max total evens odds)))

(block outer
  (loop for i from 0 return 100) ; 100 returned from LOOP
  (print "This will print")
  200)

(block outer
  (loop for i from 0 do (return-from outer 100)) ; 100 returned from BLOCK
  (print "This won't print")
  200)

(defparameter collection (loop for i upto 10 collecting i))
collection

(loop for i upto 10 when (oddp i) collect i)

(loop for x below 10
   collect (loop for y below 10
              collect (+ x y)))

(loop for i below 5
     append (loop for j below 5 collect j))
