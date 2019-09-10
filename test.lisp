;; Adds two numbers

(defun adder (x y)
  (+ x y))


;; Subtracts two numbers

(defun sub (x y)
  (- x y))


;; Prints "Hello, world"

(defun hello-world () (format t "Hello, world"))

;; Keyword parameters. Taking a variable number of 

(defun foo (&key a b c) (list a b c))

;; Exmaple of call > (foo :a 1 :c 3)


;; Keyword parameters. Distinguish explicit NIL passed
;;        default value |          | supplied p parameter
;; The supplied-p param tell us if that parameter was passed (even as NIL)
;; General form: (name default-value passed/not-passed)
(defun foo2 (&key a (b 20) (c 30 c-passed))
  (list a b c c-passed))


;; Formatting conventions

(defun equal3 (x)
  (if (= x 3)
      (print "true")
      (print "false")))


; end buffer


(defun check-all-odd (numbers)
  (do ((z numbers (rest z)))
      ((evenp (first z)) t)))

(check-all-odd '(1 2 3 4))
