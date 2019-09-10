

(defparameter *message* "abcdd")

(defparameter *soup* "abcdefghijk")


(defun check-soup (message soup)
  (loop
     for char across message
     thereis (find char soup :test 'char=)))


(defun check-soup2 (message soup)
  (let ((letters (make-hash-table :test 'equal)))
    (loop
       for letter across message
       do (incf (gethash letter letters 0)))
    (loop
       for soup-letter across soup
       do (decf (gethash soup-letter letters 0)))
    (maphash (lambda (key val)
	       (declare (ignore key))
	       (when (plusp val)
		 (return-from check-soup2 nil)))
	     letters)
    t))

(check-soup2 *message* *soup*)




(mapcar (lambda (a) (+ 1 a)) '(1 2 3 4))



;; (defun test1 (a b)
;;   (+ a b))

;; (defun test2 (a &optional (b 5))
;;   (+ a b))

;; (defun test3 (a &key (b 5))
;;   (+ a b))



;; (defun abc () 1)
;; (defvar abc 3)

;; (abc)
;; abc


;; (funcall 'abc)

;; #'abc

;; (test3 1 :b 3)



;; '(1 2 3 4)


;; (let ((a 1)
;;       (b 2)
;;       (c 3))
;;   `(a ,b c))





