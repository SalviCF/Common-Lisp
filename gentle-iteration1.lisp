(defun it-member (x xs)
  (dolist (e xs)
    (if (equal e x) (return t))))

(it-member 2 '(3 4 5 76))

(defun it-length (xs)
  (let ((length 0))
    (dolist (el xs length)
      (setf length (1+ length)))))

(it-length '(1 2 3 4))

(defun launch (n)
  (do ((cont n (1- cont)))
      ((zerop cont) (format t "Blast off!"))
    (format t "~S..." cont)))

(launch 10)

(defun c-a-o (numbers)
  (do ((z numbers (rest z)))
      ((null z) t)
    (if (evenp (first z)) (return nil))))

(c-a-o '(1 3 3 5))

(defun find-largest (numbers)
  (do* ((l numbers (rest l))
        (n (first l) (first l))
        (maxi n))
       ((null l) maxi)
    (setf maxi (max maxi n))))

(find-largest '(15 3 6 3 55 3 2 3))

(defun power-2 (n)
  (do ((result 1 (incf result result))
       (count n (1- count)))
      ((zerop count) result)))

(power-2 3)

(defun fibo (n)
  (do* ((acc1 0 acc2)
        (acc2 1 res)
        (res 0 (+ acc1 acc2))
        (cont n (1- cont)))
       ((zerop cont) res)))

(fibo 8)

(defun sumatorio (&rest args)
  (reduce #'+ args))

(average 1 2 3)
