(defun check-all-odd (numbers)
  (do ((z numbers (rest z)))
      ((evenp (first z)) t)))

(check-all-odd '(1 2 3 4))
