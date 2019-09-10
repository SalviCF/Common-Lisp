;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;          MACROS           ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; My first macro: simulating let but using fewer parenthesis
(defmacro being (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; testing
(let ((x 5))
  (* x x))

(being x 5 (* x x))

;; Another macro: same effect as fold in Haskell
(defmacro fold (fun &rest args)
  `(reduce #',(first fun) ',args))

;; testing
(reduce #'+ '(1 2 3))

(fold (*) 1 1 1 1 1 1 2 2 2)
