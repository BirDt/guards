(import
  (chicken syntax)
  guards)

(define-guarded (x
		 (y (>= 1))
		 (z (> 5) (< 15)))
  (print (+ y z)))

(x 1 10) ;; Prints 11
;;(x 1 5) <-- errors

(define-guarded (custom-add1
		 (x (number?)))
  (+ 1 x))

(print (custom-add1 2)) ;; 3
;;(print (custom-add1 "test")) <-- errors

(define-guarded ((foo (number?) (< 10))
		 (x (>= 1))) (+ 1 x))

(print (foo 8))
;; (print (foo 9)) <-- errors

