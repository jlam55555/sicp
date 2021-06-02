;;; todo ...... the earlier exercises


;;; 1.34
(define (f g)
  (g 2))

(define (square x) (* x x))
(f square)

(f (lambda (z) (* z (1+ z))))

;;; (f f)
;;; (f 2)
;;; (2 2) => cannot apply non-procedure 2
