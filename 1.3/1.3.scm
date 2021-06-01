;;; sicp 1.3: Formulating Abstractions with Higher-Order Procedures
;;; Part of the reason I am writing these out is to become more comfortable with
;;; writing Lisp; the rest of the reason is to become more comfortable with
;;; Emacs and Colemak

;;; 1.3.1: procedures as arguments
(define (sum-integers a b)
  ;; computes the sum of the integers from a to b
  (if [> a b]
      0
      (+ a (sum-integers (1+ a) b))))

(define (cube x)
  ;; cubes a number
  (* x x x))

(define (sum-cubes a b)
  ;; computes the sum of the cubes of the integers from a to b
  (if [> a b]
      0
      (+ (cube a) (sum-cubes (1+ a) b))))

;;; don't want to type out the next example...

(define (sum term a next b)
  ;; generalization of the above procedure
  (if [> a b]
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sum term a next b)
  ;; equivalent to the above, but using a named let
  (let iter ([n a])
    (if [> n b]
	0
	(+ (term n)
	   (iter (next n))))))

(define inc 1+)
(define (id n) n)
(define (sum-integers-2 a b)
  (sum id a inc b))
(define (sum-cubes-2 a b)
  (sum cube a inc b))

;;; now these do the same thing
(sum-integers 1 10)
(sum-integers-2 1 10)

(sum-cubes 1 10)
(sum-cubes-2 1 10)
