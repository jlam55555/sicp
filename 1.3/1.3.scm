;;; sicp 1.3: Formulating Abstractions with Higher-Order Procedures
;;; Part of the reason I am writing these out is to become more comfortable with
;;; writing Lisp; the rest of the reason is to become more comfortable with
;;; Emacs and Colemak
(load "../utils/utils.scm")

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

;;; an approximation for pi
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(* 8 (pi-sum 1 1000))

;;; integrals!
(define (integral f a b dx)
  (* (sum f
	  (+ a (/ dx 2))
	  (lambda (x) (+ x dx))
	  b)
     dx))

(integral cube 0 1 1e-3)

;;; already been using lambdas and let, won't reproduce all those examples here

(define (him f a b)
  ;; finding zeroes by the half-interval method

  (define (close-enough? x y)
    ;; helper method to calculate the answer with some tolerance
    (< (abs (- x y)) 0.001))

  (define (search n p)
    ;; iterative procedure to narrow down the answer
    (let ([m (/ (+ n p) 2)])
      (if [close-enough? n p]
	  m
	  (let ([test-value (f m)])
	    (cond ([positive? test-value]
		   (search n m))
		  ([negative? test-value]
		   (search m p))
		  (#t m))))))

  (let ([fa (f a)]
	[fb (f b)])
    ;; driver code and error checking
    (cond ([and (negative? fa) (positive? fb)]
	   (search a b))
	  ([and (positive? fa) (negative? fb)]
	   (search b a))
	  (#t
	   (error 'him "values not opposite sign" a b)))))

;;; test cases
(him sin 2. 4.)
(him (lambda (x) (- (cube x) (* 2 x) 3)) 1. 2.)

;;; fixed point
(define tolerance 1e-5)
(define (fp f guess)
  ;; finding the fixed point of a function
  
  (define (close-enough? a b)
    ;; helper function
    (< (abs (- a b)) tolerance))

  (let iter ([g guess])
    (let ([next (f g)])
      (if [close-enough? g next]
	  next
	  (iter next)))))

;;; test cases
(fp cos 1.)

(define (sqrt x)
  ;; a naive approach for sqrt -- will not converge
  (fp (lambda (y) (/ x y))
      1.))

(define (average x y)
  ;; I should really have these helper functions all in one place now
  (/ (+ x y) 2.0))

(define (sqrt x)
  ;; a convergent version of sqrt using the fixed point
  (fp (lambda (y) (average y (/ x y)))
      1.0))

(sqrt 49)

;;; 1.3.4: procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt x)
  ;; I need to find out a better naming system for different versions of
  ;; these functions
  (fp (average-damp (lambda (y) (/ x y)))
      1.0))

(define dx 1e-5)
(define (deriv g)
  ;; inverse of the antiderivative
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newtons-method g guess)
  ;; newton's method for finding zeroes of a function
  (fp (lambda (x)
	(- x (/ (g x) ((deriv g) x))))
      guess))

(define (sqrt x)
  ;; yet another version of sqrt
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))
