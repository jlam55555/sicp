(load "1.3.scm")

;;; 1.29: Simpson's method
(define (simpson f a b n)
  ;; simpson's method for calculating an integral
  (let ([h (/ (- b a) n)])
    (define (g x)
      ;; g is f with the [1 2 4 2 4 ... 4 2 1] coefs
      (let ([k (exact (round (/ (- x a) h)))])
	(* (cond ([or (zero? k) (= k n)] 1)
		 ([even? k] 2)
		 (#t 4))
	   (f x))))

    ;; driver
    (if [odd? n]
	(error 'simpson "n must be even")
	(* h 1/3 (sum g a (lambda (x) (+ x h)) b)))))

;;; note that simpson's rule is exact for cubics
(simpson cube 0 1 100)			; exact answer
(simpson cube 0 1 1000)
(simpson sqrt 0 1 100)			; not exact
(simpson sqrt 0 1 1000)

;;; 1.30: linear version of the sum method
(define (sum term a next b)
  ;; iterative version of sum
  (let iter ([a a]
	     [result 0])
    (if [> a b]
	result
	(iter (next a)
	      (+ result (term a))))))

;;; 1.31: analogous product procedure
(define (product term a next b)
  ;; recursive product accumulation method written analogously to sum
  (if [> a b]
      1
      (* (term a)
	 (product term (next a) b))))

(define (product term a next b)
  ;; iterative product accumulation method written analogously to sum
  (let iter ([a a]
	     [result 1])
    (if [> a b]
	result
	(iter (next a)
	      (* result (term a))))))

;;; test case: approximate pi (again)
(define (pi-product-term n)
  ;; get one term of the sequence 2/3, 4/3, 4/5, 6/5, ...
  ;; (first term is arbitrarily n=0)
  ;; numerator is n - floor((n+1)%2) + 3
  ;; denominator is n - floor(n%2) + 3
  (if [even? n]
      (/ (+ n 2)
	 (+ n 3))
      (/ (+ n 3)
	 (+ n 2))))

(define (estimate-pi n)
  ;; estimate pi using the method by John Wallis using n terms of the product
  (* 4 (product pi-product-term 0 1+ n)))

;;; test cases
(exact->inexact (estimate-pi 1))
(exact->inexact (estimate-pi 10))
(exact->inexact (estimate-pi 100))
(exact->inexact (estimate-pi 1000))

;;; 1.32: a generalization of sum and product
;;; note that the following two procedures are not always equivalent due to the
;;; fact that the recursive version builds up operations and applies them rtl
;;; while the iterative version applies them ltr. This is important for non-
;;; associative functions, e.g., building a list with cons (this is a rtl
;;; operation. See: https://stackoverflow.com/q/24370549)

(define (accumulate combiner null-value term a next b)
  ;; recursive version of accumulate (fold-right)
  (let iter ([n a])
    (if [> n b]
	null-value
	(combiner (term n)
		  (iter (next n))))))

(define (accumulate combiner null-value term a next b)
  ;; iterative version of accumulate (fold-left)
  (let iter ([n a]
	     [result null-value])
    (if [> n b]
	result
	(iter (next n)
	      (combiner result (term n))))))

(define (sum-acc term a next b)
  ;; implementation of sum using accumulate
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  ;; implementation of product using accumulate
  (accumulate * 1 term a next b))

;;; 1.33: an even more general version: accumulate and filter
(define (filtered-accumulate predicate? combiner null-value term a next b)
  ;; iterative version of filter and then accumulate (fold-left)
  (let iter ([n a]
	     [result null-value])
    (if [> n b]
	result
	(iter (next n)
	      (if [predicate? n]
		  (combiner result (term n))
		  result)))))

;;; 1.33a
(define (sum-squares-primes-interval a b)
  ;; sum of squares of primes in the inclusive interval [a,b]
  (filtered-accumulate prime? + 0 square a 1+ b))

;;; 1.33b
(define (product-coprime n)
  ;; takes the product of positive integers 0 < a < n
  (define (coprime-to-n? a) (coprime? a n))
  (filtered-accumulate coprime-to-n? * 1 identity 1 1+ (1- n)))

;;; now we have a very generic filter-map-reduce framework on an arbitrary
;;; sequence. The only thing I might change is that it might be nice to have
;;; the filter occur on the mapped values (this is easy to implement in this
;;; framework but it requires the recalculation of the mapped values) or to
;;; have the input be a list rather than passing a start/end/next (but we
;;; haven't covered lists in the book yet so maybe this is coming?)

;;; 1.34
(define (f g)
  (g 2))

(define (square x) (* x x))
(f square)

(f (lambda (z) (* z (1+ z))))

;;; (f f)
;;; (f 2)
;;; (2 2) => cannot apply non-procedure 2

;;; 1.35
;;; phi^2 = phi + 1
(fp (lambda (x) (1+ (/ 1.0 x))) 1.0)	; using fp
(/ (1+ (sqrt 5)) 2)			; true value
