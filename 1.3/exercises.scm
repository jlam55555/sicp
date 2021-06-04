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
;;; phi^2 = phi + 1 => divide both sides by phi
;;; x_{n+1} = 1 + 1/x_n
(fp (lambda (x) (1+ (/ 1.0 x))) 1.0)

;;; 1.36
(define (fp f guess)
  ;; altered to print out the values
  
  (define (close-enough? a b)
    ;; helper function
    (< (abs (- a b)) tolerance))

  (let iter ([g guess])
    (display g)
    (newline)
    (let ([next (f g)])
      (if [close-enough? g next]
	  next
	  (iter next)))))

;;; x^x = 1000
;;; x*log(x) = log(1000)
;;; x = log(1000)/log(x)
(define (fp-x^x x)
  (/ (log 1000) (log x)))
(fp fp-x^x 2.0)
(fp (average-damp fp-x^x) 2.0)

;;; 1.37: infinite continued fractions (!!!)
(define (cont-frac n d k)
  ;; recursively calculates the continued fraction in the form
  ;; n[1]/(d[1]+n[2]/(d[2]+n[3]/(d[3]+ ... +n[k]/d[k])))
  ;; where n and d are functions that generate sequences
  (let iter ([i 1])
    (if [> i k]
	0
	(/ (n i) (+ (d i) (iter (1+ i)))))))

(define (cont-frac n d k)
  ;; iteratively calculates the continued fraction in the form
  ;; n[1]/(d[1]+n[2]/(d[2]+n[3]/(d[3]+ ... +n[k]/d[k])))
  ;; where n and d are functions that generate sequences;
  ;; this iterative version necessarily calculates the fraction
  ;; from inside to out
  (let iter ([i k]
	     [result 0])
    (if [zero? i]
	result
	(iter (1- i)
	      (/ (n i) (+ (d i) result))))))

(define (one x) 1.0)
(/ 1.0 (cont-frac one one 10))
(/ 1.0 (cont-frac one one 100))
(/ 1.0 (cont-frac one one 1000))

;;; 1.38: a continued fraction approximation for e
(define (e-denom i)
  ;; generates the denominator sequence in the continued fraction appoximation
  ;; for e published by Euler in De Fractionibus Continuis
  (if [= (mod i 3) 2]
      (+ 2 (* 2 (fx/ i 3)))
      1))

(+ 2 (cont-frac one e-denom 10))

;;; 1.39: a continued fraction appoximation for tan
(define (tan-cf x)
  ;; Lambert's approximation for the tangent function
  ;; use fixed number of iterations for simplicity
  (inexact (cont-frac (lambda (i) (if [= i 1] x (- (square x))))
		      (lambda (i) (1- (* 2 i)))
		      10)))

;;; 1.40: cubic function generator
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;; x^3+27 = (x+3)(x^2+3x+3)
;;; should have a zero around -3
(newtons-method (cubic 0 0 27) -1)

;;; 1.41: doubling (procedures)
(define (double f)
  ;; applies f twice to its argument. (f should take and return one argument
  ;; of the same type)
  (lambda (x) (f (f x))))

;;; the following returns 21
;;; double => runs function twice
;;; (double double) => runs double runs on itself, runs four times (quadruple)
;;; (double (double double)) => runs quadruple on itself, runs 16 times
(((double (double double)) 1+) 5)

;;; or, more explicitly:
;;; f: x -> (f x)
;;; double: f -> (x -> (f (f x)))
;;; (double f) = x -> (f (f x))
;;; db2 = (double double) = f -> (double (double f))
;;; db3 = (double db2)) = f -> (db2 (db2 f))
;;;                     = (double (double (double (double f))))
;;; (db3 1+) = x -> (double (double (double (1+ (1+ x)))))
;;;          = x -> (double (double (1+ (1+ (1+ (1+ x))))))
;;;          = x -> (double (1+ (1+ (1+ (1+ (1+ (1+ (1+ (1+ x)))))))))
;;;          = x -> 16 times add 1

;;; 1.42: function composition
(define (compose f g)
  ;; returns the composition (f o g). Assumes f, g take one argument
  (lambda (x) (f (g x))))

;;; (6+1)^2
((compose square 1+) 6)

;;; 1.43: repeated autocomposition
(define (repeated f n)
  ;; generalized version of double: returns the function obtained by composing
  ;; f with itself n times. Assumes n >= 1
  (let iter ([n n])
    (if [= n 1]
	f
	(compose f (iter (1- n))))))

(define (double f)
  ;; an alternative way to define double using the generalized version
  (repeated f 2))

(((double (double (double double))) 1+) 5)

;;; 1.44: smoothing
(define (smooth f dx)
  ;; returns a smoothed version of f
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx))) 3)))

(define (n-smooth f dx n)
  ;; returns a n-fold smoothed version of f
  ((repeated (lambda (f) (smooth f dx)) n) f))

;;; 1.45: repeated average damping

;;; TODO

;;; 1.46: a general framework for iterative improvement

;;; TODO
