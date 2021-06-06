;;; 2.1: Abstraction to Data Abstraction
;;; In procedural abstraction, we don't care how a component procedure is
;;; implemented; similarly, we don't care how a data structure is constructed
;;; from simpler data structures. Procedures should operate on *abstract* data
;;; structures, as opposed to *concrete* data structures. The interface between
;;; abstract and concrete data structures are constructors and selectors.
(load "../utils/utils.scm")

;;; 2.1.1: Rational numbers
;;; From algebra, x and y should be from some integral domain R, which defines
;;; a sum and product on them
(define (add-rat x y)
  ;; add two rational numbers
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  ;; subtract two rational numbers
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  ;; multiply two rational numbers
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  ;; divide two rational numbers
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  ;; two rational numbers a/b = c/d iff ad=bc
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  ;; a rational number a/b is represented as a pair
  (cons n d))

(define (numer x)
  ;; numerator of a rational number
  (car x))

(define (denom x)
  ;; denominator of a rational number
  (cdr x))

;;; alternative definitions of make-rat, numer, denom
;;; note that these would reduce # of fncalls, but defeat debugging aids
;;; like trace
;; (define make-rat cons)
;; (define numer car)
;; (define denom cdr)

(define (print-rat x)
  (format #t "~a/~a\n" (numer x) (denom x)))

(define (make-rat x y)
  ;; a better make-rat
  (let ([d (gcd x y)])
    (cons (/ x d) (/ y d))))

;;; 2.1.3: What is meant by data?
(define (my-cons x y)
  ;; the most important part about an abstract data structure is that it meets
  ;; the conditions that define it, like a mathematical definition; its specific
  ;; implementation usually doesn't matter. See this implementation of cons/car/
  ;; cdr using only procedures and closures (a "procedural implementation" (?)
  ;; of a data structure) -- this cannot be distuinguished from the ordinary
  ;; implementation because its api is the same
  (lambda (i)
    (cond ([= i 0] x)
	  ([= i 1] y)
	  (#t (error 'my-cons "argument not 0 or 1")))))
(define (my-car c) (c 0))
(define (my-cdr c) (c 1))

;;; 2.1.4: Extended Exercise: Interval Arithmetic
(define (interval-add x y)
  ;; add two intervals
  (interval-make (+ (interval-lower-bound x) (interval-lower-bound y))
		 (+ (interval-upper-bound x) (interval-upper-bound y))))

(define (interval-mul x y)
  ;; multiply two intervals
  (let ([p1 (* (interval-lower-bound x) (interval-lower-bound y))]
	[p2 (* (interval-lower-bound x) (interval-upper-bound y))]
	[p3 (* (interval-upper-bound x) (interval-lower-bound y))]
	[p4 (* (interval-upper-bound x) (interval-upper-bound y))])
    (interval-make (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (interval-div x y)
  ;; divide an interval by another (multiply by the reciprocal)
  (interval-mul x
		(interval-make (/ 1.0 (interval-upper-bound y))
			       (/ 1.0 (interval-lower-bound y)))))
