;;; 2.1: Abstraction to Data Abstraction
;;; In procedural abstraction, we don't care how a component procedure is
;;; implemented; similarly, we don't care how a data structure is constructed
;;; from simpler data structures. Procedures should operate on *abstract* data
;;; structures, as opposed to *concrete* data structures. The interface between
;;; abstract and concrete data structures are constructors and selectors.

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
