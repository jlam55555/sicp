;;; this file relies on some of the code from the text
(load "1.1.scm")

;;; 1.1
10					; 10
(+ 5 3 4)				; 12
(- 9 1)					; 8
(/ 6 2)					; 3
(+ (* 2 4) (- 4 6))			; 6
(define a 3)				; void (a=3)
(define b (+ a 1))			; void (b=4)
(+ a b (* a b))				; 19
(= a b)					; #f
(if [and (> b a) (< b (* a b))]		; 4
    b
    a)
(cond ([= a 4] 6)			; 16
      ([= b 4] (+ 6 7 a))
      (else 25))
(+ 2 (if [> b a] b a))			; 6
(* (cond ([> a b] a)			; 16
	 ([< a b] b)
	 (else -1))
   (+ a 1))

;;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;; 1.3
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define ssq sum-of-squares)		; alias for convenience

(define (sum-of-squares-max x y z)	; note: denote conditionals with []
  (if [< x y]
      (if [< x z] (ssq y z) (ssq x y))
      (if [< y z] (ssq x z) (ssq x y))))

;;; 1.4
;;; This function adds b to a if b is positive, and adds b to a otherwise. This
;;; is effectively a+|b|.

;;; 1.5
(define (p) (p))

(define (test x y)
  (if [= x 0]
      0
      y))

;;; this will end badly
;; (test 0 (p))

;;; with normal-order we get the following expansion:
;; (if [= 0 0]
;;     0
;;     (p))
;; (if #t
;;     0
;;     (p))
;; 0

;;; with applicative-order we get the following (infinite) expansion:
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; ...

;;; unfortunately (or not), our interpreter is applicative order. However,
;;; even with this infinite recursion we do not get a stack overflow

;;; 1.6: rewriting if as a cond
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;;; using this will lead to INFINITE RECURSION because of applicative-order
;;; evaluation; in particular, the special construct (if) has hidden control
;;; that won't evaluate the recursive statement unnecesarily

;;; 1.7
;;; The fixed threshold of 0.001 won't work equally well across numbers of all
;;; magnitudes, intuitively. 
(sqrt 1e-10)

;;; The precision for subtracting two large numbers may not work so well
;;; because the mantissa takes up more bits. Thus the following may not complete
;;; because the |difference| > 0.001 always.
;; (sqrt 1e20)

(define frac-ratio-threshold 1e-6)
(define (good-enough?-2 guess x)
  (< (abs (- (square guess) x))
     (* guess frac-ratio-threshold)))

(define (sqrt-iter-2 guess x)
  (if [good-enough?-2 guess x]
      guess
      (sqrt-iter-2 (improve guess x)
		 x)))
(define (sqrt-2 x)
  (sqrt-iter-2 1.0 x))

;;; tests; these perform much better
(sqrt-2 1e-10)
(sqrt-2 1e20)

;;; 1.8
(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

;;; need to change this a little to work well with small numbers (same goes
;;; for sqrt)
(define (good-enough?-cbrt guess x)
  (< (abs (- (* guess guess guess) x))
     (* (min x guess) frac-ratio-threshold)))

;;; these functions are defined analogously to the sqrt case
(define (cbrt-iter guess x)
  (if [good-enough?-cbrt guess x]
      guess
      (cbrt-iter (improve-cbrt guess x)
		 x)))
(define (cbrt x)
  (cbrt-iter 1.0 x))

;;; tests
(cbrt 27)
(cbrt 231)
(cbrt 1e-24)
(cbrt 1e24)

;;; FROM THE SAMPLE PSET

;;; 3
(define p1
  (lambda (x y)
    (+ (p2 x y)
       (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a)
       (p2 b))))

;;; 4.1
(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y))
       (spindle x))))

(define spindle
  (lambda (w) (* w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

;;; 4.2
(define fact
  (lambda (n)
    (if [= n 0]
	1
	(* n (fact (- n 1))))))

;;; 4.3

;;; commands (in emacs):
;;; C-h t M-> C-p (a few times) C-Space C-n (a few times) M-w
;;; C-x RET C-y

;; Copyright (C) 1985, 1996, 1998, 2001-2021 Free Software Foundation,
;; Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
