;;; exercises from sicp 1.2
;;; as with 1.1, this may depend on some code from 1.2.scm (e.g., testcase code)
(load "1.2.scm")

;;; 1.9

;;; assume a,b>0 and that inc, dec are defined
(define (+v1 a b)
  (if [= a 0]
      b
      (inc (+v1 (dec a) b))))

(define (+v2 a b)
  (if [= a 0]
      b
      (+v1 (dec a) (inc b))))

;;; process for +v1:
;;; (commented b/c inc/dec aren't defined)
;; (+v1 4 5)
;; (inc (+v1 (dec 4) 5))
;; (inc (+v1 3 5))
;; (inc (inc (+v1 (dec 3) 5)))
;; (inc (inc (+v1 2 5)))
;; (inc (inc (inc (+v1 (dec 3) 5))))
;; (inc (inc (inc (+v1 2 5))))
;;; etc; linear recursion

;;; process for +v2:
;; (+v2 4 5)
;; (+v2 (dec 4) (inc 5))
;; (+v2 3 6)
;; (+v2 (dec 3) (inc 6))
;; (+v2 2 7)
;;; etc; linear iteration

;;; 1.10
;;; Ackermann's function
(define (A x y)
  (cond ([= y 0] 0)
	([= x 0] (* 2 y))
	([= y 1] 2)
	(else (A (1- x)
		 (A x (1- y))))))

(A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;;; seems like (A 1 n) => 2^n

(A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; ;;; (using observation above that (A 1 n) => 2^n)
;; (A 1 16)
;; ;;; (using that observation again)
;; 65536

;;; (A 2 1) => 2
;;; (A 2 2) => 2^2 = 4
;;; (A 2 3) => 2^4 = 16
;;; (A 2 4) => 2^16 = 65536
;;; (A 2 1) => 2^65536 = ???

(A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; (A 2 (A 1 (A 2 1)))
;; (A 2 (A 1 2))
;; (A 2 4)
;; ;;; using previous answer
;; 65536

(define (f n) (A 0 n))			; 2*n
(define (g n) (A 1 n))			; 2^n
(define (h n) (A 2 n))			; ^n(2) (tetration)

;;; 1.11
;;; not exactly tribonacci, but close enough

;;; recursive process
(define (trib n)
  (if [< n 3]
      n
      (+ (trib (- n 1))
	 (* 2 (trib (- n 2)))
	 (* 3 (trib (- n 3))))))

;;; iterative process
(define (trib-2 n)
  (if [< n 3]
      n
      (let iter ([a 2] [b 1] [c 0] [n (- n 2)])
	(if [zero? n]
	    a
	    (iter (+ a (* 2 b) (* 3 c))
		  a
		  b
		  (1- n))))))

;;; defined in 1.2.scm; first version is already getting a little bit slow
(tc-iota trib 30)
(tc-iota trib-2 30)

;;; 1.12: Pascal's triangle
;;; very simple version without memoization, akin to the other examples so far
;;; - row: 1-indexed row; col: 1-indexed position in row (col <= row)
(define (pascal row col)
  (if [or (< row 1)
	  (< col 1)
	  (> col row)]
      -1
      (let rec ([row row]
		[col col])
	(if [or (= col 1) (= col row)]
	    1
	    (+ (rec (1- row) (1- col))
	       (rec (1- row) col))))))

;;; generate a row of pascal's triangle using the above method
(define (pascal-row row)
  (map (lambda (col) (pascal row (1+ col)))
       (iota row)))

;;; generate the first n rows of pascals triangle
(define (pascal-tri rows)
  (map pascal-row
       (map 1+ (iota rows))))

(pascal-tri 5)

;;; 1.13: Proof of Binet's formula
;;; I did this at some point in the past:
;;; http://eis.lambdalambda.ninja/posts/proof-of-binets-formula

;;; 1.14
;;; The coin change problem is exponential in steps, linear in space (depth).
;;; It is very similar to the bad fibonacci.

;;; 1.15
;;; a. p is run 5 times in (sine 12.15).
;;; b. This is linear recursion (but not tail recursive). Thus the space will be
;;; linear w.r.t. the number of steps. Since the angle is being /3 each
;;; iteration, this is logarithmic in steps (and space).
(define (cube x) (* x x x))
(define (p x)
  ;; (display "p was run!\n")		; cheating a little bit
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if [not (> (abs angle) 0.1)]
      angle
      (p (sine (/ angle 3.0)))))

;;; 1.16
(define (pow-4 b n)
  (let iter ([b b]
	     [n n]
	     [a 1])
    (cond ([zero? n] a)
	  ([even? n] (iter (square b)
			   (/ n 2)
			   a))
	  (else (iter b
		      (1- n)
		      (* b a))))))

;;; 1.17, 1.18: These should be pretty straightforward given the previous
;;; examples.

;;; 1.19: Interesting Fibonacci implementation using successive "squaring"
(define (fib-4 n)
  (let iter ([a 1]
	     [b 0]
	     [p 0]
	     [q 1]
	     [count n])
    (cond ([zero? count] b)
	  ([even? count] (iter a
			       b
			       ;; note the following two lines
			       (+ (square p) (square q))
			       (* q (+ (* 2 p) q))
			       (/ count 2)))
	  (else (iter (+ (* b q) (* a q) (* a p))
		      (+ (* b p) (* a q))
		      p
		      q
		      (1- count))))))

(tc-iota fib-4 10)
