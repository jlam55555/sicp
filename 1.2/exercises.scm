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

;;; 1.21
(map smallest-divisor '(199 1999 19999))

;;; 1.22
(define (runtime)
  ;; mimicking sicp (runtime)
  (current-time))

(define (elapsed start stop)
  ;; elapsed time helper function (similar to time, but won't automatically
  ;; print the result
  (let ([diff (time-difference stop start)])
    (+ (time-second diff)
       (/ (time-nanosecond diff) 1e9))))

(define (timed-prime-test n)
  ;; test if number is prime and print elapsed time if prime
  (newline)
  (display n)
  (let ([start (runtime)])
    (when [prime? n]
      (display " *** ")
      (display (elapsed start (runtime))))))

(define (search-for-primes low high)
  ;; test for primes in the odd numbers of the range [low, high]
  ;; this doesn't check 2 nor exclude 1
  (let iter ([n (- (1+ low) (remainder low 2))])
    (when [<= n high]
      (timed-prime-test n)
      (iter (+ n 2)))))

;;; test cases
;;; sqrt(1e4) = sqrt(1e6)/10, so we expect that the runtimes are one order of
;;; magnitude apart for these two, and they are as expected
(search-for-primes 1e3 (+ 1e3 100))
(search-for-primes 1e4 (+ 1e4 100))
(search-for-primes 1e5 (+ 1e5 100))
(search-for-primes 1e6 (+ 1e6 100))
