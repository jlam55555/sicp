;;; Reusable functions for examples and exercises; most of these are built
;;; up through exercises, so they might be shadowed by certain exercises, but
;;; that's okay. I will try to keep the best and most generic versions of
;;; procedures here.
;;;
;;; current utils functions:
;;; - basic arithmetic:
;;;   - identity
;;;   - square
;;;   - cube
;;;   - average
;;; - primality tests:
;;;   - divides?
;;;   - coprime?
;;;   - smallest-divisor
;;;   - prime? (deterministic trial division, O(sqrt(n)), fast for small nums)
;;;   - mr-prime? (fast probabalistic test using Miller-Rabin test, O(log^2 n))
;;; - function composition:
;;;   - double
;;;   - compose
;;;   - autocompose (was called "repeated" in the exercise)
;;; 
;;; other possible functions to include here, will include if they get reused:
;;; - factorial (1.2)
;;; - fibonacci (1.2 exercises, O(log n) version fib-4; or binet's formula)
;;; - pascal (1.2 exercises)
;;; - (regular) expmod, fermat-prime? (1.2)
;;; - timing function (1.2 exercises)
;;; - fixed-point method (1.3)
;;; - integration, derivative, newton's method (1.3)
;;; - filtered-accumulate (1.3 -- waiting for a list version)
;;; - esieve (favorite algo)
;;;
;;; possible constants to include here:
;;; - pi
;;; - e
;;; - phi/psi
;;; 
;;; other cool algorithms, but already implemented in (chez) stdlib:
;;; - gcd (1.2)
;;; - expt (1.2 exercises, pow-4 (iterative with repeated squaring))
;;; - sqrt (1.3, using fixed-point method or Newton's method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic arithmetic

(define (identity x)
  ;; identity function
  x)

(define (square x)
  ;; square a number
  (* x x))

(define (cube x)
  ;; cube a number
  (* x x x))

(define (average a b)
  ;; mean of two numbers
  (/ (+ a b) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; primality tests

(define (divides? a b)
  ;; test if a|b (a divides b)
  (zero? (remainder b a)))

(define (coprime? a b)
  ;; tests whether two numbers are relatively prime
  (= 1 (gcd a b)))

(define (smallest-divisor n)
  ;; finds the smallest divisor of n
  (let find-divisor ([n n]
		     [test-divisor 2])
    (cond ([> (square test-divisor) n] n)
	  ([divides? test-divisor n] test-divisor)
	  (else (find-divisor n
			      (if [= test-divisor 2]
				  3
				  (+ test-divisor 2)))))))

(define (prime? n)
  ;; trial division primality test
  (and (> n 1)
       (= n (smallest-divisor n))))

(define (expmod-mr b n m)
  ;; calculates b^n (mod m), and signals (returns 0) if a trivial divisor of
  ;; zero (mod m) is found
  (cond ([zero? n] 1)
	([even? n]
	 (let* ([val (expmod-mr b (/ n 2) m)]
		[val-sq (remainder (square val) m)])
	   ;; discovering a nontrivial sqrt of 1 (mod m)
	   (if [and (= val-sq 1)
		    (not (or (= val 1) (= val (1- m))))]
	       0
	       val-sq)))
	(else
	 (remainder (* b (expmod-mr b (1- n) m))
		    m))))

(define (mr-test a n)
  ;; miller rabin test: a^(n-1) = 1 (mod n)
  (= (expmod-mr a (1- n) n) 1))

(define (mr-test-random n)
  ;; performs the miller rabin test on a random integer integer a < n
  (mr-test (1+ (random (1- n)))
	   n))

(define (mr-prime? n)
  ;; test if number is prime using mr test and a fixed number of iterations
  (fast-prime? mr-test-random n 5))

(define (fast-prime? random-test n times)
  ;; performs the specified probabalistic primality test (e.g. fermat or mr)
  ;; for the specified number of times
  (if [< n 2]
      #f
      (let iter ([times times])
	(cond ([zero? times])
	      ([random-test n]
	       (iter (1- times)))
	      (#t #f)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function composition

(define (double f)
  (lambda (x)
    (f (f x))))

(define (compose f g)
  ;; returns the composition (f o g). Assumes f, g take one argument
  (lambda (x) (f (g x))))

(define (autocompose f n)
  ;; generalized version of double: returns the function obtained by composing
  ;; f with itself n times. Assumes n >= 0
  (if [zero? n]
      identity
      (let iter ([n n])
	(if [= n 1]
	    f
	    (compose f (iter (1- n)))))))
