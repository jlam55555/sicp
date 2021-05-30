;;; sicp 1.2
;;; emacs notes (a substantially shorter list than last time):
;;; - C-s	isearch-forward
;;; - C-r	isearch-backward

;;; linear recursion: each item has to remain on the stack until the procedure
;;; is completed; stack maintains state between computations; i.e., linear
;;; stack space
(define (factorial n)
  (if [= n 0]
      1
      (* n (factorial (1- n)))))

;;; linear iteration (tail recursion): maintains state between computations
;;; as state variables; constant stack space
(define (factorial-2 n)
  (define (fact-iter product counter max-count)
    (if [> counter max-count]
	product
	(fact-iter (* counter product)
		   (1+ counter)
		   max-count)))
  (fact-iter 1 1 n))

(map factorial (iota 10))
(map factorial-2 (iota 10))

;;; note the differencs between a "process" and a "procedure": the latter refers
;;; to the syntactic structure, while the former refers to the flow of
;;; execution and state. The above example is a recursive procedure but not a
;;; recursive process.

;;; tree-recursive version of fib; bad
(define (fib n)
  (cond ([zero? n] 0)
	([= n 1] 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

;;; binet's formula
(define phi
  (/ (+ 1 (sqrt 5))
     2))
(define (fib-2 n)
  (exact (round (/ (expt phi n)
		   (sqrt 5)))))

;;; iterative version; rewritten to use let. Uses the realization that the state
;;; of the fibonacci process can be stored in three variables
(define (fib-3 n)
  (let fib-iter ([a 1]
		 [b 0]
		 [count n])
    (if [zero? count]
	b
	(fib-iter (+ a b)
		  a
		  (1- count)))))

;;; testcases; first version is very slow for numbers a little larger than this
(define (tc-iota fn n)
  (map fn (iota n)))
(tc-iota fib 30)
(tc-iota fib-2 30)
(tc-iota fib-3 30)

;;; counting change; rewritten to use a list
(define (count-change amount)
  (let ([coins '(1 5 10 25 50)])
    (let cc-iter ([amount amount]
		  [kinds-of-coins (1- (length coins))])
      (cond ([= amount 0] 1)
	    ([or (< amount 0)
		 (< kinds-of-coins 0)]
	     0)
	    (else (+ (cc-iter amount
			      (1- kinds-of-coins))
		     (cc-iter (- amount (list-ref coins kinds-of-coins))
			      kinds-of-coins)))))))

;;; exponentiation

;;; linear recursion for positive integer exponents
;;; (call it pow to not overwrite buitin expt)
;;; O(n) space and steps
(define (pow b n)
  (if [= n 0]
      1
      (* b (pow b (1- n)))))

;;; linear iteration for positive integer exponents
;;; O(n) steps, O(1) space
(define (pow-2 b n)
  (let iter ([counter n]
	     [product 1])
    (if [= counter 0]
	product
	(iter (1- counter)
	      (* b product)))))

;;; linear recursive method that using successive squaring
;;; O(log n) steps and space
(define (square x) (* x x))
(define (pow-3 b n)
  (cond ([zero? n] 1)
	([even? n] (square (pow-3 b (/ n 2))))
	(else (* b (pow-3 b (1- n))))))

;;; gcd using the Euclidean algorithm
(define (gcd a b)
  (if [= b 0]
      a
      (gcd b (remainder a b))))

;;; primality tests
(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ([> (square test-divisor) n] n)
	([divides? test-divisor n] test-divisor)
	(else (find-divisor n (1+ test-divisor)))))
(define (divides? a b)
  (zero? (remainder b a)))

;;; simplest (and certain) primality test
(define (prime? n)
  (= n (smallest-divisor n)))

;;; fermat test
(define (expmod b n m)
  ;; calculates b^n (mod m)
  (cond ([zero? n] 1)
	([even? n]
	 (remainder (square (expmod b (/ n 2) m))
		    m))
	(else
	 (remainder (* b (expmod b (1- n) m))
		    m))))

(define (fermat-test a n)
  ;; fermat test: a^p = a (mod p)
  (= (expmod a n n) a))

(define (fermat-test-random n)
  ;; tries the fermat test on a random integer a < n
  (fermat-test (1+ (random (1- n)))
	       n))

(define (fast-prime? random-test n times)
  ;; performs the fermat primality test for the specified number of times
  (let iter ([times times])
    (cond ([zero? times])
	  ([random-test n]
	   (iter (1- times)))
	  (#t #f))))

;;; perform some tests
;;; note that there are many Fermat pseudoprimes. 561 is a Charmichael number
;;; and necessarily shows up in the following list, but in my tests it was the
;;; only Fermat liar in the integers up to 1000
(length (filter (lambda (x) x)
		(map (lambda (n)
		       (if [fast-prime? fermat-test-random (+ n 2) 10]
			   (+ n 2)
			   #f))
		     (iota 1000))))

