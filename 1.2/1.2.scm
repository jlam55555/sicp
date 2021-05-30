;;; sicp 1.2

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

;;; binet's formula; write power using exp/log
(define (pow a b)
  (exp (* b (log a))))
(define phi
  (/ (+ 1 (sqrt 5))
     2))
(define (fib-2 n)
  (exact (round (/ (pow phi n)
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
