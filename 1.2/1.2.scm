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
