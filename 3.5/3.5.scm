;;; notes from SICP 3.5: Streams
(load "../utils/utils.scm")

;;; assume we have primitives stream-cons, stream-car, stream-cdr, stream-null?

(define (stream-ref s n)
  ;; stream equivalent of ref
  (if [= n 0]
      (stream-car s)
      (stream-ref (stream-cdr s) (1- n))))

(define (stream-map proc s)
  ;; stream equivalent of map
  (if [stream-null? s]
      *empty-stream*
      (stream-cons (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
  ;; stream equivalent of filter
  (cond ([stream-null? s] *empty-stream*)
	([pred (stream-car s)]
	 (stream-cons (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(#t (stream-filter pred (stream-cdr s)))))

(define (stream-for-each proc s)
  ;; stream equivalent of for-each
  (when [not (stream-null? s)]
    (proc (stream-car s))
    (stream-for-each proc (stream-cdr s))))

(define (stream-fold-left proc initial s)
  ;; stream equivalent of fold-left (accumulate)
  (if [stream-null? s]
      initial
      (stream-fold-left proc
			(proc (stream-car s) initial)
			(stream-cdr s))))

(define (stream-display s)
  ;; print out a stream using stream-for-each
  (stream-for-each
   (lambda (x)
     (display x)
     (newline))
   s))

;;; we need special forms `delay` and `force` -- a delayed evaluation
;;; I will call them `make-delayed` and `delayed-eval`, using the naming
;;; convention; SICP notes that:
;; (cons-stream a b)
;;; should be equivalent to
;; (cons a (make-delayed b))
;;; They use the word "promise" but it's unlike a Javascript promise, which
;;; is evaluated right away. More like a regular callback in Javascript.

(define-syntax make-delayed
  ;; naive version of make-delayed: requires a macro to delay evaluation
  (syntax-rules ()
    ([_ expr]
     (lambda () expr))))

(define-syntax make-delayed
  ;; better version of make-delayed with memoization
  (syntax-rules ()
    ([_ expr]
     (let ([already-run? #f]
	   [result #f])
       (lambda ()
	 (if [not already-run?]
	     [begin
	       (set! result expr)
	       (set! already-run? #t)
	       result]
	     result))))))

(define (delayed-eval delayed)
  ;; get the value of a delayed procedure simply by evaluating it
  (delayed))

(define stream-null? null?)
(define *empty-stream* '())
(define (stream-car s) (car s))
(define (stream-cdr s) (delayed-eval (cdr s)))

(define-syntax stream-cons
  ;; also have to define this as a macro because b should not be eagerly
  ;; evaluated; we could put the `make-delayed` implementation in here, it
  ;; doesn't get used anywhere else
  (syntax-rules ()
    ([_ a b]
     (cons a (make-delayed b)))))

(define (stream-enumerate-interval low high)
  (if [> low high]
      *empty-stream*
      (stream-cons
       low
       (stream-enumerate-interval (1+ low) high))))

;;; good example: find second prime after 10000; only evaluates what it needs
(stream-car
 (stream-cdr
  (stream-filter prime?
		 (stream-enumerate-interval 10000 100000))))

;;; other example: sum of primes in an interval
(stream-fold-left
 +
 0
 (stream-filter prime?
		(stream-enumerate-interval 1 100)))

;;; 3.5.2: infinite streams
(define (integers-from n)
  ;; infinite stream comprising sequential integers
  (stream-cons n (integers-from (1+ n))))

(define integers (integers-from 1))

(define (stream-take n s)
  ;; take the first n elements of the stream
  (if [or (stream-null? s) (zero? n)]
      *empty-stream*
      (stream-cons (stream-car s)
		   (stream-take (1- n) (stream-cdr s)))))

;;; taking from an infinite stream
(stream-for-each
 (lambda (x) '())
 (stream-take
  10
  (stream-filter
   prime?
   integers)))

;;; other infinite streams
(define no-sevens
  (stream-filter (lambda (x) (not (divides? 7 x)))
		 integers))

(stream-ref no-sevens 100)

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (stream-take-while pred s)
  ;; takes from a stream while predicate is true; another useful stream op
  (if [or (stream-null? s)
	  (not (pred (stream-car s)))]
      *empty-stream*
      (stream-cons (stream-car s)
		   (stream-take-while pred (stream-cdr s)))))

;;; project euler #2: find the sum of the even fibonacci numbers < 4 million
(stream-fold-left
 +
 0
 (stream-filter
  even?
  (stream-take-while
   (lambda (x) (< x 4000000))
   fibs)))

(define (sieve s)
  ;; a prime sieve... using streams?!?!?! but it is not actually sieve of
  ;; eratosthenes (checks divisibility of many numbers many more times than
  ;; is needed, and is actually more inefficient than trial division);
  ;; a good paper is https://cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
  ;;
  ;; it seems that we will have to use some sort of mutable state in order
  ;; to implement esieve correctly (without extreme complexity); I'm not sure.
  ;; In the paper they use priority queues.
  (stream-cons
   (stream-car s)
   (sieve (stream-filter
	   (lambda (x)
	     (not (divides? (stream-car s) x)))
	   (stream-cdr s)))))

(define primes (sieve (integers-from 2)))

(stream-display
 (stream-take 50 primes))

;;; the previous streams involved explicit "generating procedures": functions
;;; to calculate the next element based on the previous one, but we can
;;; also define infinite streams implicitly; these take advantage of the
;;; laziness of streams, and my guess is that it'll be very hard to get this
;;; sort of neat lazy syntax in languages without macros like Java

(define ones-implicit
  ;; implicit stream generating all ones
  (stream-cons 1 ones-implicit))

(define integers-implicit
  ;; implicit version of integers stream
  (stream-cons 1 (stream-map 1+ integers-implicit)))

(define (stream-zip a b)
  ;; stream version of zip
  (if [or (stream-null? a)
	  (stream-null? b)]
      *empty-stream*
      (stream-cons
       (cons (stream-car a)
	     (stream-car b))
       (stream-zip (stream-cdr a)
		   (stream-cdr b)))))

(define fibs-implicit
  ;; an implicit definition of the infinite Fibonacci stream; rather than
  ;; using the generic form of map, we use zip, like they do in
  ;; "A Gentle Introduction to Haskell"
  (stream-cons
   0
   (stream-cons
    1
    (stream-map
     (lambda (x) (+ (car x) (cdr x)))
     (stream-zip fibs-implicit (stream-cdr fibs-implicit))))))

;;; we can make a primes stream that filters only based on the previous primes,
;;; which is more efficient than plain trial division (and thus better than the
;;; previous prime sieve)
(define primes-smart
  ;; this stream uses itself to generate the next element, but not like a simple
  ;; generating function that only uses the previous output; it uses the entire
  ;; stream up to sqrt(n)
  (stream-cons 2 (stream-filter stream-prime? (integers-from 3))))

(define (stream-any? pred s)
  ;; returns #t if any element of s fulfills the predicate, and #f otherwise;
  ;; assumed that the stream is finite or contains a positive element
  (cond ([stream-null? s] #f)
	([pred (stream-car s)] #t)
	(#t (stream-any? pred (stream-cdr s)))))

(define (stream-prime? n)
  ;; helper for the above function; use additional stream primitives rather
  ;; than manual iteration
  (let ([sqrtn (sqrt n)])
    (not (stream-any? (lambda (x) (divides? x n))
		      (stream-take-while (lambda (x) (<= x sqrtn))
					 primes-smart)))))

;;; project euler #7: 10001st prime
;;; not too slow but causes a little delay
;; (stream-ref primes-smart 10000)

;;; project euler #10: sum of primes under 2 million
;;; this takes ~3 seconds on the first invocation
;; (stream-fold-left
;;  +
;;  0
;;  (stream-take-while (lambda (x) (< x 2000000)) primes-smart))

;;; 3.5.3: Exploiting the stream paradigm

;;; returning to the sqrt example by repeated average damping
(define (sqrt-stream x)
  ;;; here, successive elements of the stream are successive approximations
  
  (define (sqrt-improve guess x)
    ;; helper function
    (average guess (/ x guess)))
  
  ;; sicp says we cannot do this with let, but this is exactly the use case
  ;; for letrec
  (letrec ([guesses (stream-cons
		     1.0
		     (stream-map (lambda (guess) (sqrt-improve guess x))
				 guesses))])
    guesses))

(stream-display
 (stream-take 5 (sqrt-stream 5)))

;;; the pi example

;;; another useful helper
(define (stream-drop s n)
  ;; drops the first n elements from stream s
  (if [or (<= n 0)
	  (stream-null? s)]
      s
      (stream-drop (stream-cdr s) (1- n))))

;;; need this from exercise 3.55
(define (partial-sums s)
  ;; partial sums of the stream s; still too lazy to implement the generic
  ;; form of stream-map so will use stream-zip again
  (letrec ([part-sums (stream-cons
		       0
		       (stream-map
			(lambda (x) (+ (car x) (cdr x)))
			(stream-zip s part-sums)))])
    (stream-drop part-sums 1)))

(define (pi-summands n)
  ;; successive elements of the stream are successive summands
  ;; defined with a "generator" syntax
  (stream-cons (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  ;; successive elements in pi-stream are again successive approximations
  (stream-map (lambda (x) (* x 4))
	      (partial-sums (pi-summands 1))))

(stream-display
 (stream-take 20 pi-stream))

;;; "sequence accelerators" by Euler transform
(define (euler-transform s)
  ;; if s is the sequence of partial sums of an alternating sequence (with terms
  ;; monotonically in magnitude), then we can accelerate it with this transform
  (let ([s0 (stream-ref s 0)]
	[s1 (stream-ref s 1)]
	[s2 (stream-ref s 2)])
    (stream-cons (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(stream-display
 (stream-take 20 (euler-transform pi-stream)))

(define (make-tableau transform s)
  ;; a tableau is a stream of streams
  (stream-cons s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  ;; get the first element of each stream in the tableau
  (stream-map stream-car (make-tableau transform s)))

;;; this actually gives us nan's after only 10 terms because of limited
;;; precision
(stream-display
 (stream-take 20 (accelerated-sequence euler-transform pi-stream)))

;;; (infinite) streams of pairs of infinite streams
;;; think of it like a Cartesian product of infinite sets
;;; note that we have to be careful about the order in which we choose pairs
;;; because of the infiniteness

(define (sequence-cartesian-product s t)
  ;; takes the cartesian product of two sequences
  (stream-cons
   (cons (stream-car s) (stream-car t))
   (stream-interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
		(stream-cdr t))
    (sequence-cartesian-product (stream-cdr s) (stream-cdr t)))))

(define (stream-interleave s1 s2)
  ;; interleaves two sequences; useful for joining infinite streams in a way
  ;; such that values from both streams will be incorporated in a finite
  ;; amount of time
  (if [stream-null? s1]
      s2
      (stream-cons (stream-car s1)
		   (stream-interleave s2 (stream-cdr s1)))))

;;; sicp presents here an example of an integral as a stream, which is basically
;;; the same as the partial-sums stream

;;; i'll finally define these two because they keep getting used
(define (stream-add s1 s2)
  ;; add two streams
  (stream-map
   (lambda (x) (+ (car x) (cdr x)))
   (stream-zip s1 s2)))

(define (stream-scale s c)
  ;; scales a numeric stream by a constant
  (stream-map (lambda (x) (* x c)) s))

(define (sp n s)
  ;; stream-print: a useful helper to print the first few values of a stream
  (stream-display (stream-take n s)))

(define (stream-integral integrand initial-value dt)
  (letrec ([int
	    (stream-cons initial-value
			 (stream-add (stream-scale integrand dt)
				     int))])
    int))

;;; 3.5.4: Streams and Delayed Evaluation
;;; streams allow us to model feedback loops
(define (solve f y0 dt)
  ;; attempting to solve the differential equation dy/dt=f(y)
  ;; by iteration of y=int{dy/dt} and dy/dt=f(y)
  ;; this won't work because y and dy are mutually depentend
  (letrec* ([y (integral dy y0 dt)]
	    [dy (stream-map f y)])
    y))

;;; this will fail
;; (stream-ref (solve (lambda (y) y) 1 0.001)
;; 	    1000)

(define (stream-delayed-integral delayed-integrand initial-value dt)
  ;; redefined integral with delayed integrand; basically wait for dy
  ;; (the integrand) to be declared before using it
  (letrec ([int
	    (stream-cons
	     initial-value
	     (let ([integrand (delayed-eval delayed-integrand)])
	       (stream-add (stream-scale integrand dt)
			   int)))])
    int))

(define (solve f y0 dt)
  ;; redefined solve with delayed integrant
  (letrec* ([y (stream-delayed-integral (make-delayed dy) y0 dt)]
	    [dy (stream-map f y)])
    y))

;;; estimate e
(stream-ref (solve (lambda (y) y) 1 0.001)
	    1000)

;;; with this model of "delayed arguments," we now can create systems of
;;; (interdependent) streams with feedback -- very cool!

;;; the paragraph on normal-order evaluation makes the note that delayed
;;; evaluation plays poorly with mutability, as we often perform actions
;;; out of the expected order

;;; 3.5.5: Modularity of Functional Programs and Modularity of Objects

;;; This section reproduces the monte-carlo cesaro estimate of pi using
;;; streams of random numbers, which avoid assignment. We can think of a stream
;;; as an object's state over time, and create a new state rather than
;;; overwriting the old one. Streams make it easier to retain the idea of
;;; pure mathematical functions.

;;; The book mentions that the merging operation is problematic with streams:
;;; we need to decide on an order to which to merge things, which is similar
;;; to the concurrency issue with separate objects.
