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
  (if [zero? n]
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

(define fibs-cooler
  ;; a cooler stream implementation of fibonacci without any state variables
  ;; (i.e., composed only of generic stream operations) -- involves "tail chase"
  ;; inspiration taken from the "A Gentle Introduction to Haskell"
  (stream-cons
   0
   (stream-cons
    1
    (stream-map
     (lambda (x) (+ (car x) (cdr x)))
     (stream-zip fibs-cooler (stream-cdr fibs-cooler))))))

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
   fibs-cooler)))

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
