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
(define (stream-cons a b) (cons a (make-delayed b)))

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
