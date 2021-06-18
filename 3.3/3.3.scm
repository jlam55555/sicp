;;; notes from sicp 3.3: Modeling with Mutable Data
(load "../utils/utils.scm")

;;; compound objects with mutable state will now have mutators in addition
;;; to constructors and setters
;;;
;;; new set-car! and set-cdr! mutators for cons objects
;;;
;;; sharing and identity: note that:
;;; - list and cons always create a new object (not a reference to an existing
;;;   object)
;;; - define creates a reference (pointer?) to whatever object, whether it be
;;;   existing or newly created
;;; - symbols (and numbers, chars) are only stored once in memory (shared),
;;;   because they are immutable and thus this sharing goes undetected
;;; - eq? compares whether two objects are referring to the same object, i.e.,
;;;   if they are pointers to the same object
;;;
;;; Mutable data is a result of assignment (and vice versa; they are
;;; equipotent); once we have set!, we can implement any mutator for any
;;; compound data object, including set-car! and set-cdr! (see below example)

(define (my-cons x y)
  ;; emulating set-car! and set-cdr! mutators with set!

  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))

  (lambda (m)
    (cond ([eq? m 'car] x)
	  ([eq? m 'cdr] y)
	  ([eq? m 'set-car!] set-x!)
	  ([eq? m 'set-cdr!] set-y!)
	  (#t (error 'my-cons "undefined operation" m)))))

(define (my-car z) (z 'car))
(define (my-cdr z) (z 'cdr))
(define (my-set-car! z new-value)
  ((z 'set-car!) new-value))
(define (my-set-cdr! z new-value)
  ((z 'set-cdr!) new-value))

;;; defining a queue data structure using mutable operations
(define (queue-front-ptr q) (car q))
(define (queue-rear-ptr q) (cdr q))
(define (set-queue-front-ptr! q item) (set-car! q item))
(define (set-queue-rear-ptr! q item) (set-cdr! q item))

(define (queue-empty? q) (null? (queue-front-ptr q)))

(define (make-queue) (cons '() '()))

(define (queue-front q)
  (if [queue-empty? q]
      (error 'queue-front "empty queue")
      (car (queue-front-ptr q))))

(define (queue-insert! q item)
  (let ([new-pair (cons item '())])
    (if [queue-empty? q]
	[begin
	  (set-queue-front-ptr! q new-pair)
	  (set-queue-rear-ptr! q new-pair)
	  q]
	[begin
	  (set-cdr! (queue-rear-ptr q) new-pair)
	  (set-queue-rear-ptr! q new-pair)
	  q])))

(define (queue-delete! q)
  (if [queue-empty? q]
      (error 'queue-delete! "empty queue")
      [begin
	(set-queue-front-ptr! q (cdr (queue-front-ptr q)))
	q]))

;;; test cases
(define q (make-queue))
(queue-insert! q 1)
(queue-insert! q 2)
(queue-insert! q 3)
(queue-insert! q 4)
(queue-insert! q 5)
(queue-delete! q)
(queue-insert! q 6)
(queue-delete! q)
(queue-insert! q 7)
(queue-delete! q)
(queue-delete! q)
(queue-delete! q)
(queue-delete! q)
(queue-delete! q)
;; (queue-delete! q)			; throws empty queue

