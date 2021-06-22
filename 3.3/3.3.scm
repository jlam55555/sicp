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

;;; a assoc-table data structure
;;; SICP uses a headed list, which is useful when inserting a value into the
;;; table (we could not update the value of the table variable otherwise); thus
;;; the structure is basically a pointer to an assoclist

(define (lookup key table)
  ;; look up a value in the headed list, return it if found and #f otherwise
  ;; basically the same as assoc but returns the cdr of the record if it found
  (let ([record (my-assoc key (cdr table))])
    (and record (cdr record))))

(define (my-assoc key records)
  ;; mimicking the builtin assoc procedure: check if the head of any of the
  ;; nested lists of the assoclist is equal to key
  (cond ([null? records] #f)
	([equal? key (caar records)] (car records))
	(#t (my-assoc key (cdr records)))))

(define (insert! key value table)
  ;; inserts a key-value pair into an assoclist if not found, and updates the
  ;; entry if found
  (let ([record (my-assoc key (cdr table))])
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  ;; headed assoclist constructor
  (list '*table*))

;;; two-dimensional tables
;;; note that subtables don't need a head, because we already have a mutable
;;; reference to them with the record in the supertable

(define (2d-lookup key-1 key-2 table)
  ;; 2-d table lookup method
  (let ([subtable (my-assoc key-1 (cdr table))])
    (and subtable
	 (let ([record (my-assoc key-2 (cdr subtable))])
	   (and record (cdr record))))))

(define (2d-insert! key-1 key-2 value table)
  ;; 2-d table insert/update method
  (let ([subtable (assoc key-1 (cdr table))])
    (if subtable
	;; existing subtable
	(let ([record (assoc key-2 (cdr subtable))])
	  (if record
	      ;; update record in existing subtable
	      (set-cdr! record value)
	      ;; add new record within the existing subtable
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	;; create new subtable and record
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

;;; as usual, we can instead write this in a message-passing way, so that it
;;; looks like an object with internal state -- will not reproduce the example
;;; from the textbook here

;;; 3.3.4: A Simulator for Digital Circuits
;;; "event-driven simulation": actions ("events") trigger further events that
;;; happen at a later time, which trigger more events, and so on

;;; half-adder circuit
(define (half-adder a b s c)
  ;; a,b are the inputs, c is the carry bit (a AND b), d is a OR b,
  ;; e is a NAND b, s is the sum (a XOR b = ((a OR b) AND (a NAND b)))
  (let ([d (make-wire)]
	[e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  ;; full-adder comprised of two half-adders
  (let ([s (make-wire)]
	[c1 (make-wire)]
	[c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;;; interface
;; (get-signal wire)
;; (set-signal! wire new-value)
;; (add-action! wire thunk)
;; (after-delay delay thunk)

(define (inverter input output)
  ;; inverter block
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! (output new-value))))))
  (add-action! input invert-input)
  'ok)
