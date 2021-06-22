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
    (not-gate c e)
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

;;; wire interface
;; (make-wire)
;; (wire-get-signal wire)
;; (wire-set-signal! wire new-value)
;; (wire-add-action! wire thunk)

;;; these gates change the output one time-step after an input changes

(define (not-gate input output)
  (define (perform-not)
    (let ([new-value (not (wire-get-signal input))])
      (agenda-after-delay not-gate-delay
			  (lambda ()
			    (wire-set-signal! output new-value)))))
  (wire-add-action! input perform-not)
  'ok)

(define (and-gate input-1 input-2 output)
  (define (perform-and)
    (let ([new-value (and (wire-get-signal input-1)
			  (wire-get-signal input-2))])
      (agenda-after-delay and-gate-delay
			  (lambda ()
			    (wire-set-signal! output new-value)))))
  (wire-add-action! input-1 perform-and)
  (wire-add-action! input-2 perform-and)
  'ok)

(define (or-gate input-1 input-2 output)
  (define (perform-or)
    (let ([new-value (or (wire-get-signal input-1)
			 (wire-get-signal input-2))])
      (agenda-after-delay or-gate-delay
			  (lambda ()
			    (wire-set-signal! output new-value)))))
  (wire-add-action! input-1 perform-or)
  (wire-add-action! input-2 perform-or)
  'ok)

(define (make-wire)
  (let ([signal-value #f]
	[action-procedures '()])
    (define (wire-set-signal! new-value)
      (when [not (boolean=? signal-value new-value)]
	(set! signal-value new-value)
	(for-each (lambda (x) (x))
		  action-procedures)))
    (define (accept-action-procedure! thunk)
      (set! action-procedures
	    (cons thunk action-procedures))
      ;; note that we call thunk here! see exercise 3.31
      (thunk))
    (define (dispatch m)
      (cond ([eq? m 'wire-get-signal] signal-value)
	    ([eq? m 'wire-set-signal!] wire-set-signal!)
	    ([eq? m 'wire-add-action!] accept-action-procedure!)
	    ([eq? m 'wire-get-actions] action-procedures) ; for debugging
	    (#t (error 'make-wire "unknown operation" m))))
    dispatch))

(define (wire-get-signal wire)
  (wire 'wire-get-signal))
(define (wire-set-signal! wire new-value)
  ((wire 'wire-set-signal!) new-value))
(define (wire-add-action! wire thunk)
  ((wire 'wire-add-action!) thunk))
(define (wire-get-actions wire)
  (wire 'wire-get-actions))

;;; agenda interface
;; (make-agenda)
;; (agenda-empty? agenda)
;; (agenda-top agenda)
;; (agenda-pop! agenda)
;; (agenda-add! time action agenda)
;; (agenda-current-time agenda)
;; (agenda-after-delay delay thunk)

(define (agenda-after-delay delay action)
  ;; adds an action to the agenda after a delay
  (agenda-add! (+ delay (agenda-current-time *agenda*))
	       action
	       *agenda*))

(define (agenda-propagate)
  ;; executes all actions in an agenda until complete
  (unless [agenda-empty? *agenda*]
    (let ([first-item (agenda-top *agenda*)])
      (first-item)
      (agenda-pop! *agenda*)
      (agenda-propagate))))

(define (sim-probe name wire)
  ;; probes a wire so that every time the signal changes, it prints out
  ;; the new value
  (wire-add-action!
   wire
   (lambda ()
     (format #t "~a ~a ~a\n"
	     name
	     (agenda-current-time *agenda*)
	     (wire-get-signal wire)))))

;;; time segments are a data structure used to implement the queue
(define (make-time-segment time queue) (cons time queue))
(define (time-segment-time s) (car s))
(define (time-segment-queue s) (cdr s))

;;; note that the agenda data structure comprises the current time
;;; as well as a list of segments
(define (make-agenda) (list 0))
(define (agenda-current-time agenda) (car agenda))
(define (agenda-set-current-time! agenda time) (set-car! agenda time))
(define (agenda-segments agenda) (cdr agenda))
(define (agenda-set-segments! agenda segments) (set-cdr! agenda segments))
(define (agenda-first-segment agenda) (car (agenda-segments agenda)))
(define (agenda-rest-segments agenda) (cdr (agenda-segments agenda)))
(define (agenda-empty? agenda) (null? (agenda-segments agenda)))

(define (agenda-add! time action agenda)
  ;; add an action to an agenda; uses queue implementation from earlier
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (time-segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ([q (make-queue)])
      (queue-insert! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if [= (time-segment-time (car segments)) time]
	(queue-insert! (time-segment-queue (car segments))
		       action)
	(let ([rest (cdr segments)])
	  (if [belongs-before? rest]
	      (set-cdr! segments
			(cons (make-new-time-segment time action)
			      (cdr segments)))
	      (add-to-segments! rest)))))
  (let ([segments (agenda-segments agenda)])
    (if [belongs-before? segments]
	(agenda-set-segments!
	 agenda
	 (cons (make-new-time-segment time action) segments))
	(add-to-segments! segments))))

(define (agenda-pop! agenda)
  (let ([q (time-segment-queue (agenda-first-segment agenda))])
    (queue-delete! q)
    (when [queue-empty? q]
      (agenda-set-segments! agenda (agenda-rest-segments agenda)))))

(define (agenda-top agenda)
  (if [agenda-empty? agenda]
      (error 'agenda-top "agenda is empty")
      (let ([first-seg (agenda-first-segment agenda)])
	(agenda-set-current-time! agenda (time-segment-time first-seg))
	(queue-front (time-segment-queue first-seg)))))

(define *agenda*
  ;; single global agenda
  (make-agenda))

(define not-gate-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(sim-probe 'sum sum)
(sim-probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(wire-set-signal! input-1 #t)
(agenda-propagate)

(wire-set-signal! input-2 #t)
(agenda-propagate)
