;;; sicp section 2.4
(load "../utils/utils.scm")

;;; 2.4.1: (multiple) representations for complex numbers
;;; note: in Chez Scheme we have complexnums, see
;;; https://www.scheme.com/csug8/numeric.html
(define (complex+ z1 z2)
  (complex-make-real-imag (+ (complex-real-part z1) (complex-real-part z2))
			  (+ (complex-imag-part z1) (complex-imag-part z2))))
(define (complex- z1 z2)
  (complex-make-real-imag (- (complex-real-part z1) (complex-real-part z2))
			  (- (complex-imag-part z1) (complex-imag-part z2))))
(define (complex* z1 z2)
  (complex-make-mag-ang (* (complex-magnitude z1) (complex-magnitude z2))
			(+ (complex-angle z1) (complex-angle z2))))
(define (complex/ z1 z2)
  (complex-make-mag-ang (/ (complex-magnitude z1) (complex-magnitude z2))
			(- (complex-angle z1) (complex-angle z2))))

;;; initial form (like what we were doing earlier: write the two different
;;; representations separately, only load in one or the other) -- not very
;;; flexible, can't have both at once

;;; ben's representation: real and imaginary parts
(define (complex-real-part z) (car z))
(define (complex-imag-part z) (cdr z))
(define (complex-magnitude z)
  (sqrt (+ (square (complex-real-part z))
	   (square (complex-imag-part z)))))
(define (complex-angle z)
  (atan (complex-imag-part z)
	(complex-real-part z)))
(define (complex-make-real-imag x y) (cons x y))
(define (complex-make-mag-ang r a)
  (cons (* r (cos a))
	(* r (sin a))))

;;; alyssa's representation: magnitude/angle representation
(define (complex-real-part z)
  (* (complex-magnitude z) (cos (complex-angle z))))
(define (complex-imag-part z)
  (* (complex-magnitude z) (sin (complex-angle z))))
(define (complex-magnitude z) (car z))
(define (complex-angle z) (cdr z))
(define (complex-make-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))
(define (complex-make-mag-ang r a) (cons r a))

;;; some tests: these give the same result based on both representations,
;;; but there's no information about which representation is currently in use
(define z1 (complex-make-mag-ang 2 1))
(define z2 (complex-make-real-imag 41 1))
(define z3 (complex-make-real-imag 0 (- (/ (sqrt 3) 28))))
(complex+ (complex* z1 z2) z3)

;;; 2.4.2: tagged data
;;; if we want multiple representations, need to indicate which type the
;;; data is with a tag

(define (tag-attach type-tag contents)
  ;; attach a tag to a datum
  (cons type-tag contents))

(define (tag-type datum)
  ;; type selector for a tagged datum
  (if [pair? datum]
      (car datum)
      (error 'tag-type "bad tagged datum" datum)))

(define (tag-contents datum)
  ;; contents selector for a tagged datum
  (if [pair? datum]
      (cdr datum)
      (error 'tag-contents "bad tagged datum" datum)))

;;; predicates for tagged data
(define (rectangular? z)
  (eq? (tag-type z) 'rectangular))
(define (polar? z)
  (eq? (tag-type z) 'polar))

;;; redefining ben and alyssa's representation with tagged datatypes:
;;; 
;;; not going to copy this section because it is long and has the flaws
;;; described in the chapter. Here they redefine real-part, magnitude, ... etc
;;; that are dependent on the representation to switch on the tag type. The
;;; higher level abstractions do not need to change. However, this requires
;;; us to hardcode in each representation and write a switch statement for
;;; each representation. This also does not allow for an additive extension,
;;; so it is fairly brittle.
;;;
;;; In general, this is called ``data-directed'' programming, in which
;;; functions are generic to their input data. (This is as opposed to ``message
;;; passing,'' introduced later in this section.) In the next subsection,
;;; a more flexible version of message passing is introduced.

;;; 2.4.3: Data-directed programming and additivity
;;;
;;; We need an implementation of the put and get operators described in this
;;; section. We will use a very basic association list structure. A better
;;; version can be created using a hashtable (included in the stdlib).
;;; This will be defined in a module for better encapsulation, but it is not
;;; really necessary. In hindsight this is probably easier to do with the
;;; builtin hashtable implementation, this was just silly.
(module (put get clear-op-type-table pt)
  (define op-type-table '())
  
  (define (put op type item)
    ;; adds item to the op-type-table, indexed by op and type; updates
    ;; if already exists
    (let* ([key (cons op type)]
	   [found (assoc key op-type-table)])
      (if found
	  (set-cdr! found item)
	  (set! op-type-table
		(cons (cons (cons op type) item)
		      op-type-table)))))

  (define (get op type)
    ;; returns the function indexed by op and type in the op-type-table,
    ;; or false if not found
    (let ([found (assoc (cons op type) op-type-table)])
      (and found (cdr found))))

  (define (clear-op-type-table)
    ;; in case there's a need to clear the op-type-table
    (set! op-type-table '()))

  (define (pt)
    op-type-table))

(clear-op-type-table)
(pt)
(put 'make 'rect 32)
(put 'add '(rect tre) 2)

(get 'add '(rect tre))
