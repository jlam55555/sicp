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
;;; builtin hashtable implementation and everything in the module is exported,
;;; so this implementation is somewhat silly.
(module (put get clear-op-type-table op-type-table)
  (define op-type-table
    ;; an assoc list of functions with their associated operator and
    ;; parameter types; is exposed for debugging reasons but wouldn't
    ;; normally be exposed
    '())
  
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
    (set! op-type-table '())))

(define (install-rectangular-package)
  ;; puts the rectangular complex number procedures into the op-type table
  
  ;; internal procedures -- note we don't have to worry about namespacing
  ;; these names anymore because they are properly scoped in the install
  ;; rectangular package
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (tag-attach 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-real-imag 'rectangular
       (lambda (x y) (tag (make-real-imag x y))))
  (put 'make-mag-ang 'rectangular
       (lambda (r a) (tag (make-mag-ang r a))))

  ;; give the function a return value
  'done)

(define (install-polar-package)
  ;; puts the polar complex number procedures into the op-type table

  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (tag-attach 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-real-imag 'polar
       (lambda (x y) (tag (make-real-imag x y))))
  (put 'make-mag-ang 'polar
       (lambda (r a) (tag (make-mag-ang r a))))
  
  'done)

;;; install the generic code into the op-type table; optionally clear it
;;; beforehand (shouldn't be necessary)
;; (clear-op-type-table)
(install-rectangular-package)
(install-polar-package)

;;; introspect the op-type table
op-type-table

(define (apply-generic op . args)
  ;; apply a generic function in the op-type table by matching its op and args
  (let ([tag-types (map tag-type args)])
    (let ([proc (get op tag-types)])
      (if proc
	  (apply proc (map tag-contents args))
	  (errorf 'apply-generic
		  "No method ~a for types ~a"
		  op tag-types)))))

;;; implementing generic procedures that are directly dependent on underlying
;;; representation; these don't even have to be namespaced under complex-*
;;; because they're generic
;;;
;;; still namespace these with complex because they are only for complex numbers
;;; and to avoid shadowing the builtin functions with the same name
(define (complex-real-part z)
  (apply-generic 'real-part z))
(define (complex-imag-part z)
  (apply-generic 'imag-part z))
(define (complex-magnitude z)
  (apply-generic 'magnitude z))
(define (complex-angle z)
  (apply-generic 'angle z))

;;; implementing constructors; these are different because the inputs may not
;;; be tagged, so we use a different convention
(define (complex-make-real-imag x y)
  ((get 'make-real-imag 'rectangular) x y))
(define (complex-make-mag-ang r a)
  ((get 'make-mag-ang 'polar) r a))

;;; the operations complex+, complex-, complex*, and complex/ are built on
;;; top of these abstractions and can be used without modification

;;; some test cases
(define z1-ri
  (complex-make-real-imag 3 4))
(define z2-ri
  (let ([xy (/ (sqrt 2) 2)])
    (complex-make-real-imag xy xy)))

(define z1-ma
  (complex-make-mag-ang (complex-magnitude z1-ri)
			(complex-angle z1-ri)))
(define z2-ma
  (complex-make-mag-ang (complex-magnitude z2-ri)
			(complex-angle z2-ri)))

;;; see that the above two representations are equivalent
(complex-real-part z1-ma)
(complex-imag-part z1-ma)
(complex-real-part z2-ma)
(complex-imag-part z2-ma)

;;; can mix types! Note that this is because the arithmetic operations
;;; are built on top of abstractions and are thus not directly dependent
;;; on the internal data representation -- when it does become dependent
;;; on the data representation (as we'll see in the next section) this will
;;; become more complex
(complex+ z1-ri z2-ri)
(complex+ z1-ri z2-ma)
(complex+ z1-ma z2-ri)
(complex+ z1-ma z2-ma)

(complex/ z1-ri z2-ma)
(complex/ z1-ri z2-ri)
