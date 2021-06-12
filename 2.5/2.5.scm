;;; sicp 2.5
(load "../utils/utils.scm")

;;; a lot of this is based off of 2.4, such as apply-generic, tag-attach,
;;; get, put, the complex number package, etc.
(load "../2.4/2.4.scm")

;;; Notes: I will omit comments more often because they are often unnecessary,
;;; especially the name is apparent or the example is only used in a single
;;; example that is clearly explained in the book.
;;; 
;;; I've switched my constructor convention to look like make-TYPENAME, while
;;; selector functions and other methods will still begin with TYPENAME-*. This
;;; follows the R6RS standard for record types (compound/struct types).

;;; 2.5.1: defining a generic arithmetic package
;;; This combines the examples from the previous two sections
(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))

(define (install-scheme-number-package)

  ;; don't need to define any internal procedures, just interface existing
  ;; procedures to rest of system
  (define (tag x)
    (tag-attach 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  
  'done)

(define (install-rational-package)
  
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (tag-attach 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  
  'done)

(define (install-complex-package)

  ;; note: I was using a slightly different naming convention in the previous
  ;; section, so there will be some conversion over to this section's naming,
  ;; so this will be a little inconsistent
  ;; 
  ;; internal procedures already defined in 2.4; just need to interface with
  ;; rest of system. Using apply is less redundant than how the textbook does
  ;; it, so that's what we'll do.
  (define (tag f)
    (lambda args
      (tag-attach 'complex (apply f args))))
  (put 'add '(complex complex) (tag complex+))
  (put 'sub '(complex complex) (tag complex-))
  (put 'mul '(complex complex) (tag complex*))
  (put 'div '(complex complex) (tag complex/))

  (put 'make-from-real-imag 'complex
       (tag (get 'make-real-imag 'rectangular)))
  (put 'make-from-mag-ang 'complex
       (tag (get 'make-mag-ang 'polar)))

  'done)

(define (make-constructor type)
  ;; helper to generate constructors using this type system -- doesn't actually
  ;; make the constructors that much shorter
  (lambda args
    (apply (get 'make type) args)))

;;; install packages and constructors
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define make-scheme-number
  (make-constructor 'scheme-number))
(define make-rational-number
  (make-constructor 'rational))

;;; don't use "make" op, so have to do these manually
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; from exercise 2.77, necessary for complex-imag-part to work (see next
;;; example below)
(put 'real-part '(complex) complex-real-part)
(put 'imag-part '(complex) complex-imag-part)
(put 'magnitude '(complex) complex-magnitude)
(put 'angle '(complex) complex-angle)

;;; 2.5.2: Combining data of different types
;;; So far, all of our procedures only deal with parameters of the same type,
;;; but this is not flexible. We want it so that we can do something like
;;; add a complex number to a real, but this requires that we write functions
;;; to convert from one type to another.

;;; sample procedure to add a complex to a schemenum
;;; note that this completely breaks my naming scheme
(define (add-complex-to-schemenum z x)
  ;; z, x should be the contents of a complex and scheme-number, respectively
  (make-complex-from-real-imag (+ (complex-real-part z) x)
			       (complex-imag-part z)))

(put 'add '(complex scheme-number) add-complex-to-schemenum)

(add (make-complex-from-real-imag 2 4)	; this works now
     (make-scheme-number 42))
;; (mul (make-complex-from-real-imag 2 4)	; this doesn't work still
;;      (make-scheme-number 42))


;;; this is very inefficient, because then we have to declare a different
;;; function to handle every pair of datatypes, and is even worse if there
;;; are many more possible types and functions with larger arity.
;;; 
;;; Coercion is a better scheme: we can (implicitly or explicitly) convert
;;; data from one type to another so that it fits the types of a parameter.
;;; 
;;; SICP uses the following example with new procedures called put-coercion
;;; and get-coercion. For our purposes, the regular put and get will work
;;; by putting the desired datatype as the op parameter and the input datatype
;;; as the type parameter.

(define (scheme-number->complex n)
  ;; coerce a scheme-number into a complex
  (make-complex-from-real-imag (tag-contents n) 0))

;;; add this to the op-type table
(put 'complex 'scheme-number scheme-number->complex)

(define (apply-generic op . args)
  ;; version of apply-generic that attempts to perform coercion for the operands
  ;; (assuming binary procedures)
  (let ([tag-types (map tag-type args)])
    (let ([proc (get op tag-types)])
      (if proc
	  (apply proc (map tag-contents args))
	  (if [= (length args) 2]
	      (let ([type1 (car tag-types)]
		    [type2 (cadr tag-types)]
		    [a1 (car args)]
		    [a2 (cadr args)])
		(let ([t1->t2 (get type2 type1)]
		      [t2->t1 (get type1 type2)])
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(#t
			 (errorf 'apply-generic "no method ~a for types ~a"
				 op tag-types)))))
	      (errorf 'apply-generic "no method ~a for types ~a"
		      op tag-types))))))

(mul (make-complex-from-real-imag 2 4)	; this works now
     (make-scheme-number 42))

;;; While this solution is better, we still have to have a direct coercion
;;; method for every valid conversion in the coercion table. This may be
;;; inefficient and, while still extensible additively, not convenient.
;;;
;;; A better way can be to define a type hierarchy (if one exists), but this
;;; gets problematic with multiple inheritance. With a hierarchy, we can "raise"
;;; an object to a superclass representation or "lower" it to a narrower
;;; representation if it fulfills the criterion of the lower type.

;;; Not doing the examples with symbolic algebra on polynomials and rational
;;; functions, simply out of interest of time.
