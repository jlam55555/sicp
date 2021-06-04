;;; 2.1 exercises
(load "../2.1/2.1.scm")

;;; 2.1: improving the make-rat constructor
(define (make-rat x y)
  ;; a better make-rat with better handling of negatives and
  ;; zero denominator
  ;; if positive, both numerator and denominator will be positive;
  ;; if negative, only numerator will be negative
  (if [zero? y]
      (error 'make-rat "zero denominator")
      (let ([d (gcd x y)])
	(cons (* (if [boolean=? (negative? x) (negative? y)] 1 -1)
		 (/ (abs x) d))
	      (/ (abs y) d)))))

;;; 2.2: reprenting line segments
;;; note: I flipped the names so that they all have the same prefix
;;; (e.g., this makes autocomplete work nicely)
(define (point-make x y)
  ;; create a point representation given the x and y coordinates
  (cons x y))

(define (point-x p)
  ;; get the x coordinate of a point
  (car p))

(define (point-y p)
  ;; get the y coordinate of a point
  (cdr p))

(define (point-print p)
  ;; print a point to the screen
  (format #t "(~a,~a)\n" (point-x p) (point-y p)))

(define (segment-make start end)
  ;; create a segment representation given the two endpoints
  (cons start end))

(define (segment-start s)
  ;; get the starting point of a segment
  (car s))

(define (segment-end s)
  ;; get the ending point of a segment
  (cdr s))

(define (segment-midpoint s)
  ;; return the midpoint of a segment
  (let ([p1 (segment-start s)]
	[p2 (segment-end s)])
    (point-make (average (point-x p1) (point-x p2))
		(average (point-y p1) (point-y p2)))))

;;; test cases
(define seg (segment-make (point-make 0 1)
			  (point-make 2 -3)))
(point-print (segment-midpoint seg))

;;; 2.3: reprentation for rectangles
(define (rectangle-make p1 p2 p3)
  ;; define a rectangle in terms of three corner points
  ;; assumes that the three points are not collinear
  (error 'rectangle-make "not implemented yet"))

(define (rectangle-area r)
  ;; calculates the area of the rectangle r
  (error 'rectangle-area "not implemented yet"))

(define (rectangle-perimeter r)
  ;; calculates the perimeter of the rectangle r
  (error 'rectangle-perimeter "not implemented yet"))

;;; TODO: a second internal representation for rectangles with
;;; suitable abstraction barriers such that the same area
;;; and perimeter functions work?

;;; 2.4: another procedural implementation of cons
(define (my-cons-2 x y)
  ;; procedural implementation of cons
  (lambda (m) (m x y)))

(define (my-car-2 z)
  ;; procedural implementation of car
  (z (lambda (p q) p)))

(define (my-cdr-2 z)
  ;; procedural implementation of cdr
  (z (lambda (p q) q)))

;;; 2.5: a numeric representation for cons
;;; this works because of the Fundamental Theorem of Arithmetic
;;; (or, more generally, because 2 and 3 are coprime)
(define (my-cons-3 x y)
  ;; a numeric implementation of cons
  ;; assume x, y are nonnegative integers
  (* (expt 2 x) (expt 3 y)))

(define (my-car-3 z)
  ;; a numeric implementation of car
  ;; (is there a faster way of doing this?)
  (let iter ([n z]
	     [result 0])
    (if [not (divides? 2 n)]
	result
	(iter (fx/ n 2) (fx1+ result)))))

(define (my-cdr-3 z)
  ;; a numeric implementation of cdr
  (let iter ([n z]
	     [result 0])
    (if [not (divides? 3 n)]
	result
	(iter (fx/ n 3) (fx1+ result)))))

;;; 2.6: a procedural implementation for nonnegative integers
(define church-zero
  ;; Church numeral implementation of 0
  (lambda (f)
    (lambda (x) x)))

(define (church-1+ n)
  ;; Church numeral implementation of 1+
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;;; notes:
;;; - zero is a procedure that always returns the identity
;;;   function, so it is the same as writing:
(define (church-zero f) identity)
;;;   where identity is defined in ../utils/utils. This makes
;;;   sense as the zero of some function group.
;;;   Thus we can imagine each "number" to be represented as
;;;   a function.
;;; - From the previous observation, we can expect add-1 to
;;;   be some map on functions F -> F.

;;; The reprentation of 1 in this function space is (add-1 zero),
;;; which simplifies to:
(define (church-one f)
  (lambda (x)
    (f x)))
;;; in other words, it returns f. Even more simply:
(define (church-one f) f)
;;; or
(define church-one identity)
;;; (assuming that f is a unary procedure)

;;; From the above, we can clearly see that the representation
;;; of 2 is:
(define church-two
  (lambda (f)
    (lambda (x)
      (f (f x)))))
;;; simplifying:
(define (church-two f)
  (lambda (x)
    (f (f x))))
;;; in terms of our friends from ../1.3/exercises.scm:
(define church-two double)

;;; the generalized version of this is that the number n is represented
;;; by the function that autocomposes a function n times. Thus, to sum
;;; of two "numbers" a+b is the function that autocomposes a function n+m times
(define (church-add a b)
  ;; add two Church numerals
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;;; with this understanding, the definition of multiplication is
;;; also pretty clear:
(define (church-multiply a b)
  ;; multiply two Church numerals
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))

;;; now, it's intuitive that the church numerals are isomorphic to the
;;; nonnegative integers by the autocompose mapping (from 1.3 exercises),
;;; so the conversion to/from Church numerals is well-defined
(define (fx->church n)
  ;; generate an arbitrary church numeral from a regular nonnegative number
  (lambda (f)
    (autocompose f n)))

(define (church->fx n)
  ;; converts a church numeral into a number equivalent
  ((n 1+) 0))
