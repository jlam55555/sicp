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
    (n-fold-compose f n)))

(define (church->fx n)
  ;; converts a church numeral into a number equivalent
  ((n 1+) 0))

;;; 2.7: continuing the interval example
(define (interval-make lower upper)
  ;; construct an interval object
  (cons lower upper))

(define (interval-upper-bound i)
  ;; get the upper bound of an interval
  (cdr i))

(define (interval-lower-bound i)
  ;; get the lower bound of an interval
  (car i))

;;; 2.8: interval difference
(define (interval-sub a b)
  ;; return the difference between two intervals
  ((interval-make (- (interval-lower-bound a) (interval-upper-bound b))
		  (- (interval-upper-bound a) (interval-lower-bound b)))))

;;; 2.9: interval widths
;;; let interval1 be (l1, u1), interval2 be (l2, u2)
;;; +: w = ((u1+u2) - (l1+l2))/2 = (u1-l1)/2 + (u2-l2)/2 = w1 + w2
;;; -: w = ((u1-l2) - (l1-u2))/2 = (u1-l1)/2 + (l2-l2)/2 = w1 + w2
;;; *: [0,1] * [0,1] = [0,1] but [1,2] * [1,2] = [1,4]
;;;    input widths are the same, output widths are not
;;; /: [0,1] / [1,2] = [0,1] * [1,1/2] = [0,1]
;;;    [0,1] / [2,3] = [0,1] * [1/2,1/3] = [0,1/2]
;;;    input widths are the same, output widths are not

;;; 2.10: prevent divide by zero (interval)
(define (interval-div x y)
  ;; divide an interval by another (multiply by the reciprocal)
  ;; prevent divide by interval that spans zero (including endpoints)
  (if [and (<= (interval-lower-bound y) 0)
	   (>= (interval-upper-bound y) 0)]
      (errorf 'interval-div "interval [~a,~a] spans 0"
	      (interval-lower-bound a) (interval-upper-bound b))
      (interval-mul x
		    (make-interval (/ 1.0 (interval-upper-bound y))
				   (/ 1.0 (interval-lower-bound y))))))

;;; 2.11: ben's suggestion
(define (interval-mul x y)
  ;; break up multiplication into nine cases, only one of which requires
  ;; more than two multiplications

  ;; store endpoints because fncalls are long
  (let ([u1 (interval-upper-bound x)]
	[l1 (interval-lower-bound x)]
	[u2 (interval-upper-bound y)]
	[l2 (interval-lower-bound y)])
    ;; get signs of endpoints
    (let ([su1 (>= u1 0)]
	  [sl1 (>= l1 0)]
	  [su2 (>= u2 0)]
	  [sl2 (>= l2 0)])
      (cond
       ;; all positive or negative
       ([and sl1 su1 sl2 su2]
	(interval-make (* l1 l2) (* u1 u2)))
       ([and (not sl1) (not su1) (not sl2) (not su2)]
	(interval-make (* u1 u2) (* l1 l2)))
       ;; one positive, one negative
       ([and sl1 su1 (not sl2) (not su2)]
	(interval-make (* u1 l2) (* l1 u2)))
       ([and (not sl1) (not su1) sl2 su2]
	(interval-make (* l1 u2) (* u1 l2)))
       ;; one spanning zero
       ([and sl1 su1 (not sl2) su2]
	(interval-make (* u1 l2) (* u1 u2)))
       ([and (not sl1) (not su1) (not sl2) su2]
	(interval-make (* l1 u2) (* l1 l2)))
       ([and (not sl1) su1 sl2 su2]
	(interval-make (* l1 u2) (* u1 u2)))
       ([and (not sl1) su1 (not sl2) (not su2)]
	(interval-make (* u1 l2) (* l1 l2)))
       ;; both spanning zero
       (#t (let ([p1 (* u1 u2)]
		 [p2 (* u1 l2)]
		 [p3 (* l1 u2)]
		 [p4 (* l1 l2)])
	     (interval-make (min p1 p2 p3 p4)
			    (max p1 p2 p3 p4))))))))

;;; 2.12: different constructors
(define (interval-make-center-width c w)
  ;; constructs an interval given center and width
  (interval-make (- c w) (+ c w)))

(define (interval-center i)
  ;; the center of an interval
  (average (interval-lower-bound i) (interval-upper-bound i)))

(define (interval-width i)
  ;; the width of an interval
  (/ (- (interval-upper-bound i) (interval-lower-bound i))
     2))

(define (interval-make-center-tolerance c p)
  ;; constructs an interval given center and percentage tolerance
  (interval-make-center-width c (abs (* c p))))

(define (interval-tolerance i)
  ;; the percent tolerance of an interval around its center
  (abs (/ (interval-width i) (interval-center i))))

;;; 2.13: interval width as a function of percent
;;; (see assumptions below)
;;; We basically make a linearizing assumption because tolerances are small
;;; (A +- a) * (B +- b) ~= AB +- ab (not actually true, but we can estimate
;;; that the center is ~= AB)
;;; 
;;; Really the product is [(A-a)(B-b),(A+a)(B+b)] = [AB-Ab-Ba+ab,AB+Ab+Ba+ab]
;;; so width = ((AB+Ab+Ba+ab)-(AB-Ab-Ba+ab))/2 = 2*(Ab+Ba)/2 = Ab+Ba
;;; Let a=p1*A, b=p2*B, then w=AB*p2 + AB*p1 = AB*(p1+p2)
;;; Thus output percentage is p=w/c = AB*(p1+p2)/AB = p1+p2

(define (interval-estimate-prod-tolerance x y)
  ;; estimates the width of the product of x and y
  ;; for simplicity, assume x,y are both positive
  ;; and the percent tolerances are small
  (+ (interval-tolerance x) (interval-tolerance y)))
