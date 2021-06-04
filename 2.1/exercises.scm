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
    (make-point (average (point-x p1) (point-x p2))
		(average (point-y p1) (point-y p2)))))

;;; test cases
(define seg (segment-make (point-make 0 1)
			  (point-make 2 -3)))
(print-point (segment-midpoint seg))
