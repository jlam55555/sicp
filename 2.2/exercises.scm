(load "../2.2/2.2.scm")

;;; 2.17: getting last cons of a list
(define (last-pair lst)
  ;; returns the singleton list containing the last element of list
  ;; will elegantly fail with non-lists
  (if [or (not (pair? lst))
	  (null? (cdr lst))]
      lst
      (last-pair (cdr lst))))

(last-pair '(23 72 149 34))

;;; 2.18: reversing a list
(define (my-reverse lst)
  ;; iteratively reverses a list like stdlib reverse
  ;; don't throw a non-list at this
  (let iter ([lst lst]
	     [result '()])
    (if [null? lst]
	result
	(iter (cdr lst)
	      (cons (car lst) result)))))

(my-reverse '(1 4 9 16 25))

;;; 2.19: revisiting coin change
;;; I already wrote it in terms of lists, but I'll do it again...
(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  ;; a rewrite of the coin-change problem with a focus on using lists

  (define no-more? null?)
  (define except-first-denomination cdr)
  (define first-denomination car)
  
  (cond ([zero? amount] 1)
	([or (< amount 0) (no-more? coin-values)] 0)
	(#t (+ (cc amount (except-first-denomination coin-values))
	       (cc (- amount (first-denomination coin-values))
		   coin-values)))))

(cc 100 us-coins)
(cc 100 uk-coins)

;;; still works with reversed coins, but is less efficient
;;; (I believe this is because it overshoots more)
(cc 100 (my-reverse us-coins))
;; (cc 100 (my-reverse uk-coins)) ; this one is a little slow

;;; 2.20: dotted tail notation
;;; I love this notation; also works with lambdas

;;; I read the question wrong, so this function does something different
;;; than what the question says
(define (same-parity? . nums)
  ;; returns #t if all members of the list have the same parity, #f otherwise
  ;; explicitly handles empty list case
  (if [null? nums]
      #t
      (let ([parity (even? (car nums))])
	(let iter ([nums (cdr nums)])
	  (cond ([null? nums] #t)
		([boolean=? parity (even? (car nums))]
		 (iter (cdr nums)))
		(#t #f))))))

(same-parity? 1 2 3 4 5 6 7)
(same-parity? 2 4 6 8)
(same-parity? 1 5 9)
(same-parity? 1 5 8 9)

;;; 2.21: mapping over a list
(define (square-list lst)
  ;; squares each element of a list
  (if [null? lst]
      '()
      (cons (square (car lst))
	    (square-list (cdr lst)))))

(square-list '(1 2 3 4 5))

(define (square-list lst)
  ;; squares each element of a list using map
  (my-map square lst))

(square-list '(1 2 3 4 5))

;;; 2.22: iterative square-list
(define (square-list lst)
  ;; iterative version of square-list that produces the mapped list in
  ;; reverse order
  (let iter ([lst lst]
	     [result '()])
    (if [null? lst]
	result
	(iter (cdr lst)
	      (cons (square (car lst))
		    result)))))

;;; this builds the list backwards because he is reading the list in the
;;; forward direction but building it in the reverse direction
(square-list '(1 2 3 4 5))

(define (square-list lst)
  ;; this doesn't work either; see note below
  (let iter ([lst lst]
	     [result '()])
    (if [null? lst]
	result
	(iter (cdr lst)
	      (cons result
		    (square (car lst)))))))

;;; can't run the above because it attempts to build a list in the wrong
;;; direction (with a null at the front)
(square-list '(1 2 3 4 5))

;;; 2.23: for-each
(define (for-each proc lst)
  ;; lazy implementation of for-each: still builds a list but discards it
  ;; interestingly, builtin map seems to process things out of order, so
  ;; we can't use that here (it has no guarantees on order of evaluation,
  ;; only on the order of the result)
  (my-map proc lst)
  (void))

(for-each display '(0 1 2 3))

(define (for-each proc lst)
  ;; slightly better foreach; no temp array is built
  (let iter ([lst lst])
    (when [not (null? lst)]
      (proc (car lst))
      (iter (cdr lst)))))

(for-each display '(0 1 2 3))

;;; 2.24
(list (list 2 (list 3 4)))

;;; equivalent to
'(((2 . ((3 . 4) . ())) . ()) . ())

;;; 2.25
(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;; 2.26
(define x '(1 2 3))
(define y '(4 5 6))

(my-append x y)				; '(1 2 3 4 5 6)
(cons x y)				; '((1 2 3) 4 5 6)
(list x y)				; '((1 2 3) (4 5 6))

;;; 2.27
(define (deep-reverse tree)
  ;; reverses list and all nested lists; assumes top level is a list
  (map (lambda (subtree)
	 (if [pair? subtree]
	     (deep-reverse subtree)
	     subtree))
       (my-reverse tree)))

(my-reverse '((1 2) (3 4)))
(deep-reverse '((1 2) (3 4)))

;;; 2.28
;;; see enumerate-tree (later in section)

;;; 2.29
(define (mobile-make left right)
  ;; constructs a mobile (the object)
  (list left right))

(define (mobile-branch-make length structure)
  ;; constructs a mobile branch; structure may be a number (weight) or another
  ;; mobile
  (list length structure))

;;; 2.29a
(define (mobile-left-branch m)
  ;; getter for mobile left branch
  (car m))

(define (mobile-right-branch m)
  ;; getter for mobile right branch
  (cadr m))

(define (mobile-branch-length mb)
  ;; getter for mobile branch length
  (car mb))

(define (mobile-branch-structure mb)
  ;; getter for mobile branch weight/structure
  (cadr mb))

;;; 2.29b
(define (mobile-branch-weight mb)
  ;; get the total weight of a branch
  (let ([structure (mobile-branch-structure mb)])
    (if [atom? structure]
	structure
	(mobile-total-weight structure))))

(define (mobile-total-weight m)
  ;; get total weight of a mobile
  (+ (mobile-branch-weight (mobile-left-branch m))
     (mobile-branch-weight (mobile-right-branch m))))

(define mobile1
  (mobile-make (mobile-branch-make 10 3)
	       (mobile-branch-make 3 10)))
(define mobile2
  (mobile-make (mobile-branch-make 10 3)
	       (mobile-branch-make 4 10)))

(mobile-total-weight mobile1)
(mobile-total-weight mobile2)

;;; 2.29c
(define (mobile-branch-torque mb)
  ;; gets the torque (length * weight) of a mobile
  (* (mobile-branch-length mb) (mobile-branch-weight mb)))

(define (mobile-balanced? m)
  ;; predicate that checks if a mobile is balanced
  (= (mobile-branch-torque (mobile-left-branch m))
     (mobile-branch-torque (mobile-right-branch m))))

(mobile-balanced? mobile1)
(mobile-balanced? mobile2)

;;; 2.29d
(define (mobile-make left right)
  ;; an alternative implementation of mobile
  (cons left right))

(define (mobile-branch-make length structure)
  ;; an alternative implementation of mobile branch
  (cons length structure))

;;; the only things we need to change are the selectors. mobile-left-branch
;;; and mobile-branch-length can stay the same
(define (mobile-right-branch m)
  ;; getter for the right branch of a mobile in the alternative definition
  (cdr m))

(define (mobile-branch-structure mb)
  ;; getter for the structure of a mobile branch in the alternative definition
  (cdr mb))

;;; these still work
(define mobile1
  (mobile-make (mobile-branch-make 10 3)
	       (mobile-branch-make 3 10)))
(define mobile2
  (mobile-make (mobile-branch-make 10 3)
	       (mobile-branch-make 4 10)))

(mobile-total-weight mobile1)
(mobile-total-weight mobile2)

(mobile-balanced? mobile1)
(mobile-balanced? mobile2)

;;; 2.30
;;; too lazy to define it directly -- is basically the same as scale tree
;;; already wrote a map-tree function, will use it here

(define (square-tree tree)
  ;; recursively square each element in a tree
  (map-tree square tree))

(square-tree '(1 (2 (3 4) 5) (6 7)))

;;; 2.31: recurively mapping over a tree
;;; oops -- did this without knowing it

;;; 2.32: power set
;;; This feels somewhat similar to the permutations function that appears
;;; later in the section
(define (power-set s)
  ;; generate the power set of the set s (set => elements are unique)
  (if [null? s]
      '(())
      (let ([first (car s)]
	    [rest (power-set (cdr s))])
	(my-append rest
		   (my-map (lambda (x) (cons first x))
			   rest)))))

(power-set '(1 2 3))

;;; explanation: the power set of S is the union of all subsets of S that
;;; contain its first element s1 and all subsets of S that don't contain s1,
;;; i.e., the power set of (S \ {s1}) (= rest), which we calculate recurively.
;;; To find the former set, we take rest and add s1 to each element of rest.
