;;; sicp 2.2 exercises
(load "../utils/utils.scm")

;;; with cons, it seems really easy to implement a binary tree

;;; 2.2.1: representing sequences

;;; show that cons and list are equivalent
;;; Chez Scheme has '() rather than nil
(cons 1
      (cons 2
	    (cons 3
		  (cons 4 '()))))
(list 1 2 3 4)

(define (my-list-ref items n)
  ;; returns the n-th item of a list. Same as stdlib list-ref
  (if [zero? n]
      (car items)
      (list-ref (cdr items) (1- n))))

(define (my-length items)
  ;; returns the length of a linked list. Same as stdlib length
  (if [null? items]
      0
      (1+ (length (cdr items)))))

(define (my-append list1 list2)
  ;; appends list2 to list1. Modifies list1 but not list2. Same as stdlib append
  (if [null? list1]
      list2
      (cons (car list1) (my-append (cdr list1) list2))))

(my-list-ref '(1 2 3 4) 2)
(my-length '(1 2 3 4))
(my-append '(1 2 3 4) '(5 6 7))

(define (my-map proc items)
  ;; maps a function over each element of items. Same as stdlib map
  (if [null? items]
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(my-map 1+ '(2 3 4))

;;; 2.2.2: hierarchical structures

(define (count-leaves tree)
  ;; think of any nested list (cons) structure as a tree, and count the number
  ;; of leaves. Another way of thinking about it is taking the length of the
  ;; flattened list
  (cond ([null? tree] 0)
	([not (pair? tree)] 1)
	(#t (+ (count-leaves (car tree))
	       (count-leaves (cdr tree))))))

(count-leaves '(((1 2) 3 4) ((1 2) (3) 4)))

(define (map-tree proc tree)
  ;; maps a function over each atom in a tree structure
  ;; (similar to scale-tree in the book but with an arbitrary proc)
  (my-map (lambda (subtree)
	    (if [pair? subtree]
		(map-tree proc subtree)
		(proc subtree)))
	  tree))

(map-tree 1+ '(((1 2) 3 4) ((1 2) (3) 4)))

(define (my-filter pred? items)
  ;; selects elements which make the predicate true from the list; same
  ;; as stdlib filter
  (cond ([null? items] '())
	([pred? (car items)]
	 (cons (car items)
	       (my-filter pred? (cdr items))))
	(#t (my-filter pred? (cdr items)))))

(my-filter odd? (list 1 2 3 4 5))

(define (accumulate op initial items)
  ;; accumulates a value over a list; same as stdlib fold-left
  (if [null? items]
      initial
      (op (car items)
	  (accumulate op initial (cdr items)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
  ;; generate a range of integers, inclusive
  (if [> low high]
      '()
      (cons low (enumerate-interval (1+ low) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  ;; enumerates over the leaves of a tree by postorder dfs
  ;; same as the fringe procedure from exercise 2.28
  (cond ([null? tree] '())
	([not (pair? tree)] (list tree))
	(#t (my-append (enumerate-tree (car tree))
		       (enumerate-tree (cdr tree))))))

(enumerate-tree '(1 (2 (3 4)) 5))

;;; example: sum of primes under 2 million (commented b/c a little slow)
;; (accumulate + 0 (filter prime? (enumerate-interval 1 2000000)))

;;; this process of building complex procedures out of basic primitives
;;; (e.g., filter, map, accumulate) can be called "modular construction"

(define (cartesian-product n)
  ;; generate the Cartesian product {(i,j)}, where 0 <= i,j <= n
  ;; a specific case of the below (flatmap)
  (accumulate append
	      '()
	      (my-map (lambda (i)
			(my-map (lambda (j) (list i j))
				(enumerate-interval 0 n)))
		      (enumerate-interval 0 n))))

(cartesian-product 5)

(define (flatmap proc seq)
  ;; map each element of seq to a list, and then flatten it
  (accumulate append '() (my-map proc seq)))

(flatmap iota (iota 10))

(define (permutations s)
  ;; generates the set of permutations of a set s (elements are unique)
  (if [null? s]
      '(())
      (flatmap (lambda (x)
		 (my-map (lambda (p) (cons x p))
			 (permutations (remove x s))))
	       s)))

(permutations (iota 3))

(define (my-remove item sequence)
  ;; removes all instances of item from sequence; same as stdlib remove
  (my-filter (lambda (x) (not (= x item)))
	     sequence))

(remove 5 '(1 5 2 5 3 5 4 5))
