;;; sicp section 2.3: symbolic data
;;; extending data to arbitrary symbols rather than just numbers
(load "../utils/utils.scm")

;;; I have already been using quotation for list and cons literals for
;;; convenience, but not yet for symbolic values
(define a 1)
(define b 2)

(list a b)
(list 'a 'b)
(list 'a b)

;;; list literals
(car '(a b c))
(cdr '(a b c))
'()

;;; the following are equivalent
'a
(quote a)

;;; comparing equality of symbols
(eq? 'a 'a)
(eq? 'a 'b)
(eq? 2 2)

(define (my-memq sym lst)
  ;; if sym exists as a symbol in lst, return the part of lst beginning
  ;; with sym; same as stdlib memq
  (cond ([null? lst] #f)
	([eq? sym (car lst)] lst)
	(#t (my-memq sym (cdr lst)))))

(my-memq 'apple '(pear banana prune))

;;; 2.3.2: symbolic differentiation
(define (sym-deriv exp var)
  ;; perform symbolic differentiation on an expression composed of multiplies
  ;; and adds only
  (cond ([number? exp] 0)
	([variable? exp]
	 (if [same-variable? exp var] 1 0))
	([sum? exp]
	 (sum-make (sym-deriv (addend exp) var)
		   (sym-deriv (augend exp) var)))
	([product? exp]
	 (sum-make
	  (product-make (multiplier exp)
			(sym-deriv (multiplicand exp) var))
	  (product-make (sym-deriv (multiplier exp) var)
			(multiplicand exp))))
	(#t
	 (error 'sym-deriv "unknown expression type" exp))))

(define (variable? x)
  ;; variables are represented with symbols
  (symbol? x))

(define (same-variable? x y)
  ;; symbols are compared with eq?
  (and (variable? x) (variable? y) (eq? x y)))

(define (sum-make a1 a2)
  ;; sums are represented as a list
  (list '+ a1 a2))

(define (product-make m1 m2)
  ;; products are represented as a list
  (list '* m1 m2))

(define (sum? x)
  ;; check if an expression is a sum
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  ;; check if an expression is a product
  (and (pair? x) (eq? (car x) '*)))

(define (addend x)
  ;; selector for sum
  (cadr x))

(define (augend x)
  ;; selector for sum
  (caddr x))

(define (multiplier x)
  ;; selector for product
  (cadr x))

(define (multiplicand x)
  ;; selector for product
  (caddr x))

;;; testing
(sym-deriv '(+ x 3) 'x)
(sym-deriv '(* x y) 'x)
(sym-deriv '(* (* x y) (+ x 3)) 'x)

;;; perform some simplifications
(define (sum-make a1 a2)
  ;; improved sum-make that attempts to perform some simplifications for
  ;; known simple forms
  (cond ([=number? a1 0] a2)
	([=number? a2 0] a1)
	([and (number? a1) (number? a2)] (+ a1 a2))
	(#t (list '+ a1 a2))))

(define (=number? exp num)
  ;; helper function checks if an expression is equal to a given number
  (and (number? exp) (= exp num)))

(define (product-make m1 m2)
  ;; improved product-make that attempts to perform some simplifications for
  ;; known simple forms
  (cond ([or (=number? m1 0) (=number? m2 0)] 0)
	([=number? m1 1] m2)
	([=number? m2 1] m1)
	([and (number? m1) (number? m2)] (* m1 m2))
	(#t (list '* m1 m2))))

;;; testing
(sym-deriv '(+ x 3) 'x)
(sym-deriv '(* x y) 'x)
(sym-deriv '(* (* x y) (+ x 3)) 'x)

;;; 2.3.3: representing sets

;;; representing sets as unordered lists (ulset-*)
(define (ulset-element? x set)
  ;; linear scan through elements: O(N)
  ;; uses equal? rather than eq? -- this works for more than just symbols
  (cond ([null? set] #f)
	([equal? x (car set)] #t)
	(#t (ulset-element? x (cdr set)))))

(define (ulset-adjoin x set)
  ;; add to set: O(N)
  (if [ulset-element? x set]
      set
      (cons x set)))

(define (ulset-intersection set1 set2)
  ;; takes the set set-intersection: O(N^2)
  (cond ([or (null? set1) (null? set2)] '())
	([ulset-element? (car set1) set2]
	 (cons (car set1)
	       (ulset-intersection (cdr set1) set2)))
	(#t (ulset-intersection (cdr set1) set2))))

;; implementation of a set using ordered lists (olset-*)
(define (olset-element? x set)
  ;; O(N/2) implementation of finding an element in a set
  ;; (note that we can do better with a binary search, but we haven't
  ;; defined a vector representation with O(1) lookups (yet?) in SICP)
  (cond ([or (null? set) (< x (car set))] #f)
	([= x (car set)] #t)
	(#t (set-element? x (cdr set)))))

(define (olset-adjoin set1 set2)
  ;; will be implemented as exercise 2.51
  (error 'olset-adjoin "not implemented yet"))

(define (olset-intersection set1 set2)
  ;; ordered list representation of ; O(N) rather than O(N^2)
  (if [or (null? set1) (null? set2)]
      '()
      (let ([x1 (car set1)]
	    [x2 (car set2)])
	(cond ([= x1 x2]
	       (cons x1
		     (olset-intersection (cdr set1)
					 (cdr set2))))
	      ([< x1 x2]
	       (olset-intersection (cdr set1) set2))
	      ([< x2 x1]
	       (olset-intersection set1 (cdr set2)))))))

;; binary search tree implementation of a set (btset-*)

;; helpers
(define (btset-entry tree) (car tree))
(define (btset-left-branch tree) (cadr tree))
(define (btset-right-branch tree) (caddr tree))
(define (btset-tree-make entry left right)
  (list entry left right))

(define (btset-set-element? x set)
  ;; O(log(N)) implementation to search for element in set
  (cond ([null? set] #f)
	([= x (btset-entry set)] #t)
	([< x (btset-entry set)]
	 (btset-element? x (btset-left-branch set)))
	([> x (btset-entry set)]
	 (btset-element? x (btset-right-branch set)))))

(define (btset-adjoin x set)
  ;; add an element to a set: O(log(N))
  (cond ([null? set]
	 (btset-tree-make x '() '()))
	([= x (btset-entry set)] set)
	([< x (btset-entry set)]
	 (btset-tree-make (btset-entry set)
			  (btset-adjoin x (btset-left-branch set))
			  (btset-right-branch set)))
	([> x (btset-entry set)]
	 (btset-tree-make (btset-entry set)
			  (btset-left-branch set)
			  (btset-adjoin x (btset-right-branch set))))))

(define (btset-intersection set1 set2)
  ;; will be implemented as exercise 2.65
  (error 'btset-intersection "not implemented yet"))

;;; examples
(ulset-adjoin 5 '(5 2 3 4 1))
(ulset-adjoin 6 '(5 2 3 4 1))

;;; sample BST:
;;;    3
;;;  2   5
;;; 1   4
(btset-adjoin 5 '(3 (2 (1 () ()) ()) (5 (4 () ()) ())))
(btset-adjoin 6 '(3 (2 (1 () ()) ()) (5 (4 () ()) ())))
(btset-adjoin 3.5 '(3 (2 (1 () ()) ()) (5 (4 () ()) ())))

(ulset-intersection '(5 2 3 4 1) '(1 -1 5 7 3))
(olset-intersection '(1 2 3 4 5) '(-1 1 3 5 7))

;;; 2.3.4: Huffman Encoding Trees

;; helper functions: leaves represent a member of the alphabet
(define (huffman-leaf-make symbol weight)
  (list 'leaf symbol weight))
(define (huffman-leaf? object)
  (eq? (car object) 'leaf))
(define (huffman-leaf-symbol x) (cadr x))
(define (huffman-leaf-weight x) (caddr x))

(define (huffman-tree-make left right)
  ;; represent a huffman (sub)tree as the left child, the right child,
  ;; the members of the alphabet in this tree, and the weight of the tree
  (list left
	right
	(append (huffman-tree-symbols left) (huffman-tree-symbols right))
	(+ (huffman-tree-weight left) (huffman-tree-weight right))))

(define (huffman-tree-left-branch tree) (car tree))
(define (huffman-tree-right-branch tree) (cadr tree))

(define (huffman-tree-symbols tree)
  ;; get symbol from leaf or symbols from subtree as a list
  (if [huffman-leaf? tree]
      (list (huffman-leaf-symbol tree))
      (caddr tree)))

(define (huffman-tree-weight tree)
  ;; get weight of a leaf or subtree
  (if [huffman-leaf? tree]
      (huffman-leaf-weight tree)
      (cadddr tree)))

(define (huffman-decode bits tree)
  ;; huffman-decode a string of bits with a huffman tree

  (define (choose-branch bit branch)
    ;; helper for huffman-decode
    (cond ([= bit 0] (huffman-tree-left-branch branch))
	  ([= bit 1] (huffman-tree-right-branch branch))
	  (#t (error 'choose-branch "bad bit" bit))))
  
  (let huffman-decode-1 ([bits bits]
			 [current-branch tree])
    (if [null? bits]
	'()
	(let ([next-branch
	       (choose-branch (car bits) current-branch)])
	  (if [huffman-leaf? next-branch]
	      (cons (huffman-leaf-symbol next-branch)
		    (huffman-decode-1 (cdr bits) tree))
	      (huffman-decode-1 (cdr bits) next-branch))))))

(define (huffman-leaf-set-adjoin x set)
  ;; similar to the set-adjoin function for sets; helper for leaf-set-make
  ;; keeps track of the set as an ordered list (ordered by weight)
  (cond ([null? set]
	 (list x))
	([< (huffman-tree-weight x) (huffman-tree-weight (car set))]
	 (cons x set))
	(#t (cons (car set)
		  (huffman-leaf-set-adjoin x (cdr set))))))

(define (huffman-leaf-set-make pairs)
  ;; makes an (ordered) set of leaves from association list of the alphabet
  (if [null? pairs]
      '()
      (let ([pair (car pairs)])
	(huffman-leaf-set-adjoin (huffman-leaf-make (car pair)
						    (cdr pair))
				 (huffman-leaf-set-make (cdr pairs))))))
