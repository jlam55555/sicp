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

;;; will start using modules when I have multiple versions of the same procedure
;;; - note: emacs doesn't recognize ``module'' as a special form (module syntax
;;;   is mostly specific to implementation I think?), so have to add this to
;;;   .emacs file to get proper indentation:
;; (put 'module 'scheme-indent-function 2)
;;;   ref: https://stackoverflow.com/a/4200242
;;; - TODO: try to separate interfaces from modules using a macro: see
;;;   https://www.scheme.com/csug8/syntax.html
;;; - note: modules provide an automatic way to prefix/namespace variables with
;;;   the import prefix syntax, e.g.:
;; (import (prefix set-unordered-list set::))
;;;   so that you can do C++-like namespaces:
;; (set::element? x set)
;;;   see the examples below

(module set-unordered-list (element?
			    adjoin
			    intersection)
  ;; representing sets as unordered lists
  
  (define (element? x set)
    ;; linear scan through elements: O(N)
    ;; uses equal? rather than eq? -- this works for more than just symbols
    (cond ([null? set] #f)
	  ([equal? x (car set)] #t)
	  (#t (element? x (cdr set)))))

  (define (adjoin x set)
    ;; add to set: O(N)
    (if [element? x set]
	set
	(cons x set)))

  (define (intersection set1 set2)
    ;; takes the set intersection: O(N^2)
    (cond ([or (null? set1) (null? set2)] '())
	  ([element? (car set1) set2]
	   (cons (car set1)
		 (intersection (cdr set1) set2)))
	  (#t (intersection (cdr set1) set2)))))

(module set-ordered-list (element?
			  adjoin
			  intersection)
  ;; implementation of a set using ordered lists
  
  (define (element? x set)
    ;; O(N/2) implementation of finding an element in a set
    ;; (note that we can do better with a binary search, but we haven't
    ;; defined a vector representation with O(1) lookups (yet?) in SICP)
    (cond ([or (null? set) (< x (car set))] #f)
	  ([= x (car set)] #t)
	  (#t (element? x (cdr set)))))

  ;; TODO: implement adjoin (exercise 2.61)
  ;; for now, just import the one from the previous module
  (import (only set-unordered-list adjoin))

  (define (intersection set1 set2)
    ;; ordered list representation of ; O(N) rather than O(N^2)
    (if [or (null? set1) (null? set2)]
	'()
	(let ([x1 (car set1)]
	      [x2 (car set2)])
	  (cond ([= x1 x2]
		 (cons x1
		       (intersection (cdr set1)
					 (cdr set2))))
		([< x1 x2]
		 (intersection (cdr set1) set2))
		([< x2 x1]
		 (intersection set1 (cdr set2))))))))

(module set-binary-tree (element?
			 adjoin
			 intersection)
  ;; binary search tree implementation of a set

  ;; helpers
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (tree-make entry left right)
    (list entry left right))

  (define (element? x set)
    ;; O(log(N)) implementation to search for element in set
    (cond ([null? set] #f)
	  ([= x (entry set)] #t)
	  ([< x (entry set)]
	   (element? x (left-branch set)))
	  ([> x (entry set)]
	   (element? x (right-branch set)))))

  (define (adjoin x set)
    ;; add an element to a set: O(log(N))
    (cond ([null? set]
	   (tree-make x '() '()))
	  ([= x (entry set)] set)
	  ([< x (entry set)]
	   (tree-make (entry set)
		      (adjoin x (left-branch set))
		      (right-branch set)))
	  ([> x (entry set)]
	   (tree-make (entry set)
		      (left-branch set)
		      (adjoin x (right-branch set))))))

  (define (intersection set1 set2)
    ;; will be implemented as exercise 2.65
    (error 'intersection "not implemented yet")))

;;; TODO: add some examples of sets and module namespacing here

;;; 2.3.4: Huffman Encoding Trees
(module huffman-encoding (tree-make
			  leaf-make
			  decode)
  ;; an implementation of the huffman encoding

  ;; helper functions: leaves represent a member of the alphabet
  (define (leaf-make symbol weight)
    (list 'leaf symbol weight))
  (define (leaf? object)
    (eq? (car object) 'leaf))
  (define (leaf-symbol x) (cadr x))
  (define (leaf-weight x) (caddr x))
  
  (define (tree-make left right)
    ;; represent a huffman (sub)tree as the left child, the right child,
    ;; the members of the alphabet in this tree, and the weight of the tree
    (list left
	  right
	  (append (tree-symbols left) (tree-symbols right))
	  (+ (tree-weight left) (tree-weight right))))

  (define (tree-left-branch tree) (car tree))
  (define (tree-right-branch tree) (cadr tree))

  (define (tree-symbols tree)
    ;; get symbol from leaf or symbols from subtree as a list
    (if [leaf? tree]
	(list (leaf-symbol tree))
	(caddr tree)))

  (define (tree-weight tree)
    ;; get weight of a leaf or subtree
    (if [leaf? tree]
	(leaf-weight tree)
	(cadddr tree)))

  (define (choose-branch bit branch)
    ;; helper for decode
    (cond ([= bit 0] (tree-left-branch branch))
	  ([= bit 1] (tree-right-branch branch))
	  (#t (error 'choose-branch "bad bit" bit))))

  (define (decode bits tree)
    ;; decode a string of bits with a huffman tree
    (let decode-1 ([bits bits]
		   [current-branch tree])
      (if [null? bits]
	  '()
	  (let ([next-branch
		 (choose-branch (car bits) current-branch)])
	    (if [leaf? next-branch]
		(cons (leaf-symbol next-branch)
		      (decode-1 (cdr bits) tree))
		(decode-1 (cdr bits) next-branch))))))

  (define (adjoin-set x set)
    ;; TODO
    222)

  (define (make-leaf-set pairs)
    ;; TODO
    222))

;;; 2.67
(import (prefix huffman-encoding huff::))
(define sample-tree
  (huff::tree-make (huff::leaf-make 'A 4)
		   (huff::tree-make
		    (huff::leaf-make 'B 2)
		    (huff::tree-make (huff::leaf-make 'D 1)
				     (huff::leaf-make 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(huff::decode sample-message sample-tree)
