;;; exercises for sicp 2.3
;;; modules are a little buggy in geiser (because they are a syntactic construct
;;; rather than a value) so I have to execute the following line before
;;; the rest of the buffer to get this to work
(load "../2.3/2.3.scm")

;;; 2.67
(define sample-tree
  (huffman-tree-make (huffman-leaf-make 'A 4)
		     (huffman-tree-make
		      (huffman-leaf-make 'B 2)
		      (huffman-tree-make (huffman-leaf-make 'D 1)
					 (huffman-leaf-make 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(huffman-decode sample-message sample-tree)

;;; 2.68: adding the encode method
(define (huffman-encode message tree)
  ;; encode a message using a huffman tree

  (define (huffman-encode-symbol x tree)
    ;; encode a single symbol using the huffman tree; assumes tree is valid
    ;; uses memq to determine whether a symbol is in a set -- better time
    ;; complexity to use a set (or even better, a hashtable)
    (cond ([huffman-leaf? tree] '())
	  ([my-memq x (huffman-tree-symbols (huffman-tree-left-branch tree))]
	   (cons 0
		 (huffman-encode-symbol x (huffman-tree-left-branch tree))))
	  (#t
	   (cons 1
		 (huffman-encode-symbol x (huffman-tree-right-branch tree))))))
  
  (if [null? message]
      '()
      (append (huffman-encode-symbol (car message) tree)
	      (huffman-encode (cdr message) tree))))

(huffman-encode '(A D A B B C A) sample-tree)

;;; 2.69: generating a huffman tree
(define (huffman-tree-generate pairs)
  ;; generates a huffman tree from an assoclist of symbols and frequencies;
  ;; not to be confused with huffman-tree-make, which is an internal function
  ;; used to create a subtree from leaf nodes

  (define (successive-merge trees)
    ;; combines huffman trees into a single huffman tree; works by merging
    ;; the two smallest trees, inserting it in the correct (sorted) position,
    ;; and then repeating
    ;; assumes trees are sorted by weight (as huffman-make-leaf-set does)
    (if [< (length trees) 2]
	trees
	(successive-merge
	 (huffman-leaf-set-adjoin (huffman-tree-make (car trees) (cadr trees))
				  (cddr trees)))))
  
  (car (successive-merge (huffman-leaf-set-make pairs))))

(huffman-tree-generate '((A . 4)
			 (B . 2)
			 (C . 1)
			 (D . 1)))

;;; 2.70: a test case
(define 1950s-rock-huffman-code
  (huffman-tree-generate '((A . 2) (NA . 16)
			   (BOOM . 1) (SHA . 3)
			   (GET . 2) (YIP . 9)
			   (JOB . 2) (WAH . 1))))

(define 1950s-encoded
  (huffman-encode '(GET A JOB SHA NA NA NA NA NA NA NA NA
			GET A JOB SHA NA NA NA NA NA NA NA NA
			WAH YIP YIP YIP YIP YIP YIP YIP YIP
			SHA BOOM)
		  1950s-rock-huffman-code))

(huffman-decode 1950s-encoded 1950s-rock-huffman-code) ; sanity check

(length 1950s-encoded)			; length is 82

;;; this would require 3 bits per symbol in a fixed bit alphabet, requiring
;;; 36 symbols * 3 bits/symbol = 108; we save 26 bits, ~25%

;;; 2.71
;;; TODO

;;; 2.72
;;; TODO
