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
    (if [null? (cdr trees)]
	(car trees)
	(successive-merge
	 (huffman-leaf-set-adjoin (huffman-tree-make (car trees) (cadr trees))
				  (cddr trees)))))
  
  (successive-merge (huffman-leaf-set-make pairs)))

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
;;;
;;; using a slightly simplified notation (omitting symbols):
;;; note that the depth of a leaf is its number of bits required
;;; 
;; (leaf 1)				; n=1; this is a bit of an edge case
;; 					; since we cannot encode anything with
;; 					; zero bits per symbol
;; ((leaf 1)
;;  () 1)				; more practical n=1
;; ((leaf 1)
;;  (leaf 2) 3)				; n=2
;; (((leaf 1)
;;   (leaf 2) 3)
;;  (leaf 4) 7)				; n=3
;; ((((leaf 1)
;;     (leaf 2) 3)
;;    (leaf 4) 7)
;;  (leaf 8) 15)			; n=4
;; (((((leaf 1)
;;     (leaf 2) 3)
;;    (leaf 4) 7)
;;   (leaf 8) 15)
;;  (leaf 16) 31)			; n=5
;;; 
;;; the most frequent symbol (weight 2^(n-1)) always takes one bit to encode,
;;; while the least frequent symbol (weight 1) always takes n-1 bits to encode
;;; (except for n=1 case)

;;; 2.72: orders of growth for Huffman encoding
;;; 
;;; The order of growth is proportional to the time it takes to make a branch
;;; decision times the depth of a symbol. This is hard because there are a lot
;;; of possible variations of tree shapes. For now we will assume that the
;;; branch decision takes O(N) time (i.e., using a linear search method as used
;;; above) rather than O(log(N)) or O(1) with a set or hashtable representation.
;;;
;;; Further note the following implementation details:
;;; - when making the tree, the successive-merge procedure always puts the
;;;   subtree with the smaller weight as the left child
;;; - when encoding a string, the encode procedure always scans the symbol set
;;;   of the left child (only)
;;;
;;; For the example in 2.71, the analysis is easy. The depth of the symbols are
;;; (1 2 3 4 ... n-1). Due to the implementation notes above, at depth d there
;;; will be and (n-d-1) symbols on the left and 1 symbol on the right, so we
;;; will have to scan on the order of O(n-d-1) at each depth d.
;;;
;;; For the symbol at depth 1, we would have n-0-1 ~= n operations
;;; For depth 2, (n-1)+(n-2) operations ~= n^2 ops
;;; For depth 3, (n-1)+(n-2)+(n-3) ops ~= n^3 ops
;;; ...
;;; For depth n-1, ~= n^(n-1) ops (two leaf nodes at this depth)
;;;
;;; Lastly, we have to average these according to their average frequencies.
;;;
;;; n/2 + n^2/2^2 + ... n^(n-1)/2^(n-1) + n^(n-1)/2^n
;;; = \sum_{d=1}^{n}{(n/2)^d}
;;; (approximate with infinite series: \sum r^n = a1/(1-r), |r| < 1)
;;; = 2*n/2 = n
;;; 
;;; So each symbol takes O(N) to encode, where N is the size of the alphabet.
;;;
;;; (May not have gotten these calculations entirely correct, will need to
;;; double-check with someone)
;;;
;;; Clearly there are a lot of repeated checks and this would be a lot better
;;; if we scanned the right subtree's symbol list rather than the left subtree,
;;; or if we put the subtree with the smaller weight on the right. And if we do
;;; this, then the time complexity will worsen for some other case. This was an
;;; unfortunate implementation detail caused by the use of a bad data structure
;;; for sets.
;;;
;;; In the swapped situation described above, each branching decision would take
;;; O(1) time, so the weighted sum would be instead:
;;;
;;; 1/2 + 2/2^2 + 3/2^3 + ... (n-1)/2^(n-1) + (n-1)/2^n
;;; = \sum_{d=1}^{d}{d/2^d}
;;; = 2 (in the infinite sum)
;;;
;;; So this implementation would take O(1) time to encode a symbol, which is
;;; what you might expect given the relative frequencies of the symbol.
