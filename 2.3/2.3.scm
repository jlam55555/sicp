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
  (cond ([null? x] #f)
	([eq? sym (car lst)] lst)
	(#t (my-memq sym (cdr lst)))))

(my-memq 'apple '(pear banana prune))

;;; 2.3.2: symbolic differentiation
