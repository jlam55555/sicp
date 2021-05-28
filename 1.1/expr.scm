;;; emacs notes for myself:
;;; - M-j	indent-new-comment-line
;;; - M-;	paredit-comment-dwim (Do What I Mean)
;;; - C-M-x	geiser-eval-definition
;;; - M-m	back-to-indentation (go to first character on line)
;;; - C-a C-k	move-beginning-of-line kill-line (effectively ViM dd)
;;; - C-u 0 C-k	kill-line but backwards (more negative means more lines)
;;; - C-d	same as <Delete> key, but on the homerow (kind of)
;;; - C-q TAB	literal tab character
;;; - C-j	paredit-newline (like <Enter>)
;;; - M-{	backward-paragraph
;;; - M-}	forward-paragraph
;;; - M-h	mark-paragraph
;;; - C-e C-m	move-end-of-line newline (also C-e C-j or C-e <Enter>)
;;; - C-a C-o	move-beginning-of-line open-line
;;; - C-x h	mark-whole-buffer
;;; - M-g M-g	goto-line
;;; - C-- C-x o negative-argument other-window (goes in other order)
;;; - C--	negative-argument (same as C-u -)
;;; - C-c C-e [	geiser-squarify
;;; - M-n	comint-next-matching-input-from-input (like down arrow in bash)
;;; 		(for use in REPL or in minibuffer)
;;; - M-p	comint-previous-matching-input-from-input (like up arrow)
;;; - M-\	delete-horizontal-space
;;; - C-M-\	indent-region
;;; - C-x h	mark-whole-buffer

;;; simple arithmetic operations
486				
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

;;; prefix notation is useful; binary operators are fixed-arity at best,
;;; prefix operators are unambiguously variadic
(+ 21 35 12 7)
(* 25 4 12)

;;; nested expressions
;;; view evaluation as a tree; "tree-accumulation" value calculation
;;; (related: "substitution model" later in chapter)
(+ (* 3 5) (- 10 6))

;;; long expression
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

;;; same expression but pretty-printed
(+ (* 3
      (+ (* 2 4)
	 (+ 3 5)))
   (+ (- 10 7)
      6))

;;; giving a name to an expression
(define size 2)
size
(* 5 size)

(define pi 3.14159265358979323846264)
(define radius 10)
(* pi (* radius radius))
(* 2 pi radius)

;;; compound procedures
(define (square x)
  (* x x))

(* pi (square radius))

;;; substitution model; applicative model vs. normal model
;;; - applicative model: fully expand and then reduce
;;; - normal model: evaluate arguments and then reduce
;;; applicative model creates many repeated subexpressions, but both should
;;; give the same result if the function can be modeled using the substition
;;; model and yield "legitimate" values

;;; conditional expression (this overrides the builtin abs)
(define (abs x)
  (cond ([> x 0] x)
	([= x 0] 0)
	([< x 0] (- x))))

;;; alternative definition
(define (abs x)
  (cond ([< x 0] (- x))
	(else x)))

;;; other conditional expressions and predicates
;;; (all of these have implicit control flow)
(define (>= x y)
  (or (> x y) (= x y)))

;;; newton's approximation for sqrt(x)
(define (sqrt-iter guess x)
  (if [good-enough? guess x]
      guess
      (sqrt-iter (improve guess x)
		 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; consistent renaming
;;; https://www.cqse.eu/fileadmin/content/news/publications/2005-concise-and-consistent-naming.pdf
;;; terminology: free, bound, bind/capture a variable
;;; lexical scoping "dictates that free variables in a procedure are taken to
;;; refer to bindings made by enclosing definitions; that is, they are looked
;;; up in the environment in which the procedure was defined."

;;; lexically scoped version of sqrt; this definition is completely
;;; self-contained (sqrt-2 is defined in exercises.scm)
(define (sqrt-3 x)
  (define frac-ratio-threshold 1e-6)
  (define (square x) (* x x))
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x))
       (* (min x guess) frac-ratio-threshold)))
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if [good-enough? guess]
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;; factory function to generate an arbitrary Newton's rule estimator
(define (nr-factory f^-1 threshold-ratio improve init-guess)
  (lambda (x)
    (define (good-enough? guess)
      (< (abs (- (f^-1 guess) x))
	 (* (min (abs x) (abs guess)) threshold-ratio)))
    (define (average x y) (/ (+ x y) 2))
    (define (iter guess)
      (if [good-enough? guess]
	  guess
	  (iter (improve guess x))))
    (iter init-guess)))

;;; sample usage: sqrt and cbrt
(define (average x y)
  (/ (+ x y) 2))
(define sqrt-4
  (nr-factory (lambda (x) (* x x))
	      1e-12
	      (lambda (guess x) (average guess (/ x guess)))
	      1.0))

(define cbrt-2
  (nr-factory (lambda (x) (* x x x))
	      1e-12
	      (lambda (guess x)
		(/ (+ (/ x (* guess guess))
		      (* 2 guess))
		   3))
	      1.0))

;;; test cases (note: an input value of 0 will fail because it will never
;;; converge to 0)
(sqrt-4 16)
(sqrt-4 27)
(sqrt-4 1e240)
(sqrt-4 1e-240)

(cbrt-2 16)
(cbrt-2 27)
(cbrt-2 1e240)
(cbrt-2 1e-240)
