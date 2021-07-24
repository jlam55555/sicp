;;; Notes from SICP 4.2: Variations on a Scheme -- Lazy Evaluation
;;; "snarf"

;;; slight difference between lazy and normal-order evaluation; a different
;;; and definition than what the book provides can be found here:
;;; https://stackoverflow.com/a/4634631/2397327. SICP calls this call-by-name
;;; vs. call-by-need (memoized) lazy evaluation

;;; non-strict vs. strict when referring to specific parameters of a function
;;; (as opposed to an entire language); this is the same language that Haskell
;;; uses to describe the evaluation of its operators; non-strict means that the
;;; body of the function may be entered before evaluating all of its operands

;;; origin of the word "thunk"
(load "../4.1/4.1.scm")

;;; 4.2.2: An Interpreter with Lazy Evaluation
;;; note: I use `eval-force` rather than `actual-value`, don't think the latter
;;; name is as indicative

(define (mi::eval exp env)
  ;; updated eval with lazy evaluation
  (cond ([mi::self-evaluating? exp] exp)
	([mi::variable? exp]
	 (mi::lookup-variable-value exp env))
	([mi::quoted? exp]
	 (mi::text-of-quotation exp))
	([mi::assignment? exp]
	 (mi::eval-assignment exp env))
	([mi::definition? exp]
	 (mi::eval-definition exp env))
	([mi::if? exp]
	 (mi::eval-if exp env))
	([mi::lambda? exp]
	 (mi::make-procedure (mi::lambda-parameters exp)
			     (mi::lambda-body exp)
			     env))
	([mi::begin? exp]
	 (mi::eval-sequence (mi::begin-actions exp) env))
	([mi::cond? exp]
	 (mi::eval (mi::cond->if exp) env))

	;; this clause is changed; the operator is forced rather than calling
	;; the default-lazy mi::eval, and the arguments are not evaluated now
	([mi::application? exp]
	 (mi::apply (mi::eval-force (mi::operator exp) env)
		    (mi::operands exp)
		    env))

	(#t
	 (error 'mi::eval "unknown expression type" exp))))

(define (mi::eval-force exp env)
  ;; forced eval
  (mi::force (mi::eval exp env)))

(define (mi::apply procedure arguments env)
  ;; primitive procedures are strict, non-primitives (compound) are non-strict
  (cond ([mi::primitive-procedure? procedure]
	 (mi::apply-primitive-procedure
	  procedure
	  (mi::list-of-strict-values arguments env))) ; new
	([mi::compound-procedure? procedure]
	 (mi::eval-sequence
	  (mi::procedure-body procedure)
	  (mi::extend-environment
	   (mi::procedure-parameters procedure)
	   (mi::list-of-delayed-values arguments env)
	   (mi::procedure-environment procedure)))) ; new
	(#t
	 (error 'mi::apply "unknown procedure type" procedure))))

(define (mi::list-of-strict-values exps env)
  ;; like `list-of-values`, but uses eval-strict
  (map (lambda (exp) (mi::eval-force exp env)) exps))

(define (mi::list-of-delayed-values exps env)
  ;; like `list-of-values`, but creates delayed values
  (map (lambda (exp) (mi::make-delayed exp env)) exps))

(define (mi::eval-if exp env)
  ;; updated lazy if statement
  (if [mi::true? (mi::eval-force (mi::if-predicate exp) env)]
      (mi::eval (mi::if-consequent exp) env)
      (mi::eval (mi::if-alternative exp) env)))

(define mi::*input-prompt* ";;; L-Eval input: ")
(define mi::*output-prompt* ";;; L-Eval output: ")
(define (mi::driver-loop)
  ;; updated prompt that forces the result
  (mi::prompt-for-input mi::*input-prompt*)
  (let* ([input (read)]
	 [output (mi::eval input mi::*global-environment*)])
    (mi::announce-output mi::*output-prompt*)
    (mi::user-print output))
  (mi::driver-loop))

;;; representing thunks

;;; this is a simple way without memoization

(define (mi::force obj)
  ;; primitive for lazy evaluation; we expect the lazy expression to be
  ;; expressed as a thunk, and we force it simply by invoking it
  (if [mi::thunk? obj]
      (mi::eval-force (mi::thunk-exp obj) (mi::thunk-env obj))
      obj))

(define (mi::make-delayed exp env)
  ;; create a delayed expression thunk
  (list 'thunk exp env))

(define (mi::thunk? obj)
  ;; check if an object is a thunk
  (mi::tagged-list? obj 'thunk))

(define (mi::thunk-exp thunk)
  ;; extract the thunk function from the thunk object
  (cadr thunk))

(define (mi::thunk-env thunk)
  ;; extract the thunk environment from the thunk object
  (caddr thunk))

;;; a better version with memoization

(define (mi::evaluated-thunk? obj)
  ;; check if the object is an evaluated thunk
  (mi::tagged-list? obj 'evaluated-thunk))

(define (mi::thunk-value evaluated-thunk)
  ;; retrieve value of evaluated thunk
  (cadr evaluated-thunk))

(define (mi::force obj)
  ;; better version of mi::force using memoization
  (cond ([mi::thunk? obj]
	 (let ([result (mi::eval-force
			(mi::thunk-exp obj)
			(mi::thunk-env obj))])
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result)
	   (set-cdr! (cdr obj) '())
	   result))
	([mi::evaluated-thunk? obj]
	 (mi::thunk-value obj))
	(#t obj)))
