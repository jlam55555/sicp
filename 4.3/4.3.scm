;;; Notes from SICP 4.3: Variations on a Scheme -- Nondeterministic Computing
;;; (in other words, solving declarative problem statements)

;;; the evaluator from 4.1 rather than 4.2 (lazy) is loaded for simplicity
(load "../4.1/4.1.scm")

;;; Need the `amb` operator, introduced by John McCarthy, that chooses an
;;; arbitrary value from a set of values that matches the given condition.
;;; Note that it does the systematic search for us, but that is abstracted
;;; away. `amb` has an associated operator called `try-again`.

;;; TODO: examples (at some later time, don't have much time rn)

;;; 4.3.3: Implementing the `amb` evaluator
(define (mi::amb? exp)
  (mi::tagged-list? exp 'amb))

(define (mi::amb-choices exp)
  (cdr exp))

(define (mi::analyze exp)
  ;; updated mi::analyze with amb case
  (cond ([mi::self-evaluating? exp]
	 (mi::analyze-self-evaluating exp))
	([mi::quoted? exp]
	 (mi::analyze-quoted exp))
	([mi::amb? exp]
	 (mi::analyze-amb exp))
	([mi::variable? exp]
	 (mi::analyze-variable exp))
	([mi::assignment? exp]
	 (mi::analyze-assignment exp))
	([mi::definition? exp]
	 (mi::analyze-definition exp))
	([mi::if? exp]
	 (mi::analyze-if exp))
	([mi::lambda? exp]
	 (mi::analyze-lambda exp))
	([mi::begin? exp]
	 (mi::analyze-sequence (mi::begin-actions exp)))
	([mi::cond? exp]
	 (mi::analyze (mi::cond->if exp)))
	([mi::application? exp]
	 (mi::analyze-application exp))
	(#t
	 (error 'mi::analyze "unknown expression type" exp))))

(define (mi::ambeval exp env succeed fail)
  ;; analyzes the given expression and applies the resulting execution procedure
  ;; to the given environment, together with two given continuations
  ((mi::analyze exp) env succeed fail))

;;; we need new `analyze` methods, now that the structure of expressions has
;;; changed to including its continuation; these primitives cannot fail, so
;;; we pass their ordinary values as succeed and passthrough the fail values
;;;
;;; note the form of the analyzed procedures:
;; (lambda (env succeed fail)
;;   ;; succeed is (lambda (value fail) ...)
;;   ;; fail is (lambda () ...)
;;   ...)

(define (mi::analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (mi::analyze-quoted exp)
  (let ([qval (mi::text-of-quotation exp)])
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (mi::analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (mi::lookup-variable-value exp env)
	     fail)))

(define (mi::analyze-lambda exp)
  (let ([vars (mi::lambda-parameters exp)]
	[bproc (mi::analyze-sequence (mi::lambda-body exp))])
    (lambda (env succeed fail)
      (succeed (mi::make-procedure vars bproc env)
	       fail))))

(define (mi::analyze-if exp)
  (let ([pproc (mi::analyze (mi::if-predicate exp))]
	[cproc (mi::analyze (mi::if-consequent exp))]
	[aproc (mi::analyze (mi::if-alternative exp))])
    (lambda (env succeed fail)
      (pproc env
	     ;; success continuation for evaluating the predicate
	     (lambda (pred-value fail2)
	       (if [mi::true? pred-value]
		   (cproc env succeed fail2)
		   (aproc env succeed fail2)))
	     ;; failure continuatioon for evaluating the predicate
	     fail))))

(define (mi::analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
	 ;; success continuation for calling a
	 (lambda (a-value fail2)
	   (b env succeed fail2))
	 ;; failure continuation for calling a
	 fail)))
  (define (loop first-proc rest-procs)
    (if [null? rest-procs]
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ([procs (map mi::analyze exps)])
    (if [null? procs]
	(error 'mi::analyze-sequence "empty sequence"))
    (loop (car procs) (cdr procs))))

(define (mi::analyze-definition exp)
  (let ([var (mi::definition-variable exp)]
	[vproc (mi::analyze (mi::definition-value exp))])
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (mi::define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))

(define (mi::analyze-assignment exp)
  ;; this is more interesting, because we have a "side-effect" that has to be
  ;; undone in the fail operation, rather than simply passing it around
  (let ([var (mi::assignment-variable exp)]
	(vproc (mi::analyze (mi::assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (let ([old-value
		      (mi::lookup-variable-value var env)])
		 (mi::set-variable-value! var val env)
		 (succeed 'ok
			  (lambda ()
			    (mi::set-variable-value! var old-value env)
			    (fail2)))))
	     fail))))

(define (mi::analyze-application exp)
  ;; there is additional complexity here because evaluating the operands may
  ;; fail, so we introduce the `get-args` procedure to handle this explicitly
  (let ([fproc (mi::analyze (mi::operator exp))]
	[aprocs (map mi::analyze (mi::operands exp))])
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc fail2)
	       (mi::get-args aprocs
			     env
			     (lambda (args fail3)
			       (mi::execute-application
				proc args succeed fail3))
			     fail2))
	     fail))))

(define (mi::get-args aprocs env succeed fail)
  ;; see above
  (if [null? aprocs]
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
	 (mi::get-args (cdr aprocs)
		       env
		       (lambda (args fail3)
			 (succeed (cons arg args)
				  fail3))
		       fail2))
       fail)))

(define (mi::execute-application proc args succeed fail)
  (cond ([mi::primitive-procedure? proc]
	 (succeed (mi::apply-primitive-procedure proc args)
		  fail))
	([mi::compound-procedure? proc]
	 ((mi::procedure-body proc)
	  (mi::extend-environment (mi::procedure-parameters proc)
				  args
				  (mi::procedure-environment proc))
	  succeed
	  fail))
	(else
	 (error 'mi::execute-application "unknown procedure type" proc))))

;;; finally we get to actually evaluating `amb` expressions

(define (mi::analyze-amb exp)
  (let ([cprocs (map mi::analyze (mi::amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
	(if [null? choices]
	    (fail)
	    ((car choices)
	     env
	     succeed
	     (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;;; driver loop

;;; this driver loop allows the user to try to find additional solutions
;;; by using the `try-again` operator; this is performed by calling the `fail`
;;; procedure on a successful solution, since this would call the internal
;;; `try-next` procedure inside `analyze-amb`. If there are no more values,
;;; then the user is informed and the driver loop moves on to the next
;;; computation

(define mi::*input-prompt*
  ";;; Amb-Eval input: ")
(define mi::*output-prompt**
  ";;; Amb-Eval output: ")
(define (mi::driver-loop)
  (define (internal-loop try-again)
    (mi::prompt-for-input mi::*input-prompt*)
    (let ([input (read)])
      (if [eq? input 'try-again]
	  (try-again)
	  [begin
	    (newline)
	    (display ";;; Starting a new problem ")
	    (mi::ambeval input
			 mi::*global-environment*
			 ;; ambeval success
			 (lambda (val next-alternative)
			   (mi::announce-output mi::*output-prompt*)
			   (mi::user-print val)
			   (internal-loop next-alternative))
			 ;; ambeval failure
			 (lambda ()
			   (mi::announce-output
			    ";;; There are no more values of ")
			   (mi::user-print input)
			   (mi::driver-loop)))])))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (mi::driver-loop))))

;;; some sample code

(mi::ambeval
 '[begin
    (define (require p)
      (if (not p) (amb)))
    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))
    (define (an-integer-starting-from n)
      (amb n (an-integer-starting-from (+ n 1))))]
 mi::*global-environment*
 (lambda _ '())
 (lambda _ '()))

;;; sample thing to try:
;; [begin
;;   (define ([a (an-integer-starting-from 0)]))
;;   (require (< a 5))]
;; try-again
;; try-again
;; try-again
;;; this will go into an infinite loop after a few operations
