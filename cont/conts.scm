;;; Implementing (multiple) continuations in Scheme
;;; Heavily based on 4.3's continuations, but now exposing them to the user
(load "../4.1/4.1.scm")

;;; see cont.scm; this is a generalization to an arbitrary number of
;;; continuations. Will rename `ret` to `ccs` because `ret` is only semantically
;;; common for the main continuation; `ccs` now refers to the "generic set
;;; of current continuations"

;;; form of the analyzed procedures:
;; (lambda (env ccs)
;;   ;; ccs is ((lambda (val) ...) ...)
;;   ...)

;;; stick to the convention that the first cc is the main `cc`; maybe also
;;; stick to the convention that the second is the error `cc`; and user-defined
;;; cc's come after that

(define (mi::main-cc ccs)
  ;; following the convention stated above
  (car ccs))

(define (mi::error-cc ccs)
  ;; following the convention stated above; checks that there is a second
  ;; continuation
  (if [not (null? (cdr ccs))]
      (cadr ccs)
      (error 'mi::error-cc "no error continuation")))

(define (mi::analyze-self-evaluating exp)
  (lambda (env ccs)
    ((mi::main-cc ccs) exp)))

(define (mi::analyze-quoted exp)
  (let ([qval (mi::text-of-quotation exp)])
    (lambda (env ccs)
      ((mi::main-cc ccs) qval))))

(define (mi::analyze-variable exp)
  (lambda (env ccs)
    ((mi::main-cc ccs) (mi::lookup-variable-value exp env))))

(define (mi::analyze-lambda exp)
  (let ([vars (mi::lambda-parameters exp)]
	[bproc (mi::analyze-sequence (mi::lambda-body exp))])
    (lambda (env ccs)
      ((mi::main-cc ccs) (mi::make-procedure vars bproc env)))))

(define (mi::analyze-if exp)
  (let ([pproc (mi::analyze (mi::if-predicate exp))]
	[cproc (mi::analyze (mi::if-consequent exp))]
	[aproc (mi::analyze (mi::if-alternative exp))])
    (lambda (env ccs)
      (pproc env
	     (cons
	      (lambda (pred-value)
		(if [mi::true? pred-value]
		    (cproc env ccs)
		    (aproc env ccs)))
	      (cdr ccs))))))

(define (mi::analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env ccs)
      (a env
	 (cons
	  (lambda (a-value)
	    (b env ccs))
	  (cdr ccs)))))
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
    (lambda (env ccs)
      (vproc env
	     (cons
	      (lambda (val)
		(mi::define-variable! var val env)
		((mi::main-cc ccs) 'ok))
	      (cdr ccs))))))

(define (mi::analyze-assignment exp)
  (let ([var (mi::assignment-variable exp)]
	(vproc (mi::analyze (mi::assignment-value exp))))
    (lambda (env ccs)
      (vproc env
	     (cons
	      (lambda (val)
		(let ([old-value
		       (mi::lookup-variable-value var env)])
		  (mi::set-variable-value! var val env)
		  ((mi::main-cc ccs) 'ok)))
	      (cdr ccs))))))

(define (mi::analyze-application exp)
  (let ([fproc (mi::analyze (mi::operator exp))]
	[aprocs (map mi::analyze (mi::operands exp))])
    (lambda (env ccs)
      (fproc env
	     (cons
	      (lambda (proc)
		(mi::get-args aprocs
			      env
			      (cons
			       (lambda (args)
				 (mi::execute-application proc args ccs))
			       (cdr ccs))))
	      (cdr ccs))))))

(define (mi::get-args aprocs env ccs)
  ;; for use in `mi::analyze-application`
  (if [null? aprocs]
      ((mi::main-cc ccs) '())
      ((car aprocs)
       env
       (cons
	(lambda (arg)
	  (mi::get-args (cdr aprocs)
			env
			(cons
			 (lambda (args)
			   ((mi::main-cc ccs) (cons arg args)))
			 (cdr ccs))))
	(cdr ccs)))))

(define (mi::execute-application proc args ccs)
  (cond ([mi::primitive-procedure? proc]
	 ((mi::main-cc ccs) (mi::apply-primitive-procedure proc args)))
	([mi::compound-procedure? proc]
	 ((mi::procedure-body proc)
	  (mi::extend-environment (mi::procedure-parameters proc)
				  args
				  (mi::procedure-environment proc))
	  ccs))
	(else
	 (error 'mi::execute-application "unknown procedure type" proc))))

(define mi::*input-prompt*
  ";;; Conts-Eval input: ")
(define mi::*output-prompt**
  ";;; Conts-Eval output: ")

(define (mi::conts-eval exp env ccs)
  ;; similar to `mi::cont-eval`, but with array of continuations
  ((mi::analyze exp) env ccs))

(define (mi::driver-loop)
  ;; modified to evaluate and print the value (last continuation is
  ;; `mi::user-print`)
  (mi::prompt-for-input mi::*input-prompt*)
  (let ([input (read)])
    (mi::conts-eval input
		    mi::*global-environment*
		    (list
		     ;; default main continuations
		     (lambda (val)
		       (mi::user-print val)
		       (mi::driver-loop))
		     ;; default error continuation
		     (lambda (val)
		       (mi::error-print val)
		       (mi::driver-loop))))))

(define (mi::error-print object)
  ;; show eval output to user
  (format #t "error: top-level error continuation called with irritant ")
  (mi::user-print object))

;;; FINISHED MULTI CONTINUATION EXAMPLE

;;; add new operators:
;;; - `(call/cc f)`             regular, provides (main) continuation
;;; - `(call/ccs f)`            provides list of all current continuations
;;; - `(call/new-ccs f exp)`    allows you to set custom ccs

(define (mi::analyze exp)
  ;; add call/cc clause
  (cond ([mi::self-evaluating? exp]
	 (mi::analyze-self-evaluating exp))
	([mi::quoted? exp]
	 (mi::analyze-quoted exp))
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

	;; multiple continuations scenario; generalization of call/cc
	([mi::call/ccs? exp]
	 (mi::analyze-call/ccs exp))

	;; allows you to set auxiliary continuations
	([mi::call/new-ccs? exp]
	 (mi::analyze-call/new-ccs exp))
	
	([mi::application? exp]
	 (mi::analyze-application exp))
	(#t
	 (error 'mi::analyze "unknown expression type" exp))))

;;; implementing `call/ccs`
(define (mi::call/ccs? exp)
  (mi::tagged-list? exp 'call/ccs))

(define (mi::call/ccs-arg exp)
  (cadr exp))

(define (mi::analyze-call/ccs exp)
  ;; for (call/cc f), call f, binding the continuation as a primitive
  ;; procedure to the argument of f
  (let ([fproc (mi::analyze (mi::call/ccs-arg exp))])
    (lambda (env ccs)
      (fproc env
	     (cons
	      (lambda (proc)
		(mi::execute-application
		 proc
		 (list
		  (map (lambda (cc) (list 'primitive cc)) ccs))
		 ccs))
	      (cdr ccs))))))

;;; implementing `call/new-ccs`
;;; sample usage:
;; (call/new-ccs
;;  ;; set an error continuation
;;  (lambda (ccs)
;;    (list
;;     (lambda (val)
;;       (display "custom error continuation"))))
;;  ;; expression to call with that continuation
;;  (+ 2 (call/ccs
;;        (lambda (ccs)
;; 	 ;; call the error continuation
;; 	 ((car (cdr ccs)) 4)))))

(define (mi::call/new-ccs? exp)
  (mi::tagged-list? exp 'call/new-ccs))

(define (mi::call/new-ccs-generator exp)
  (cadr exp))

(define (mi::call/new-ccs-body exp)
  (caddr exp))

;; (define (mi::analyze-call/new-ccs exp)
;;   (let ([fproc (mi::analyze (mi::call/new-ccs-arg exp))]
;; 	[body (mi::analyze (mi::call/new-ccs-body exp))])
;;     (lambda (env ccs)
;;       (fproc env
;; 	     (cons
;; 	      (lambda (proc)
;; 		(mi::execute-application
;; 		 proc
;; 		 (list
;; 		  (map (lambda (cc) (list 'primitive cc)) ccs))
;; 		 ccs))
;; 	      (cdr ccs))))))

(define (mi::analyze-call/new-ccs exp)
  (let ([fproc (mi::analyze (mi::call/new-ccs-generator exp))]
	[body (mi::analyze (mi::call/new-ccs-body exp))])
    (lambda (env ccs)
      (fproc env
	     (cons
	      (lambda (proc)
		(mi::execute-application
		 proc
		 (list
		  (map (lambda (cc) (list 'primitive cc)) ccs))
		 (cons
		  (lambda (new-ccs)
		    ;; (inspect new-ccs)
		    ;; (inspect body)
		    ;; no safety checks!!!
		    ;; run the body with the user-supplied ccs; need to map
		    ;; them to regular functions
		    (body env
			  (map
			   (lambda (cc)
			     (lambda (val)
			       ;; TODO: working here
			       (mi::execute-application
				cc
				(list val)
				ccs)))
			   new-ccs)))
		  (cdr ccs))))
	      (cdr ccs))))))

;;; TODO: examples of all functions
;;; TODO: allow setting multiple continuations

(define (mi::eval exp)
  (mi::conts-eval
   exp
   mi::*global-environment*
   (list
    (lambda (x) '())
    (lambda (x) '()))))

;;; write call/cc (base case) in terms of more general case (call/ccs)
(mi::eval
 '(define (call/cc f)
    (call/ccs (lambda (ccs)
		(f (car ccs))))))

;;; TODO: what is the intended behavior with multiple return calls?
;;; i.e., if there is a continuation that is not a tail-call?
;; (mi::eval-test
;;  '(display (+ 2 (call/cc (lambda (cc) (cc 4) 5)))))
