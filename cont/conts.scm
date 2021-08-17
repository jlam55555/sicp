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

;;; TODO: remove this
;; (define mi::main-cc
;;   (trace-lambda mi::main-cc (ccs)
;;     (car ccs)))

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
		    (cproc env (mi::main-cc ccs))
		    (aproc env (mi::main-cc ccs))))
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
		     (lambda (val)
		       (mi::user-print val)
		       (mi::driver-loop))))))

;;; FINISHED MULTI CONTINUATION EXAMPLE

;;; add new operators:
;;; - `(call/cc f)`         regular, provides (main) continuation
;;; - `(call/ccs f)`        provides list of continuations
;;; - `(call/new-ccs ccs f)`      allows you to set custom ccs

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

	;; same as before
	([mi::call/cc? exp]
	 (mi::analyze-call/cc exp))
	
	([mi::application? exp]
	 (mi::analyze-application exp))
	(#t
	 (error 'mi::analyze "unknown expression type" exp))))

(define (mi::call/cc? exp)
  ;; whether exp is a call/cc expression
  (mi::tagged-list? exp 'call/cc))

(define (mi::call/cc-arg exp)
  ;; get argument of call/cc (should be the second and last subexpression
  ;; of `exp`)
  (cadr exp))

;;; What we want to achieve (CPS form):
;; (call/cc f ret)
;;      =>
;; (define (call/cc f ret)
;;   (f (lambda (x _) (ret x)) ret))
;;   
;;; Same without CPS form (note that ret comes from the interpreter and is not
;;; usually exposed):
;; (call/cc f)
;;      =>
;; (define (call/cc f)
;;   (f ret))

(define (mi::analyze-call/cc exp)
  ;; for (call/cc f), call f, binding the continuation as a primitive
  ;; procedure to the argument of f
  (let ([fproc (mi::analyze (mi::call/cc-arg exp))])
    (lambda (env ccs)
      (fproc env
	     (cons
	      (lambda (proc)
		(mi::execute-application
		 proc
		 (list (list 'primitive (mi::main-cc ccs)))
		 ccs))
	      (cdr ccs))))))

;;; TODO: examples of call/cc
;; (call/cc (lambda (r1) (call/cc (lambda (r2) (r1 (r2 (r1 3)))))))
;; (+ 2 (call/cc (lambda (ret) (ret 4) 5)))
;; (+ 2 (call/cc (lambda (ret) 5)))

;;; TODO: multiple continuations (optional error continuation)
