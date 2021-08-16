;;; Implementing continuations in Scheme
;;; Heavily based on 4.3's continuations, but now exposing them to the user
(load "../4.1/4.1.scm")

;;; the continuation will be called `ret` in the spirit of "return"; other
;;; appropriate names would be `cc` or `cont`

;;; the following code is more or less copied from 4.3, and simplified by
;;; removing the failure continuations

;;; form of the analyzed procedures:
;; (lambda (env ret)
;;   ;; ret is (lambda (val) ...)
;;   ...)

(define (mi::analyze-self-evaluating exp)
  (lambda (env ret)
    (ret exp)))

(define (mi::analyze-quoted exp)
  (let ([qval (mi::text-of-quotation exp)])
    (lambda (env ret)
      (ret qval))))

(define (mi::analyze-variable exp)
  (lambda (env ret)
    (ret (mi::lookup-variable-value exp env))))

(define (mi::analyze-lambda exp)
  (let ([vars (mi::lambda-parameters exp)]
	[bproc (mi::analyze-sequence (mi::lambda-body exp))])
    (lambda (env ret)
      (ret (mi::make-procedure vars bproc env)))))

(define (mi::analyze-if exp)
  (let ([pproc (mi::analyze (mi::if-predicate exp))]
	[cproc (mi::analyze (mi::if-consequent exp))]
	[aproc (mi::analyze (mi::if-alternative exp))])
    (lambda (env ret)
      (pproc env
	     (lambda (pred-value)
	       (if [mi::true? pred-value]
		   (cproc env ret)
		   (aproc env ret)))))))

(define (mi::analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env ret)
      (a env
	 (lambda (a-value ret)
	   (b env ret)))))
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
    (lambda (env ret)
      (vproc env
	     (lambda (val)
	       (mi::define-variable! var val env)
	       (ret 'ok))))))

(define (mi::analyze-assignment exp)
  (let ([var (mi::assignment-variable exp)]
	(vproc (mi::analyze (mi::assignment-value exp))))
    (lambda (env ret)
      (vproc env
	     (lambda (val)
	       (let ([old-value
		      (mi::lookup-variable-value var env)])
		 (mi::set-variable-value! var val env)
		 (ret 'ok)))))))

;;; TODO: simplify this now that it can't fail?
(define (mi::analyze-application exp)
  (let ([fproc (mi::analyze (mi::operator exp))]
	[aprocs (map mi::analyze (mi::operands exp))])
    (lambda (env ret)
      (fproc env
	     (lambda (proc)
	       (mi::get-args aprocs
			     env
			     (lambda (args)
			       (mi::execute-application
				proc args ret))))))))

(define (mi::get-args aprocs env ret)
  ;; for use in `mi::analyze-application`
  (if [null? aprocs]
      (ret '())
      ((car aprocs)
       env
       (lambda (arg)
	 (mi::get-args (cdr aprocs)
		       env
		       (lambda (args)
			 (ret (cons arg args))))))))

(define (mi::execute-application proc args ret)
  (cond ([mi::primitive-procedure? proc]
	 (ret (mi::apply-primitive-procedure proc args)))
	([mi::compound-procedure? proc]
	 ((mi::procedure-body proc)
	  (mi::extend-environment (mi::procedure-parameters proc)
				  args
				  (mi::procedure-environment proc))
	  ret))
	(else
	 (error 'mi::execute-application "unknown procedure type" proc))))

(define mi::*input-prompt*
  ";;; Cont-Eval input: ")
(define mi::*output-prompt**
  ";;; Cont-Eval output: ")

(define (mi::cont-eval exp env ret)
  ;; similar to ambeval, but only with a singular continuation
  ((mi::analyze exp) env ret))

(define (mi::driver-loop)
  ;; modified to evaluate and print the value (last continuation is
  ;; `mi::user-print`)
  (mi::prompt-for-input mi::*input-prompt*)
  (let ([input (read)])
    (mi::cont-eval input
		   mi::*global-environment*
		   (lambda (val)
		     (mi::user-print val)
		     (mi::driver-loop)))))

;;; FINISHED SIMPLE EXAMPLE

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

	;; this is new
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
    (lambda (env ret)
      (fproc env
	     (lambda (proc)
	       (mi::execute-application
		proc
		(list (list 'primitive ret))
		ret))))))

;;; TODO: examples of call/cc
;; (call/cc (lambda (r1) (call/cc (lambda (r2) (r1 (r2 (r1 3)))))))
;; (+ 2 (call/cc (lambda (ret) (ret 4) 5)))
;; (+ 2 (call/cc (lambda (ret) 5)))

;;; TODO: multiple continuations (optional error continuation)
