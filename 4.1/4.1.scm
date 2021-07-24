;;; SICP 4.1: The Metacircular Evaluator
;;; The evaluator core comprises two functions: eval (evaluates a structured
;;; expression, including special forms, and handles the environment) and
;;; apply (applicative-order function application)

;;; All procedures related to the interpreter will be prefixed with 'mi::' for
;;; 'my interpreter'. This can be done automatically using Chez modules, but
;;; its integration with geiser seems to be a bit buggy, so I'll just type it
;;; out every time. This way I can make sure I don't accidentally use any of
;;; the builtins and don't accidentally forget to implement a function that is
;;; shadows a builtin name.

;;; 4.1.1: The Core of the Evaluator

(define (mi::eval exp env)
  ;; evaluates an expression in an environment
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
	([mi::application? exp]
	 (mi::apply (mi::eval (mi::operator exp) env)
		    (mi::list-of-values (mi::operands exp) env)))
	(else
	 (error 'mi::eval "unknown expression type" exp))))

(define (mi::apply procedure arguments)
  ;; applies a procedure
  (cond ([mi::primitive-procedure? procedure]
	 (mi::apply-primitive-procedure procedure arguments))
	([mi::compound-procedure? procedure]
	 (mi::eval-sequence
	  (mi::procedure-body procedure)
	  (mi::extend-environment
	   (mi::procedure-parameters procedure)
	   arguments
	   (mi::procedure-environment procedure))))))

(define (mi::list-of-values exps env)
  ;; converts a list of symbols to a list of evaluated values
  (map (lambda (exp) (mi::eval exp env)) exps))

(define (mi::eval-if exp env)
  ;; evals an `if` expression with proper control flow
  (if [mi::true? (mi::eval (mi::if-predicate exp) env)]
      (mi::eval (mi::if-consequent exp) env)
      (mi::eval (mi::if-alternative exp) env)))

(define (mi::eval-sequence exps env)
  ;; evaluates a sequence of expressions in the containing scope
  (cond ([mi::last-exp? exps]
	 (mi::eval (mi::first-exp exps) env))
	(#t
	 (mi::eval (mi::first-exp exps) env)
	 (mi::eval-sequence (mi::rest-exps exps) env))))

(define (mi::eval-assignment exp env)
  ;; performs an assignment in the given environment
  (mi::set-variable-value!
   (mi::assignment-variable exp)
   (mi::eval (mi::assignment-value exp) env)
   env)
  'ok)

(define (mi::eval-definition exp env)
  ;; performs a declaration in the given environment
  (mi::define-variable!
   (mi::definition-variable exp)
   (mi::eval (mi::definition-value exp) env)
   env)
  'ok)

;;; 4.1.2: Representing Expressions
;;; Here we define the syntax of the language; this is a layer of abstraction
;;; on top of the core evaluator, and we can change the syntax of the language
;;; by changing what occurs here.

(define (mi::self-evaluating? exp)
  ;; the only self-evaluating items are numbers and strings
  (or (number? exp)
      (string? exp)))

(define (mi::variable? exp)
  ;; variables are represented by symbols
  (symbol? exp))

(define (mi::quoted? exp)
  ;; quotations have the form `(quote <text-of-quotation>)` (note that the
  ;; literal quotation mark (') gets expanded automatically before the
  ;; interpreter interprets it
  (mi::tagged-list? exp 'quote))

(define (mi::text-of-quotation exp)
  ;; extract quoted text
  (cadr exp))

(define (mi::tagged-list? exp tag)
  ;; check if the first element of a syntax list is the tag
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (mi::assignment? exp)
  ;; check if expression is an assignment
  (mi::tagged-list? exp 'set!))

(define (mi::assignment-variable exp)
  ;; extract variable from assignment expression
  (cadr exp))

(define (mi::assignment-value exp)
  ;; extract value from assignment expression
  (caddr exp))

(define (mi::definition? exp)
  ;; check if an expression is a definition
  (mi::tagged-list? exp 'define))

(define (mi::definition-variable exp)
  ;; extract variable name from assignment expression; note that this also
  ;; handles the function shorthand syntax
  (if [symbol? (cadr exp)]
      (cadr exp)
      (caadr exp)))

(define (mi::definition-value exp)
  ;; extract variable value from assignment expression; note that this also
  ;; handles the function shorthand syntax, transforming the value into a lambda
  ;; expression
  (if [symbol? (cadr exp)]
      (caddr exp)
      (mi::make-lambda (cdadr exp)	; formal paramaters
		       (cddr exp))))	; body

(define (mi::lambda? exp)
  ;; check if an expression is a lambda
  (mi::tagged-list? exp 'lambda))

(define (mi::lambda-parameters exp)
  ;; extract formal parameters from a lambda expression
  (cadr exp))

(define (mi::lambda-body exp)
  ;; extract body from a lambda expression
  (cddr exp))

(define (mi::make-lambda parameters body)
  ;; constructor for lambda expressions (for function shorthand define syntax)
  (cons 'lambda (cons parameters body)))

(define (mi::if? exp)
  ;; check if an expression is an `if` expression
  (mi::tagged-list? exp 'if))

(define (mi::if-predicate exp)
  ;; extract predicate from an `if` expression
  (cadr exp))

(define (mi::if-consequent exp)
  ;; extract consequent from an `if` expression
  (caddr exp))

(define (mi::if-alternative exp)
  ;; extract alternative from an `if` statement, else `#f`, just like an
  ;; `and` expression (Chez Scheme implements it differently, returning the
  ;; `(void)` value if the alternative branch is missing)
  (and (not (null? (cdddr exp)))
       (cadddr exp)))

(define (mi::make-if predicate consequent alternative)
  ;; constructor for an `if` expression, to be used by `cond->if`
  (list 'if predicate consequent alternative))

(define (mi::begin? exp)
  ;; check if an expression is a `begin` block
  (mi::tagged-list? exp 'begin))

(define (mi::begin-actions exp)
  ;; extract the list of expressions from a `begin` block
  (cdr exp))

(define (mi::last-exp? seq)
  ;; check if an expression is the last in a sequence of expressions
  (null? (cdr seq)))

(define (mi::first-exp seq)
  ;; gets the first expression from a list of expressions
  (car seq))

(define (mi::rest-seq seq)
  ;; gets the rest of the expressions from a list of expression
  (cdr seq))

(define (mi::seq->exp seq)
  ;; transforms a sequence of expressions into a single expression, using
  ;; a `begin` block as necessary; used when converting `cond` to `if` blocks
  (cond ([null? seq] seq)
	([mi::last-exp? seq]
	 (mi::first-exp seq))
	(#t
	 (mi::make-begin seq))))

(define (mi::make-begin seq)
  ;; constructor for a `begin` block; used by `mi::seq->exp`
  (cons 'begin seq))

(define (mi::application? exp)
  ;; checks if an expression is a function application
  ;; (assumes all other special forms have already been checked and ruled out)
  (pair? exp))

(define (mi::operator exp)
  ;; gets the operator/function from a function application
  (car exp))

(define (mi::operands exp)
  ;; gets the operands from a function application
  (cdr exp))

(define (mi::no-operands? ops)
  ;; checks if an operand list is empty
  (null? ops))

(define (mi::first-operand ops)
  ;; gets the first operand from an operand list
  (car ops))

(define (mi::rest-operands ops)
  ;; gets the rest of the operands from an operand list
  (cdr ops))

;;; Derived expressions
;;; note that `cond` expressions are "derived expressions," which are
;;; syntactic sugar for simpler forms. We can generate them using
;;; syntactic transformations called "macros." Other derived expressions
;;; include `and`, `or`, `when`, `let`, and the `define` shorthand syntax.
;;; Interpreting these derived forms is probably much simpler if we have
;;; a syntax transformer in our interpreter.

(define (mi::cond? exp)
  ;; check if an expression is a `cond` expression
  (mi::tagged-list? exp 'cond))

(define (mi::cond-clauses exp)
  ;; extract the clauses from a `cond` expression
  (cdr exp))

(define (mi::cond-else-clause? clause)
  ;; check if a clause is an else clause
  (eq? (mi::cond-predicate clause) 'else))

(define (mi::cond-predicate clause)
  ;; extract the predicate from a `cond` clause
  (car clause))

(define (mi::cond-actions clause)
  ;; extract the actions from a `cond` clause
  (cdr clause))

(define (mi::cond->if exp)
  ;; rewrite `cond` expression in terms of `if` expressions
  (mi::expand-clauses (mi::cond-clauses exp)))

(define (mi::expand-clauses clauses)
  ;; expand the clauses of a `cond` expression into equivalent `if` expressions
  (if [null? clause]
      #f				; no else clause
      (let ([first (car clauses)]
	    [rest (cdr clauses)])
	(if [mi::cond-else-clause? first]
	    (if [null? rest]
		(mi::seq->exp (mi::cond-actions first))
		(error 'mi::expand-clauses "else clause isn't last" clauses))
	    (mi::make-if (mi::cond-predicate first)
			 (mi::seq->exp (mi::cond-predicate first))
			 (mi::expand-clauses rest))))))

;;; 4.1.3: Evaluator Data Structures

;;; testing predicates

(define (mi::true? x)
  ;; defines a truthy value (for use in a condition); anything that is not
  ;; the `#f` value
  (not (eq? x #f)))

(define (mi::false? x)
  ;; defines the only falsy value (for use in a condition) to be exactly `#f`
  (eq? x #f))

;;; representing procedures
;;; assume that we have the primitives `mi::apply-primitive-procedure` and
;;; `mi::primitive-procedure?`, which will be defined later

(define (mi::make-procedure parameters body env)
  ;; construct the syntax for a compound procedure
  (list 'procedure parameters body env))

(define (mi::compound-procedure? p)
  ;; check if a procedure is a compound procedure (if the procedure is not
  ;; directly a function value)
  (mi::tagged-list? p 'procedure))

(define (mi::procedure-parameters p)
  ;; extract the parameters from a compound procedure
  (cadr p))

(define (mi::procedure-body p)
  ;; extract the body from a compound procedure
  (caddr p))

(define (mi::procedure-environment p)
  ;; extract the environment from a compound procedure
  (cadddr p))

;;; operations on environments
;;; we represent the environment as a linked list (stack) of frames; the
;;; enclosing environment of a frame is the `cdr` of the list. The base
;;; environment is simply the empty list. Each frame is a table of variable
;;; bindings: a list of symbols and another list of their corresponding values.

(define (mi::enclosing-environment env)
  ;; gets the enclosing environment of env
  (cdr env))

(define (mi::first-frame env)
  ;; gets the innermost frame of env
  (car env))

(define mi::*empty-environment*
  ;; the empty environment
  '())

(define (mi::make-frame variables values)
  ;; create the frame table; the frame comprises two lists: the list of symbols
  ;; and the list of corresponding values
  (cons variables values))

(define (mi::frame-variables frame)
  ;; get the list of symbols from the frame
  (car frame))

(define (mi::frame-values frame)
  ;; get the list of values from the frame
  (cdr frame))

(define (mi::add-binding-to-frame! var val frame)
  ;; bind a new value to the current frame
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (mi::extend-environment vars vals base-env)
  ;; create a new environment with a new frame on top of base-env
  (if [= (length vars) (length vals)]
      (cons (mi::make-frame vars vals) base-env)
      (if [< (length vars) (length vals)]
	  (error 'mi::extend-environment "too many arguments supplied"
		 vars vals)
	  (error 'mi::extend-environment "too few arguments supplied"
		 vars vals))))

(define (mi::lookup-variable-value var env)
  ;; lookup a symbol in the environment, starting from the current frame and
  ;; moving upwards in the stack, until the variable is found or the empty
  ;; environment is reached
  (define (env-loop env)
    (define (scan vars vals)
      (cond ([null? vars]
	     (env-loop (mi::enclosing-environment env)))
	    ([eq? var (car vars)]
	     (car vals))
	    (#t
	     (scan (cdr vars) (cdr vals)))))
    (if [eq? env mi::*empty-environment*]
	(error 'mi::lookup-variable-value "unbound variable" var)
	(let ([frame (mi::first-frame env)])
	  (scan (mi::frame-variables frame)
		(mi::frame-values frame)))))
  (env-loop env))

(define (mi::set-variable-value! var val env)
  ;; sets the value of a symbol in the environment if found; if not
  ;; found, error
  (define (env-loop env)
    (define (scan vars vals)
      (cond ([null? vars]
	     (env-loop (mi::enclosing-environment env)))
	    ([eq? var (car vars)]
	     (set-car! vals val))
	    (#t
	     (scan (cdr vars) (cdr vals)))))
    (if [eq? env mi::*empty-environment*]
	(error 'mi::set-variable-value! "unbound variable" var)
	(let ([frame (mi::first-frame env)])
	  (scan (mi::frame-variables frame)
		(mi::frame-values frame)))))
  (env-loop env))

(define (mi::define-variable! var val env)
  ;; define a variable: change the binding if it exists in the first frame;
  ;; if not found, create a new binding (in the first frame)
  (let ([frame (mi::first-frame env)])
    (define (scan vars vals)
      (cond ([null? vars]
	     (mi::add-binding-to-frame! var val frame))
	    ([eq? var (car vals)]
	     (set-car! vals val))
	    (#t
	     (scan (cdr vars) (cdr vals)))))
    (scan (mi::frame-variables frame)
	  (mi::frame-values frame))))

;;; possible extensions from the exercises: change the way the frames are
;;; implemented (an even better way is to use the hashtable implementation in
;;; the standard library); and create a function to unbind a variable

;;; 4.1.4: Running the Evaluator as a Program

(define (mi::setup-environment)
  ;; a thunk to be called to set up the global environment with primitive
  ;; functions (functions that are not defined by the user)
  (let ([initial-env
	 (mi::extend-environment
	  (mi::*primitive-procedure-names*)
	  (mi::*primitive-procedure-objects*)
	  mi::*empty-environment*)])
    (mi::define-variable! 'true #t initial-env)
    (mi::define-variable! 'false #f initial-env)
    initial-env))

(define (mi::primitive-procedure? proc)
  ;; check if a function is a primitive procedure
  (mi::tagged-list? proc 'primitive))

(define (mi::primitive-implementation proc)
  ;; extract the implementation of a primitive procedure
  (cadr proc))

(define mi::*primitive-procedures*
  ;; list of primitive procedures and their implementations
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '+ +)
	;; can add more primitives here
	))

(define (mi::*primitive-procedure-names*)
  ;; list of the names of the primitive procedures
  (map car mi::*primitive-procedures*))

(define (mi::*primitive-procedure-objects*)
  ;; list of the values of the primitive procedures
  (map (lambda (proc) (list 'primitive (cadr proc)))
       mi::*primitive-procedures*))

(define (mi::apply-primitive-procedure proc args)
  ;; apply a primitive procedure; note that here we use the builtin `apply`
  ;; method to run the primitives
  (apply (mi::primitive-implementation proc) args))

(define mi::*input-prompt*
  ;; repl input prompt
  ";;; M-Eval input: ")

(define mi::*output-prompt*
  ;; repl output prompt
  ";;; M-Eval output: ")

(define (mi::driver-loop)
  ;; repl loop
  (mi::prompt-for-input mi::*input-prompt*)
  (let ([input (read)])
    (let ([output (mi::eval input mi::*global-environment*)])
      (mi::announce-output mi::*output-prompt*)
      (mi::user-print output)))
  (mi::driver-loop))

(define (mi::prompt-for-input string)
  ;; show repl input prompt
  (format #t "\n\n~a\n" string))

(define (mi::announce-output string)
  ;; show repl output prompt
  (format #t "\n~a\n" string))

(define (mi::user-print object)
  ;; show eval output to user
  (if [mi::compound-procedure? object]
      (display (list 'compound-procedure
		     (mi::procedure-parameters object)
		     (mi::procedure-body object)
		     '<procedure-env>))
      (display object)))

(define mi::*global-environment*
  ;; set up the global environment
  (mi::setup-environment))

;;; 4.1.7. Separating Syntactic Analsys from Execution
;;; We can compile our code to an intermediate form during analysis and prevent
;;; code from being analyzed each time it is executed.

(define (mi::eval exp env)
  ;; rewriting eval to only parse the expression once
  ((mi::analyze exp) env))

(define (mi::analyze exp)
  ;; code analysis only happens once
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
	([mi::application? exp]
	 (mi::analyze-application exp))
	(#t
	 (error 'mi::analyze "unknown expression type" exp))))

(define (mi::analyze-self-evaluating exp)
  ;; syntactic analysis of a self-evaluating expression
  (lambda (env) exp))

(define (mi::analyze-quoted exp)
  ;; syntactic analysis of a quoted expression
  (let ([qval (mi::text-of-quotation exp)])
    (lambda (env) qval)))

(define (mi::analyze-variable exp)
  ;; syntactic analysis of a variable
  (lambda (env) (mi::lookup-variable-value exp env)))

(define (mi::analyze-assignment exp)
  ;; syntactic analysis of an assignment expression
  (let ([var (mi::assignment-variable exp)]
	[vproc (mi::analyze (mi::assignment-value exp))])
    (lambda (env)
      (mi::set-variable-value! var (vproc env) exp)
      'ok)))

(define (mi::analyze-definition exp)
  ;; syntactic analysis of a definition expression
  (let ([var (mi::definition-variable exp)]
	[vproc (mi::analyze (mi::definition-value exp))])
    (lambda (env)
      (mi::define-variable! var (vproc env) env)
      'ok)))

(define (mi::analyze-if exp)
  ;; syntactic analysis of an if expression
  (let ([pproc (mi::analyze (mi::if-predicate exp))]
	[cproc (mi::analyze (mi::if-consequent exp))]
	[aproc (mi::analyze (mi::if-alternative exp))])
    (lambda (env)
      (if (mi::true? (pproc env))
	  (cproc env)
	  (aproc env)))))

(define (mi::analyze-lambda exp)
  ;; syntactic analysis of a lambda expression
  (let ([vars (mi::lambda-parameters exp)]
	[bproc (mi::analyze-sequence (mi::lambda-body exp))])
    (lambda (env) (mi::make-procedure vars bproc env))))

(define (mi::analyze-sequence exps)
  ;; syntactic analysis of a sequence of expressions
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if [null? rest-procs]
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ([procs (map mi::analyze exps)])
    (if [null? procs]
	(error 'mi::analyze-sequence "empty sequence"))
    (loop (car procs) (cdr procs))))

(define (mi::analyze-application exp)
  ;; syntactic analysis of a function application
  (let ([fproc (mi::analyze (mi::operator exp))]
	(aprocs (map mi::analyze (mi::operands exp))))
    (lambda (env)
      (mi::execute-application (fproc env)
			       (map (lambda (aproc) (aproc env))
				    aprocs)))))

(define (mi::execute-application proc args)
  ;; execution of an analyzed expression
  (cond ([mi::primitive-procedure? proc]
	 (mi::apply-primitive-procedure proc args))
	([mi::compound-procedure? proc]
	 ((mi::procedure-body proc)
	  (mi::extend-environment
	   (mi::procedure-parameters proc)
	   args
	   (mi::procedure-environment proc))))
	(#t
	 (error 'mi::execute-application "unknown procedure type" proc))))
