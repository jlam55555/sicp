;;; notes from sicp 3.1: Assignment and Local State
(load "../utils/utils.scm")


;;; note that our functions are now stateful, and thus not pure;
;;; before, in the substitution model, every call to a function with the
;;; same set of parameters would give the same result (except where we
;;; used get/put (stateful functions) or used random functions (PRNGs are
;;; stateful, truly-random RNGs are not stateful but aren't substitutive either)
;;;
;;; other things to note:
;;; - set! and other functions that modify variables or data structures end
;;;   with an exclamation mark
;;; - set! should only be used for its effect, not its return value; the return
;;;   value is UB
;;; - we use the begin special form here, which allows for a sequence of
;;;   expressions where normally only one is allowed; the value of the begin
;;;   expression is the value of the last expression in the begin expression;
;;;   there is an implicit begin statement in the body of a lambda and some
;;;   block statements (consequents of cond, when, unless)

(define balance
  ;; begin with 100
  100)

(define (withdraw amount)
  ;; stateful function to remove amount from balance, if possible
  (if [>= balance amount]
      [begin
	(set! balance (- balance amount))
	balance]
      "Insufficient funds"))

(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 15)

(define new-withdraw
  ;; make balance scoped within new-withdraw so that no other procedure
  ;; can modify it; we have created a new *environment* inside which the local
  ;; variable balance is bound
  (let ([balance 100])
    (lambda (amount)
      (if [>= balance amount]
	  [begin (set! balance (- balance amount))
		 balance]
	  "Insufficient funds"))))

(new-withdraw 25)
(new-withdraw 25)
(new-withdraw 60)
(new-withdraw 15)

(define (make-withdraw balance)
  ;; now we use the function body closure as the environment; this is very
  ;; similar to how Javascript objects are really just function closures
  (lambda (amount)
    (if [>= balance amount]
	[begin (set! balance (- balance amount))
	       balance]
	"Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
(W2 70)
(W2 40)
(W1 40)

(define (make-account balance)
  ;; same idea as above, but now we can deposit as well as withdraw; recall
  ;; the "message-passing" object model from 2.4
  
  (define (withdraw amount)
    (if [>= balance amount]
	[begin
	  (set! balance (- balance amount))
	  balance]
	"Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount)))

  ;; return dispatch lambda
  (lambda (m)
    (cond ([eq? m 'withdraw] withdraw)
	  ([eq? m 'deposit] deposit)
	  (#t (error 'make-account "unknown request" m)))))

(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

;;; 3.1.2: The Benefits of Introducing Assignment

;;; these are just to make the following definition compile, but this is
;;; clearly not a working RNG -- use builtin random procedure
(define random-init -1)
(define (rand-update x) -1)

(define my-rand
  ;; stateful RNG using initial value random-init and update fn rand-update
  (let ([x random-init])
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  ;; a monte-carlo simulation for estimating pi by the fact that the probability
  ;; that two integers chosen at random will be coprime is 6/pi^2

  (define (cesaro-test)
    ;; check if two random numbers are coprime
    (= (gcd (random 1000) (random 1000)) 1))

  (define (monte-carlo trials experiment)
    ;; monte carlo method
    (let iter ([trials-remaining trials]
	       [trials-passed 0])
      (if [zero? trials-remaining]
	  (/ trials-passed trials)
	  (iter (1- trials-remaining)
		(+ trials-passed
		   (if [experiment] 1 0))))))

  ;; driver
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(estimate-pi 1e5)

;;; the same example, but keeping track of x and using rand-update (internal
;;; state of the rand procedure) is a lot messier and betrays modularity,
;;; because now the role of managing state is bubbled up to both the cesaro
;;; and monte-carlo methods. I won't reproduce the textbook's example here

;;; 3.1.3: The Costs of Introducing State

;;; "So long as we do not use assignments, two evalueations of the same
;;; procedure with the same arguments will produce the same result, so that
;;; procedures can be viewed as computing mathematical functions. Programming
;;; without the use of assignments, as we did throughout the first two chapters
;;; of this book, is accordingly known as *functional programming*."

;;; In other words, before we had symbols that were bound to a single value,
;;; but now symbols are names for a region of memory, whose contents may change
;;; over time, and the substitution model cannot distinguish between separate
;;; invocations of a stateful procedure.

;;; "A language that supports the concept that 'equals can be substituted for
;;; equals' in an expression without changing the value of the expression is
;;; said to be *referentially transparent*. Referential transparency is violated
;;; when we include set! in our computer language. This makes it tricky to
;;; determine when we can simplify expressions by substituting equivalent
;;; expressions. Consequently, reasoning about programs that use assignment
;;; becomes drastically more difficult."

;;; SICP proposes the following example. In this first case, peter-acc and
;;; paul-acc are separate entities with separate state, even if they are the
;;; created in the same way:
(define peter-acc (make-account 100))
(define paul-acc (make-account 100))

;;; versus here: peter-acc and paul-acc refer to the same place in memory, as
;;; if they were a single connected bank account (think like Java/C++ object
;;; references). The footnote notes that this is called aliasing, and it can
;;; make life harder for compiler writers (because they have to keep track of
;;; all aliases of an object to do things like data flow analysis and GC) and
;;; programmers (because of "side-effect bugs")
(define peter-acc (make-account 100))
(define paul-acc peter-acc)

;;; side note (related to footnote 9): the assignment operator set! (like the
;;; walrus operator, which is another convention) makes more sense than the
;;; C convention of using =, because mathematically, both sides are most often
;;; not equal.
;;; 
;;; Abelson and Sussman also note that if a compound data structure is
;;; immutable (i.e., it never changes, then we can say two objects are
;;; "equal" iff their parts are all the same. Whereas, we can usually only say
;;; two objects are the same if they are references to the same place in memory.
;;; 
;;; "Programming that makes extensive use of assignment is known as
;;; *imperative programming*."
;;; 
;;; The text also provides an example of a broken factorial implementation
;;; that uses a mutable state variables rather than immutable state with let
;;; statements. The danger is that, if variables change, and if the values of
;;; some variables depend on the value of other (changing) variables, then
;;; we have to think carefully about the order of assignments (which the
;;; authors believe to obscure the important parts of programming).
;;;
;;; Footnote 11 is especially relevant to the concerns about functional
;;; programming and how many students are taught imperative programming.
