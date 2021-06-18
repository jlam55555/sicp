;;; notes from SICP 3.2: The Environment Model of Evaluation
(load "../utils/utils.scm")

;;; this is a short section without any new code, just the definitions
;;; of environments and worked examples of how they work
;;; 
;;; in the substitution model, we evaluated an expression (procedure) by
;;; substituting the formal arguments into the body of the procedure
;;;
;;; Now, in the environment model, we envision variables as designating a
;;; "place" in which values can be stored. These places are contained in
;;; structures we call "environments."
;;; 
;;; I don't think the description of environments and frames here is very
;;; easy to understand. We have:
;;; - environment: hierarchical set of frames
;;; - frame: table of variable bindings, and a reference to the enclosing
;;;   environment (enclosing frame)
;;; - each environment has an enclosing frame, except for the global frame
;;; - the value of a variable w.r.t. an environment is the value given by
;;;   the closest frame (hierarchically) in the environment, if it is bound;
;;;   if it is not bound in a parent frame in the environment, then it is free
;;;   (unbound) in the environment
;;;
;;; note the terminology:
;;; - value of a variable *w.r.t.* an environment
;;; - when they say "first frame," it means first frame starting from the said
;;;   environment and traveling upwards to the global environment
;;; - a name that is bound in a child environment with the same name in a
;;;   predecessor is said to *shadow* the predecessor's binding
;;;
;;; now I finally understand what a "frame" in the debugger means -- kind of
;;; like a stack frame, but for each lexical block (environment frame) rather
;;; than a function
;;;
;;; each procedure is now a pair of the body of the function as well as the
;;; enclosing environment
;;;
;;; creating a procedure creates a new binding within the enclosing frame,
;;; and associates that new object with the function body and the enclosing
;;; frame
;;;
;;; evaluating a procedure involves creating a new environment that binds
;;; the parameters to the values of its arguments, and evaluate the body
;;; within this new frame
