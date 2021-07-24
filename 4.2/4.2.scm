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
(import "../4.1/4.1.scm")

;;; 4.2.2: An Interpreter with Lazy Evaluation

