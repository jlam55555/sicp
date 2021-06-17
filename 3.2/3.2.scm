;;; notes from SICP 3.2: The Environment Model of Evaluation
(load "../utils/utils.scm")

;;; in the substitution model, we evaluated an expression (procedure) by
;;; substituting the formal arguments into the body of the procedure
;;;
;;; Now, in the environment model, we envision variables as designating a
;;; "place" in which values can be stored. These places are contained in
;;; structures we call "environments."


