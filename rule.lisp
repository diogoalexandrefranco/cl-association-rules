;;
;; This file contains the code for defining
;; the rule structure.
;;

(in-package :cl-association-rules)

(defstruct (rule
            (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (if (or (rule-pretuple struct)
                        (rule-posttuple struct)
                        (rule-support struct)
                        (rule-confidence struct))
                  (format stream "~a => ~a. Support is ~a and confidence is ~a.~%"
                          (rule-pretuple struct)
                          (rule-posttuple struct)
                          (rule-support struct)
                          (rule-confidence struct))
                  (format stream "New empty rule.~%")))))

  pretuple posttuple support confidence)
