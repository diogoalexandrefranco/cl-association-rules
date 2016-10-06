(in-package :cl-user)

(defpackage :cl-association-rules
  (:use :common-lisp)
  (:nicknames #:rules)
  (:shadow)
  (:export
    #:apriori
    #:make-rule
    #:rule-pretuple
    #:rule-posttuple
    #:rule-support
    #:rule-confidence))
