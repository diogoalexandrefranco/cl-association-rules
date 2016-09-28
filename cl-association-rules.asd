(defsystem #:cl-association-rules
           :name "cl-association-rules"
           :version "0.0.1"
           :description "An implementation of the apriori algorithm to mine association rules in Common Lisp."
           :author "Diogo Franco"
           :license "MIT"
           :serial t
           :components ((:file "package")
                        (:file "cl-association-rules")))

(defsystem #:cl-association-rules-tests
           :name "cl-association-rules-tests"
           :version "0.0.1"
           :description "The tests for the cl-association-rules system."
           :author "Diogo Franco"
           :depends-on (#:cl-association-rules #:prove)
           :components ((:file "tests")))
