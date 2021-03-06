(in-package :cl-association-rules)

(defun set-equal-p (set-1 set-2 test)
  "Returns t if two sets of elements are equal"
  (unless (= (length set-1) (length set-2))
    (return-from set-equal-p nil))

  (loop for el in set-1
    do (unless (member el set-2 :test test)
        (return-from set-equal-p nil)))
  t)

(defun get-all-items (dataset test)
  "From a dataset, return the unique items as a list."
  (let ((item-set))
    (loop for transaction in dataset
      do (loop for item in transaction
            do (unless (member item item-set :test test)
                  (push item item-set))))

    (mapcar #'list (nreverse item-set))))

(defun get-frequent-items (candidate-set min-support dataset test frequent-items-hash)
  "From a candidate set and support threshold, return
  the items that are frequent and non-frequent in the
  dataset."
  (labels ((%candidate-in-transaction-p (candidate transaction test)
            (loop for item in candidate
              do (unless (member item transaction :test test)
                  (return-from %candidate-in-transaction-p nil)))
            t))

    (let ((count (make-hash-table :size (length candidate-set) :test #'equalp)))
      (loop for transaction in dataset
        do (loop for candidate in candidate-set
                 for candidate-in-count = (gethash candidate count)
              do (if (%candidate-in-transaction-p candidate transaction test)
                  (if candidate-in-count
                      (incf (gethash candidate count))
                      (setf (gethash candidate count) 1))
                  (unless candidate-in-count
                    (setf (gethash candidate count) 0)))))

      (let ((frequent-items)
            (non-frequent-items))
        (maphash #'(lambda (k v)
                    (if (< v min-support)
                        (push k non-frequent-items)
                        (progn
                          (setf (gethash k frequent-items-hash) v)
                          (push k frequent-items))))
                  count)

        (values frequent-items non-frequent-items)))))

(defun generate-candidates (frequent-items non-frequent-items all-items test)
  "Generates the next candidate set, from the previous frequent and
  non-frequent itemsets."
  (labels ((%is-set-contained (small big)
            (loop for el in small
              do (unless (member el big :test test)
                  (return-from %is-set-contained nil)))
            t)
           (%can-be-pruned (non-frequent-items new-candidate)
            (loop for non-frequent in non-frequent-items
              do (when (%is-set-contained non-frequent new-candidate)
                       (return-from %can-be-pruned t)))
            nil))

    (let ((candidates))
      (loop for f-item in frequent-items
        do (loop for item in all-items
                 for new-candidate = (append f-item item)
            do (unless (or
                        (member (first item) f-item :test test) ;; item already in the set
                        (member new-candidate candidates :test #'(lambda (set-1 set-2)
                                                                  (set-equal-p set-1
                                                                               set-2
                                                                               test))) ;; new candidate already in candidates
                        (%can-be-pruned non-frequent-items new-candidate)) ;; new candidate can be pruned
                (push new-candidate candidates))))

      candidates)))

(defun generate-rules (frequent-itemsets min-confidence num-transactions test)
  "From the frequent itemsets found by apriori,
  generate association rules for a given min-confidence."
  (labels ((subsets (set &optional (so-far nil) (output nil))
            (unless set
              (unless (null so-far) (push so-far output))
              (return-from subsets output))

            (setf output (subsets (rest set) (cons (first set) so-far) output))
            (setf output (subsets (rest set) so-far output)))

           (set-diff (total subset test)
            (if (null total)
                nil
                (if (member (first total) subset :test test)
                    (set-diff (rest total) subset test)
                    (cons (first total) (set-diff (rest total) subset test))))))

    (let ((mined-rules))
      (maphash #'(lambda (itemset support)
                  (loop for subset in (subsets itemset)
                        for set-diff = (set-diff itemset subset test)
                        for confidence = (or (ignore-errors
                                              (/ support
                                                 (gethash set-diff frequent-itemsets)))
                                             0)
                    do (when (>= confidence min-confidence)
                        (push (make-rule :pretuple set-diff
                                         :posttuple subset
                                         :support (/ support num-transactions)
                                         :confidence confidence)
                              mined-rules))))

        frequent-itemsets)
      mined-rules)))

(defun apriori (dataset &key (support 0.17) (confidence 0.68) (test #'equalp))
  "Calculates the association rules in the dataset using the apriori
  algorithm. Expects a dataset of the form
  ((1 2 3 4)
   (3 2 7 9)
   (9)
   (2 3 8)
   (2 0)),
   where each line is a transaction.
   Returns a list of the mined rules."
   (check-type support number "a number")
   (check-type confidence number "a number")
   (let* ((items (get-all-items dataset test))
          (min-support (ceiling (* (length dataset) support)))
          (frequent-itemsets (make-hash-table :test #'equalp)))

    (multiple-value-bind (frequent-items non-frequent-items)
      (get-frequent-items items min-support dataset test frequent-itemsets)

      (loop while (not (null frequent-items))
        for candidates = (generate-candidates frequent-items
                                              non-frequent-items
                                              items
                                              test)
        do
           (multiple-value-setq (frequent-items non-frequent-items)
            (get-frequent-items candidates min-support dataset test frequent-itemsets)))

      (generate-rules frequent-itemsets confidence (length dataset) test))))
