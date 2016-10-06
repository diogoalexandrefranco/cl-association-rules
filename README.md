# cl-association-rules
This project aims to implement the most well known strategies and utilities when
mining association rules from a dataset of transactions. For now, only the apriori algorithm is implemented.
It works at least on sbcl, ecl, ccl, abcl and clisp.

* [How do i use it?](#how-do-i-use-it)
* [Example](#example)
* [API](#api)
* [Contributing](#contributing)
* [License](#license)

## How do i use it?
This section assumes you use quicklisp. If you don't, you should! Download and
learn about it [here](https://www.quicklisp.org/beta/).

Once you have quicklisp loaded, simply do:  
```lisp
(ql:quickload :cl-association-rules)
```
And it's all up and running. To run the tests do:
```lisp
(ql:quickload :cl-association-rules-tests)
```
Please report if any tests fail in your Common Lisp implementation.

To make it easier to write, the cl-association-rules package also has the
nickname "rules".

## Example
```lisp
> (ql:quickload :cl-association-rules)
(:CL-ASSOCIATION-RULES)  

> (rules:apriori '((1 2 3 4) ;; each of these lines is a transaction
                   (1 2 4)
                   (1 2)
                   (2 3 4)
                   (2 3)
                   (3 4)
                   (2 4)))
((3) => (2). Support is 3/7 and confidence is 3/4.
 (4 1) => (2). Support is 2/7 and confidence is 1.
 (4) => (2). Support is 4/7 and confidence is 4/5.
 (1) => (2). Support is 3/7 and confidence is 1.
 (3) => (4). Support is 3/7 and confidence is 3/4.)
```

## API
#### (apriori dataset &key (support 0.17) (confidence 0.68) (test #'equalp))
Apriori calculates the association rules in "dataset" using the [apriori
algorithm](https://en.wikipedia.org/wiki/Apriori_algorithm). Expects a dataset of the form
  ((1 2 3 4)
   (3 2 7 9)
   (9)
   (2 3 8)
   (2 0)),
 where each line is a transaction. You can also costumize the support (defaults to 0.17), the confidence (defaults to 0.68) and the equality operator (defaults to the lisp "equalp" function).
 The output is a list of mined rules, where each rule is an instance of a struct with fields posttuple, pretuple, support and confidence.
 This method may signal *type-error*.
```lisp
> (defvar *mined-rules* (rules:apriori '((1 2 3 4)
                                         (1 2 3 7)
                                         (1 9)
                                         (2 10 15 4)
                                         (1 3 4 11)
                                         (15 3 1 20))))
*MINED-RULES* ;; ((2 1) => (3). Support is 1/3 and confidence is 1.
              ;;  (3 2) => (1). Support is 1/3 and confidence is 1.
              ;;  (1) => (3). Support is 2/3 and confidence is 4/5.
              ;;  (3) => (1). Support is 2/3 and confidence is 1.)

> (rules:rule-pretuple (first *MINED-RULES*))
(2 1) ;; accessing member "pretuple" of the rule struct. Other members are
      ;; "posttuple", "support" and "confidence".
```

## Contributing
If you have any suggestions, bug reports, etc, please fill in an issue
describing it. If you have the time and want to contribute, that is even better!
Submit some tests too :)

Here is what I'm thinking might make sense to implement next:
* FP-Growth Algorithm.

## License
MIT
