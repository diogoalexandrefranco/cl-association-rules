# cl-association-rules
An implementation of the apriori algorithm to mine association rules in Common Lisp.
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

## Example
```lisp
> (ql:quickload :cl-association-rules)
(:CL-ASSOCIATION-RULES)  

> (use-package :cl-association-rules)
T  

> (defparameter *num* (parse-number "-3.1e3"))
*NUM* ;; -3100.0  

> (format-number *num* :precision 3 :decimal-separator "." :order-separator ",")
"-3,100.000"
```

## API
#### (parse-number number-str &key (decimal-separator #\\.) (order-separator nil))
parse-number returns a number from a string, without using the reader (CL has
parse-integer but no equivalent for other number types). It accepts integers,
floats, fractional and scientific notations. It also accepts both chars and
one character strings for the separators. This method may signal *parse-error*.
```lisp
(parse-number "-3.1e2") ;; -310.0
(parse-number "1 234,9" :decimal-separator "," :order-separator " ") ;; 1234.9
```

#### (format-number number &key (precision 0) (decimal-separator ".") (order-separator ",")
format-number returns a string from a number. It's possible to set the precision
(decimals), and the separators as chars or strings of length one.
```lisp
(format-number 1234.326 :precision 2 :decimal-separator "," :order-separator " ") ;; "1 234,33"
```

## Contributing
If you have any suggestions, bug reports, etc, please fill in an issue
describing it. If you have the time and want to contribute, that is even better!
Submit some tests too :)

Here is what I'm thinking might make sense to implement next:

## License
MIT
