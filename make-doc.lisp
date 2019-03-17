(defparameter *head*
  "~
Just Getopt Parser
==================

**Getopt-like command-line parser for the Common Lisp language**


Introduction
------------

This Common Lisp package implements Unix Getopt command-line parser.
Package's main interface is `getopt` function which parses the command
line options and organizes them to valid options, other arguments and
unknown arguments. For full documentation on package's programming
interface see section _The Programming Interface_ down below.


Examples
--------

Example command line:

    $ some-program -d3 -f one --file=two -xyz foo --none bar -v -- -v

That command line could be parserd with the followin function call:

    (getopt '(\"-d3\" \"-f\" \"one\" \"--file=two\" \"-xyz\" \"foo\"
              \"--none\" \"bar\" \"-v\" \"--\" \"-v\")
            '((:debug #\\d :optional)
              (:file #\\f :required)
              (:file \"file\" :required)
              (:verbose #\\v))
            :options-everywhere t)

The function returns three values: (1) valid options and their
arguments, (2) other arguments and (3) unknown options:

    ((:DEBUG . \"3\") (:FILE . \"one\") (:FILE . \"two\") (:VERBOSE))
    (\"foo\" \"bar\" \"-v\")
    (#\\x #\\y #\\z \"none\")

In programs it is probably convenient to call this function through
`cl:multiple-value-bind` so that the return values are bound to
different variables:

    (multiple-value-bind (options other unknown)
        (getopt COMMAND-LINE-ARGUMENTS '(...))

      ...
      )

In practice there is probably also `cl:handler-bind` macro which handles
error conditions by printing error messages, invoking `skip-option`
restarts or transferring the program control elsewhere. Here is more
thorough example:

    (handler-bind
        ((ambiguous-option
           (lambda (condition)
             (format *error-output* \"~~A~~%\" condition)
             (invoke-restart 'skip-option)))
         (unknown-option
           (lambda (condition)
             (format *error-output* \"~~A~~%\" condition)
             (invoke-restart 'skip-option)))
         (required-argument-missing
           (lambda (condition)
             (format *error-output* \"~~A~~%\" condition)
             (exit-program :code 1)))
         (argument-not-allowed
           (lambda (condition)
             (format *error-output* \"~~A~~%\" condition)
             (exit-program :code 1))))

      (multiple-value-bind (options other unknown)
          (getopt COMMAND-LINE-ARGUMENTS '(...)
                  :prefix-match-long-options t
                  :error-on-ambiguous-option t
                  :error-on-unknown-option t
                  :error-on-argument-missing t
                  :error-on-argument-not-allowed t)

        ...
        ))


License and Source Code
-----------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

License: [Creative Commons CC0][CC0] (public domain dedication)

The source code repository:
<https://github.com/tlikonen/cl-just-getopt-parser>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/legalcode


The Programming Interface
-------------------------

")

(require 'sb-posix)
(require 'sb-introspect)

(load "just-getopt-parser.lisp")

(defun symbol-doc-type (symbol)
  (let (docs)
    (flet ((doc (symbol type key)
             (push (list symbol key (documentation symbol type)) docs)))
      (cond ((ignore-errors (macro-function symbol))
             (doc symbol 'function :macro))
            ((ignore-errors (symbol-function symbol))
             (doc symbol 'function :function)))
      (when (ignore-errors (symbol-value symbol))
        (doc symbol 'variable :variable))
      (cond ((subtypep symbol 'condition)
             (doc symbol 'type :condition))
            ((ignore-errors (find-class symbol))
             (doc symbol 'type :class))))
    docs))

(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (format stream *head*)
  (loop :with *package* := (find-package package)
        :with *print-right-margin* := 72
        :with *print-case* := :downcase
        :with symbols := (loop :for symbol
                                 :being :each :external-symbol :in package
                               :collect symbol)

        :for (symbol type doc) :in (sort (mapcan #'symbol-doc-type symbols)
                                         (lambda (l1 l2)
                                           (let ((s1 (symbol-name (first l1)))
                                                 (s2 (symbol-name (first l2)))
                                                 (t1 (symbol-name (second l1)))
                                                 (t2 (symbol-name (second l2))))
                                             (or (string-lessp t1 t2)
                                                 (and (string-equal t1 t2)
                                                      (string-lessp s1 s2))))))
        :if doc :do

          (format stream "~A" prefix)
          (case type
            (:function
             (format stream "Function: `~A`" symbol)
             (let ((ll (sb-introspect:function-lambda-list symbol)))
               (when ll
                 (format stream "~%~%The lambda list:~%~%     ~S" ll))))
            (:macro
             (format stream "Macro: `~A`" symbol)
             (let ((ll (sb-introspect:function-lambda-list symbol)))
               (when ll
                 (format stream "~%~%The lambda list:~%~%     ~S" ll))))
            (:variable (format stream "Variable: `~A`" symbol))
            (:condition (format stream "Condition: `~A`" symbol))
            (:class (format stream "Class: `~A`" symbol)))
          (format stream "~%~%~A~%~%~%" doc)))


(handler-case (print-doc "JUST-GETOPT-PARSER")
  (error (c)
    (format *error-output* "~A~%" c)))
