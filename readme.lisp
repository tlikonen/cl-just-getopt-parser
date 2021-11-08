(defparameter *head*
  "~
Just Getopt Parser
==================

**Getopt-like command-line parser for the Common Lisp language**


Introduction
------------

_Just Getopt Parser_ is a Common Lisp package that implements Unix
Getopt command-line parser. The intention is to be just that: the parser
functionality with clear Lisp programming interface and nothing more.
Package's main interface is `getopt` function which parses the command
line options and organizes them to valid options, other arguments and
unknown arguments. There are also optional Lisp conditions for error
situations. For full documentation on package's programming interface
see section _The Programming Interface_ below.


Examples
--------

Example command line:

    $ some-program -d3 -f one --file=two -xyz foo --none bar -v -- -v

That command line could be parsed with the following function call:

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
             (exit-program :code 1)))
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

OpenPGP key: [6965F03973F0D4CA22B9410F0F2CAE0E07608462][PGP]

License: [Creative Commons CC0][CC0] (public domain dedication)

The source code repository:
<https://github.com/tlikonen/cl-just-getopt-parser>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/legalcode


The Programming Interface
-------------------------

")

(load "just-getopt-parser.lisp")
(load "print-doc.lisp")
(format t *head*)
(print-doc:print-doc "JUST-GETOPT-PARSER")
