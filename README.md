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
unknown arguments. There are also optional conditions for error
situations. For full documentation on package's programming interface
see section _The Programming Interface_ down below.


Examples
--------

Example command line:

    $ some-program -d3 -f one --file=two -xyz foo --none bar -v -- -v

That command line could be parserd with the followin function call:

    (getopt '("-d3" "-f" "one" "--file=two" "-xyz" "foo"
              "--none" "bar" "-v" "--" "-v")
            '((:debug #\d :optional)
              (:file #\f :required)
              (:file "file" :required)
              (:verbose #\v))
            :options-everywhere t)

The function returns three values: (1) valid options and their
arguments, (2) other arguments and (3) unknown options:

    ((:DEBUG . "3") (:FILE . "one") (:FILE . "two") (:VERBOSE))
    ("foo" "bar" "-v")
    (#\x #\y #\z "none")

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
             (format *error-output* "~A~%" condition)
             (invoke-restart 'skip-option)))
         (unknown-option
           (lambda (condition)
             (format *error-output* "~A~%" condition)
             (invoke-restart 'skip-option)))
         (required-argument-missing
           (lambda (condition)
             (format *error-output* "~A~%" condition)
             (exit-program :code 1)))
         (argument-not-allowed
           (lambda (condition)
             (format *error-output* "~A~%" condition)
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

### Condition: `ambiguous-option`

`getopt` function may signal this condition when it parses a
partially-written option name that matches to two or more long option
names. Function `option-name` can be used to read option's name from the
condition object. Function `option-matches` will return the matching
options.


### Condition: `argument-not-allowed`

`getopt` function may signal this condition when it parses an option
that does not allow an argument but one is given with "--foo=...".
Function `option-name` can be used to read option's name from the
condition object.


### Condition: `required-argument-missing`

`getopt` function may signal this condition when it parses an option
that required an argument but there is none. Function `option-name` can
be used to read option's name from the condition object.


### Condition: `unknown-option`

`getopt` function may signal this condition when it finds an unknown
condition. Function `option-name` can be used to read option's name from
the condition object.


### Function: `getopt`

The lambda list:

     (arguments option-specification &key options-everywhere
      prefix-match-long-options error-on-ambiguous-option
      error-on-unknown-option error-on-argument-missing
      error-on-argument-not-allowed)

Parse command-line arguments like getopt.

The _arguments_ is a list of strings and contains the command-line
arguments that typically come from program's user.

_option-specification_ argument is the specification of valid
command-line options. It is a list that contains lists of the following
format (in lambda list format):

    (symbol option-name &optional option-argument)

The first element _symbol_ is any symbol which identifies this
command-line option (for example keyword symbol `:help`). The identifier
is used in function's return value to identify that this particular
option was present in the command line.

The second element _option-name_ is either

 1. a character specifying a short option name (for example `#\h`,
    entered as `-h` in command line)

 2. a string specifying a long option (for example `"help"`, entered
    as `--help` in command line). The string must be at least two
    characters long.

The third element _option-argument_ is optional but if it is non-nil it
must be one of the following keyword symbols: `:required` means that
this option requires an argument; `:optional` means that this option has
an optional argument. Example value for this function's
_option-specification_ argument:

    ((:help #\h)     ; short option -h for help (no option argument)
     (:help "help")  ; long option --help (no option argument)
     (:file "file" :required) ; --file option which requires argument
     (:debug #\d :optional))  ; -d option with optional argument

Note that several options may have the same identifier _symbol_. This
makes sense when short and long option represent the same meaning. See
the `:help` keyword symbol above. All options must have unique
_option-name_ though.

If function's key argument `options-everywhere` is nil (the default) the
option parsing stops when the first non-option argument is found. Rest
of the command line is parsed as non-options. If `options-everywhere` is
non-nil then options can be found anywhere in the command line, even
after non-option arguments. In all cases the option parsing stops when
the pseudo-option `--` is found in the command line. Then all remaining
arguments are parsed as non-option arguments.

If key argument `prefix-match-long-options` is non-nil then long options
don't need to be written in full in the command line. They can be
shortened as long as there are enough characters to find unique prefix
match. If there are more than one match the option is classified as
unknown. If also key argument `error-on-ambiguous-option` is non-nil the
function will signal error condition `ambiguous-option`. The condition
object contains the option's name and it can be read with function
call `(option-name condition)`. Function call `(option-matches
condition)` returns a list of option matches (strings). Also, the
condition object can be printed as an error message for user. There is
also `skip-option` restart available. When it is invoked the ambiguous
option is skipped and the function will continue parsing the command
line. Ambiguous options are always also unknown options: if
`ambiguous-option` condition is not signaled then the condition for
unknown option can be signaled. See the next paragraph.

If function's key argument `error-on-unknown-option` is non-nil and the
function finds an uknown option on the command line the function signals
error condition `unknown-option`. The condition object includes the name
of the unknown option which can be read with function `(option-name
condition)`. The return value is of type character or string for short
or long options respectively. You can also just print the condition
object: it gives a reasonable error message. There is also `skip-option`
restart available. The invoked restart skips the unknown option and
continues parsing the command line.

Function's key argument `error-on-argument-missing`, if non-nil, causes
the function to signal error condition `required-argument-missing` if it
sees an option which required argument (keyword `:required`) but there
is none. The condition object contains the name of the option which can
be read with function `(option-name condition)`. You can also just
print the condition object for user. It's the error message. There are
two restarts available: `give-argument` restart can be invoked with a
optional argument (string or nil) which will be passed as a new argument
for the option; restart `skip-option` will just skip this option and
continue parsing.

Key argument `error-on-argument-not-allowed`, if non-nil, makes this
function to signal error condition `argument-not-allowed` if there is an
argument for a long option which does not allow argument (`--foo=...`).
Such option is always listed as unknown option with name `"foo="` in
function's return value. The condition object can be printed to user as
error message. The object also contains the name of the option which can
be read with `(option-name condition)` function call. There is
`skip-option` restart available. When the restart is invoked the
function continues parsing the command line.


#### Return values

The function returns three values:

 1. List of parsed options. List's items are cons cells: the CAR part of
    the cons cell is the identifier symbol for the option; the CDR part
    of the cons cell is either nil (if there is no argument for this
    option) or a string containing option's argument.

 2. List of non-option arguments (strings).

 3. List of unknown options. List's items are either characters or
    strings which represent command-line options which were not defined
    in the _option-specification_.

In all return values the list's items are in the same order as they were
in the original command line.


#### Parsing rules for short options

Short options in the command line start with the `-` character and the
option character follows (`-c`).

If option requires an argument (keyword `:required`) the argument must
be entered either directly after the option character (`-cARG`) or as
the next command-line argument (`-c ARG`). In the latter case anything
that follows `-c` will be parsed as option's argument.

If option has optional argument (keyword `:optional`) it must always be
entered directly after the option character (`-cARG`). Otherwise there
is no argument for this option.

Several short options can be entered together after one `-`
character (`-abc`) but then only the last option in the series may have
required or optional argument.


#### Parsing rules for long options

Long options start with `--` characters and the option name comes
directly after it (`--foo`).

If option requires an argument (keyword `:required`) it must be entered
either directly after the option name and `=` character (`--foo=ARG`) or
as the next command-line argument (`--foo ARG`). In the latter case
anything that follows `--foo` will be parsed as its argument.

If option has optional argument (keyword `:optional`) the argument must
always be entered directly after the option name and `=`
character (`--foo=ARG`). Otherwise (like in `--foo`) there is no
argument for this option.

Option `--foo=` is valid format when option has required or optional
argument. It means that the argument is empty string.


