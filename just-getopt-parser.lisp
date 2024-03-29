;;;; Getopt-like parser for command-line options and arguments
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:just-getopt-parser
  (:use #:cl)
  (:export #:getopt
           #:unknown-option
           #:ambiguous-option
           #:argument-not-allowed
           #:required-argument-missing
           #:option-name #:option-matches
           #:invalid-option-specification
           #:error-string
           #:skip-option
           #:give-argument
           ))

(in-package #:just-getopt-parser)

(defun format-option-name (name)
  (etypecase name
    (string (format nil "--~A" name))
    (character (format nil "-~C" name))))

(define-condition argument-error (error)
  ((option :reader option-name :initarg :option
           :type (or character string))))

(define-condition unknown-option (argument-error)
  ()
  (:documentation
   "`getopt` function may signal this condition when it finds an unknown
option. Function `option-name` can be used to read option's name from
the condition object.")
  (:report
   (lambda (condition stream)
     (format stream "Unknown option \"~A\"."
             (format-option-name (option-name condition))))))

(define-condition ambiguous-option (argument-error)
  ((matches :reader option-matches :initarg :matches))
  (:documentation
   "`getopt` function may signal this condition when it parses a
partially-written option name that matches to two or more long option
names. Function `option-name` can be used to read option's name from the
condition object. Function `option-matches` will return the matching
options.")
  (:report
   (lambda (condition stream)
     (format stream "Option \"~A\" is ambiguous: ~{\"--~A\"~^, ~}."
             (format-option-name (option-name condition))
             (option-matches condition)))))

(define-condition required-argument-missing (argument-error)
  ()
  (:documentation
   "`getopt` function may signal this condition when it parses an option
that required an argument but there is none. Function `option-name` can
be used to read option's name from the condition object.")
  (:report
   (lambda (condition stream)
     (format stream "Required argument missing for option \"~A\"."
             (format-option-name (option-name condition))))))

(define-condition argument-not-allowed (argument-error)
  ()
  (:documentation
   "`getopt` function may signal this condition when it parses an option
that does not allow an argument but one is given with \"--foo=...\".
Function `option-name` can be used to read option's name from the
condition object.")
  (:report
   (lambda (condition stream)
     (format stream "Argument is not allowed for option \"~A\"."
             (format-option-name (option-name condition))))))

(define-condition invalid-option-specification (error)
  ((string :reader error-string :initarg :string))
  (:documentation
   "`getopt` function signals this condition if its
`option-specification` argument is invalid. Function `error-string` can
be used to read error string from the condition object.")
  (:report (lambda (condition stream)
             (format stream "~A" (error-string condition)))))

(defun check-option-identifier (identifier)
  (assert (symbolp identifier)
          nil 'invalid-option-specification
          :string (format nil "Option identifier ~S is not of type SYMBOL."
                          identifier)))

(defun check-short-option-character (name)
  (assert (and (graphic-char-p name)
               (not (find name " -")))
          nil 'invalid-option-specification
          :string (format nil "Invalid character in short option name (~A)."
                          name)))

(defun check-long-option-string (string)
  (assert (>= (length string) 2)
          nil 'invalid-option-specification
          :string "Long option string must be at least 2 characters long.")

  (assert (every (lambda (char)
                   (and (graphic-char-p char)
                        (not (find char " ="))))
                 string)
          nil 'invalid-option-specification
          :string (format nil "Invalid character(s) in long option ~
                        name \"~A\"." string)))

(defun check-option-name (name)
  (assert (or (characterp name)
              (stringp name))
          nil 'invalid-option-specification
          :string (format
                   nil "Option name ~S is not of type CHARACTER or STRING."
                   name))
  (etypecase name
    (character (check-short-option-character name))
    (string (check-long-option-string name))))

(defun check-option-argument (field)
  (assert (or (null field)
              (eql :required field)
              (eql :optional field))
          nil 'invalid-option-specification
          :string (format nil "Options' argument type must be symbol ~
                :REQUIRED or :OPTIONAL.")))

(defun check-duplicate-names (specification)
  (let* ((names (loop :for option-spec :in specification
                      :collect (second option-spec)))
         (test (remove-duplicates names :test #'equal)))
    (assert (= (length names)
               (length test))
            nil 'invalid-option-specification
            :string "Duplicate option names are not allowed.")))

(defun check-option-specification (specification)
  (assert (listp specification)
          nil 'invalid-option-specification
          :string "Option specification must be of type LIST.")
  (loop :for option-spec :in specification
     :do (assert (listp option-spec)
                 nil 'invalid-option-specification
                 :string (format nil "Option specification must consist of ~
                        two- or three-item lists."))
       (check-option-identifier (first option-spec))
       (check-option-name (second option-spec))
       (check-option-argument (third option-spec)))
  (check-duplicate-names specification))

(defun match-long-option (name specification &key prefix)
  (loop :with matches
        :for opt-spec :in specification
        :for option-name := (second opt-spec)
        :if (stringp option-name)
          :do (cond ((string= name option-name)
                     (return (list opt-spec)))
                    ((and prefix
                          (string= name option-name
                                   :end2 (min (length name)
                                              (length option-name))))
                     (push opt-spec matches)))
        :finally
           (return (sort matches #'string< :key #'second))))

(defun getopt (arguments option-specification
               &key options-everywhere
                 prefix-match-long-options error-on-ambiguous-option
                 error-on-unknown-option error-on-argument-missing
                 error-on-argument-not-allowed)

  "Parse command-line arguments like getopt.

The `arguments` is a list of strings and contains the command-line
arguments that typically come from program's user.

`option-specification` argument is the specification of valid
command-line options. It is a list that contains lists of the following
format (in lambda list format):

    (symbol option-name &optional option-argument)

The first element `symbol` is any symbol which identifies this
command-line option (for example keyword symbol `:help`). The identifier
is used in function's return value to identify that this particular
option was present in the command line.

The second element `option-name` is either

 1. a character specifying a short option name (for example `#\\h`,
    entered as `-h` in command line)

 2. a string specifying a long option (for example `\"help\"`, entered
    as `--help` in command line). The string must be at least two
    characters long.

The third element `option-argument` is optional but if it is non-nil it
must be one of the following keyword symbols: `:required` means that
this option requires an argument; `:optional` means that this option has
an optional argument. Example value for this function's
`option-specification` argument:

    ((:help #\\h)     ; short option -h for help (no option argument)
     (:help \"help\")  ; long option --help (no option argument)
     (:file \"file\" :required) ; --file option which requires argument
     (:debug #\\d :optional))  ; -d option with optional argument

Note that several options may have the same identifier `symbol`. This
makes sense when short and long option represent the same meaning. See
the `:help` keyword symbol above. All options must have unique
`option-name` though.

If `option-specification` argument is not in correct form an error of
type `invalid-option-specification` is signaled.

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
`skip-option` restart available. When it is invoked the ambiguous option
is skipped and the function will continue parsing the command line.
Ambiguous options are always also unknown options: if `ambiguous-option`
condition is not signaled then the condition for unknown option can be
signaled. See the next paragraph.

If function's key argument `error-on-unknown-option` is non-nil and the
function finds an unknown option on the command line the function
signals error condition `unknown-option`. The condition object includes
the name of the unknown option which can be read with
function `(option-name condition)`. The return value is of type
character or string for short or long options respectively. You can also
just print the condition object: it gives a reasonable error message.
There is active `skip-option` restart. The invoked restart skips the
unknown option and continues parsing the command line.

Function's key argument `error-on-argument-missing` (if non-nil) causes
the function to signal error condition `required-argument-missing` if it
sees an option which requires argument (keyword `:required`) but there
is none. The condition object contains the name of the option which can
be read with function call `(option-name condition)`. You can also just
print the condition object for user. It is an error message. There are
two restarts available: `give-argument` restart can be invoked with an
optional argument (string or nil) which will be passed as a new argument
for the option; restart `skip-option` will just skip this option and
continue parsing.

Key argument `error-on-argument-not-allowed` (if non-nil) makes this
function to signal error condition `argument-not-allowed` if there is an
argument for a long option which does not allow argument (`--foo=...`).
Such option is always listed as unknown option with name `\"foo=\"` in
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
    strings which represent unknown short or long command-line options
    which were not defined in the `option-specification`.

In all three return values the list's items are in the same order as
they were given in the function's `arguments` argument.


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

Option `--foo=` is valid format when the option requires argument or
accepts optional argument. It means that the argument is empty string."

  (check-option-specification option-specification)

  (loop
    :with parsed-options
    :with unknown-options
    :with other-arguments
    :with argument
    :while arguments
    :if (setf argument (pop arguments)) :do

      (cond
        ((string= "--" argument)
         (loop :for arg :in arguments
               :do (push arg other-arguments))
         (loop-finish))

        ;; Long options
        ((and (> (length argument) 2)
              (string= "--" argument :end2 2))
         (restart-case
             (let* ((name-arg (subseq argument 2))
                    (equal-pos (position #\= name-arg))
                    (opt-name (subseq name-arg 0 equal-pos))
                    (opt-arg (if equal-pos (subseq name-arg (1+ equal-pos))))
                    (option-spec (match-long-option
                                  opt-name option-specification
                                  :prefix prefix-match-long-options))
                    (osymbol (first (first option-spec)))
                    (oname (second (first option-spec)))
                    (oargument (third (first option-spec))))

               (cond
                 ((null option-spec)
                  (push opt-name unknown-options)
                  (when error-on-unknown-option
                    (error 'unknown-option :option opt-name)))

                 ((and prefix-match-long-options
                       (> (length option-spec) 1))
                  (push opt-name unknown-options)
                  (when error-on-ambiguous-option
                    (error 'ambiguous-option
                           :option opt-name
                           :matches (mapcar #'second option-spec)))
                  (when error-on-unknown-option
                    (error 'unknown-option :option opt-name)))

                 ((and (not oargument)
                       opt-arg)
                  (push (format nil "~A=" oname) unknown-options)
                  (when error-on-argument-not-allowed
                    (error 'argument-not-allowed :option oname)))

                 ((and (not oargument)
                       (not opt-arg))
                  (push (cons osymbol nil) parsed-options))

                 ((and (eql :required oargument)
                       (not opt-arg))
                  (restart-case
                      (let ((arg (pop arguments)))
                        (when (and (null arg) error-on-argument-missing)
                          (error 'required-argument-missing
                                 :option oname))
                        (push (cons osymbol arg) parsed-options))
                    (give-argument (&optional argument)
                      :report "Give argument for the option."
                      (check-type argument (or string null))
                      (push (cons osymbol argument) parsed-options))))

                 ((and (eql :required oargument)
                       opt-arg)
                  (push (cons osymbol opt-arg) parsed-options))

                 ((eql :optional oargument)
                  (push (cons osymbol opt-arg) parsed-options))))

           (skip-option ()
             :report "Skip the option and continue.")))

        ;; Short options
        ((and (> (length argument) 1)
              (char= #\- (aref argument 0)))
         (loop
           :named this-series
           :with series := (subseq argument 1)
           :for character :across series
           :for i :upfrom 1
           :for remaining := (subseq series i)
           :do

              (restart-case
                  (let* ((option-spec (first (member character
                                                     option-specification
                                                     :key #'second
                                                     :test #'eql)))
                         (osymbol (first option-spec))
                         (oargument (third option-spec)))

                    (cond
                      ((null option-spec)
                       (push character unknown-options)
                       (when error-on-unknown-option
                         (error 'unknown-option :option character)))

                      ((not oargument)
                       (push (cons osymbol nil) parsed-options))

                      ((and (eql :required oargument)
                            (plusp (length remaining)))
                       (push (cons osymbol remaining) parsed-options)
                       (return-from this-series))

                      ((and (eql :required oargument)
                            (zerop (length remaining)))
                       (restart-case
                           (let ((arg (pop arguments)))
                             (if (and (null arg)
                                      error-on-argument-missing)
                                 (error 'required-argument-missing
                                        :option character)
                                 (push (cons osymbol arg) parsed-options)))
                         (give-argument (&optional argument)
                           :report "Give argument for the option."
                           (check-type argument (or string null))
                           (push (cons osymbol argument) parsed-options))))

                      ((and (eql :optional oargument)
                            (plusp (length remaining)))
                       (push (cons osymbol remaining) parsed-options)
                       (return-from this-series))

                      ((and (eql :optional oargument)
                            (zerop (length remaining)))
                       (push (cons osymbol nil) parsed-options))))

                (skip-option ()
                  :report "Skip the option and continue."))))

        ;; Other arguments
        (t (push argument other-arguments)
           (unless options-everywhere
             (loop :for arg :in arguments
                   :do (push arg other-arguments))
             (loop-finish))))

    :finally
       (return (values (nreverse parsed-options)
                       (nreverse other-arguments)
                       (nreverse unknown-options)))))
