README.md: just-getopt-parser.lisp make-doc.lisp
	sbcl --script make-doc.lisp >$@

clean:
	rm -f *.fasl

.PHONY: clean
