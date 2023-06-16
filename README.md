# SAINT
An implementation of the elf functions as described by the original Saint papers.

3/31/2023
Elf functions mostly complete.  The next task is to clean up the elf code either by using the match functions from Racket or
possibly writing a match function so the code can be ported over to Scheme.

4/7/2023
I decided to write the whole thing in Common Lisp using SBCL with the Trivia pattern matching library.

6/16/2023
I'm going to use the format (+ a b) to make the parsing easier for the Sigma test and for the product rule.  With that, negative expressions
will be expressed as '(+ (-a) b c (-d)).  The Elf functions will have to be changed to accomodate that.
