
Example: Implementing a simple calculator
=========================================

.. Note: Try to keep this documentation up-to-date with the file
   example_calculator.py

This section shows how to implement a simple calculator language using the
Typped package.  It is an evaluating calculator with a simple
read-evaluate-print loop (REPL).  It implements basic math operations.  It does
not have many predefined functions, but it would be simple to add them
according to the pattern of the current ones.  The juxtaposition operator is
defined for multiplication, so any two terms next to each other (by default
separated by a space character) are multiplied together.  Simple variables can
also be set and used.

This is a top-down description of the code, even though the actual file is in
the reverse order.  The working code can be found in the file
`example_calculator.py
<https://github.com/abarker/typped/blob/master/examples/example_calculator.py>`_.
Just run that file to start up the calculator.  A few the lines of that file
have been left out of the code listed below for readability.

The Python code has been broken up into various high-level functions for
readability and to simplify the display and discussion of the code.  These
high-level functions will be called "procedures" in the discussion, to avoid
confusion with the functions in the calculator language or with lower-level
Python functions inside the parser.

The main procedure
------------------

The main procedure is called ``define_and_run_basic_calculator``.  It
is called as the last line of the calculator module.

.. code-block:: python

   import math
   import operator
   import typped as pp

   def define_and_run_basic_calculator():
       """Get a parser, define the calculator language, and start the REP loop."""
       parser = pp.PrattParser()
       define_basic_calculator(parser)
       read_eval_print_loop(parser)

The code first defines a ``PrattParser`` instance.  It then passes that
instance to a procedure which sets up the parser with the grammar and evaluation
functions for the calculator language.

Finally, the read-evaluate-print loop is called, passed the parser instance as
an argument.  The three procedures above will be described shortly.  First, to
give a preview of what the calculator does, here is a simple dialog of the
program running::

   Enter ^C to exit, and 'toggle' to toggle tree display.
   > 5
   5.0
   > 5 + 5
   10.0
   > 5 * (4 - 3)
   5.0
   > # A comment line.
   > 4 4 # Juxtaposition with space is multiplication.
   16.0
   > 5 (log(3) - 9)/44.90
   -0.8798872729768252
   > pi # Pi and e are predefined constants.
   3.141592653589793
   > tau = 2 pi # Simple variables can be assigned.
   6.283185307179586
   > sin(tau)
   -2.4492935982947064e-16
   > toggle
   > 2 pi cos(tau) - 1E3 # Scientific notation.

   <k_minus,'-'>
       <k_jop,None>
           <k_jop,None>
               <k_float,'2'>
               <k_identifier,'pi'>
           <k_cos,'cos'>
               <k_identifier,'tau'>
       <k_float,'1E3'>

   -993.7168146928204
   > 25 - 5.0^2

   <k_minus,'-'>
       <k_float,'25'>
       <k_double_ast,'^'>
           <k_float,'5.0'>
           <k_float,'2'>

   0.0
   > 25 - 5.0**2

   <k_minus,'-'>
       <k_float,'25'>
       <k_double_ast,'**'>
           <k_float,'5.0'>
           <k_float,'2'>

   0.0
   > 
   Bye.

Notice that when the ``toggle`` command is given the program prints out the
expression tree for the expressions.  Each line is a token in the tree.  The
representation ``<k_minus,'-'>`` is for the token that was assigned the string
label ``k_minus``.  The convention of starting token labels with ``k_`` is
generally used in the code.  The string ``'-'`` is the value of the token, i.e.,
the actual symbol in the parsed expression text that the lexer matched as being
a ``k_minus`` token.

The top line of the tree representation is the root of the tree.  Indented
lines below are for children.  Each level of indentation is another level of
children in the tree.

The read-evaluate-print loop
----------------------------

Continuing with the top-down presentation, the REP loop is shown next.  The
code is basic Python, and can be skimmed by people familiar with the language.
The code shows how a Typped parser is used at the higher level.

The ``cmd`` module in the standard Python library can also be used to write the
REP loop.  The example file also has an alternative version of the procedure
which is implemented using that library.

.. code-block:: python

   def read_eval_print_loop(parser):
       """Implement the REP loop."""
       import readline

       try:
           read_input = raw_input # Python 2.
       except NameError:
           read_input = input # Python 3.

       print("Enter ^C to exit, and 'toggle' to toggle tree display.")

       show_tree = False # Toggled in the loop below.
       while True:
           try:
               line = read_input("> ")
           except (KeyboardInterrupt, EOFError):
               print("\nBye.")
               break
           if not line:
               continue
           if line == "toggle":
               show_tree = not show_tree
           elif line.strip().startswith("#"): # Tries to parse empty line.
               continue

           try:
               parse_tree = parser.parse(line)
               eval_value = parse_tree.eval_subtree()
           except (ValueError, ZeroDivisionError,
                   pp.ParserException, pp.LexerException) as e:
               print(e)
               continue

           if show_tree:
               print("\n", parse_tree.tree_repr(), sep="")
           print(eval_value)

The code starts by importing ``readline``.  Just importing that module provides
nice features for the Python ``input`` command, such as command history with the
up and down arrows.  The code then prints a prompt and waits for the user to
enter a line, which should contain an expression in the calculator language.

Notice that ^C can be used to exit the program.  If the user types in the
command ``toggle`` it will toggle the printing of expression trees for the
user-entered expressions.

The passed-in ``parser`` argument is used inside a ``try`` loop in order to catch
errors and continue running.  As with all Typped parsing operations, the full
expression tree for the expression that was input by the user created by this
line:

.. code-block:: python

    parse_tree = parser.parse(line)

Here ``line`` is the user's input.  The value returned from ``parse`` is a
token instance, which the ``parse`` function has converted into the root node
of an expression tree of tokens.  These are the expression trees that were
displayed in the above dialog after the ``toggle`` command was issued.

After the expression tree is returned it is evaluated with the line
``parse_tree.eval_subtree()``, which is a recursive evaluation function started
at the root of the expression tree.  Evaluation functions are provided when the
grammar for the language is defined, in the next section.

Finally, the values are printed out and the loop continues.

Defining the grammar
--------------------

The only high-level procedure left to describe is the
``define_basic_calculator`` procedure.  This is the procedure that really shows
how to set up and use the ``PrattParser`` class (at least the basic usage).  To
keep the procedure from being too long it has been broken up into several
sub-procedures doing particular tasks.  This is the top-level procedure:

.. code-block:: python

   def define_basic_calculator(parser):
       """Define the calculator language in the parser instance."""
       define_general_tokens_and_literals(parser)
       define_functions_and_operators(parser)
       define_juxtaposition_operators(parser)
       define_assignment_operator(parser)
       define_comments(parser)
       define_semicolon_separator(parser)


Each procedure does what the name implies.  The code for each sub-procedure, in
sequence, will be shown and discussed next.  The first procedure defines some
general tokens and literals in the calculator language:

.. code-block:: python

   def define_general_tokens_and_literals(parser):
       """Define some general tokens and literals in the calculator language.
       Other tokens such as for functions in the language will be defined
       later."""
       #
       # Tokens.
       #

       parser.def_default_whitespace()

       tok = parser.def_token
       tok("k_plus", r"\+")
       tok("k_minus", r"\-")
       tok("k_fslash", r"/")
       tok("k_ast", r"\*")
       tok("k_lpar", r"\(")
       tok("k_rpar", r"\)")
       tok("k_lbrac", r"\[")
       tok("k_rbrac", r"\]")
       tok("k_comma", r",")
       tok("k_bang", r"!")
       tok("k_equals", r"=")
       tok("k_double_ast", r"(?:\*\*|\^)") # Note ^ is a synonym for **.

       # This token definition for a float is based on the regex from
       # https://docs.python.org/2/library/re.html#simulating-scanf
       #   r"[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?"
       # But if we used the Python doc form exactly then 4 -4 would be interpreted
       # as a multiplication jop for rather than correctly, as subtraction.  So
       # the [+-] part is left off and is implemented as a prefix operator instead.
       tok("k_float", r"(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?")

       #
       # Literals.
       #

       parser.def_literal("k_float", eval_fun=lambda t: float(t.value))

We now have tokens for operators and other basic symbols in the language.
Notice that ``^`` and ``**`` are both defined to produce the token labeled
``double_ast``.  An alternate way to do this would be to define two separate
tokens and give them the same function definition.

Floating point literals are defined and provided with an evaluation function.
This evaluation function just takes a token with label ``k_float`` as its
argument ``t`` and converts the string value (returned by the lexer) into a
Python float.  The floating point value is returned.

The next procedure contains a group of definitions for the calculator language
which will define almost all of the functions in the language.  This includes
standard functions like ``sin`` and operators like ``*`` and ``!``.  The
definitions are made using built-in methods of the ``PrattParser`` class.  Note
in particular the precedence values assigned to the operators.

Every function is also provided with an evaluation function, which, at
evaluation time, runs the Python version of the function on the arguments.  The
evaluation functions are passed a node ``t``, which is a node in the final
expression tree.  The children of the node are then accessed as ``t[0]``,
``t[1]``, etc., depending on how many arguments there are.  The evaluations are
top-down recursive, and call the method ``eval_subtree`` on the child or
children (i.e., to evaluate the function arguments).

.. code-block:: python

   def define_functions_and_operators(parser):
       """Define the all the functions and operators for the calculator.
       Evaluation functions are also supplied for each one.  Parentheses and
       brackets are also defined here, since they have a precedence in the order
       of evaluations."""

       #
       # Parens and brackets, highest precedence (since they have a head function).
       #

       parser.def_bracket_pair("k_lpar", "k_rpar",
                               eval_fun=lambda t: t[0].eval_subtree())
       parser.def_bracket_pair("k_lbrac", "k_rbrac",
                               eval_fun=lambda t: t[0].eval_subtree())

       #
       # Standard functions.
       #

       parser.def_token("k_sin", r"sin")
       parser.def_stdfun("k_sin", "k_lpar", "k_rpar", "k_comma", num_args=1,
                         eval_fun=lambda t: math.sin(t[0].eval_subtree()))
       parser.def_token("k_cos", r"cos")
       parser.def_stdfun("k_cos", "k_lpar", "k_rpar", "k_comma", num_args=1,
                         eval_fun=lambda t: math.cos(t[0].eval_subtree()))
       parser.def_token("k_sqrt", r"sqrt")
       parser.def_stdfun("k_sqrt", "k_lpar", "k_rpar", "k_comma", num_args=1,
                         eval_fun=lambda t: math.sqrt(t[0].eval_subtree()))

       # Note that log is overloaded because different numbers of arguments are
       # specified.  The two versions have different eval funs.
       parser.def_token("k_log", r"log")
       parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma", num_args=1,
                         eval_fun=lambda t: math.log(t[0].eval_subtree()))
       parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma", num_args=2,
                  eval_fun=lambda t: math.log(t[0].eval_subtree(), t[1].eval_subtree()))

       #
       # Basic operators, from highest to lowest precedence.
       #

       parser.def_prefix_op("k_plus", 50,
                            eval_fun=lambda t: operator.pos(t[0].eval_subtree()))
       parser.def_prefix_op("k_minus", 50,
                            eval_fun=lambda t: operator.neg(t[0].eval_subtree()))

       parser.def_postfix_op("k_bang", 40, allow_ignored_before=False,
                             eval_fun=lambda t: math.factorial(t[0].eval_subtree()))

       parser.def_infix_op("k_double_ast", 30, "right",
               eval_fun=lambda t: operator.pow(t[0].eval_subtree(), t[1].eval_subtree()))

       parser.def_infix_op("k_ast", 20, "left",
               eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))
       parser.def_infix_op("k_fslash", 20, "left",
               eval_fun=lambda t: operator.truediv(t[0].eval_subtree(), t[1].eval_subtree()))

       parser.def_infix_op("k_plus", 10, "left",
               eval_fun=lambda t: operator.add(t[0].eval_subtree(), t[1].eval_subtree()))
       parser.def_infix_op("k_minus", 10, "left",
               eval_fun=lambda t: operator.sub(t[0].eval_subtree(), t[1].eval_subtree()))

The definitions above actually define the ``log`` function twice, with a
different number of arguments each time.  This results in function overloading.
Each overload can have a different evaluation function.  In this case the
two-place version takes an extra argument giving the base, like in the Python
math library (which uses a default parameter value of ``math.e`` for the
single-argument form).

At this point we have a working calculator.  The code up to this point can
already be run to do basic operations.  The next three procedures just add
extra features to the calculator.

The previous procedure defined all the usual arithmetic functions, but it did
not define the juxtaposition operator.  This procedure defines the juxtaposition
operator as a synonym for multiplication.

.. code-block:: python

   def define_juxtaposition_operators(parser):
       """Define the juxtaposition operator (jop) as synonym for multiplication."""

       jop_required_token = "k_space" # Can be set to None to not require any whitespace.
       parser.def_jop_token("k_jop", jop_required_token)
       parser.def_jop(20, "left", # Same precedence and assoc. as ordinary multiplication.
               eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))

The ``jop_required_token`` argument to the method ``def_jop_token`` is a token
which is required to be present in order for a juxtaposition operator to be
inferred.  The setting above requires a space between two tokens in order for a
jop to possibly be inferred.  After these definitions strings like ``2
sin(3.3)`` can be evaluated with implicit multiplication.

Next, the grammar for and implementation of simple assignment statements is
defined for the calculator language.  Two symbols, for ``pi`` and ``e`` are
predefined to the associated math constants.

.. code-block:: python

   def define_assignment_operator(parser):
       """Define assignment and reading of simple variables."""

       parser.calculator_symbol_dict = {} # Store symbol dict as a new parser attribute.
       symbol_dict = parser.calculator_symbol_dict

       symbol_dict["pi"] = math.pi # Predefine pi.
       symbol_dict["e"] = math.e # Predefine e.

       # Note that on_ties for identifiers is set to -1, so that when string
       # lengths are equal defined function names will take precedence over
       # identifiers (which are only defined as a group regex).
       parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
       parser.def_literal("k_identifier",
               eval_fun=lambda t: symbol_dict.get(t.value, 0.0))

       def eval_assign(t):
           """Evaluate the identifier token `t` and save the value in `symbol_dict`."""
           rhs = t[1].eval_subtree()
           symbol_dict[t[0].value] = rhs
           return rhs

       parser.def_infix_op("k_equals", 5, "right",
                   precond_fun=lambda tok, lex: lex.peek(-1).token_label == "k_identifier",
                   precond_label="lhs must be identifier",
                   eval_fun=eval_assign)

Notice that the ``def_infix_op`` method is used to define the ``=`` operator
for assignment.  A preconditions function, along with a unique label for it, is
used to restrict the definition to the case where the left hand side of the
assignment is an identifier.

Now expressions like ``sin(2 pi)``, ``x = 5``, and ``x^2`` can be used in the
calculator language.  Uninitialized variables default to zero, and the
variables ``pi`` and ``e`` are predefined.  Assignment returns the assigned
value as its operator value, so, while it is probably not a good idea, you can
have expressions like ``sin(tau = 2 pi)``.

The next feature which will be added to the calculator language is comments.
Comments are just like comments in Python.  They are defined by defining a
token with a regex that recognizes comments, and telling the lexer to ignore
all such tokens.

.. code-block:: python

   def define_comments(parser):
       """Define comments in the calculator.  Everything from '#' to EOL is a
       comment.  Defined using an ignored token pattern."""

       parser.def_ignored_token("k_comment_to_EOL", r"\#[^\r\n]*$", on_ties=10)

The language has now been defined and the calculator can be run as shown above
in the interactive dialog.  As one final feature, we will define the semicolon
operator as a statement separator.  This can be done simply by using an infix
operator with a very low precedence value.

.. code-block:: python

   def define_semicolon_separator(parser):
       """Define semicolon to separate expressions, returning the value of the last one."""

       def eval_semicolon(t):
           t[0].eval_subtree()
           return t[1].eval_subtree()

       parser.def_token("k_semicolon", r";")
       parser.def_literal("k_semicolon")
       parser.def_infix_op("k_semicolon", 1, "right",
                           eval_fun=eval_semicolon)

Now expressions like ``w = sqrt(4); w^2`` can be used.

