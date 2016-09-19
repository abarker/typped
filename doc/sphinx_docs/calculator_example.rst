
Example: Implementing a simple calculator
=========================================

This section shows how to implement a simple calculator language using the
Typped package.  It is an evaluating calculator with a simple
read-evaluate-print (REP) loop.  It implements basic math operations.  It does
not have many predefined functions, but it would be simple to add them
according to the pattern of the current ones.  The juxtaposition operator is
defined for multiplication, so any two terms next to each other (by default
separated by a space character) are multiplied together.  Simple variables can
also be set and used.

This is a top-down description of the code, even though the actual file is in
the reverse order.  The working code can be found in the file
`example_pratt_calculator.py <http://www.df_TODO_dfdfdfd.com>`_.  Just run that
file to start up the calculator.

The main function
-----------------

The main function is called `define_and_run_basic_calculator`.  This function
is called as the last line of the file.  The function is defined as follows.

.. code-block:: python

   import typped as pp

   def define_and_run_basic_calculator():
       parser = pp.PrattParser()
       define_basic_calculator(parser)
       read_eval_print_loop(parser)

The code first defines a `PrattParser` instance.  It then passes that instance
to a function which sets up the parser with the grammar and evaluation
functions for the calculator language.

Finally, the read-evaluate-print loop is called, passed the parser instance.

To get a preview of what the calculator does, here is a simple dialog of the
program running:

::

   Press ENTER or type command to continue
   Enter ^C to exit, and 'toggletree' to toggle tree display.
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
   > toggletree
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

Notice that when the `toggletree` command is given the program prints out the
expression tree for the expressions.  Each line is a token in the tree.  The
representation `<k_minus,'-'>` is for the token that was assigned the string
label `k_minus`.  The convention of starting token labels with `k_` is
generally used in the code.  The string `'-'` is the value of the token, i.e.,
the actual symbol in the parsed expression text that the lexer matched as being
a `k_minus` token.

The top line of the tree representation is the root of the tree.  Indented
lines below are for children.  Each level of indentation is another level of
children in the tree.

The read-evaluate-print loop
----------------------------

Continuing with the top-down presentation, the REP loop is shown next.  The
code is mostly basic Python, but it shows how the parser can be used in such a
loop.

.. code-block:: python

   def read_eval_print_loop(parser):
       import readline

       try:
           read_input = raw_input # Python 2.
       except NameError:
           read_input = input # Python 3.

       print("Enter ^C to exit, and 'toggletree' to toggle tree display.")

       show_tree = False # Toggled in the loop below.
       while True:
           try:
               line = read_input("> ")
           except (KeyboardInterrupt, EOFError):
               print("\nBye.")
               break
           if not line:
               continue

           if line == "toggletree":
               show_tree = not show_tree
               continue

           try:
              parse_tree = parser.parse(line)
              eval_value = parse_tree.eval_subtree()
           except pp.CalledEndTokenHandler:
              continue # Comment on empty line, don't show error message.
           except (ValueError, ZeroDivisionError,
                   pp.ParserException, pp.LexerException) as e:
               print(e)
               continue
           else:
               if show_tree:
                   print("\n", parse_tree.tree_repr(), sep="")
               print(eval_value)

The code starts by importing `readline`.  Just importing that module provides
nice features for the Python `input` command, such as command history with the
up and down arrows.

Notice that ^C can be used to exit the program.  If the user types in the
command `toggletree` it will toggle the printing of expression trees for the
user-entered expressions.

The passed-in `parser` argument is used inside a `try` loop to catch errors and
continue running.  As with all Typped parsing operations, the full tree for an
expression is first created by the line `parse_tree = parser.parse(line)`,
where `line` is the user's input.  The returned value is a token instance,
which is the root node of an expression tree of tokens.  These are the trees
that were displayed in the above dialog when the `toggletree` command was
issued.

After the expression tree is returned it is evaluated with the line
`parse_tree.eval_subtree()` (run from the root of the tree).  Evaluation
functions were provided with the grammar for the language, discussed in the
next section.

Finally, the values are printed out and the loop continues.

Defining the grammar
--------------------

The only function left to describe is the `define_basic_calculator` function.
This is the function that really shows how to set up and use the `PrattParser`
class -- at least the basic parts of it.  Because that is a fairly long
function it will be displayed and described in blocks rather than all at once.

TODO

Extending the calculator
------------------------

Suppose you wanted to extend the calculator to be a matrix calculator, using
numpy.  In that case you might make the juxtaposition operator represent the
`dot` function.  Here we show how to the PrattParser can be modified so it can
read in a literal matrix.

TODO
