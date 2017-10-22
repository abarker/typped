
Basic usage
===========

..
   keep synced with file basic_usage_examples.py in examples dir

This section gives an overview of the basic usage of the Typped package.  Later
sections go into many more details.  Executable code for all the examples on
this page can be found in the file `examples/basic_usage_examples.py
<https://github.com/abarker/typped/blob/master/examples/basic_usage_section_examples.py>`_.

The ``PrattParser`` class is the main class provided by the Typped package.  It
is the only class that is needed for basic usage of the parser.

Example: Parsing a simple expression
------------------------------------

This example parses simple expressions such as ``x * 4 + 4`` using the builtin
methods of the ``PrattParser`` class.  The builtin parsing methods that can be
called for a ``PrattParser`` instance are documented here:
:py:mod:`typped.builtin_parse_methods`.  Only a few are used in this
example.  Example 2 will parse the same language without using any of the
builtin parsing methods.  Instead, it will directly use Pratt parsing
techniques.

The language consists of integers, identifiers representing integers, addition
and multiplication operators, and parentheses.  Types are not used in this
example.  Strings of the language are parsed to an expression tree.

1. Create an instance of the ``PrattParser`` class::

      import typped as pp
      parser = pp.PrattParser()

2. Define each token that will appear in the language, including a string label
   and a regex to recognize that kind of token.  If necessary, the appropriate
   ``on_ties`` values can be set to break ties in case of equal match lengths.
   The lexer will always take the longest match over all the defined tokens,
   with ties broken by any `on_ties` values (which by default equal zero).  The
   order in which definitions are made does not affect the lexical analysis.  ::

       parser.def_default_whitespace()
       tok = parser.def_token
       tok("k_number", r"\d+")
       tok("k_lpar", r"\(")
       tok("k_rpar", r"\)")
       tok("k_ast", r"\*")
       tok("k_plus", r"\+")
       tok("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

   In these examples the string ``k_`` is used by convention as a prefix for
   token labels.  The prefix ``t_`` will be used for types.

   The call to ``def_default_whitespace`` above sets up default whitespace.  It
   is equivalent to the following code::
   
       parser.def_ignored_token("k_space", r"[ \t]+")
       parser.def_ignored_token("k_newline", r"[\n\f\r\v]+")

   Whitespace is defined as space, tab, and newline characters and is set to be
   ignored by the lexer.  The space and newline sequences are made into
   separate tokens; both are ignored by the lexer.  (Ignored tokens can be
   accessed via the ``ignored_before`` attribute of a token.)

3. Define the syntactical elements of the language being parsed.  Any necessary
   token labels must have already been defined (as in the previous step).  The
   predefined syntax-definition methods of ``PrattParser`` take as arguments
   token labels, type information, etc.

   Note that token literals must still be defined as syntactical elements of
   the grammar being parsed, after being defined as tokens.  The token
   definition just creates a kind of token to be scanned and returned by the
   lexer.  If typing is being used then any type information should also be set
   for the token literals since they are the leaves of the expression trees. ::

       parser.def_literal("k_number")
       parser.def_literal("k_identifier")

   Infix operators are defined below, with a precedence of 10 for addition and 20
   for multiplication.  Both are left-associative.  Parentheses for grouping are
   also defined. ::

       parser.def_infix_op("k_plus", 10, "left")
       parser.def_infix_op("k_ast", 20, "left")

       parser.def_bracket_pair("k_lpar", "k_rpar")

4. Pass the parser instance a string of text to parse, and save the returned
   expression tree.  The returned tree has token instances as its nodes. ::

       result_tree = parser.parse("x + (4 + 3)*5")
       print(result_tree.tree_repr())

   The result of running the above code is::

    <k_plus,'+'>
        <k_identifier,'x'>
        <k_ast,'*'>
            <k_lpar,'('>
                <k_plus,'+'>
                    <k_number,'4'>
                    <k_number,'3'>
            <k_number,'5'>


See ":ref:`calculator_example:Example: Implementing a simple calculator`" for a
similar example with many more features and which also evaluates the
expressions.

Example: Parsing a simple expression without using builtins
-----------------------------------------------------------

This example parses the same language as Example 1, but none of the builtin
parsing routines of the ``PrattParser`` class are used.  Raw Pratt parsing is
used, explicitly defining the head and tail handler functions and registering
them with the parser as a construct.  (Head and tail handler functions are
called null denotations and left denotations, respectively, in traditional
Pratt Parser terminology.)

See the section ":ref:`pratt_parsing_intro:Introduction to Pratt parsing and
its terminology`" for background information.  A construct here can be thought
of as simply a container class that holds a token label and a corresponding
head or tail handler function which is triggered by that kind of token (though
actually constructs are more general than that).  See the section
":ref:`dispatching:Constructs and preconditioned dispatching`" for more
information on constructs.

The definitions of the initial parser instance and the tokens are exactly the
same in this example as in the previous example, so that portion of the code
above is not repeated.  The below discussion starts at Step 3 above, assuming
the code for steps 1 and 2 has already been run.

First we define the token literals, which are tokens that represent themselves
in the final expression tree.  The head handler function for such a token
simply returns the token itself.  Such a head-handler function is registered
with the parser for each kind of token which should be a token literal:

.. code-block:: python

    def literal_head_handler(tok, lex):
        return tok
    parser.def_construct(pp.HEAD, literal_head_handler, "k_number")
    parser.def_construct(pp.HEAD, literal_head_handler, "k_identifier")

Next, we define the infix operators, starting with addition.  First, we need a
tail handler function:

.. code-block:: python

    def infix_op_tail_handler_10(tok, lex, left):
        tok.append_children(left, tok.recursive_parse(10)) # Use 9 for right assoc.
        return tok

This handler function has a hardcoded left-association precedence value of 10
(for right-association 9 would be used instead).  When called, the ``tok``
parameter will hold the token for the ``"k_plus"`` operator which triggers this
particular handler function.  The function simply sets the left child of
``tok`` to the passed-in ``left`` argument (which holds the expression to the
left that was already processed).  It sets the right child to the result of the
``recursive_parse`` function, which parses the next expression.  So the left
and right operands are both set to expressions.

The ``def_construct`` method is now used to register the handler with the
parser as a head-handler triggered by ``"k_plus"`` tokens:

.. code-block:: python

   parser.def_construct(pp.TAIL, infix_op_tail_handler_10, "k_plus", prec=10)

Notice that the precedence value of 10 is also passed to ``def_construct``.

The construct for parsing ``+`` operators has now been defined for the
language.  The code for multiplication is similar, except that a precedence of
20 is hardcoded:

.. code-block:: python

   def infix_op_tail_handler_20(tok, lex, left):
       tok.append_children(left, tok.recursive_parse(20)) # Use 19 for right assoc.
       return tok
   parser.def_construct(pp.TAIL, infix_op_tail_handler_20, "k_ast", prec=20)

Finally, we need to define the construct for parsing parentheses.  This is done
by defining a head-handler for the left parenthesis token.  The handler just
calls ``recursive_parse`` to get the expression inside the parentheses,
consumes the closing parenthesis, and returns the expression inside:

.. code-block:: python

   def paren_head_handler(tok, lex):
       expr = tok.recursive_parse(0)
       lex.match_next("k_rpar", raise_on_fail=True)
       return expr # Do not include the parens themselves, just the arg.
   parser.def_construct(pp.HEAD, paren_head_handler, "k_lpar")

This finishes the definition of the parser for the simple language, without
using any of the builtin parsing methods.  Now this code can be run::

       result_tree = parser.parse("x + (4 + 3)*5")
       print(result_tree.tree_repr())

The result is shown here::

   <k_plus,'+'>
       <k_identifier,'x'>
       <k_ast,'*'>
           <k_plus,'+'>
               <k_number,'4'>
               <k_number,'3'>
           <k_number,'5'>

Notice that the expression tree created using the ``def_bracket_pair`` builtin
in Example 1 included the ``k_lpar`` token in the tree.  This handler function
does not; it simply returns the expression inside the parentheses.  To get that
kind of behavior with ``def_bracket_pair`` you can set the keyword ``in_tree``
to false.

The builtin methods of ``PrattParser`` are basically just wrapper functions
that do things like defining and registering handler functions behind the
scenes.  They are written for much more generality than the above code, and
they tend to have various options.  If you need to write your own wrapper
functions it can be useful to look at the code for the builtin parse routines
in the file ``builtin_parse_methods.py`` documented in
:py:mod:`typped.builtin_parse_methods`.

Example: A simple string and number language with evaluation and dynamic typing
-------------------------------------------------------------------------------

This next example is a simple language that operates on both quoted strings and
integers.  The only allowed operations are addition and multiplication.  The
operations on integers give the usual results.  The operations on strings are
like in Python: addition concatenates and multiplication of a string by an
integer (on the left or right) repeats it that many times.  Addition of strings
and integers is a syntax error.

Identifier variables can also be defined and assigned values.  This example
uses dynamic typing, like an interpreted language.  Type errors are reported at
parse-time, based on the types implicitly defined by the previously-executed
coded.  For example, assigning ``x = "house"`` implicitly defines ``x`` as a
string.

This example illustrates how to define evaluation functions to interpret the
parsed expression trees.  It also shows how to use the basic type mechanism.
The example code in the ``examples`` directory runs the language in a
read-evaluate-print loop (REPL).

..
   Just replace this whole code block from the basic_usage_section_examples.py file
   whenever it is updated.

.. code-block:: python

   def setup_string_language_parser_dynamic_typing():
       """A simple dynamically-typed language that uses `+` to add integers and
       concatenate strings.  Multiplication of a number by a string repeats the
       string.  Multiplication of a string by a string is not defined.  It also
       has simple variables which can represent either numbers or strings."""
       parser = pp.PrattParser()

       # Define the tokens.

       parser.def_default_whitespace()
       tok = parser.def_token
       tok("k_int", r"-?\d+")
       tok("k_lpar", r"\(")
       tok("k_rpar", r"\)")
       tok("k_ast", r"\*")
       tok("k_plus", r"\+")
       tok("k_equals", r"=")
       tok("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
       tok("k_string", r"(\"(.|[\r\n])*?\")")

       # Define the types.

       t_int = parser.def_type("t_int") # Integer type.
       t_str = parser.def_type("t_str") # String type.

       # Define the syntax of the language, supplying evaluation functions.

       parser.def_literal("k_int", val_type=t_int, eval_fun=lambda t: int(t.value))
       parser.def_literal("k_string", val_type=t_str, eval_fun=lambda t: t.value)

       parser.def_literal_typed_from_dict("k_identifier", create_eval_fun=True,
                                          default_type=t_int, default_eval_value=0)

       parser.def_bracket_pair("k_lpar", "k_rpar", eval_fun=lambda t: t[0].eval_subtree())

       infix = parser.def_infix_op
       infix("k_plus", 10, "left",
             val_type=t_int, arg_types=[t_int, t_int],
             eval_fun=lambda t: t[0].eval_subtree() + t[1].eval_subtree())
       infix("k_plus", 10, "left",
             val_type=t_str, arg_types=[t_str, t_str],
             eval_fun=lambda t: t[0].eval_subtree()[:-1] + t[1].eval_subtree()[1:])

       infix("k_ast", 20, "left",
             val_type=t_int, arg_types=[t_int, t_int],
             eval_fun=lambda t: t[0].eval_subtree() * t[1].eval_subtree())
       infix("k_ast", 20, "left",
             val_type=t_str, arg_types=[t_str, t_int],
             eval_fun=lambda t: (
                      '"' + (t[0].eval_subtree()[1:-1] * t[1].eval_subtree()) + '"'))
       infix("k_ast", 20, "left",
             val_type=t_str, arg_types=[t_int, t_str],
             eval_fun=lambda t: (
                      '"' + (t[1].eval_subtree()[1:-1] * t[0].eval_subtree()) + '"'))

       # Define assignment as an infix equals operator.
       parser.def_assignment_op_dynamic("k_equals", 5, "left", "k_identifier",
                                        val_type=None, allowed_types=[t_int, t_str],
                                        create_eval_fun=True)
       return parser

Example 4: A simple string and number language with evaluation and static typing
--------------------------------------------------------------------------------

The language being parsed in this example is basically the same as the previous
one except that the language is statically typed rather than dynamically typed.
This is like parsing a statically-typed compiled language.  Type errors are
caught at parse-time, before any interpretation or translation into machine
code.  This language translates the simple string-number language to Python
code, which is then executed

Static typing in a language requires some mechanism for declaring types (either
implicitly or explicitly).  This language has a C-like type declaration syntax.
None of the builtin parse methods work for this special-purpose construct, so a
new construct is defined for type declarations.  Builtin methods are used for
the rest of the parsing.

This example illustrates how to use static typing and how to define custom
parsing functions when the builtin methods are not sufficient.  The example
code in the ``examples`` directory runs the language in a read-evaluate-print
loop.  It prints out the parsed expression tree, the translation to Python,
and the result of evaluating the Python code with Python's ``eval``.

The definitions of the parser instance, tokens, and types are basically the
same as in Example 3.

Defining a new construct for type definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We want to allow C-style typed variable declarations like the following in the
language::

   int x
   str y

Since the builtin parsing methods do not cover this, we need to define a new
construct and register it with the parser.  Later sections will cover this
in more detail, so readers can skim this subsection for now if necessary.

There are several ways to do this parsing.  We could define new tokens for the
keywords ``int`` and ``str`` (or a token for all such keywords) with a higher
``on_ties`` value than general identifiers.  Then we would have the handler
functions for those tokens do the corresponding parsing.  Instead, we use
preconditioned dispatching on identifier tokens.  In this way, a different
head-handler function can be dispatched to handle a type name identifier versus
a general identifier.  This makes it easy to add new type names later (since
they are stored in a dict).

In this example we have used a precondition on a head-handler function instead
of using a tail-handler function defined for identifiers.  A tail-handler could
have been used, but in that case ``int x = 4`` would be more difficult to
parse.  The ``recursive_parse`` routine would consume the ``x`` as an operator,
left value of ``int``.  The expression ``x = 4`` could not then be evaluated in
the usual way with ``recursive_parse`` without going back one token in the
lexer (such as with the ``go_back`` method).

