
Constructs and preconditioned dispatching
=========================================

This section describes the use of preconditioned dispatching in the
``PrattParser`` class.  Many Typped users will never need to explicitly use the
techniques described here, since the Typped parser comes with various built-in
methods which hide the use of precondition functions.  The calulator example,
for example, uses only built-in methods of the ``PrattParser`` class.  This
description is mainly for users who need to define their own, custom parsing
methods or those who are simply interested in the details of how dispatching
works.

What is preconditioned dispatching?
-----------------------------------

In a standard Pratt parser each token can have associated with it a single,
fixed head handler function, a single, fixed tail handler function, or one of
each.  Preconditioned dispatching generalizes this: Each token can have
multiple possible head and/or tail handler functions associated with it.  The
choice of which of the possible handler functions to use to process a token is
made at at the time the token is parsed, based on the conditions (e.g., the
parser and lexer state) at that time.  The conditions which can be taken into
account include, for example, the actual string value that the lexer matched in
the program text and the kind of token that is one peek ahead in the lexer.
Note that this generalized behavior in Typped is optional and can easily be
ignored if one only wants to use standard Pratt parser techniques.

Since in Pratt parsing each head and tail handler essentially parses a
different part of the grammar, the Typped packages uses an abstraction of
handler functions called a **syntactic construct**, or simply a **construct**.
A construct represents a particular kind of grammatical subexpression that is
parsed and returned by a handler function.  Since Pratt parsers are top-down
these grammatical parts tend to correspond to subtrees of the final expression
tree.  A construct containing a head handler will be called a **head
construct** and a construct containing a tail handler will be called a **tail
construct**.  Constructs also contain other attributes, as we will see.

In a standard Pratt parser a handler function is triggered whenever a
particular kind of token is consumed from the lexer in the ``recursive_parse``
routine.  Either the head handler or the tail handler function associated
with that kind of token is run, depending on whether or not the token is the
first token in the subexpression being parsed.

In a preconditioned-dispatching Pratt parser each handler function is
associated with a unique construct, and each construct has associated with it
the label of a **triggering token** which triggers that construct as
potentially the one to use.  A single kind of token can potentially trigger
multiple constructs (either head constructs or tail constructs, depending on
the token's position).

When a token is consumed in ``recursive_parse`` it triggers a collection of
constructs.  The choice of which one to actually use is based on the evaluation
of **preconditions functions** associated with the constructs.  Preconditions
functions are simply boolean-valued functions which are executed at the time
when a handler function is required.  A preconditions function which returns
true when evaluated is said to **match** in the current state.  The matching
construct with the highest **preconditions priority** is selected, and its
handler is run.

Constructs are implemented as instances of ``Construct`` objects.  They contain
the following attributes:

* a kind of token which triggers the construct
* a head or tail handler function
* a preconditions function
* a preconditions priority
* a string label for the construct
* other data, such as evaluation functions and type signatures

Constructs are **registered** with a parser instance in order to define a
particular grammar on the tokens of the language (the tokens must be separately
defined with ``def_token``).  The preconditions priority is a number which
defaults to zero.

Whenever the ``recursive_parse`` routine consumes a particular kind of token
from the lexer, in a head or tail position, it sequentially executes the
preconditions functions for all the constructs triggered by that kind of
token, in that position.  The execution sequence is ordered by the
preconditions priority values.  The construct associated with the first
matching preconditions function is selected.  Its handler function is then
dispatched as the one to be run by the ``recursive_parse`` routine.  If there
is no clear winner among the highest-priority matching preconditions functions
(i.e., more than one match has the highest priority) then an exception is
raised.

This algorithm clearly reduces to ordinary Pratt parsing in the case where
there is at most one head construct and one tail construct per kind of token
and the preconditions functions always evaluate as true.

Using dispatching
-----------------

The ``PrattParser`` method which registers a construct with the parser is
called ``def_construct``.  It is used, for example, inside the builtin methods
after defining the handler functions and any preconditions functions.  To
define custom constructs it needs to be explicitly called.

One of the optional arguments to ``def_construct`` is ``precond_fun``, which
can be passed a function taking two parameters, ``tok`` and ``lex``.  It should
return ``True`` or ``False`` (or the equivalent).  When a token is read in
``recursive_parse`` all the preconditions functions for all the constructs
triggered by that kind of token are run in priority ordering until one is true.
The associated construct is the "winner" and is dispatched to be called.  When
the preconditions function is called, ``tok`` is the triggering token and
``lex`` is the lexer.  (Usually ``tok == lex.token``, except in the case of
some "virtual" tokens like null-space tokens and jop-tokens which are both
discussed in later sections.)

See the documentation for the ``def_construct`` method at
:py:meth:`typped.pratt_parser.PrattParser.def_construct`.  The basic
specification is::

   def def_construct(head_or_tail, handler_fun, trigger_token_label,
                     prec=0, construct_label=None,
                     precond_fun=None, precond_priority=0,
                     val_type=None, arg_types=None,
                     eval_fun=None, ast_data=None,
                     token_value_key=None, dummy_handler=False):

For a simple example of defining a construct, see :ref:`basic_usage:Example:
Parsing a simple expression without using builtins`.  A more general example is
given in the next section.

Example: Defining standard functions with lookahead
---------------------------------------------------

As an example of dispatching, consider the parsing of function evaluations such
as ``f(x)`` in a Pratt parser.   The "usual" way is to define a tail handler
for the left-paren token.  Then that symbol acts like an infix operator with
the function name as its first argument and the function arguments and closing
paren as its second argument.  If parentheses are also used for grouping then a
head-handler for left paren is defined for that use.  The resolution between
the two uses is based on whether the left paren is in a head or tail position
in a subexpression.  In the case of the function evaluation, the token for the
function name ``f`` is the head of the subexpression.

This usual way of parsing function evaluations can lead to complications in
more-complex grammars where left paren is used in various contexts.  If a
juxtaposition operator is being used, for example, then an expression like
``pi (x+y)`` can cause problems with the usual method.  The name ``pi`` might
be a constant or a function name.  (At the least the left paren tail handler
would need to be conditioned on a space occurring before it, but this example
takes a different approach.)

By using a precondition that the lookahead token be a left paren with no
intervening space the head handler for a standard function identifier can parse
the whole subexpression rather than waiting to be picked up as the left operand of
the infix left paren operator.  A second, lower-priority default head handler
can still be defined for all other identifiers.  (Other preconditions can also
be placed on other head handlers for identifiers).  These two head handler
definitions are largely independent, except via their respective priorities.
They can occur in different sections of code, where the different constructs
are defined.  Both handlers are registered for the identifier token, and the
rest is handled automatically.

The code for this example can be found in a runnable form in the file
`example_stdfun_lookahead.py
<https://github.com/abarker/typped/blob/master/examples/example_stdfun_lookahead.py>`_.

In this example the ``PrattParser`` class is extended by creating a subclass
with additional methods.  In particular, a general method is added which parses
standard functions.  If a general method is not required then the code could
instead just define the handler and preconditions function and call
``def_construct``.

For a general parsing method it is not strictly necessary to create a subclass
of ``PrattParser``.  An ordinary function can also be used.   Just rename the
``self`` variable to something like ``parser`` and explicitly pass in a parser
instance when calling it.  Extending the class has the advantage that the newer
methods are called in the same way as the built-in ones, and the parser
instance's namespace is convenient for accessing the function.

In this example the method ``def_stdfun_lookahead`` is added to the
``PrattParser``.  This is only an example, since the ``PrattParser`` class
already has a ``def_stdfun`` method which uses lookahead and also incorporates
types, etc.  Before calling this method all of the tokens involved must have
already been defined along with their labels (via the ``def_token`` method).
Ignored whitespace tokens must also have been defined already.  The lpar, rpar,
and comma tokens must already have been defined as literal tokens (via the
``def_literal`` method).

Recall that the head-handler function will be called to process a subexpression
starting from the beginning.  That head-handler is then responsible for parsing
the full subexpression -- though it can itself call ``recursive_parse`` to
parse sub-subexpressions.  We are defining a head-handler that only matches a
function name in the case where the peek token is an lpar with no intervening
space.

.. TODO: Keep up-to-date with the code in latest version from Python file
   ``example_stdfun_lookahead.py``  Add a test file in tests dir to test it.

.. code-block:: python

   def define_parser_subclass():

       class MyParser(pp.PrattParser):
           """Subclass and add a new method to the `PrattParser` class as an example."""

           def __init__(self, *args, **kwargs):
               """Call the superclass initializer."""
               super(MyParser, self).__init__(*args, **kwargs)

           def def_stdfun_lookahead(self, fname_token_label, lpar_token_label,
                                    rpar_token_label, comma_token_label, num_args,
                                    precond_priority=1):
               """Define a standard function with a fixed number of arguments."""

               # Define the preconditions function.
               def preconditions(tok, lex):
                   peek_tok = lex.peek()
                   if peek_tok.ignored_before: # No space allowed between name and lpar.
                       return False
                   if peek_tok.token_label != lpar_token_label:
                       return False
                   return True

               # Define the head-handler function.
               def head_handler(tok, lex):
                   # Below match_next is for a precondition, so it will match and consume.
                   lex.match_next(lpar_token_label, raise_on_fail=True)

                   # Read comma-separated subexpressions as arguments.
                   for i in range(num_args-1):
                       tok.append_children(tok.recursive_parse(0))
                       lex.match_next(comma_token_label, raise_on_fail=True)
                       lex.match_next(rpar_token_label, raise_on_success=True) # Error.
                   if num_args != 0:
                       tok.append_children(tok.recursive_parse(0))
                   # Consume closing paren.
                   lex.match_next(rpar_token_label, raise_on_fail=True)
                   return tok

               # Register the construct with the parser.
               construct_label = "function call using precondition on function name"
               self.def_construct(pp.HEAD, head_handler, fname_token_label, prec=0,
                                  construct_label=construct_label,
                                  precond_fun=preconditions,
                                  precond_priority=precond_priority)
       return MyParser

In parsing the full function call the handler defined above uses both the
helper function ``match_next`` as well as calls to the lexer and
``recursive_parse``.  Generally, tokens which will appear in the final parse
tree, even literal tokens, should be retrieved with ``recursive_parse``.  That
is because it performs some extra processing the nodes such as setting their
actual types.  Tokens which do not appear in the final parse tree, such as the
final closing rpar token of the function arguments, can simply be consumed by
``match_next`` or an explicit call to ``lex.next()`` and discarded.

The function defined above could be called as follows:

.. code-block:: python

   def define_grammar(MyParser):
       parser = MyParser()
       parser.def_default_whitespace()

       tok = parser.def_token
       tok("k_number", r"\d+"),
       tok("k_lpar", r"\("),
       tok("k_rpar", r"\)"),
       tok("k_comma", r","),
       tok("k_add", r"add"),
       tok("k_sub", r"sub"),

       lit = parser.def_literal
       lit("k_number")
       lit("k_lpar")
       lit("k_rpar")

       parser.def_stdfun_lookahead("k_add", "k_lpar", "k_rpar", "k_comma", 2)
       parser.def_stdfun_lookahead("k_sub", "k_lpar", "k_rpar", "k_comma", 2)

       return parser

Now this code can be run:

.. code-block:: python

    MyParser = define_parser_subclass()
    parser_instance = define_grammar(MyParser)
    expr = "add(4, sub(5,6))"
    expr_tree = parser_instance.parse(expr)
    print(expr_tree.tree_repr(indent=3))

When run, the above code produces this output:

::

   <k_add,'add'>
       <k_number,'4'>
       <k_sub,'sub'>
           <k_number,'5'>
           <k_number,'6'>

This example works, but is simplified from the actual ``def_stdfun`` method of
the Pratt parser class.  It assumes a fixed number of arguments and does not
make use of type data.  The function is still fairly general, though.  Note
that this function does not allow whitespace (ignored tokens) to occur between
the function name and the left parenthesis.  The preconditions function is
defined as a nested function, but it could alternately be passed in as another
argument to ``def_stdfun`` (along with its label). 


.. topic:: Two ways to parse identifiers

   The Typped parser and lexer are both dynamic and can be updated on-the-fly.
   This flexibility allows for a different style of defining identifiers than
   is traditionally used.  Consider an example where function name
   identifiers are being parsed.  Assume that the language being parsed has
   some sort of definition mechanism where function names must be defined
   before they are used.  (The principle is more general, including cases
   where, say, functions and variables share the same namespace or for
   kinds of token other than identifiers.)
   
   In the traditional parser design a generic function-name identifier is
   defined for the lexer and any further processing is done by the parser, based
   on the actual string value found in the program text.  This allows a
   fixed lexer to be used.  When the lexer is dynamic, though, it is possible
   to define a new token for each definition of an identifier.
   
   Suppose we have functions ``add`` and ``exp``.  In the traditional approach
   the lexer would identify each as a function name identifier, and return that
   information along with the actual text string.  In the dynamic-lexer
   approach you would define a new token for ``add`` at the time it is defined.
   Similarly for the ``exp`` function.  The lexer would then return a unique
   token for each function, pushing some of the parsing down to the lexer
   level.

   An advantage of the dynamic approach is that it can help to avoid
   ambiguities in parsing complex languages.  The disadvantages are that it may
   take slightly more space to define the new tokens, it may be slower to scan
   with so many possible tokens, and the function names (and hence their
   tokens) must be defined before being used.

   A disadvantage of using a common identifier token for all function names is
   evaluation functions then cannot be automatically associated with the
   tokens.  To get around this the `def_construct` method takes a keyword
   argument `value_key` can be passed strings like `add` and `exp`.  The
   evaluation functions are then keyed on those values, too.  During lookup
   the actual text string for the token is used to look back up the evaluation
   function.

   As far as the efficiency of defining many tokens, the Typped lexer is
   designed to very efficiently scan large numbers of tokens provided they have
   a simple pattern.  The `Matcher` used by the lexer can use one of several
   hybrid approaches.  For example, simple patterns (currently restricted to
   fixed strings for this speedup) can be automatically stored in a trie data
   structure and essentially all scanned in parallel by walking down the trie.
   Their insert and delete time is linear in the pattern length.  So, while the
   Typped parser can be used in either way, the use of dynamic token
   definitions is worth considering.

Modifications to ``recursive_parse``
------------------------------------
In generalizing to preconditioned dispatching the ``recursive_parse`` routine
is slightly modified from the one in the previous section.  A simplified
version is shown here:

.. code-block:: python

   def recursive_parse(lex, subexp_prec):
       curr_token = lex.next()
       head_handler = curr_token.dispatch_handler(HEAD, lex)
       processed_left = head_handler()

       while lex.peek().prec() > subexp_prec:
           curr_token = lex.next()
           tail_handler = curr_token.dispatch_handler(TAIL, lex, processed_left)
           processed_left = tail_handler()

Instead of directly calling a fixed head or tail handler for a token, the
``recursive_parse`` function instead calls a function ``dispatch_handler``.
This function takes an argument which specifies whether to fetch a head or a
tail handler.  This function selects a construct, as described above, and
returns the handler function (which is actually a wrapper function that first
runs the handler and then does type checking on the returned subtree).  For
convenience the arguments to the handler are bound, since they are already
known.

.. note::

   Preconditioned dispatching is only a slight generalization of the usual
   Pratt parser.  A similar thing could be accomplished with ordinary head and
   tail functions via a case statement inside each one, performing different
   actions based on the conditions at the time and ordered in the case
   statement by priority. An advantage of using function dispatching instead
   is that it allows for modularity in defining the head and tail handlers for
   a particular kind of token.
   
   With dispatching, what would otherwise be a case statement in a handler
   function is essentially split up into many separate functions, one for each
   case.  So each case in such a case statement can be defined in the place
   where that syntactic construct is generally being defined, rather than
   having to be placed in one centralized and separate location.  This makes it
   easier to create essentially independent functional interfaces for different
   syntactical constructs.  The syntax of the language can be decomposed into
   subunits, with separately-defined handlers to parse them.  For example, the
   `PrattParser` class comes with methods predefined to easily perform common
   syntax-related tasks such as defining an infix operator, defining a grouping
   operator, defining a standard function, etc.  If one big case statement were
   being used in a single head or tail handler for each token then one of those
   case statement would have to be modified for each such method.

Uniqueness of constructs
------------------------

Equality or non-equality of two constructs in the sense of being triggered by
identical conditions is determined by equality of triples of the form::

   (head_or_tail, trigger_token_label, precond_fun)

The preconditions priority is not included because it determines the
interaction between different constructs match.  If two constructs match in the
above tuple but have different ``precond_priority`` values then one will always
shadow the other.  The shadowed construct will never run.

Unfortunately it is impractical to determine in general when two preconditions
functions are identical in the sense that they compute the same thing.

Recall that function overloading based on argument types is used for
syntactical constructs which parse the same (i.e., with the same preconditions
and using the same handler function) but which are then resolved into different
semantic objects based on the actual types of the arguments which are processed
at parse-time.  Overloading can also involve the type of the function's return
value.

Overloading must be explicitly specified, via a call to the ``overload`` method
of a previously-defined construct instance.  Because of the difficulty of
determining equivalence of preconditions functions, described above,
overloading cannot be done by simply calling ``def_construct`` again with the
same arguments and a different type.  

.. topic:: Overloading versus preconditions functions

   An alternative way that Typped could have implemented overloading would have
   been to always use a unique construct label for each overload --- perhaps by
   appending a string representation of the type to the label.  But this would
   also complicate the resolution of constructs.
  
   Constructs as currently implemented must be uniquely resolvable
   at parse-time.  They then uniquely determine the handler function to call.
   If different preconditions labels are used for overloading then overloading
   will cause multiple constructs to match as a normal thing.  These ties will not
   be uniquely resolvable by a priority system.
   
   To resolve an overload the expression must first be parsed to find the
   actual types.  Resolving the actual types requires a handler function, which
   is stored with a construct.  This is circular if separate constructs are
   used for each overload.  One approach might be to assume that if there are
   multiple constructs which match at the same priority then they all have the
   same handler function.  You could then just pick one to call, but that could
   mask some error conditions.  After the actual types are found a unique
   construct would still need to be determined from among the matches in order
   to access the associated evaluation function and AST data.  It seems simpler
   to just to store all the overloaded signatures and their associated data
   with a single construct.

