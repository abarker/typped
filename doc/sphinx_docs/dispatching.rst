
Preconditioned dispatching
==========================

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
ignored if one wants to use only standard Pratt parser techniques.

Define a **syntactic construct**, or simply a **construct**, to be some
particular kind of grammatical subexpression that is parsed and returned by a
handler function (either head or tail).  Since Pratt parsers are top-down these
tend to correspond to subtrees of the final expression tree.

In a standard Pratt parser a handler function, and hence a construct, is
**triggered** whenever a particular kind of token is read in from the lexer in
the ``recursive_parse`` routine.  Either the head handler or the tail handler
associated with that kind of token is run, depending on whether or not the
token is the first token in the subexpression being parsed or is a later one.

In this generalization a particular kind of token can have multiple head and
tail handlers.  The triggering of which one to run is based on **preconditions
functions**.  These are simply boolean-valued functions which are executed at
the time when the choice needs to be made.  A preconditions function which
returns true when evaluated is said to **match** in the current state.

In a preconditioned dispatching Pratt parser a construct is expanded to include:

* a kind of token which triggers the construct
* a head or tail handler
* a preconditions function
* a preconditions priority
* a string label for the construct
* other data, such as evaluation functions and type signatures

These constructs are **registered** with the parser by a user in order to
define a particular grammar on the tokens.  A construct with a head handler
may be called a head construct, and a construct with a tail handler may be
called a tail construct.

Whenever the ``recursive_parse`` routine consumes a particular kind of token
from the lexer, in a head or tail position, it sequentially executes the
preconditions functions for all the constructs associated with that kind of
token, in that position.  The execution is ordered by the preconditions
priorities.  The construct associated with the first matching preconditions
function is triggered.  Its handler function is then dispatched as the one to
be run by the ``recursive_parse`` routine.  If there is no clear winner among
the choices of matching preconditions functions with equal priorities an
exception is raised.

In the case where there is at most one head construct and one tail construct
per kind of token this algorithm clearly reduces to ordinary Pratt parsing.

Using dispatching
-----------------

The ``PrattParser`` method which registers a construct with the parser is
called ``def_construct``.  It is used inside the predefined methods after
defining the handler functions and any preconditions functions.  To define
custom constructs it needs to be explicitly called.

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
tail handler.  This function select a construct, as described above, and
returns the handler function.  For convenience the arguments to the handler are
bound, since they are already known.

The typing system which is implemented in the Typped parser is also based on
the preconditioned dispatching design.  Type-signature information can
optionally be associated with any particular head or tail handler function.
The type system is discussed more in later sections.

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
   syntactical constructs.  For example, the `PrattParser` class comes with
   methods predefined to easily perform common syntax-related tasks such as
   defining an infix operator, define a grouping operator, define a standard
   function, etc.  If one big case statement were being used in a single head
   or tail handler then that case statement would have to be modified for each
   such method.

Uniqueness of constructs
------------------------

TODO: update from here to below to discuss in terms of the newer constructs and
construct labels.

Each preconditions function for a token is associated with a head and/or tail
handler function.  If a head or tail handler is re-registered with the token
along with *the same* preconditions function then the previously-registered
handler is overwritten.  But if the type signatures also differ then the
previous type information *is* saved even thought the handler function is
overwritten, since this allows for function overloading.  So it is obviously
important to have have a very clear definition of when two preconditions
functions are considered equal and when they are not.

In order to avoid problems in determining when functions are identical, every
preconditions function (or distinct use of a preconditions function) **must**
be assigned a string label.  The preconditions functions are then registered in
a dict (a ``TokenTable`` instance) with that label as the key.  For two
preconditions functions to be considered identical they must have the *exact
same* label.  For them to be considered different they must have *different*
labels.  **This can be important in defining general parser methods and can
cause subtle problems if it is not done correctly.**

The preconditions labels *define* identity or non-identity between
preconditions functions.  Sometimes you want two different preconditions
functions to be considered identical, such as when overloading based on
argument types but the function definition has been re-evaluated to a different
function object.  Sometimes you want the same function to be considered as
different preconditions functions, such as when the function makes use of
closure variables or global state.

Recall that function overloading based on argument types is used for
syntactical constructs which parse the same (i.e., using the same handler
function) but then are resolved into different things based on the actual type
signatures of the arguments.  To use overloading the handler function which
parses a construct should be re-registered with each different type signature
but using *the same* preconditions function (i.e., a preconditions function
with the same label).  All head or tail handler functions which are registered
using the same preconditions function are treated as being overloaded if their
type signatures differ.  Only the last-registered handler function is actually
saved and used, but all the type information is saved and is used in resolving
the final signature (and in looking up evaluation functions, etc.)

.. topic:: Two ways to parse identifiers

   The Typped parser and lexer are dynamic; both can be updated on-the-fly.
   This flexibility allows for a different style of defining identifiers than
   is traditionally used.  Consider an example where function name
   identifiers are being parsed.  Assume that the language being parsed has
   some sort of definition mechanism where function names must be defined
   before they are used.  (The principle is more general, including cases
   where, say, functions and variables share the same namespace or for
   kinds of token other than identifiers.)
   
   In the traditional parser design a generic function-name identifier is
   defined for the lexer and any further processing is done by the parser, based
   on the actual string value found in the program text.  This allows for a
   fixed lexer to be used.  When the lexer is dynamic, though, it is possible
   to define a new token for each definition of an identifier.
   
   Suppose we have functions ``add`` and ``exp``.  In the traditional approach
   the lexer would identify each as a function name identifier, and return that
   information along with the actual text string.  In the dynamic approach you
   would define a new token for ``exp`` at the time it is defined (and might
   not even need a general identifier token).  Similarly for the ``add``
   function.  The lexer would then return a unique token for each function,
   pushing some of the parsing down to the lexer level.

   An advantage of the dynamic approach is that it can help to avoid
   ambiguities in parsing complex languages.  The disadvantages are that it may
   take more space to define the new tokens, it may be slower to parse with so
   many possible tokens, and the function names (and hence their tokens) must
   be defined before being used.

   Recall that Pratt parsers are based on tokens (rather than production rules
   in a grammar like recursive descent).  Defining a new token type for each
   function name opens some possibilities.  This is especially true in the
   Typped package where type signature information is also stored with the
   tokens.

   In order for Typped type-checking to work on functions, functions with
   different signatures (ignoring overloading) must be handled by the different
   handler functions.  This requires either 1) a different precondition for
   each such function, or 2) a different token for each such function.  The
   latter is sometimes easier.  (The same holds for using function overloading,
   except that the *same* handler must be used for each overload redefinition.
   By the definition of overloading, the function overloads parse the same; the
   actual argument types must be examined to resolve the overload.)
  
   While there are still some disadvantages, the Typped lexer is designed to
   efficiently scan large numbers of tokens provided they have a simple
   pattern.  The patterns (currently restricted to fixed strings for this
   speedup) are stored in a trie data structure and are essentially all
   scanned in parallel.  The dynamic approach can also reduce the need to
   define preconditions functions for more-generic handlers (such as for
   looking at the string value for a token in a precondition).  It can also
   help avoid problems with overloading.

   So, while the Typped parser can be used in either way, the use of dynamic
   token definitions is worth considering.

.. topic:: Overloading summary:

   Overloading is assumed and automatically used in cases where a head (tail)
   handler function is registered with a given token via ``modify_token`` and
   the following conditions hold:

   1. The associated preconditions function has previously been associated with
      a different head (tail) handler function that is currently registered
      with the token.  Remember that preconditions functions are considered
      identical iff their labels are identical.

   2. The associated type specification is distinct from all all the type
      specifications currently associated with the handler function.

   The last-registered handler function object is the one that is stored and
   called.  All type specs are saved with the current handler function to be
   resolved handler is called.  Then the original formal signature that matches
   is used to look up any evaluation function or AST data in a dict attribute
   of the token keyed on the precondition label (for a given token precondition
   labels are one-to-one with handlers) and the signature.

.. topic:: Overloading versus preconditions functions

   An alternative way to implement overloading might be to always use a unique
   preconditions function label for each head and tail handler --- perhaps by
   appending a string representation of the type to the label.  Using unique
   preconditions functions would then result in a unique type signature being
   associated with each registered preconditions function.  But this also
   complicates the resolution of handler functions.
  
   Preconditions functions as currently implemented must be uniquely resolvable
   at parse-time.  They then uniquely determine the handler function to call.
   If different preconditions labels are used for overloading then overloading
   will cause multiple preconditions functions to match.  These ties will not
   be uniquely resolvable by a priority system.  The expression must be parsed
   to find the actual types, which requires a handler function.
   
   One approach might be to assume that all the corresponding handler functions
   are identical in case of ties and just pick one to call, but that could mask
   some error conditions.  Since type information is only resolved after the
   handler functions are called, the actual types and matching formal signature
   would need to be found after calling the handler.  Then the associated
   evaluation function and AST data would still need to then be selected from
   among the collection of matching handlers.  It seems simpler to just treat
   overloading and preconditions as separate and store all the overloaded
   signatures for a handler (and their associated information) a common
   handler.
   
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
juxtaposition operator is being used, for example, then and expression like
``pi (x+y)`` can cause problems with the usual method.  The name ``pi`` might
be a constant or a function name.  (At the least the left paren tail handler
would need to be conditioned on a space occurring before it, but this example
takes a different approach.)

By using a precondition that the lookahead token be a left paren with no
intervening space the head handler for a standard function identifier can parse
the whole subexpression rather than waiting to be picked up as an "argument" to
the infix left paren operator.  A second, lower-priority default head handler
can still be defined for all other identifiers.  (Other preconditions can also
be placed on other head handlers for identifiers).  These two head handler
definitions are largely independent, except via their respective priorities.
They can occur in different sections of code, where the different constructs
are defined.  Both handlers are registered for the identifier token, and the
rest is handled automatically.

The code for this example can be found in a runnable form in the file
``example_stdfun_lookahead.py``.

In this example the ``PrattParser`` class is extended by creating a subclass
with additional methods.  It is not strictly necessary to create a subclass,
however.  An ordinary function could be used, just renaming the ``self``
variable to something like ``parser`` and then explicitly passing in a parser
instance when calling it.  Extending the class has the advantage that the newer
methods are accessed in the same way as the built-in ones and are in the parser
instance's namespace.

In this example the method ``def_stdfun_lookahead`` is added to the
``PrattParser``.  (This is only an example, since the ``PrattParser`` class
already has a ``def_stdfun`` method which uses lookahead and also incorporates
types, etc.)  Before calling this method all of the tokens involved must have
already been defined along with their labels (via the ``def_token`` method).
Ignored whitespace tokens must also have been defined already.  The lpar, rpar,
and comma tokens must already have been defined as literal tokens (via the
``def_literal`` method).

Recall that the head-handler will be called to process a subexpression starting
from the beginning.  That head-handler is then responsible for parsing the full
subexpression -- though it can itself call ``recursive_parse`` to parse
sub-subexpressions.  We are defining a head-handler that only matches a
function name in the case when the peek token is an lpar with no intervening
space.

.. TODO: Keep up-to-date with the code in latest version from Python file
   ``example_stdfun_lookahead.py``  Maybe add more tests
   (maybe as a pytest file).

.. code-block:: python

   class MyParser(PrattParser):
       """Subclass and add a new method to the `PrattParser` class as an example."""

       def __init__(self, *args, **kwargs):
           """Call the superclass initializer."""
           super(MyParser, self).__init__(*args, **kwargs)

       def def_stdfun_lookahead(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label, num_args,
                      precond_priority=1):
           """Define a standard function with a fixed number of arguments."""

           # Define the preconditions function and a unique label for it.
           def preconditions(lex, lookbehind):
               # Note that helper functions like `match_next` could also be used.
               peek_tok = lex.peek()
               if peek_tok.ignored_before: return False
               if peek_tok.token_label != lpar_token_label: return False
               return True
           precond_label = "lpar after, no whitespace between" # Some unique label.

           # Define the head-handler function.
           def head_handler(tok, lex):
               # Below match is for a precondition, so it will match and consume.
               lex.match_next(lpar_token_label, raise_on_fail=True)

               # Read comma-separated subexpressions as arguments.
               for i in range(num_args-1):
                   tok.append_children(tok.recursive_parse(0))
                   lex.match_next(comma_token_label, raise_on_fail=True)
                   lex.match_next(rpar_token_label, raise_on_true=True) # Error.
               if num_args != 0:
                   tok.append_children(tok.recursive_parse(0))
               lex.match_next(rpar_token_label, raise_on_fail=True)

               # Always call this function at the end of a handler function.
               tok.process_and_check_node(head_handler)
               return tok

           # Register the handler function with the token, associated with the
           # preconditions function.
           self.modify_token(fname_token_label, prec=0,
                             head=head_handler,
                             precond_label=precond_label,
                             precond_fun=preconditions,
                             precond_priority=precond_priority)

In parsing the full function call the handler defined above uses both the
helper function ``match_next`` as well as calls to the lexer and
``recursive_parse``.  The general rule is that tokens which will appear in the
final parse tree, even literal tokens, should always be retrieved with
``recursive_parse``.  This is because it processes the nodes to adds some extra
attributes which are needed by other tree operations.  Tokens which do not
appear in the final parse tree, such as the final closing rpar token of the
function arguments, can simply be consumed by ``match_next`` or an explicit
call to ``lex.next()`` and discarded.  (If you must include a directly-consumed
token in the tree, it must at least have its ``process_and_check_node`` method
called with an overridden type signature to mimic what the handler for literal
tokens does.)

The function defined above could be called as follows.

.. code-block:: python

    parser = MyParser()
    parser.def_default_whitespace()

    tokens = [("k_number", r"\d+"),
              ("k_lpar", r"\("),
              ("k_rpar", r"\)"),
              ("k_comma", r","),
              ("k_add", r"add"),
              ("k_sub", r"sub"),
             ]
    parser.def_multi_tokens(tokens)

    literals = [("k_number"),
                ("k_lpar"),
                ("k_rpar"),
               ]
    parser.def_multi_literals(literals)

    parser.def_stdfun("k_add", "k_lpar", "k_rpar", "k_comma", 2)
    parser.def_stdfun("k_sub", "k_lpar", "k_rpar", "k_comma", 2)

    print(parser.parse("add(4, sub(5, 6)").tree_repr())

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

Implementation details
----------------------

Some of the low-level implementation details are discussed on the linked page
below.  Most users will not need to be concerned with these.

   :doc:`dispatching_implementation_details`

