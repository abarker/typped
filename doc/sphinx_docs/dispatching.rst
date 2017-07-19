
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
ignored if one only wants to use standard Pratt parser techniques.

Since in Pratt parsing each head and tail handler essentially parses a
different part of the grammar, our abstraction of handler functions is called a
**syntactic construct**, or simply a **construct**.  A construct represents a
particular kind of grammatical subexpression that is parsed and returned by a
handler function.  Since Pratt parsers are top-down these grammatical parts
tend to correspond to subtrees of the final expression tree.  A construct
containing a head handler will be called a **head construct** and a construct
containing a tail handler will be called a **tail construct**.  Constructs also
contain other attributes, as we will see.

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
* a unique string label for the construct
* other data, such as evaluation functions and type signatures

Constructs are **registered** with a parser instance in order to define a
particular grammar on the tokens of the language (which must have been
previously defined).  The preconditions priority is a number which defaults to
zero.

Whenever the ``recursive_parse`` routine consumes a particular kind of token
from the lexer, in a head or tail position, it sequentially executes the
preconditions functions for all the constructs triggered by with that kind of
token, in that position.  The execution sequence is ordered by the
preconditions priorities.  The construct associated with the first matching
preconditions function is selected.  Its handler function is then dispatched as
the one to be run by the ``recursive_parse`` routine.  If there is no clear
winner among the highest-priority matching preconditions functions (i.e., more
than one match has the highest priority) then an exception is raised.

This algorithm clearly reduces to ordinary Pratt parsing in the case where
there is at most one head construct and one tail construct per kind of token
and the preconditions functions always evaluate as true.

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
tail handler.  This function selects a construct, as described above, and
returns the handler function (actually a wrapper function that first runs the
handler and then does type checking on the returned subtree).  For convenience
the arguments to the handler are bound, since they are already known.

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

A construct must always have a string label associated with it.  Equality or
non-equality of two constructs for a given token in a give position (head or
tail) is *defined* to be equality of their labels.  This is used to determine
when a construct is being redefined.

When a construct is redefined and is passed the same type signature as the
previous definition the new definition simply overwrites the old one.  When the
type signatures of the two types are different, though, the construct is
assumed to be overloaded based on types.

Recall that function overloading based on argument types is used for
syntactical constructs which parse the same (i.e., using the same handler
function) but are then resolved into different things based on the actual types
of the arguments (and possibly the return value).

Redefining a construct for a given token and position acts like overwriting in
the sense that only the new handler, preconditions function, and preconditions
priority is saved.  In this case, though, any previous type signatures and any
data associated with those signatures (such as evaluation functions) is saved
along with the new ones.

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

   TODO --> Below is a case for overload on value, too.  Just save a dict
   mapping values to signatures and look up that way.

   In order to use the built-in Typped type checking a single construct
   (triggered by, say, the function identifier token) cannot be used to parse
   all functions.  That is because the association of function-name values to
   type signatures and evaluation functions would be lost.  You would need to
   define a unique new construct for each one by changing the construct name
   slightly on each definition (such as by appending the function-name value to
   it).  This new construct would need a precondition of seeing the actual
   function name as the token value to avoid ties with the other such
   constructs.

   An alternative approach is to dynamically define a different token for each
   such function.  Then separate constructs would result from each definition
   because the triggering token for each one would be different.  This is
   sometimes easier.
   
   While there are some disadvantages to defining many tokens, the Typped lexer
   is designed to efficiently scan large numbers of tokens provided they have a
   simple pattern.  Simple patterns (currently restricted to fixed strings for
   this speedup) are stored in a trie data structure and are essentially all
   scanned in parallel by walking down the trie.  The insert and delete time is
   linear in the pattern length.  So, while the Typped parser can be used in
   either way, the use of dynamic token definitions is worth considering.

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
``example_stdfun_lookahead.py``.

In this example the ``PrattParser`` class is extended by creating a subclass
with additional methods.  It is not strictly necessary to create a subclass,
however.  An ordinary function can also be used just by renaming the ``self``
variable to something like ``parser`` and then explicitly passing in a parser
instance when calling it.  Extending the class has the advantage that the newer
methods are accessed in the same way as the built-in ones and can be easily
accessed in the parser instance's namespace.

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

           # Register the construct with the parser.
           construct_label = "parse function with lpar, no space after name"
           self.def_construct(fname_token_label, prec=0,
                              head=head_handler,
                              construct_label=construct_label,
                              precond_fun=preconditions,
                              precond_priority=precond_priority)

In parsing the full function call the handler defined above uses both the
helper function ``match_next`` as well as calls to the lexer and
``recursive_parse``.  Generally, tokens which will appear in the final parse
tree, even literal tokens, should be retrieved with ``recursive_parse``.  That
is because it peforms some extra processing the nodes such as setting their
actual types.  Tokens which do not appear in the final parse tree, such as the
final closing rpar token of the function arguments, can simply be consumed by
``match_next`` or an explicit call to ``lex.next()`` and discarded.

The function defined above could be called as follows:

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
   
   To resolve an overload with multiple constructs the expression must first be
   parsed to find the actual types.  This requires a handler function, which is
   circular since the construct determines the handler.  One approach might be
   to assume that all the corresponding handler functions are identical in case
   of ties and just pick one to call, but that could mask some error
   conditions.  The associated evaluation function and AST data would still
   need to be selected from among the collection of matching constructs.  It
   seems simpler to just to store all the overloaded signatures and their
   associated data with a construct.

