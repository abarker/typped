
Preconditioned dispatching
==========================

This section describes the use of preconditioned dispatching in the
`PrattParser` class.  The parser comes with various built-in methods which
encapsulate the definition and use of preconditioning functions, so many Typped
users will never need to explicitly use the techniques described here.  See the
calulator example, for example, which uses only built-in methods.  This
description is mainly for those interested in the details of how it works and
for those who want or need to extend the built-in collection of methods.

What is preconditioned dispatching?
-----------------------------------

In the usual Pratt parser each token has a fixed head and/or tail handler
function associated with it.  In this generalization, each token can have
multiple possible head and/or tail handler functions associated with it.  At
parse-time the choice of which of the possible handler functions is made based
on the conditions at the time (for example, the peek token in the lexer).  This
feature is optional and can easily be ignored to use traditional Pratt parser
techniques.

The preconditions required for any head or tail handler function are defined by
the user by providing a function at the time when the handler function itself
is defined.  A priority value can also be provided, to be used in case of
multiple matches.  In cases where multiple preconditions functions (and
associated handlers) are registered for a token and they are not mutually
exclusive in their boolean result values precondition priorities **must** be
used.  Otherwise a runtime exception will be raised (since there will be no
clear winner among the choices in that situation).

So, instead of directly calling a fixed head or tail handler for a token, the
``recursive_parse`` function instead calls ``dispatch_handler`` (which is also
told whether fetch a head or tail handler).  This dispatching function goes
down a list of boolean-valued precondition-testing functions which have been
registered for the current token, each of which is associated with a particular
handler function.  The handler function associated with the highest-priority
precondition-testing function which evaluates to true in the current conditions
is chosen to handle the token in the given context.  The chosen handler
function is returned (with its arguments bound, since they are known) and is
then called in `recursive_parse`.  See the example below for an example of the
code.

.. note::

   Preconditioned dispatching is only a slight generalization of the usual
   Pratt parser.  A similar thing could be accomplished with ordinary head and
   tail functions via a case statement inside each one, performing different
   actions based on the conditions at the time and ordered in the case
   statement by priority.  An advantage of using function dispatching instead
   is that it allows for modularity in defining the head and tail handlers for
   a particular kind of token.  The overall case statement in a handler
   function is essentially split up, so each part can be defined in the place
   where that syntactic construct is generally being defined, rather than
   having to be placed in one centralized and separate location.  This makes it
   easier to create essentially independent functional interfaces for different
   syntactical constructs.  For example, the `PrattParser` class comes with
   methods predefined to easily perform common syntax-related tasks such as
   defining an infix operator, define a grouping operator, define a standard
   function, etc.

The typing system which is implemented in this parser is also based on the
preconditioned dispatching design.  Type-signature information is associated
with each particular handler function, i.e., with the particular function
chosen and dispatched as the head or tail handler.  The handler functions, in
turn, are associated with particular preconditions functions.  The type system
is discussed more in later sections.

Uniqueness of preconditions functions
-------------------------------------

Each preconditions function for a token is associated with a head and/or tail
handler function.  Any re-registration of a head or tail handler with *the same*
preconditions function results in overwriting of the previously-registered
handler.  (If the type signatures also differ then, to allow overloading, that
type information *is* saved but the handler function itself is overwritten.)  It
is important, then, to have a clear definition of when two functions are
considered equal.

In order to avoid problems in determining when functions are identical, every
preconditions function (or use of a preconditions function) **must** be assigned
a unique label.  These functions are then registered in a dict using methods of
the `TokenTable` class.

These preconditions labels **define** identity or non-identity between
preconditions functions.  Handler functions registered using the same
preconditions function are treated as being overloaded if their type signatures
differ; otherwise it is taken as a redefinition.

Example: Defining standard functions with lookahead
---------------------------------------------------

As an example of dispatching, consider the parsing of function evaluations
``f(x)`` in a Pratt parser.   The "usual" way is to define a tail for the
left-paren token.  Then that symbol acts like an infix operator with the
function name as its first argument and the function arguments and closing
paren as its second argument.  The head-handler for left paren is then called
for grouping parentheses, and the tail-handler is called for function
evaluations (after the token for ``f`` is read).  But this can lead to
complications in more-complex grammars where left paren is used in various
contexts.

By using a precondition that the lookahead token be a left parenthesis with no
intervening space, the handler for standard functions can instead be a
head-handler for the function-name token.  A second, lower-priority default
head handler can still be defined for all other identifiers.  (Other
preconditions can also be placed on other head handlers for identifiers).
These two head handler definitions are largely independent (except via their
respective priorities), and can occur in different sections of code.  They are
both registered for the identifier token, and the rest is handled
automatically.

The code for this example can be found in a runnable form in the file
`example_stdfun_lookahead.py`.  <TODO make link>

.. note::

   In this example the lexer could be set up to recognize the function name as
   a generic identifier token, and variables could be treated the same way.
   The precondition on the opening lpar for a function would differentiate
   them.  With the Typped parser, though, it is generally a good idea to make
   every function name into its own separate token type, if possible.  This is
   possible, when, for example, all functions in the language must be declared
   ahead of time.
   
   Having separate tokens for each name helps to avoid possible ambiguities
   which can arise in the interactions of multiple grammatical constructs in
   more-complex grammars.  It also helps with type-checking and overloading.

   In order for type-checking to work on functions, each function with the same
   signature must be handled by the same handler function.  This requires
   either 1) a different precondition for each one, or 2) a different token for
   each one.  The latter is generally easier.  The same holds for using
   function overloading, except that the *same* handler must be used for each
   overload redefinition.  (By the definition of overloading, the functions
   parse the same and the actual argument types must be examined to resolve the
   overload.)
  
   The Typped lexer is designed to efficiently deal with multiple token
   definitions of this sort.  It is dynamically modifiable, and it stores
   simple names in a trie so it can search them all in parallel.

In this example the `PrattParser` class is extended by creating a subclass with
additional methods.  It is not strictly necessary to create a subclass,
however.  You could use an ordinary function by just renaming the `self`
variable to something like `parser` and then explicitly pass in a parser
instance when calling it.  Extending the class has the advantage the newer
methods are accessed in the same way as the built-in ones.

In this example the method `def_stdfun` is added to the `PrattParser`.  Before
calling this method all of the tokens with passed-in labels must be defined
(via the `def_token` method), as must ignored whitespace.  The lpar, rpar, and
comma tokens must also be defined a literals (via the `def_literal` method).

Recall that the head-handler will be called to process a subexpression starting
from the beginning.  That head-handler is then responsible for parsing the full
subexpression -- though it can itself call `recursive_parse` to parse
sub-subexpressions.  We are defining a head-handler that only matches a
function name in the case when the peek token is an lpar with no intervening
space.

TODO: update code with latest version from Python file, after cleanup, etc.

.. code-block:: python

   class MyParser(PrattParser):
       """Add a new method to the `PrattParser` class as an example."""
       def __init__(self, *args, **kwargs):
            """Call the superclass initializer."""
           super(MyParser, self).__init__(*args, **kwargs)

       def def_stdfun(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label, num_args,
                      precond_priority=1):
           """Define a standard function with a fixed number of arguments."""
           # Define the preconditions function and a unique label for it.
           def preconditions(lex, lookbehind):
                peek_tok = lex.peek()
                if peek_tok.ignored_before(): return False
                if peek_tok.token_label != lpar_token_label: return False
                return True
           precond_label = "lpar after, no whitespace between" # Some unique label.

           def head_handler(tok, lex):
               # Below match is for a precondition, so it will match and consume.
               tok.match_next(lpar_token_label, raise_on_fail=True)

               # Read comma-separated subexpressions as arguments.
               for i in range(num_args-1):
                   tok.append_children(tok.recursive_parse(0))
                   tok.match_next(comma_token_label, raise_on_fail=True)
                   tok.match_next(rpar_token_label, raise_on_true=True) # Error.
               if num_args != 0:
                   tok.append_children(tok.recursive_parse(0))
               tok.match_next(rpar_token_label, raise_on_fail=True)
          
               # Always call this function at the end of a handler function.
               tok.process_and_check_node(head_handler)
               return tok

           # Always call this function to register handler funs with the token.
           self.modify_token_subclass(fname_token_label, prec=0,
                                      head=head_handler,
                                      precond_label=precond_label,
                                      precond_fun=preconditions,
                                      precond_priority=precond_priority)

In parsing the full function call the handler defined above uses both the
helper function `match_next` as well as calls to the lexer and
`recursive_parse`.  The general rule is that tokens which will appear in the
final parse tree, even literals, should always be retrieved with
`recursive_parse` (since it also processes the nodes and adds some extra
attributes).  Tokens which do not appear in the final parse tree, such as the
final closing rpar token of the function arguments, can simply be consumed by
`match_next` or an explicit call to `lex.next()` and discarded.  (If you must
include a directly-consumed token in the tree, it must at least have its
`process_and_check_node` method called with an overridden type signature to
mimic what the handler for literals does.)

The function defined above could be called as follows.  Note that literals in
the sense of the parser are any leaves (terminals) of the parse tree.

.. code-block:: python

    parser = MyParser()
    parser.def_token("k_space", r"[ \t]+", ignore=True) # note + NOT *
    parser.def_token("k_newline", r"[\n\f\r\v]+", ignore=True) # note + NOT

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

This example works, but is simplified from the actual `def_stdfun` method of
the Pratt parser class.  It assumes a fixed number of arguments and does not
make use of type data.  The function is still fairly general, though.  Note
that this function does not allow whitespace (ignored tokens) to occur between
the function name and the left parenthesis.  The preconditions function is
defined as a nested function, but it could alternately be passed in as another
argument to `def_stdfun`. 

Implementation
--------------

This section contains some low-level implementation details and can be skipped
by most users of the Typped package.

As far as the implementation of dispatching, the method ``dispatch_handler`` of
``TokenNode`` does the lookup and call of the handler functions.  Most users
will have no need to modify the basic parsing routines ``parse`` and
``recursive_parse``.  Nevertheless, this is what the code looks like when
dispatching is used.  It is a little simplified from the actual code in Typped
because it does not handler jops, null-string tokens, or error-checking.

.. code-block:: python

   def recursive_parse(subexp_prec):
       lex = self.token_table.lex
       curr_token = lex.next()
       head_handler = curr_token.dispatch_handler(HEAD, lex)
       processed_left = head_handler()
       lookbehind = [processed_left]

       while lex.peek().prec() > subexp_prec:
           curr_token = lex.next()
           tail_handler = curr_token.dispatch_handler(
                                  TAIL, lex, processed_left, lookbehind)
           processed_left = tail_handler()
           lookbehind.append(processed_left)

The lookup is performed by getting the list of precondition functions, ordered
by priority, and calling each one until one returns ``True`` based on the
current conditions.  The associated handler function is then executed.

The stored items in the dict are tuples containing the handler functions
themselves as well as other information, such as the precondition priority and
the associated handler function.

All the registered handler functions for a token label are stored in a static
dict attribute of the corresponding ``TokenNode`` subclass (after being passed
into ``modify_token_subclass`` via keyword arguments).  The dict is called
``handler_funs`` and is keyed by `HEAD` or `TAIL`.  For each type of handler
function, head or tail, there is an `OrderedDict` named tuples keyed by
precondition labels and having the following format::

     (precond_fun, precond_priority, handler_fun)

Each such ordered dict is sorted by the precondition priorities.

Internally, the preconditions functions for a token label are stored in a
static dict attribute of the corresponding ``TokenNode`` subclass called
``preconditions_dict``.  There are methods to register functions and
unregister them, as well as use a parser-global dict.  This dict is keyed by
the unique labels required for unique preconditions functions.

Defined type signatures (possibly overloaded, as a list) are stored as
attributes of the handler functions themselves.  Duplicates are not allowed,
and equality is defined by the `TypeSig` class' definition of `==`.  Note that
handler functions are in one-to-one correspondence with precondition labels
(possibly a default one if one is not specified), not overloaded signatures.
If something needs to have a unique handler function then it needs to have a
unique precondition label.  Evaluation functions, however, are saved with every
overloaded type signature associated with every handler function (i.e.,
one-to-one with the Cartesian product of the two).

