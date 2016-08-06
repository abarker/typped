
Preconditioned dispatching
==========================

In the usual Pratt parser each token has a fixed head and/or tail handler
function associated with it.  In this generalization, each token can have
multiple possible head and/or tail handler functions associated with it.  At
parse-time the choice of which of the possible handler functions is made based
on the conditions at the time (e.g., the peek token in the lexer).  This feature
is optional and can easily be ignored to use traditional Pratt parser
techniques.

Instead of calling the head or tail handlers for a token directly, the
``recursive_parse`` function instead calls ``dispatch_and_call_handler`` (which
is also passed information on whether to call a head or tail handler).  That
function goes down a list of boolean-valued precondition-testing functions
which have been registered for the current token, each associated with a
particular handler function.  The preconditions associated with any head or
tail handler function are defined by the user when the handler function itself
is defined, along with a priority value to be used for break ties.  The handler
function associated with the highest-priority precondition-testing function
which evaluates to true in the current conditions is chosen to handle the
token in the given context, and is called.

Preconditioned dispatching is only a slight generalization of the usual Pratt
parser.  A similar thing could be accomplished with ordinary head and tail
functions via a case statement inside each one, performing different actions
based on the conditions at the time and ordered in the case statement by
priority.  An advantage of using function dispatching instead is that it allows
for modularity in defining the head and tail handlers for a particular kind of
token.  The overall case statement in a handler function is essentially split
up, so each part can be defined in the place where that syntactic construct is
generally being defined, rather than having to be placed in one centralized and
separate location.  This makes it easier to create essentially independent
functional interfaces for different syntactical constructs.  For example,
functions are defined to easily perform common syntax-related tasks such as
defining an infix operator, define a grouping operator, define a standard
function, etc.

As an example of dispatching, the usual way to parse function evaluations
``f(x)`` in a Pratt parser is to define a tail for the left-paren token.  The
head for left paren is then called for grouping parentheses, and the tail is
called for function evaluations after the head for the identifier ``f``.  But
this can get complicated in more complex grammars where left paren is used in
various contexts.  Using lookahead, a function evaluation can be parsed by
defining a head handler for identifiers with a precondition that it be followed
by an lpar with no space in-between.  A second, lower-priority default head
handler can also be defined for all other identifiers.  (Other preconditions
can also be placed on other head handlers for identifiers).  These two head
handler definitions are largely independent (except for their priorities), and
can occur in different sections of code.  They are both registered for the
identifier token, and the rest is handled automatically.

The typing system which is implemented in this parser is also based on the
preconditioned dispatching design.  Type-signature information is associated
with each particular handler function, i.e., with the particular function
chosen and dispatched as the head or tail handler.  Consider the above example.
When types are defined for functions the function names should be made into
individual tokens in the lexer, rather than using a single identifier token for
all identifiers.  Then, when the token for ``f`` is processed, the expected
signature is also available.  The type system is discussed more below.

Uniqueness of preconditions functions
-------------------------------------

In order to avoid problems in determining when functions are identical,
every preconditions function must be associated with a unique label.
These functions are then registered in a dict using methods of the
`TokenSubclass` class.

These preconditions labels **define** identity or non-identity between
preconditions functions.  Handler functions registered using the same
preconditions function are treated as being overloaded if their type
signatures differ; otherwise it is taken as a redefinition.

Using preconditioning to define syntactic constructs
----------------------------------------------------

TODO, maybe stdfun using lookahead.

Implementation
--------------

This section contains some low-level implementation details and can be skipped
by most users of the Typped package.  TODO, move some of this to doc section of
the code file.

As far as the implementation of dispatching, the method
``dispatch_and_call_handler`` of ``TokenNode`` does the lookup and call of the
handler functions.  Most users will have no need to modify the basic parsing
routines ``parse`` and ``recursive_parse``.  Nevertheless, this is what the
code looks like when dispatching is used (though without the jop code, discussed
later):

TODO, update this code, changes made

.. code::

   def recursive_parse(lex, subexp_prec):
       curr_token = lex.next()
       processed_left = curr_token.dispatch_and_call_handler(HEAD, lex)
       lookbehind = [processed_left]

       while lex.peek().prec() > subexp_prec:
           curr_token = lex.next()
           processed_left = curr_token.dispatch_and_call_handler(
                                  TAIL, lex, processed_left, lookbehind)
           lookbehind.append(processed_left)

The lookup is performed by getting the list of precondition functions, ordered
by priority, and calling each one until one returns ``True`` based on the
current conditions.  The associated handler function is then executed.

All the preconditions functions for a token label are stored in a static dict
attribute of the corresponding ``TokenNode`` subclass.  The dict is called
``preconditions_dict``.  There are functions to register functions and
unregister them, as well as use a parser-global dict.  This dict is keyed by
the unique labels required for unique preconditions functions.

The stored items in the dict are tuples containing the handler functions
themselves as well as other information, such as the precondition priority and
the associated handler function.

All the registered handler functions for a token label are also stored in a
static dict attribute of the corresponding ``TokenNode`` subclass (after being
passed into ``modify_token_subclass`` via keyword arguments).  The dict is
called ``handler_funs`` and is keyed by `HEAD` or `TAIL`.  For each type
of handler function, head or tail, there is a sorted list of items having
the following format::

     (precond_label, precond_fun, precond_priority, handler_fun)

Defined type signatures (possibly overloaded as a list) are stored as
attributes of the handler functions themselves.

Note that handler functions are in one-to-one correspondence with precondition
labels.  If it needs to have a unique handler function then it needs to have a
unique precondition label.  Evaluation functions, however, are saved with every
overloaded type signature associated with every handler function (i.e., the
Cartesian product of the two).

Using preconditions similarly to recursive descent parsing
----------------------------------------------------------

It is possible to use preconditions to fake a recursive descent parser for a
BNF or EBNF grammar.  For each production you need to know all of the tokens
which can start that production, as well as any required disambiguating
lookahead.  That is like the case statement or conditionals in the function
implementing a production in a recursive descent parser.  You maintain a stack
of states for the production being parsed, pushing and popping as defined
below.

To implement the parser for a production you define and register a head for each
type of token which can begin the production as a literal.  For the "or" cases
where a recursive call is immediately made you can implicitly define a head for
all tokens by setting a default token with only the production-state as the
precondition (TODO maybe).  Inside each head you process the relevant "or" cases
of the production.  To immediately do a recursive production evaluation you
push back the token which was read, change the production-state to the one you
want to read, and then call ``recursive_parse``.  That returns the parse tree
for the sub-production, and you can then continue to evaluate the production in
much the same way as for recursive descent.  At the end of each 

Consider this example of a very simple expression grammar (even though the
expression parts of grammars are better evaluated with Pratt-style parsing).
The ``identifier`` and ``number`` productions are assumed to be implemented as
tokens from the lexer.

.. productionlist::
   expression : ["+"|"-"] term {("+"|"-") term}
   term       : factor {("*"|"/") factor}
   factor     : `identifier` | `number` | "(" expression ")"

The production for ``expression`` would be a default head, and would always
execute in the state ``"expression"``.  It would be implemented by a loop.  The
loop first checks whether the current token is "+" or "-".  If not, the first
token would be pushed back.  Then the state ``"term"`` would be pushed on the
stack and ``recursive_parse`` would be called.  That returns a processed
subtree which is combined with any previous subtree to build the parse tree
as usual.

The implemention of the production for ``term`` would be similar to
``expression``.  Before returning, however, it should pop the state stack.

The ``factor`` production could be implemented either as a default or by
defining heads for the identifier, number, and left paren token types.  Each
such head should also pop the state stack before returning.

- Should you define these default things to not even read a token, maybe?
  Then no pushback and you use peek.

Lookbehind
----------

Another minor generalization to Pratt parsing is the use of "lookbehind"
information.  A Pratt parser can use lookahead information from the lexer in
defining preconditions, etc.  In some cases lookbehind information, looking at
the previous ``processed_left`` values for the current subexpression, could be
useful.  This is a simple modification, which has been implemented.  In the
``recursive_parse`` function, whenever the ``processed_left`` variable is
assigned a new value, the value is also appended to a list called
``lookbehind``.  That list is passed as an argument to all tail handler
functions in addition to the ``processed_left`` value.

Since the lookbehind tokens have already been processed they allow the
preconditions to make use of information such as resolved type information (not
just token label information).  Of course you already can look at the ``left``
variable in a tail handler and see the type of the subexpression for, say, the
type of the left operand of an operator.  If nothing else, the lookbehind list
tells you how many subexpressions preceed the current one (at its same level in
the recursion).

This is not a feature which will be commonly used, but it may have use cases.
Note, though, that the ``lookbehind`` list contains references and so the
previous values will generally be modified versions of what they were when they
were first appended to the list.

.. _References:

References
----------

Vaughan R. Pratt, "Top down operator precedence," 1973.  The original
article.  Paywalled at the ACM site.
http://dl.acm.org/citation.cfm?id=512931

Fredrik Lundh, July 2008.  Excellent explanation and good code examples
in Python.  http://effbot.org/zone/simple-top-down-parsing.htm Related
articles by Lundh on Pratt parsing and lexing with regexes:
http://effbot.org/zone/tdop-index.htm

Eli Bendersky, 1/2/2010.  An article based on Lundh's article above.  It
also uses Python.
http://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/

Douglas Crockford 2007-02-21, using JavaScript.
http://javascript.crockford.com/tdop/tdop.html

Bob Nystrom, 3/19/2011, using Java.
http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/

