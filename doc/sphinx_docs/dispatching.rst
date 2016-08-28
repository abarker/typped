
Preconditioned dispatching
==========================

In this section we describe the use of preconditioned dispatching in the
`PrattParser` class.  The parser comes with various built-in methods which
encapsulate the definition and use of preconditioning functions, so many Typped
users will never need to use the techniques described here.  See the calulator
example, for example, which uses only built-in methods.  This description is
mainly for those interested in the details of how it works and for those who
want to extend the built-in collection of methods.

What is preconditioned dispatching?
-----------------------------------

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
signature is also available.  The type system is discussed more in later sections.

Uniqueness of preconditions functions
-------------------------------------

In order to avoid problems in determining when functions are identical,
every preconditions function must be associated with a unique label.
These functions are then registered in a dict using methods of the
`TokenTable` class.

These preconditions labels **define** identity or non-identity between
preconditions functions.  Handler functions registered using the same
preconditions function are treated as being overloaded if their type
signatures differ; otherwise it is taken as a redefinition.

Example: Defining standard functions with lookahead
---------------------------------------------------

TODO: note that the builtins are pretty powerful, so many user will not
need to subclass PrattParser.

A common way to define standard function syntax in a Pratt parser is to
define a tail handler for the left parenthesis.  Then that symbol acts like
an infix operator with the function name as its first argument and the function
arguments and closing paren as its second argument.  With preconditioned
dispatching it is possible to define a standard function by using lookahead
in the lexer, looking for the left parenthesis.

This example works, but is simplified from the actual `def_stdfun` method of
the Pratt parser class.  It assumes a fixed number of arguments and does not
make use of type data.  The function is still fairly general, though.  Note
that this function does not allow whitespace (ignored tokens) to occur between
the function name and the left parenthesis.  The preconditions function is
defined as a nested function, but it could also be passed in as another
argument. 

TODO: maybe make non-subclass version the example.... less confusing to newbies.

TODO: the whole extending_prattparser.rst file may be moot if the example here
just does it.... consolidate somehow (not much in that file yet, anyway)

In this example the `PrattParser` class is extended by defining a subclass with
additional methods.  It is not strictly necessary to create a subclass,
however.  You would just rename the `self` variable to something like `parser`
and explicitly pass in a parser instance when calling it.  Note that the
example file `example_stdfun_lookahead` has both versions.  Extending the
class has the advantage the the newer methods are accessed in the same way
as the previous ones. ::

     class MyParser(PrattParser):
        """Add a new method to the `PrattParser` class as an example."""
        def __init__(self, *args, **kwargs):
            super(MyParser, self).__init__(*args, **kwargs)

        def def_stdfun(self, fname_token_label, lpar_token_label,
                       rpar_token_label, comma_token_label, num_args,
                       precond_priority=1):
              
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
            if num_args != 0:
                tok.append_children(tok.recursive_parse(0))
            tok.match_next(rpar_token_label, raise_on_fail=True)
            
            # Always call this function at the end of a handler function.
            tok.process_and_check_node(head_handler)
            return tok

        # Always call this function to register a handler function with the token.
        return self.modify_token_subclass(fname_token_label, prec=0,
                                   head=head_handler,
                                   precond_label=precond_label,
                                   precond_fun=preconditions,
                                   precond_priority=precond_priority)

TODO: get the current version of both of these, and mention example file to
download.

The function defined above could be called as follows.  Note that literals in
the sense of the parser are any leaves (terminals) of the parse tree. ::

    parser = MyParser()
    parser.def_token("k_space", r"[ \t]+", ignore=True) # note + NOT *
    parser.def_token("k_newline", r"[\n\f\r\v]+", ignore=True) # note + NOT
    tokens = [("k_number", r"\d+"),
              ("k_lpar", r"\("),
              ("k_rpar", r"\)"),
              ("k_add"),
              ("k_sub"),
             ]
    parser.def_multi_tokens(tokens)
    literals = [("k_number"),
                ("k_lpar"),
                ("k_rpar"),
               ]
    parser.def_multi_literals(literals)

    parser.def_stdfun("k_add", "k_lpar", "k_rpar", "k_comma", 2)
    parser.def_stdfun("k_sub", "k_lpar", "k_rpar", "k_comma", 2)

    print(parser.parse("add(4, sub(5, 6)"))

Implementation
--------------

TODO
   The implementation changed a little... now there is a dispatch_handler function
   which returns the function (with all arguments bound) but doesn't call it.
   So then you need to explicitly call it, too, with no arguments.

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

Using preconditions similarly to recursive descent parsing
----------------------------------------------------------

This section discusses some similiarities and differences between Pratt parsing
with conditioned dispatching and recursive descent parsing.  It also discusses
ways to use a Pratt-style parsing to do the same thing.  Of course recursive
descent parsing is not all that difficult with a good lexer; it is possible to
just implement a traditional recursive descent parser with functions calling
the lexer, and then pass that lexer to a `PrattParser` instance to parse certain
subexpression.

Similarities and differences
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pratt parsing is similar to recursive descent parsing in the sense that both
are top-down recursive methods.  Pratt parsing is just based on tokens whereas
recursive descent parsing is based on production rules in a grammar.  The use
of dispatched handlers based on preconditions makes a Pratt parser even more
similar to a recursive descent parser.

If all the productions in a grammar begin with some literal (such as in a
regular grammar) then a Pratt parser with preconditioned dispatching can
be used to implement it.  Each rule begins with a token, which can be set with
the head handler to process the rule.  You keep a stack of states and use
that along with lookahead in the preconditions.  This effectively mimics
separate recursive functions for each production rule (with the code now
in head handler functions).  Precondition preferences can be used to mimic
left-to-right evaluation of combined productions, containing "or" symbols.

When a production does not necessarily start with literal then there is a
problem as far as how to apply a Pratt parser while keeping the grammar
structure.  To help deal with this, the Typped has an experimental feature
called **null-string tokens**.  These are tokens that match the null string.
Before each call to `next` in the lexer to get a token the parser first checks
to see if any null-string tokens match.  If so, then the special null-string
token is made into the current token, and the matching handler function is
called to process the next subexpression.

The experimental implementation of null-string tokens is currently not very
efficient, though there no penalty if you do not use them.  In many cases
efficiency is not all that important.  If the feature turns out to be useful
there are various ways to optimize it.

Example
~~~~~~~

We will assume that the stack is in a list called `pstack`, and holds string
labels for the names of the productions.

To implement the parser for a production you define and register a head handler
for each type of token which can begin the production as a literal.  For the
"or" cases you can either define a separate head for each disjunct in the
production, or you can use "or" conditionals inside a single precondition
function for a single head function.  Inside each head you process the relevant
case or cases of the production.

Note that some productions immediately do a recursive production evaluation.
For those case you can push back the token which was read, change the
production-state to the one you want to process, and then call
``recursive_parse``.  That returns the parse tree for the sub-production, with
which you can continue to evaluate the production in much the same way as for
recursive descent.

As a possible idea for the "or" cases where a recursive call is immediately,
made you can implicitly define a head for all tokens by setting a default token
with only the production-state as the precondition (TODO maybe).  Could these
handle the general recursive descent in a better way?  Just define with
preconditions based on the top label in the production stack....

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

