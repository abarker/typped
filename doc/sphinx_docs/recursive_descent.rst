Using preconditions for recursive descent parsing
=================================================

This section discusses some similarities and differences between Pratt parsing
with conditioned dispatching and recursive descent parsing.  It also discusses
ways to use a Pratt-style parsing to do the same thing.  Of course recursive
descent parsing is not all that difficult with a good lexer; it is possible to
just implement a traditional recursive descent parser with functions calling
the lexer, and then pass that lexer to a `PrattParser` instance to parse certain
subexpression.

Similarities and differences
----------------------------

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
-------

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

