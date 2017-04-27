Recursive descent parsing in Typped
===================================

This section discusses recursive descent parsing and how it relates to Pratt
parsing and the Typped package.  Recursive descent is implemented in the Typped
parser by the use of conditioned dispatching and by the introduction of a new
kind of token.

The section starts with a comparision of the recursive descent parsing
algorithm with the Pratt parser algorithm using conditioned dispatching.  A new
token type is introduced which covers one important gap between the two.  This
leads to a discussion of how recursive descent is implemented in Typped.

The Typped package provides a higher-level EBNF grammar language, implemented
by operator overloading, which makes it easy to implement grammar-based
languages or partial languages.  This is discussed in a later section.

It should be noted that recursive descent parsing is not all that difficult to
implement by hand with a good lexer.  It is also possible to implement a
traditional recursive descent parser using the same lexer as a ``PrattParser``
instance.  That parser can then be called from handler functions to parse
subexpressions, or else the recursive descent parser can call the Pratt parser
to handle subexpressions.  That approach is not discussed here, but the method
that is discussed essentially automates such a process.

.. warning:: Some of these features are still experimental.

Similarities and differences between the parsing methods
--------------------------------------------------------

Pratt parsing is similar to recursive descent parsing in the sense that both
are top-down recursive methods.  Pratt parsing, however, is based on tokens
whereas recursive descent parsing is based on production rules in a grammar.
The ability to dispatch handlers based on preconditions makes the Typped Pratt
parser even more similar to a recursive descent parser.  The terminal symbols
in a grammar are essentially the literal tokens in a Pratt parser.  The below
discussion assumes this correspondence, so terminals are essentially defined by
regular expressions (as are the tokens in Typped).

Suppose all the productions in a grammar begin with a literal terminal
(represented by the regular expression of some corresponding token).  This is
called a **right-regular grammar**.  A Pratt parser with preconditioned
dispatching can be used to implement recursive descent parsing on such a
grammar.  Since each rule in such a grammar begins with a terminal
corresponding to a literal token the head-handler for each such token can be
set up to process the rule.

In order to do this you keep a stack of production rule labels, updated to have
the current production rule on the top.  In a ``PrattParser`` instance this
stack is stored in a list attribute called ``pstate``.  You can then use a
precondition on the top label in that stack in all the head-handlers for tokens
which start some production.  These head-handler functions are also responsible
for updating the ``pstate`` stack as necessary.  Using this, along with
lookahead to the upcoming tokens in the preconditions, gives the power to do
recursive descent parsing of right regular grammars.

These head-handler functions effectively mimic the separate recursive functions
for each production rule.  They are just triggered by preconditions matches.
The ``pstate`` stack keeps track of the recursion in the tree defined by the
grammar.  If the collection of production rules for a given terminal are
ordered by priority then the head handler for the token which starts the first
one can be used to loop through and process the whole collection (like in
recursive descent).

When a production does not necessarily start with a terminal then there is a
problem as far as how to apply a Pratt parser while keeping the grammar
structure.  Tokens do not correspond to a nonterminals like they do to
terminals.  To deal with this, the Typped package has an experimental feature
called a **null-string token**.  This is a special token which acts as though
its regular expression "matches the null string" before any upcoming text.
Other than that it is a regular token, with handlers, preconditions, etc.

This is implemented in the ``recursive_parse`` function.  Before each call to
``next`` to get the next token from the the lexer, it first checks to see if
the null-string token is defined and if any of its preconditions match.  If so,
then the special null-string token is made into the current token, and the
matching handler function is called to process the next subexpression.

The use of null-string tokens combined with keeping a stack of production
rule state-labels is enough to implement general recursive descent parsing
within the framework of a Pratt parser with dispatching.  The method is
essentially the same as described above for right-regular grammars.  You
just use the null-string token to recognize the production rules that
start with nonterminals.

The current implementation of null-string tokens is not especially efficient,
but there no penalty if you do not use them.  For many applications it should
be fast enough.  There are various ways the performance can be improved in
later versions if necessary.

Example
-------

Consider this example of a very simple expression grammar (even though the
expression parts of grammars are better evaluated with Pratt-style parsing).
The ``identifier`` and ``number`` productions are assumed to be implemented as
tokens from the lexer defined by regular expressions.

.. productionlist::
   expression : ["+"|"-"] term {("+"|"-") term}
   term       : factor {("*"|"/") factor}
   factor     : `identifier` | `number` | "(" expression ")"

Initialy the ``pstate`` stack only hold the string ``"expression"``.  A
head-handler would be registered for the null-string token with the
precondition that that state is at the top of the stack.  This head handler
would first check whether the peek token is "+" or "-".  If so it would consume
it.  Then the state ``"term"`` would be pushed on the stack and
``recursive_parse`` would be called.  If that returns a processed subtree it is
incorporated into the expression tree and the stack is popped.  A loop would
continue this way until the preconditions fail.

The head-handler for the null-string token would have another head handler
registered with it using the precondition that ``"term"`` is on the top of the
stack.  This would be responsible for parsing terms.  It would work in the same
general way that was described for expressions above.

The ``factor`` production could be implemented either as a handler for the
null-string token or by head handlers for the identifier, number, and left
paren token types.

TODO: consider also this version of the simple expression grammar.
https://en.wikipedia.org/wiki/Syntax_diagram

.. productionlist::

   expression : term | expression "+"  term;
   term       : factor | term "*"  factor;
   factor     : constant | variable | "("  expression  ")";
   variable   : "x" | "y" | "z"; 
   constant   : digit  {digit};
   digit      : "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

Recursive descent with Typped's EBNF grammar
--------------------------------------------

The Typped package comes with a EBNF grammar defined via Python overloads.
This essentially automates the procecure described above.

This is a simple example of using the EBNF grammar:

.. code-block:: python


When the grammar is "compiled" with respect to a ``PrattParser`` instance it
produces a recursive descent parser for the grammar within the Pratt parser
framework.  The generated parsers currently use full backtracking search, and
stop-sets are not yet implemented.

The EBNF language is currently bare-bones as far as what can be compile into a
parser instance.  (The EBNF language itself, defined with Python overloading,
is mostly written.)

For details of the current state of the Python EBNF language see the docs for
the module ``production_rules.py``.

.. TODO: add link to the production_rules.py file or wherever that documentation
   of the Python overloads ends up.

