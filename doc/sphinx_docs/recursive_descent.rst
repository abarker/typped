Recursive descent parsing in Typped
===================================

This section discusses recursive descent parsing and how it relates to Pratt
parsing and the Typped package.  Recursive descent is implemented in the Typped
parser via the use of preconditioned dispatching and the introduction of a new
kind of token.

The section begins with a comparision of the recursive descent parsing
algorithm with the Pratt parser algorithm using preconditioned dispatching.  A
new kind of token is introduced (the null-string token) which bridges over one
important gap between the two methods.  This leads to a discussion of how
recursive descent is implemented in Typped.

The Typped package provides a higher-level EBNF-like grammar specification
language, implemented by operator overloading, which makes it easy to implement
grammar-based languages or partial languages.  Readers who are only interested
in using the EBNF-like language can skip to :ref:`EBNF`.

It should be noted that recursive descent parsing is not all that difficult to
implement by hand, especially with a good lexer.  It is possible to implement a
traditional recursive descent parser using the same lexer as a ``PrattParser``
instance.  Then either the recursive descent parser can call the Pratt parser
to handle subexpressions, or the handlers of the Pratt parser can call the
recursive descent functions to parse sub-grammars.

.. warning::

   Some of these features are still experimental, as is the current partial
   implementation.

Similarities and differences between the parsing methods
--------------------------------------------------------

Pratt parsing is similar to recursive descent parsing in the sense that both
are top-down recursive methods.  Pratt parsing, however, is token-based while
recursive descent parsing is based on production rules in a grammar.  The
ability to dispatch handlers based on preconditions makes the Typped Pratt
parser even more similar to a recursive descent parser.  The terminal symbols
in a grammar are essentially the literal tokens in a Pratt parser.  The
discussion below assumes this correspondence, so terminals are essentially
defined by regular expressions (like the literal tokens in Typped).

Suppose all the productions in a grammar begin with a terminal symbol
(represented by the regular expression of some corresponding token).  This is
called a **right-regular grammar**.  A Pratt parser with preconditioned
dispatching can be used to directly implement recursive descent parsing on such
a grammar.  Since each rule in such a grammar begins with a terminal symbol,
corresponding to a literal token, the head-handler for each such literal token
can be set up to process the rule.  In the Typped dispatching terminology, the
literal token can be used to trigger a syntactic construct (containing the
handler function, a preconditions function, and related data) which will
then process the full production rule.

In order to implement the recursive descent part you can keep a stack of
production rule labels, updated to always have the current production rule on
the top.  In a ``PrattParser`` instance this stack is stored in a list
attribute called ``pstate``.  The construct for handling a production rule is
given a precondition that the top label in that stack is the label for the rule
it processes.  The head-handler functions in these constructs are also
responsible for updating the ``pstate`` stack as necessary.  Using this, along
with lookahead to the upcoming tokens in the preconditions, gives the power to
do recursive descent parsing of right regular grammars.

These head-handler functions effectively mimic the separate recursive functions
for each production rule.  They are triggered by literal token when the
preconditions match.  The ``pstate`` stack keeps track of the recursion in the
tree defined by the grammar.  Either a single one can handle all the cases of
the nonterminal, or separate ones can be triggered to handle the cases
separately.

In extending this approach to general recursive descent, a problem arises when
a production starts with a nonterminal symbol.  Nonterminals do not correspond
to a tokens like they do with terminals.  So there is no token to trigger the
construct for parsing the rule.  To deal with this, the Typped package has an
experimental feature called a **null-string token**.  This is a special token
which acts as though its regular expression "matches the null string" just
before any upcoming text.  Other than that it is a regular token, with
handlers, preconditions, etc.

This is implemented in the ``recursive_parse`` function.  Before each call to
``next`` to get the next token from the the lexer it first checks to see if the
null-string token is defined for the parser instance.  If so, it checks whether
the preconditions of any registered null-string-triggered constructs match.  If
they do then the special null-string token is returned as the current token,
and the handler function of the winning construct is called to process the next
subexpression.

The use of null-string tokens combined with keeping a stack of production rule
state-labels is enough to implement general recursive descent parsing within
the framework of a Pratt parser with preconditioned dispatching.  The method is
essentially the same as described earlier for right-regular grammars.  You just
use the null-string token to recognize the production rules that start with
nonterminals (using the top of the ``pstate`` stack in preconditions).

The current implementation of null-string tokens is not especially efficient,
but there no penalty if you do not use them.  For many applications it should
be fast enough.  There are various ways the performance can be improved in
later versions if necessary.

Example
-------

Consider this example of a very simple expression grammar in EBNF (even though
the expression parts of a grammar might be better evaluated with Pratt-style
parsing).  The ``identifier`` and ``number`` productions are assumed to be
implemented as tokens from the lexer, defined by regular expressions.  Here the
square brackets are optional parts, and curly braces mean "zero or more." The
``(x|y)`` construct means either ``x`` or ``y``.

..
   TODO: consider this, especially w.r.t. associativity:
   http://homepage.divms.uiowa.edu/~jones/compiler/spring13/notes/10.shtml

.. productionlist::
   expression : ["+"|"-"] term {("+"|"-") term}
   term       : factor {("*"|"/") factor}
   factor     : `identifier` | `number` | "(" expression ")"

Initialy the ``pstate`` stack would only hold the string ``"expression"``.  A
head construct would be registered for the null-string token with the
precondition that the ``expression`` state be at the top of the ``pstate``
stack.  The head handler for the construct would first check whether the peek
token is "+" or "-".  If so, it would consume it.  Then the string ``"term"``
would be pushed onto the stack and ``recursive_parse`` would be called.  The
call to ``recursive_parse`` returns a processed subtree, which is incorporated
into the expression tree.  A loop would continue this way until the peek token
is not ``+`` or ``-``.

Another head construct would be registered for the null-string token with the
precondition that ``"term"`` be at the top of the stack.  Its head-handler
function would be responsible for parsing terms.  It would work in the same
general way as described above for expressions.

The ``factor`` production could be implemented either as a handler for the
null-string token or by separate constructs for the identifier, number, and
left-paren token types.

..
   TODO: consider also this version of the simple expression grammar.
   https://en.wikipedia.org/wiki/Syntax_diagram
   X
   .. productionlist::
   X
      expression : term | expression "+"  term;
      term       : factor | term "*"  factor;
      factor     : constant | variable | "("  expression  ")";
      variable   : "x" | "y" | "z"; 
      constant   : digit  {digit};
      digit      : "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

.. _EBNF:

Recursive descent with Typped's EBNF-like grammar
-------------------------------------------------

The Typped package comes with a EBNF grammar defined via Python overloads.
This essentially automates the procecure described above.

This is a simple example of using the EBNF grammar.

.. note::

   For now, see the test ``test_parsing_from_basic_expression_grammar`` in the
   test file ``test_production_rules.py``.  The current implementation is
   basically a proof-of-concept.

..
   TODO: Keep this example synced with the test file.

.. code-block:: python


When the grammar is "compiled" with respect to a ``PrattParser`` instance it
produces a recursive descent parser for the grammar within the Pratt parser
framework.  The generated parsers currently use full backtracking search, and
stop-sets are not yet implemented.

The EBNF language is currently bare-bones as far as what can be compile into a
parser instance.  (The EBNF language itself, defined with Python overloading,
is mostly implemented.)

For details of the current state of the Python EBNF language see the docs for
the module ``production_rules.py``.

.. TODO: add link to the production_rules.py file or wherever that documentation
   of the Python overloads ends up.

