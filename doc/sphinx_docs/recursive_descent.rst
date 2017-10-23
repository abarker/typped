Recursive descent parsing in Typped
===================================

This section discusses recursive descent parsing and how it relates to Pratt
parsing and the Typped package.  Recursive descent is implemented in the Typped
parser via the use of preconditioned dispatching and the introduction of a new
kind of token.

The section begins with a comparison of the recursive descent parsing
algorithm with the Pratt parser algorithm using preconditioned dispatching.  A
new kind of token is introduced (the null-string token) which bridges over one
important gap between the two methods.  This leads to a discussion of how
recursive descent is implemented in Typped.

The Typped package provides a higher-level EBNF-like grammar specification
language, implemented by operator overloading, which makes it easy to implement
grammar-based languages or partial languages.  Readers who are only interested
in using the EBNF-like language can skip to ":ref:`EBNF`."

It should be noted that recursive descent parsing is not all that difficult to
implement by hand, especially with a good lexer.  It is possible to implement a
traditional recursive descent parser using the same lexer as a ``PrattParser``
instance.  Then either the recursive descent parser can call the Pratt parser
to handle subexpressions, or the handlers of the Pratt parser can call the
recursive descent functions to parse sub-grammars.

.. note::

   Some of these features are still experimental.  Some features are only
   partially implemented.  Only basic BNF currently works in the grammar
   parser.  Type-checking and Pratt-style precedences in the grammars are not
   yet implemented.  The implementation still needs to be optimized.

Similarities and differences between the parsing methods
--------------------------------------------------------

Pratt parsing is similar to recursive descent parsing in the sense that both
are top-down recursive methods.  Pratt parsing, however, is token-based while
recursive descent parsing is based on production rules in a grammar.  The
ability to dispatch handlers based on preconditions makes the Typped Pratt
parser even more similar to a recursive descent parser.  The terminal symbols
in a grammar are the token literals in a Pratt parser.  The discussion below
assumes this correspondence, so terminals are defined by the regular
expressions in the definition of the literal tokens.

Suppose all the productions in a grammar begin with a terminal symbol
(represented by the regular expression of some corresponding token).  This is
called a **right-regular grammar**.  A Pratt parser with preconditioned
dispatching can be used to directly implement recursive descent parsing on such
a grammar.  Since each rule in such a grammar begins with a terminal symbol,
corresponding to a token literal, the head-handler triggered by each such
literal token can be set up to process the rule.  In the Typped dispatching
terminology, the literal token can be used to trigger a syntactic construct
(containing the handler function, a preconditions function, and related data)
which will then process the full production rule.

In order to implement the recursive descent part you can keep a stack of
production rule labels, updated to always have the current production rule on
the top.  In a ``PrattParser`` instance this stack is stored in a list
attribute called ``pstate``.  The construct for handling a production rule is
given a precondition that the top label in that stack is the label for the rule
it processes.  The head-handler functions in these constructs are also
responsible for updating the ``pstate`` stack as necessary.  Using this, along
with lookahead to the upcoming tokens in the preconditions, gives the power to
do recursive descent parsing of right regular grammars.

These head-handler functions mimic the separate recursive functions for each
production rule in recursive descent.  They are triggered by the corresponding
literal tokens when the preconditions match (or in general by first-set
elements).  The ``pstate`` stack keeps track of the recursion in the tree
defined  by the grammar.  Either a single one can handle all the cases of the
nonterminal, or separate ones can be triggered to handle the cases separately.

In extending this approach to general recursive descent, a problem arises when
a production starts with a nonterminal symbol.  Nonterminals do not correspond
to a tokens like they do with terminals.  So there is no token to trigger the
construct for parsing the rule.  To deal with this, the Typped package has an
experimental feature called a **null-string token**.  This is a special token
which acts as though its regular expression "matches the null string" just
before any upcoming text.  Other than that it is a regular token, with
handlers, preconditions, etc.

This is implemented in the ``recursive_parse`` function.  Before each call to
``next`` to get the next token from the lexer it first checks to see if the
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

Example
-------

Consider a very simple expression grammar in EBNF (even though the expression
parts of a grammar might be better evaluated with Pratt-style parsing).  The
``identifier`` and ``number`` productions are assumed to be implemented as
tokens from the lexer, defined by regular expressions.  Here the square
brackets are optional parts, and curly braces mean "zero or more." The
``(x|y)`` construct here means either ``x`` or ``y``.

..
   TODO: consider this, especially w.r.t. associativity:
   http://homepage.divms.uiowa.edu/~jones/compiler/spring13/notes/10.shtml

.. productionlist::
   expression : ["+"|"-"] term {("+"|"-") term}
   term       : factor {("*"|"/") factor}
   factor     : `identifier` | `number` | "(" expression ")"

Initially the ``pstate`` stack would only hold the string ``"expression"``.  A
head construct would be registered for the null-string token with the
precondition that the ``expression`` state be at the top of the ``pstate``
stack.  The head handler for the construct would first check whether the peek
token is ``+`` or ``-``.  If so, it would consume it.  Then the string
``"term"`` would be pushed onto the stack and ``recursive_parse`` would be
called.  The call to ``recursive_parse`` returns a processed subtree, which is
incorporated into the expression tree.  A loop would continue this way until
the peek token is not ``+`` or ``-``.

Another head construct would be registered for the null-string token with the
precondition that ``"term"`` be at the top of the stack.  Its head-handler
function would be responsible for parsing terms.  It would work in the same
general way as described above for expressions.

The ``factor`` production could be implemented either as a handler for the
null-string token or by separate constructs for the identifier, number, and
left-paren token types.

A similar expression grammar in plain BNF is as follows:

.. productionlist::
      expression : term "+" expression | term "-" expression | term;
      term       : factor "*" term | factor "/" term | factor;
      factor     : constant | variable | "("  expression  ")";
      variable   : identifier | "-" identifier
      constant   : number

In this case the ``identifier`` and ``number`` nonterminals would be token
literals defined by the corresponding regex passed to the ``def_token`` call.

.. _EBNF:

Recursive descent with Typped's EBNF-like grammar
-------------------------------------------------

The Typped package comes with an EBNF grammar definable via Python overloads.
It essentially automates the procedure described above to map recursive descent
to a generalized Pratt parser.  A grammar in Python, while not as concise as a
parsed EBNF string, is easy to work with and has syntax highlighting.  It is
easy to define aliases for complicated components.

When the grammar is "compiled" with respect to a ``PrattParser`` instance it
produces a recursive descent parser for the grammar within the Pratt parser
framework.  The generated parsers currently use full backtracking search
(The use of first-sets is not fully implemented, but fits nicely into the
precondition-triggering model.)

This feature is still in development and experimental.  The code is not
optimized and parts are currently inefficient.

The EBNF language is currently bare-bones as far as what can be compiled into a
parser instance.  It does basic BNF.  (The EBNF language itself, defined via
Python overloading, is mostly implemented but is not yet compilable into a
parser instance.  For details of the current state of the Python EBNF language
see the docs for the module ``ebnf_classes_and_operators.py``.)

Below is a simple example running example which parses the BNF expression
grammar above.  (It also allows signed integers, but not signed variables, and
full identifiers as variables.)  See the file ``example_expression_grammar.py``
in the examples directory for the code.

..
   TODO: Keep this example synced with the test file.

.. code-block:: python

    import typped as pp
    parser = pp.PrattParser()
    parser.def_default_whitespace()
    parser.def_default_single_char_tokens()
    k_int = parser.def_default_int_token()
    k_identifier = parser.def_default_identifier_token()

    expression = ( Rule("term") + Tok("k_plus") + Rule("expression")
                 | Rule("term") + Tok("k_minus") + Rule("expression")
                 | Rule("term"))
    term       = ( Rule("factor") + Tok("k_ast") + Rule("term")
                 | Rule("factor") + Tok("k_slash") + Rule("term")
                 | Rule("factor"))
    factor     = ( Rule("constant")
                 | Rule("variable")
                 | Tok("k_lpar") + Rule("expression") + Tok("k_rpar"))
    variable   = Tok(k_identifier) | Tok("k_minus") + k_identifier
    constant   = k_int

    g = pp.Grammar("expression", parser, locals())
    tree = parser.parse("4 + my_var * (3 - 1)", pstate="expression")
    print(tree.tree_repr())

This example uses several of the helper methods functions to quickly define
tokens.  The tokens must all be defined, but they do not need to be explicitly
made into token literals (at least not for grammar-based parsing alone).  They
are simply read in as tokens from the lexer because the grammar specifies what
to look for.

Notice that token instances can appear directly in the grammar as token
literals.  The token named by its token label appears as, for example,
``Tok("k_plus")``.  Token instances can also appear inside ``Tok`` calls.

The output from the above code is as follows::

   <k_null-string,'expression'>
       <k_null-string,'term'>
           <k_null-string,'factor'>
               <k_null-string,'constant'>
                   <k_int,'4'>
       <k_plus,'+'>
       <k_null-string,'expression'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_null-string,'variable'>
                       <k_identifier,'my_var'>
               <k_ast,'*'>
               <k_null-string,'term'>
                   <k_null-string,'factor'>
                       <k_lpar,'('>
                       <k_null-string,'expression'>
                           <k_null-string,'term'>
                               <k_null-string,'factor'>
                                   <k_null-string,'constant'>
                                       <k_int,'3'>
                           <k_minus,'-'>
                           <k_null-string,'expression'>
                               <k_null-string,'term'>
                                   <k_null-string,'factor'>
                                       <k_null-string,'constant'>
                                           <k_int,'1'>
                       <k_rpar,')'>

At some point the ability to suppress null-string tokens representing
nonterminals from appearing in the tree will be added.

