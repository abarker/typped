
Introduction to Pratt parsing and its terminology
=================================================

This section provides an introduction to the general concept of Pratt
parsing, as well as the terminology used in the Typped documentation and
code.

What is a Pratt parser?
-----------------------

Pratt parsing is a type of parsing introduced by Vaughan Pratt in a 1973 paper
(see :ref:`References`).  It is also known as "top-down operator-precedence
parsing" because it is a top-down, recursive algorithm which can easily handle
operator precedences.  Pratt describes it as a modification of the Floyd
operator-precedence parser to work top-down.  Pratt parsing fell into relative
obscurity for some years, but it has recently experienced something of a
revival as a parsing technique that is well-suited for dynamic languages.

The well-known recursive descent parsing algorithm is also a top-down,
recursive parsing method.  In recursive descent parsing each *nonterminal in
the grammar* (BNF, EBNF, etc.) of a language is associated with a function
which is designed to parse productions for that nonterminal.  By contrast, in a
Pratt parser each *type of token* has one or more functions associated with it.
These functions will be called **handler functions**, since they handle the
parsing for that particular kind of token.

Some of Pratt's original terminology is less-than-intuitive in a modern
context.  That original terminology is still commonly used in descriptions of
Pratt parsing and the code examples.  The Typped package instead uses an
alternate terminology which is hopefully more intuitive --- at least in the
context of a Python implementation.  The correspondences between the terms in
Pratt's original terminology and the terms used in Typped are noted below when
the terms are defined.  They are also summarized in a table below.

Basic assumptions
-----------------

In this discussion it is assumed that a lexical scanner (lexer) has been
defined as ``lex`` and instantiated with the text to be parsed.  The lexer is
assumed to provide a ``lex.next()`` function which returns the next token, and
a ``lex.peek()`` function which peeks at the next token without consuming it.
The **current token** is the last one returned by ``next``, and is also assumed
to be available as ``lex.token`` from the lexer.  The parser will consume tokens
from the lexer.

Many of the presentations of the Pratt parser essentially fake a lexer with
one-token ``peek`` lookahead from a lexer without built-in lookahead.  This is
done by first calling ``next`` to get a token which acts as a peek.  Then, when
getting another token, this peek token is saved as the current token and
``next`` is called to get the next peek token.  When the Pratt parser algorithm
is written assuming that the lexer provides both a ``next`` and a ``peek``
function it requires slightly less code, and, more importantly, is easier to
follow.

A parser parses a **program** or an **expression** from text that is passed to
the parser's ``parse`` function.   In this writeup we tend to refer to parsing
expressions, but it could also be a full program.  Every expression is assumed
to be made up of **subexpressions**.  Subexpressions are defined by the
particular handler-function implementations and by **operator precedences**.
Subexpressions can be made up of sub-subexpressions, and so forth.  A Pratt
parser recursively parses these expressions, subexpressions, etc., top-down.

An **expression tree** is a tree for an expression such that each function or
operator corresponds to an interior node of the tree, and the
arguments/parameters of the function are the ordered child nodes.  The leaves
of an expression tree are the **literal tokens**, i.e., the tokens which are
also their own subtrees in the final expression tree.  The other tokens appear
in the tree as interior nodes.

A **parse tree** or **derivation tree**, on the other hand, corresponds to a
grammar.  The internal nodes all correspond to grammar productions.  In a parse
tree the leaves of the tree are all tokens (literal and non-literal) returned
from the lexer.  An **abstract syntax tree (AST)** is an abstract
representation of the information in a parse tree or expression tree, in some
format chosen to be convenient.  A Pratt parser can produce any of the above
kinds of trees, depending on how the handler functions are defined, but
naturally produces expression trees.  The term **syntax tree** will generally
refer to any of the above types of trees.

In common usage the **parsing** of text tends to refer to any kind of formal
decomposition into a predefined structure, particularly into a tree structure.
This may include parsing text to a derivation tree, an expression tree, an
AST, or the use of various ad hoc methods for breaking the text down into
components.  Informally, expression trees are often referred to as parse trees.

We assume that a function called ``parse`` will be passed both a lexer instance
(initialized to recognize the tokens of the language) and the expression to be
parsed.  The ``parse`` function should then return a syntax tree for the
expression.  In general, many parsers do not return a syntax tree but instead
evaluate, interpret, or otherwise process the expressions as they go along.  We
assume that any such evaluation or interpretation is applied at a later stage,
based on the returned syntax tree.  (Pratt parsers in general can easily
interpret or evaluate languages on-the-fly, but for simplicity the Typped
package always first forms a syntax tree.)

.. _Operator precedence:

Operator precedence
-------------------

Consider this simple expression: ``2 + 5 * 8`` There are five tokens in this
expression: ``2``, ``+``, ``5``, ``*``, and ``8``.  Parsing this expression
should produce the expression tree represented by::

   +
      2
      *
         5
         8
         
where an indented column under a token represent its children/arguments.  Note
that the leaves of the tree are always literal tokens such as ``2`` and ``5``
which form their own subtrees.

In producing the parse tree above it has been assumed that the usual operator
precedence rules in mathematics hold: ``*`` has higher precedence than ``+``.
In most computer languages this is implemented by assigning a fixed
**precedence** value to each operator, and the Pratt parser does the same
thing.

Every kind of token has a fixed, non-changing precedence value associated with
it.  This is called its **token precedence**.  The default token precedence
value is zero, which is also the minimum possible token precedence value.
Infix operators *must* have a token precedence > 0, as we will see.  When it is
clear in the context the token precedence will simply be called **precedence** or
**prec**.

.. note::

   In Pratt's terminology a token's precedence is called its **left binding
   power** or **lbp**.

Subexpressions
--------------

By definition, every subtree in a parse tree represents a subexpression.  In
this sense, the token precedence values define the subexpression structure of
infix operators.  In the simple example above, the top-level expression is
represented by the full tree, with root at the operator ``+``.  Each literal
token also defines a (trivial) subexpression.  The operator ``*`` defines a
non-trivial subexpression which corresponds to the text ``5 * 8``.

In Pratt parsing, recursion is used to parse subexpressions (starting top-down,
from the full expression).  A crucial distinction in this parsing method is
whether or not the token is the *first* token in the current subexpression or
is a *later* one (e.g., the infix operator in subexpression ``5 * 8``).  Every
subexpression has a first token, and some have later tokens after the first
one.

It was mentioned earler that in Pratt parsing each token can have one or more
**handler functions** defined for it.  The handler function for when the token
is the first token in a subexpression is called the **head handler** function.
The handler function for when the token is not the first token in a
subexpression is called the **tail handler** function.

.. note::

   In Pratt's terminology the head handler function is called the **null
   denotation** or **nud**.  The tail handler function is called the **left
   denotation** or **led**.  The left denotation is passed the
   previously-evaluated left part as an argument, while the null denotation
   receives no such argument.  Pratt's terminology can seem confusing since the
   left denotation is actually called for tokens in the rightmost part of a
   subexpression (the returned value becomes the new, evaluated left part).

Basic parsing
-------------

The parser parses text left-to-right, getting tokens sequentially from the
lexer.  The top-down recursion used in the main function ``parse`` is
implemented by calling another function, called ``recursive_parse``.  Each call
of the ``recursive_parse`` function returns the parse tree for the largest
subexpression to the right of the current token (which is usually one subtree
of the full parse tree).  Thus, the ``parse`` function itself only needs to do
some initialization and then call ``recursive_parse`` and return the result.
So this is the basic code for ``parse``

.. code-block:: python

    def parse(lex, program):
        lex.set_text(program)
        output = PrattParser.recursive_parse(lex, 0)
        return(output)

Since the code for ``parse`` basically just makes a recursive call to
``recursive_parse``, we really need to focus on how ``recursive_parse`` works.
Here is the code for ``recursive_parse``, which will be discussed next

.. code-block:: python

    def recursive_parse(lex, subexp_prec):
        curr_token = lex.next()
        processed_left = curr_token.head_handler(lex)

        while lex.peek().prec() > subexp_prec:
            curr_token = lex.next()
            processed_left = curr_token.tail_handler(lex, processed_left)

        return processed_left

The first thing that ``recursive_parse`` does is get a token from the lexer as
the current token.  This token will always be the first token of a
subexpression (the full expression is a trivial subexpression, and by
definition it is only called at other times when that condition holds).  So,
the next thing that ``recursive_parse`` does is call the head handler for that
token (and a head must be defined for it).  Recall that the head handler for a
token is a function that defines the meaning of the token when it is the first
token in a subexpression.  The result is stored as ``processed_left``, which is
the processed leftmost part of the current subexpression, currently just the
result of the head handler evaluation on the first token.

The ``recursive_parse`` function now needs to evaluate the rest of its
subexpression, calling the tail handler in a while loop for each token that is
not the first in its subexpression.  The results each time will be combined
with the current ``processed_left`` to produce the new ``processed_left``
(which will eventually be returned at the end as the final result).  The only
tricky part is how ``recursive_parse`` determines when it has reached the end
of its subexpression and should return its result.  This is where precedences
come into play.

Each call of ``recursive_parse`` is passed both a lexer and a numerical value
called the **subexpression precedence** or **subexp-prec** for short.  The
subexpression precedence is just a number that gives the precedence of the
subexpression that this call of ``recursive_parse`` is processing.  The
subexpression precedence value passed in is fixed within the function
evaluation, and is compared to the fixed token precedence for individual
tokens.

.. note::

   In Pratt's terminology the subexpression precedence is called the **right
   binding power**, or **rbp**.  In the while loop the precedence or left
   binding power of the next token (to the right) is compared to the current
   subexpression on the left's precedence or right binding power.

In particular, the while loop continues getting tokens and calling their tail
handler functions until the subexpression precedence ``subexp_prec`` is less
than the prec of the upcoming token, given by ``lex.peek().prec()``.  You can
think of the loop ending when the power of the subexpression to bind to the
right and get another token (the subexpression's precedence) is not strong
enough to overcome the power of the next token to bind to the left (the next
token's prec value).  The subexpression ends when that occurs, and the result
``processed_left`` is returned.

The initial call of ``recursive_parse`` from ``parse`` always starts with a
subexpression precedence of 0.  Literal tokens and the end token always have a
token precedence of 0, so subexpressions always end when the next token is the
end token or the next token is a literal token.  That makes sense, since all
subexpressions need to end on the end token, and literal tokens form their own
subexpressions, i.e., subtrees (leaves) of the parse tree.

Generally, any token with only a head handler definition must have a prec of 0.
Only tokens which have a tail handler ever use the token prec value.  The prec
of a token with a tail *must* be greater than 0, or else it will always fail the
test in the while loop of ``recursive_parse`` and thus never be called (since
tail handlers are only called inside the while loop).

This completes the discussion of the higher-level top-down recursion
routines ``parse`` and ``recursive_parse``.  You might have noticed, though,
that there are no explicit recursive calls to ``recursive_parse``.  This is
because the recursion is really a mutual recursion: the head and tail handlers
can call ``recursive_parse`` to evaluate subexpressions, and, in turn, the
``recursive_parse`` function is the only place where head and tail handlers
are called.

In the next section we discuss the head and tail handlers, to complete the
recursion.

.. topic:: Some notes on this subsection.

   - The current implementation of ``recursive_parse`` in this package is
     actually a generalization which calls ``head_dispatcher`` instead of
     ``head_handler``, and ``tail_dispatcher`` instead ``tail_handler`` (this
     will be discussed later).  The general principle, however, is the same.

   - The ``processed_left`` structure can in general be a partial parse tree,
     the result of a numerical evaluation, or anything else.  The handler
     functions can build and return any processed form for their tokens.  The
     Typped package, however, always builds a parse tree out of token nodes
     (which can be evaluated later, if desired). 

   - Outside of an error condition the algorithm never even looks at the
     precedence of a token having only a head handler (i.e., a token which can
     only occur in the beginning position of an expression).  The precedence of
     such a head-only token is usually taken to be 0, but it really does not
     need to be defined at all.  So, precedences can be treated as properties
     associated with tail-handler functions.

This table summarizes the correspondence between Pratt's terminology and the
terminology that is used in this documentation and in the code:

   +----------------------------------+--------------------------+
   | This description                 | Pratt's terminology      |
   +==================================+==========================+
   | token precedence, prec           | left binding power, lbp  |
   +----------------------------------+--------------------------+
   | subexpression precedence         | right binding power, rbp |
   +----------------------------------+--------------------------+
   | head handler function            | null denotation, nud     |
   +----------------------------------+--------------------------+
   | tail handler function            | left denotation, led     |
   +----------------------------------+--------------------------+

The handler functions head and tail
-----------------------------------

In order a token to be processed in an expression it must have defined for it
either a head handler, a tail handler, or both.  As mentioned earlier, the head
function is called in evaluating a subexpression when the token is the first
token in a subexpression, and the tail handler is called when the token appears
at any other position in the subexpression.  We have not yet described what
exactly these functions do.

In general, there are no restrictions on what a head or tail handler can do.
They are simply functions which return some kind of value which is set to the
new ``processed_left`` variable in ``recursive_parse`` which in our case must
eventually result in the processed parse tree for the subexpression.  They
could, for example, call a completely different parser.  Below we describe what
they usually do, and give an example of processing the simple expression used
in the :ref:`Operator precedence` section.

The literal tokens in a grammar always have a head handler, since they are
themselves subexpressions.  The head handler for literal tokens is trivial: the
head function simply returns a parse subtree for a leaf node containing that
token.  Note that any mutual recursion always ends with literal tokens because
all the leaves of a parse tree are literal tokens and these head handlers do
not make any recursive calls.

Every token is represented by a unique subclass of the ``TokenNode`` class.
The defined precedences for tokens are saved as attributes of the
corresponding subclass.  Instances of that class represent individual tokens,
and the lexer returns such an instance for every token it finds.  We will build
the parse tree using the token representations returned by the lexer as the
nodes.

The head for literal tokens basically just need to return the token instance
itself, since literal tokens are the leaves of the parse tree and so form their
own subtrees:

.. code-block:: python

     def head_handler_literal(self, lex):
         return self

At the time when they are defined these head handlers are registered as new
methods of the subclass of ``TokenNode`` which represents the corresponding
literal token (hence the ``self`` argument to the function).  The same holds
for head and tail handlers for any tokens.

Generally, head and tail handlers do two things while constructing the result
value to return: they read in more tokens, and they call ``recursive_parse`` to
evaluate sub-subexpressions of their subexpression.  This is the definition of
the tail handler for the ``+`` operator:

.. code-block:: python

     def tail_handler_plus(self, lex, left):
         self.append_children(left, recursive_parse(lex, self.prec))
         return self

This tail handler (like all tail handlers) is passed the current
``processed_left`` expression evaluation as ``left``.  It needs to build and
return its parse subtree, with its own ``+`` node as the subtree root.  The
``left`` argument passed in should contain the previously-evaluated subtree for
the left operand of ``+``.  So that subtree is set as the left child of the
current ``+`` node.  To get the right operand, the ``recursive_parse`` function
is called.  It returns the subtree for the next subexpression (following the
current ``+`` token), which is set as the right child of the ``+`` node.  The
completed subtree is then returned.

The tail handler for the ``*`` operator is identical to the definition for
``+`` except it becomes a method of the subclass representing ``*``.  We will
assume that the precedence defined for ``+`` is 3, and that the precedence for
``*`` is 4.

We now have enough to parse the five tokens in the expression ``2 + 5 * 8``.
The parse is roughly described in the box below.

.. topic:: Parsing the expression ``2 + 5 * 8``

   This is an rough English description of parsing the expression ``2 + 5 * 8``
   with a Pratt parser as defined above.  Indents occur on recursive calls,
   and the corresponding dedents indicate a return to that level.  Remember
   that this is a mutual recursion, between the ``recursive_parse`` routine and
   the head and tail handler functions associated with tokens.  The tokens
   themselves (represented by subclasses of ``TokenNode``) are used as nodes in
   the expression tree.
   
   The ``recursive_parse`` code is repeated here for easy reference::

       def recursive_parse(lex, subexp_prec):
           curr_token = lex.next()
           processed_left = curr_token.head_handler(lex)

           while lex.peek().prec() > subexp_prec:
               curr_token = lex.next()
               processed_left = curr_token.tail_handler(lex, processed_left)

           return processed_left

   The handler functions are as defined earlier.  The parsing proceeds as
   follows.

   First, the ``parse`` function is called with the lexer and the expression
   text to be parsed.  This function just does some setup and then calls the
   recursive routine to do the real work.  The ``parse`` function initializes
   the lexer and then calls ``recursive_parse`` to parse the full expression.
   The full expression is always associated with a subexpression precedence of
   zero, so the ``subexp_prec`` argument to ``recursive_parse`` is 0.

      The ``recursive_parse`` function at the top level first consumes a token
      from the lexer, which is the token for ``2``.  It then and calls the head
      handler associated with it.

         The head handler for the token ``2`` returns the token for ``2``
         itself as the corresponding node in the subtree, since literal tokens
         are their own subtrees (leaves) of the final expression tree.
      
      The ``processed_left`` variable is set to the returned node, which is the
      token ``2``.
      
      The while loop in ``recursive_parse`` is now run to handle the tail of
      the subexpression.  It looks ahead and sees that the ``+`` operator has a
      higher token precedence than the current precedence of 0 for the
      subexpression, so the loop executes.  It consumes another token from the
      lexer, the ``+`` token.  It then calls the tail handler associated with
      the ``+`` token, passing it the current ``processed_left`` (which
      currently points to the node ``2``) as the ``left`` argument.
      
         The tail handler for ``+`` sets the left child of the token/node for
         ``+`` to be the passed-in subtree ``left`` (is currently the node
         ``2``).  This sets the left operand for ``+``.  To get the right
         operand the tail handler for ``+`` calls ``recursive_parse``
         recursively, passing in the ``prec`` value of 3 (which is the
         precedence value we assumed for the ``+`` operator) as the
         subexpression precedence argument ``subexp_prec``.
      
            This recursive call of ``recursive_parse`` consumes another token, the
            token for ``5``, and calls the head handler for that token.
            
               The head handler returns the node for ``5`` as the subtree, since
               it is a literal token.
               
            The returned node/subtree for ``5`` is set as the initial value for
            ``processed_left`` at this level of recursion.

            The while loop now looks ahead and sees that the token precedence of 4
            for the ``*`` operator is greater than its own subexpression
            precedence (``subexp_prec`` equals 3), so the loop executes.  The next
            token, ``*``, is consumed from the lexer.  The tail handler for that
            token is called, passed the ``processed_left`` value at this level of
            recursion (which points to the node ``5``).
            
               The tail handler for ``*`` sets that passed-in ``left`` value to
               be the left child of the ``*`` node, so the left child is set to
               the node for ``5``.  It then calls ``recursive_parse`` to get
               the right operand/child.  The ``*`` token's precedence value of
               4 is passed to ``recursive_parse`` as the subexpression
               precedence argument ``subexp_prec``.
      
                  This call of ``recursive_parse`` first consumes the token
                  ``8`` from the lexer and calls the head handler for it.
                     
                     The head handler for ``8`` returns the node itself.

                  The ``processed_left`` variable at this level of recursion is
                  now set to the returned node ``8``.  The while loop looks ahead and
                  sees the end-token, which always has a precedence of 0.  Since
                  that is less than the current subexpression precedence of 4, the
                  while loop does not execute.  The token ``8`` is returned.
                  
               The tail handler for ``*`` now sets the node/token ``8`` as the
               right child of the ``*`` node.  It then returns the ``*`` node.
         
            While loop in ``recursive_parse`` at this level looks ahead and
            does not execute upon seeing end-token, so the subtree for ``*``
            (which now has two children, ``5`` and ``8``) is returned.
         
         The tail handler for ``+`` now sets the returned subtree (the subtree
         for `*`, with its children already set) as the right subtree for the
         ``+`` token/node.  The ``+`` token is returned.
      
      Back at the top level of ``recursive_parse`` the while loop looks ahead
      and sees the end-token, so it does not execute.  The subtree for ``+`` is
      returned to the ``parse`` routine.
      
   The ``parse`` routine returns the result returned by the ``recursive_parse``
   call as its value.  So it returns the node for ``+``, now with children, as
   the final expression tree of token nodes.

Note that when ``recursive_parse`` is called recursively in the tail of an
infix operator it is called with a ``subexp_prec`` argument equal to the
current node's prec.  That gives left-to-right precedence evaluation (left
associative) for infix operators with equal prec values.  To get right-to-left
evaluation (right associative), ``recursive_parse`` should instead be passed
the current prec *minus one* as the value for ``subexp_prec``.  Interested
readers can consider the evaluation of ``2 ^ 5 ^ 8`` (similar to the box above)
in the case where for ``^`` is defined as left associative.

.. _References:

References
----------

Vaughan R. Pratt, "`Top down operator precedence
<http://dl.acm.org/citation.cfm?id=512931>`_," 1973.
The original article, at the ACM site (paywall).

Fredrik Lundh, "`Simple Top-Down Parsing in Python
<http://effbot.org/zone/simple-top-down-parsing.htm>`_," July 2008.  Excellent explanation
and good code examples in Python.  Influenced the design and implementation of
the Typped package.  See also the `related articles by Lundh on Pratt parsing
and lexing with regexes <http://effbot.org/zone/tdop-index.htm>`_.

Eli Bendersky, "`Top-Down operator precedence parsing
<http://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/>`_,"
Jan. 2, 2010.  An article based on Lundh's article above.  It also uses Python
and has some useful discussion.

Douglas Crockford, "`Top Down Operator Precedence
<http://javascript.crockford.com/tdop/tdop.html>`_," Feb. 21, 2007.  Uses
JavaScript.

Bob Nystrom, "`Pratt Parsers: Expression Parsing Made Easy
<http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/>`_,"
Mar. 19, 2011.  Uses Java.

