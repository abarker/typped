
Pratt parsing
=============

What is a Pratt parser?
-----------------------

Pratt parsing is a type of parsing introduced by Vaughan Pratt in a 1973 paper
(see :ref:`References`).  It is also known as "top-down operator-precedence
parsing" because it is a top-down, recursive algorithm which can easily handle
operator precedences.  Pratt describes it as a modification of the Floyd
operator-precedence parser to work top-down.

Recursive descent parsing is another, more commonly known, top-down recursive
parsing method.  In recursive descent each production in the grammar (BNF,
EBNF, etc.) of a language is associated with a function which is designed to
parse that production.  By contrast, in a Pratt parser each type of token has
one or more **handler functions** associated with it.

Pratt parsing fell into relative obscurity for some years, but it has
recently experienced a revival as a parsing technique that is
well-suited for dynamic languages.

Some of Pratt's original terminology is less-than-intuitive in a modern
context, yet his original terminology is still commonly used to describe the
parsing method.  We have used simplified terminology which is hopefully more
intuitive, at least in the context of a Python implementation.  The
correspondences of terms to Pratt's original terms are noted both where they
are defined and in a summarizing table below.

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
one-token ``peek`` lookahead from a lexer without built-in lookahead.  First,
``next`` is called to get a token which acts as a peek.  Then to get a token
the peek token is saved as the current token and ``next`` is called to get the
next peek token.  When the Pratt parser algorithm is written assuming both a
``next`` and a ``peek`` function it requires slightly less code, and, more
importantly, is easier to follow.

A parser parses a **program** or an **expression** from text that is passed to
the parser's `parse` function.  We will use the term expression, but it could
also be a full program.  Every expression is assumed to be made up of
**subexpressions**, as defined by the particular handler-function
implementations and by **operator precedences**.  Subexpressions can
be made up of sub-subexpressions, and so forth.  The Pratt parser recursively
parses these expressions top-down.

We assume that a function called ``parse`` is passed both a lexer and the
expression to be parsed and returns the **parse tree** for the expression.  In
general, some parsers do not return a parse tree but instead evaluate,
interpret, or otherwise process the expressions as they go along.  We assume
that any such evaluation or interpretation is applied at a later stage, based
on the returned parse tree.

Operator precedence
-------------------

Consider this simple expression: ``2 + 5 * 8`` There are five tokens in this
expression: ``2``, ``+``, ``5``, ``*``, and ``8``.  Parsing this expression
should produce the **parse tree** represented by::

   +
      2
      *
         5
         8
         
where an indented column under a token represent its children/arguments.  Note
that the leaves of the tree are always **literals** such as ``2`` and ``5``.

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

By definition, every subtree in a parse tree represents a subexpression.
In this sense, the token precedence values define the subexpression
structure of infix operators.  In the simple example above, the top-level
expression is represented by the full tree, with root at the operator
``+``.  Each literal also defines a (trivial) subexpression.  The operator
``*`` defines a non-trivial subexpression which corresponds to the text
``5 * 8``.

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
   denotation** or **led**.  Pratt's terminology can seem confusing since the
   left denotation is actually called for tokens in the rightmost part of a
   subexpression.  The left denotation is passed the previously-evaluated left
   part as an argument, while the null denotation receives no such argument.

Basic parsing
-------------

The parser parses text left-to-right, getting tokens sequentially from the
lexer.  The top-down recursion used in the main function ``parse`` is
implemented by calling another function, called ``recursive_parse``.  Each call
of the ``recursive_parse`` function returns the parse tree for the largest
subexpression to the right of the current token (which is usually one subtree
of the full parse tree).  Thus, the ``parse`` function itself only needs to do
some initialization and then call ``recursive_parse`` and return the result.
So this is the basic code for ``parse``::

    def parse(lex, program):
        lex.set_text(program)
        output = PrattParser.recursive_parse(lex, 0)
        return(output)

Since the code for ``parse`` basically just makes a recursive call to
``recursive_parse``, we really need to focus on how ``recursive_parse`` works.
Here is the code for ``recursive_parse``, which will be discussed next::

    def recursive_parse(lex, subexp_prec):
        curr_token = lex.next()
        processed_left = curr_token.head_denote(lex)

        while lex.peek().prec() > subexp_prec:
            curr_token = lex.next()
            processed_left = curr_token.tail_denote(lex, processed_left)

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
subexpression precedence of 0.  Literals and the end token always have a token
precedence of 0, so subexpressions always end when the next token is the end
token or the next token is a literal.  That makes sense, since all
subexpressions need to end on the end token, and literals form their own
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

.. topic:: Some notes on this section.

   - The current implementation of ``recursive_parse`` in this package is
     actually a generalization which calls ``head_dispatcher`` instead of
     ``head_handler``, and ``tail_dispatcher`` instead ``tail_handler`` (this
     will be discussed later).  The general principle, however, is the same.

   - The ``processed_left`` structure can generally be the result of a
     numerical evaluation, a partial parse tree, or anything else.  The handler
     functions can build and return any processed form for their tokens.  (The
     current program always builds a token tree, which can be evaluated later
     if desired.) In the current implementation the handler functions always
     build a parse tree from the token nodes.

   - Outside of an error condition the algorithm never even looks at the prec of
     a token with only a head (i.e., a token which can only occur in the
     beginning position of an expression).  The prec of a head-only token is
     usually taken to be 0, but it does not need to be defined at all.  So, the
     prec can be treated as a property of the tail handler.  This turns out to
     be useful for a generalization.

This table summarizes the correspondence between Pratt's terminology and the
terminology that is used in this documentation and in the code:

   +----------------------------------+--------------------------+
   | This description                 | Pratt's terminology      |
   +==================================+==========================+
   | token precedence                 | left binding power, lbp  |
   +----------------------------------+--------------------------+
   | subexpression precedence         | right binding power, rbp |
   +----------------------------------+--------------------------+
   | head handler function            | null denotation, nud     |
   +----------------------------------+--------------------------+
   | tail handler function            | left denotation, led     |
   +----------------------------------+--------------------------+

The handler functions head and tail
---------------------------------

In order a token to be processed in an expression it must have defined for it
either a head handler, a tail handler, or both.  As mentioned earlier, the head
function is called in evaluating a subexpression when the token is the first
token in a subexpression, and the tail handler is called when the token appears
at any other position in the subexpression.  We have not yet described what
exactly these functions do.

In general, there are no restrictions on what a head or tail handler can do.
They are simply functions which return some kind of value which is set to the
new ``left_processed`` variable in ``recursive_parse`` which in our case must
eventually result in the processed parse tree for the subexpression.  They
could, for example, call a completely different parser.  Below we describe what
they usually do, and give an example of processing the simple expression used
in the :ref:`Operator precedence` section.

The literals in a grammar always have a head handler, since they are themselves
atomic subexpressions.  The head handler for literals is trivial: the head
function simply returns a parse subtree for a leaf node containing that
literal.  Note that any mutual recursion always ends with literals because all
the leaves of a parse tree are literals and these head handlers do not make any
recursive calls.

Every token is represented by a unique subclass of the ``TokenNode`` class.
The defined precedences for tokens are saved as attributes of the
corresponding subclass.  Instances of that class represent individual tokens,
and the lexer returns such an instance for every token it finds.  We will build
the parse tree using the token representations returned by the lexer as the
nodes.

The head for literals basically just needs to return the token instance itself,
since literals are the leaves of the parse tree::

     def head_handler_literal(self, lex):
         return self

At the time when they are defined these head handlers are "pasted on" as new
methods of the subclass of ``TokenNode`` which represents the corresponding
literal (hence the ``self`` argument to the function).  The same holds for
head and tail handlers for any tokens.

Beyond just literals, the head and tail handlers do two things while
constructing the result value to return: they read in more tokens, and they
call ``recursive_parse`` to evaluate sub-subexpressions of their subexpression.
This is the definition of the tail handler for the ``+`` operator::

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
assume that the prec defined for ``+`` is 3, and that the prec for
``*`` is 4.

We now have enough to parse the five tokens in the expression ``2 + 5 * 8``.
The parse is roughly described in the box below, which interested readers can
follow in the code above.

.. topic:: Parsing the expression ``2 + 5 * 8``

   This is an rough English description of parsing the expression ``2 + 5 * 8``
   with a Pratt parser, as defined above.  Paragraph splits and indents occur on
   recursive calls to ``recursive_parse``, and similarly for returns to the
   higher level.

   First, ``recursive_parse`` is called on the full expression, with a
   ``subexp_prec`` value of ``0``.  That function first consumes a token from the
   lexer (the token for ``2``) and calls the head handler associated with it.
   The head handler returns the node for ``2``, since, as a literal, it forms
   its own subtree of the final parse tree.  The ``processed_left`` variable is
   set to the returned ``2`` node.  The while loop in ``recursive_parse`` looks
   ahead and sees that the ``+`` operator has a higher token prec than
   the current ``0`` precedence for the subexpression, so the loop
   executes.  It gets another token from the lexer, the ``+`` token.  It then
   calls the tail handler associated with that token, passing it the current
   ``processed_left`` (which is ``2``) as the ``left`` argument.  The tail
   function for ``+`` sets the left child of ``+`` to be the passed-in subtree
   ``left``, which sets the node ``2`` as the left operand.  To get the right
   operand, ``recursive_parse`` is called, passing in the ``prec`` value of 3
   (the value which we assumed for the ``+`` operator) as the subexp-prec argument
   ``subexp_prec``.
   
      This recursive call of ``recursive_parse`` reads the node ``5`` and calls
      its head, which returns the node for ``5`` as the subtree.  That
      node/subtree is set as the initial value for ``processed_left``.  The
      while loop then looks ahead and sees that the token prec 4 of the ``*``
      operator is greater than its own subexpression precedence
      ``subexp_prec``, so the loop executes.  The next token, ``*``, is
      consumed from the lexer.  The tail for that token is called, with the
      ``processed_left`` value (at this level of recursion, which is ``5``)
      passed in as ``left``.  The tail handler for ``*`` sets that passed-in
      ``left`` value to be the left child of the ``*`` node, and then calls
      ``recursive_parse`` to get the right operand/child.  The ``*`` token's
      prec value of ``4`` is passed to ``recursive_parse`` as the subexpression
      precedence argument ``subexp_prec``.
   
         That call of ``recursive_parse`` consumes the token ``8`` from the
         lexer and calls the head for it, which sets the initial
         ``processed_left`` at that level of recursion to ``8``.  The while
         loop looks ahead and sees the end-token, with a precedence of 0.
         Since that is less than the current subexpression precedence of 4, the
         while loop does not execute.  The token ``8`` is returned.
         
      Back at the previous recursion level the token for ``8`` is set as the
      right child of the ``*`` node.  The while loop again does not execute
      upon seeing end-token, and the subtree for ``*`` is returned from this
      level.
      
   Back at the next level up the returned subtree is made into the right
   subtree for the ``+`` token.  The while loop again does not execute for
   end-token, and the subtree for ``+`` is returned as the final result.

Note that when ``recursive_parse`` is called recursively in the tail of an infix
operator it is called with a ``subexp_prec`` argument equal to the current node's
prec.  That gives left-to-right precedence evaluation (left associative) for
infix operators with equal prec values.  To get right-to-left evaluation (right
associative), ``recursive_parse`` should instead be passed the current prec
*minus one* as the subexp-prec value for ``subexp_prec``.  Interested readers can consider
the evaluation of ``2 + 5 + 8`` in the case where the tail for ``+`` is defined
as left versus right associative.

We have defined some terminology and the basics of Pratt parsing.  Some details
have been omitted, but the general picture of how the top-down parsing works
should be clear.  In the following sections various generalizations and
enhancements to the basic algorithm are described.

Preconditioned dispatching
--------------------------

In the usual Pratt parser each token has a fixed head and/or tail handler
function associated with it.  In this generalization, each token can have
multiple possible head and/or tail handler functions associated with it.  At
parse-time the choice of which of the possible handler functions is based on
the conditions at the time.  This feature is optional and can easily be ignored
to use traditional Pratt parser techniques.

Instead of calling the head or tail handlers for a token directly, a function
``dispatch_and_call_handler`` is called.  That function goes down a list of
precondition-testing functions for the current token, each of which is
associated with a particular handler function.  The preconditions associated
with any head or tail handler function are defined by the user when the handler
function itself is defined, along with a priority value to be used for break
ties.  The handler function associated with the highest-priority
precondition-testing function which returns true in the current conditions is
executed to handle the token in the given context.

Preconditioned dispatching is only a slight generalization of the usual Pratt
parser.  A similar thing could be accomplished with ordinary head and tail
functions with a case statement inside each one, performing different actions
based on the conditions at the time and ordered in the case statement by
priority.  An advantage of using function dispatching is that it allows for
modularity in defining the head and tail handlers for a particular kind of
token.  The overall case statement in a handler function can essentially be
split up and defined where those cases occur syntactically rather than having
to be placed in one fixed location.  This makes it easier to separate the
interface into handlers for individual syntactical elements.  This allows for
convenience functions to easily perform common syntax-related tasks like define
an infix operator, define a grouping operator, define a standard function, etc.

As an example of dispatching, the usual way to parse function evaluations
``f(x)`` in a Pratt parser is to define a tail for the left-paren token.  The
head for left paren is then called for grouping parentheses, and the tail is
called for function evaluations after the head for the identifier ``f``.  But
this can get complicated in more complex grammars where left paren is used in
various contexts.  Using lookahead a function evaluation can be parsed by
defining a head for identifiers with a precondition that it be followed by an
lpar with no space between, and a lower-priority default head for identifiers
otherwise.  (Other preconditions can also be placed on other heads for
identifiers).  These two head definitions are essentially independent, and can
occur in different sections of code.  They are both registered for the
identifier token, and the rest is handled automatically.

The typing system which is implemented in this parser is also based on the
preconditioned dispatching design.  Type-signature information is associated
with each particular handler function, i.e., with the particular function
chosen and dispatched as the head or tail handler.  Consider the above example.
When types are defined for functions the function names should be made into
individual tokens in the lexer, rather than using a single identifier token for
all identifiers.  Then, when the token for ``f`` is processed, the expected
signature is also available.  The type system is discussed more below.

Implementation
^^^^^^^^^^^^^^

As far as the implementation of dispatching, the methods ``head_dispatcher`` and
``tail_dispatcher`` are fixed in the definition of the subclass of ``TokenNode``
which represents tokens.  Within the basic parsing routines one should always
call a token's ``head_dispatcher`` or ``tail_dispatcher`` function.  (Most users
will have no need to modify the basic parsing routines ``parse`` and
``recursive_parse``).

When the ``head_dispatcher`` or ``tail_dispatcher`` method of a token is called
it performs the appropriate lookup and calls the correct handler function.
This lookup is performed by getting the list of precondition functions, ordered
by priority, and calling each one until one returns ``True`` based on the
current conditions.  The associated head or tail handler is then executed.  (The
handler functions themselves are stored in static dict attributes of the
``TokenNode`` subclass, after being passed into ``modify_token_subclass`` via
keyword arguments.)

Using preconditions to do recursive descent parsing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

Basic type-checking
-------------------

This parser implements a general type definition and type checking mechanism.
It is optional, however, and can be ignored for untyped languages.  When they
are defined the types will be automatically checked at parse-time (according to
any options which are set).  The type system also allows for operator
overloading, including optional overloading on return types.

In this implementation type information is associated with head and tail handler
functions.  That is because type specifications and type checking are closely
related to the nodes of the final parse tree of the expression.  Child nodes in
the tree are function arguments of their parent node, and the type of the
parent nodes correspond to a function's return value.  In the usual usage of
Pratt parsing each head or tail handler produces one node in the parse tree,
possibly with children.  Every node in the final parse tree was originally a
token from the lexer that was made into a subtree via a call to one of its head
or tail handlers.

The head and the tail handlers of the same token can correspond to language
constructs which have different value types and/or which take different types
as arguments.  For example, a token as a prefix operator would take different
arguments of possibly different types, and might return a different type than
the same token as an infix operator.

Any call to a particular head or tail handler is assumed to produce a parse tree
node (possibly the root of a subtree) where the possible type specifications
for the node are saved with the handler functions themselves as a collection of
type signatures (the multiple possibilities correspond to possible
overloading).  In a top-down parser the parse tree is essentially constructed
bottom-up, on the way back up the recursion.  So the leaves are the first nodes
created and they can have their types checked.  Each node farther up has the
types of its children/arguments as well as its own type checked at the time
when its subtree of the parse tree is constructed.

Based on the above, each constructed tree is guaranteed to be resolved for
types when it is first constructed, provided that overloading is only on
function arguments.  Overloading on return types requires another pass down the
parse tree (not necessarily the full tree, but it can be in a worst case).  As
soon as a node with a unique signature is created the types in the subtree are
resolved.

Note that each possibly-uniquely typed symbol in the language should generally
be defined as its own token type.  So, for overloaded functions the function
names should each be registered as corresponding to a unique kind of token.
This is in contrast to having a single token for all identifiers and then
resolving which are functions and which signatures apply based on the actual
value for the token's string.

Implementation details
^^^^^^^^^^^^^^^^^^^^^^

Type signatures can be declared whenever a head or tail is defined (or redefined
for overloading).  It is passed in kwargs to the ``modify_token_subclass``
routine whenever a head or tail is defined.  That routine then looks up the token
subclass in the symbol table for token subclasses and stored the provided head
or tail in one of the dictionaries for the token.  It also pastes the type
information onto the head and/or tail handlers as an attribute (in a set of
function signature tuples).  If the head or tail already exists it assumes that
overloading is intended, and the type signature is unioned with any existing
ones.

After the tokens are defined the ``recursive_parse`` routine runs to do the
actual parsing.  When any head or tail is run it should call the utility function
``process_and_check_node`` just before returning a value.  That function
retrieves the type information which was stored pasted onto the head or tail
function as attributes.  This is exactly the type information it needs right
then, and it checks that the types of the children in the token tree (which
were processed already, since we're on the way back up the recursion) exactly
match one sig in the stored collection of possible sigs (with None as
wildcard).  If one matches, then it sets the ``val_type`` attribute of the
``TokenNodeSubclass`` instance being returned in order to set the type of the
return value to the one matching a signature.  Going up the tree, the next node
can now look at those ``val_type`` values (of its children) and match them
against its signatures, etc.

Overloading on return values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Overloading on return values is more difficult.  Suppose there are two possible
return values for the same argument signature.  With overloading only on
argument types that would be an error -- or at least an ambiguity that some
other rule would need to break.  When return value overloading is allowed then
either choice is possible, depending on the possibilities higher up in the
tree.

Suppose we have these signatures (in C-like notation)::

    int f(real, int)
    int f(bool, real)

    bool g(bool)
    real g(int)

    bool h(int)
    int h(int)
    
We want to parse this expression::

    f(g(h(int), int)

where the int values are from literals at the leaves of the parse tree.  When
we reach the bottom of the tree and start going up we cannot immediately choose
the signature of ``h`` to use.  They both match arguments to ``g``.  But only
one argument to ``g`` also matches the argument to ``f`` since we can rule out
the second signature of ``f``.

We might consider passing the expected argument down the tree, so that when we
reach ``h`` we will know that ``g`` needs to return a real so it has to take an
``int`` argument.  But what about when the second argument to ``f`` also has a
tree? The full signature of ``f`` must match like it is an "and", not like an
"or".  At the bottom of the tree, evaluating ``h``, we do not know how any of
its siblings or other relatives in other subtrees will resolve.

Backtracking is one possible solution.  We could choose one, and have the
parent raise an error to backtrack if it fails to match.  But backtracking can
be computationally expensive.

A better approach is to use a two-pass system.  Note that a parent node can
force any of its children to assume any one of its possible return types.  So
the type-value of any child can in that case be set independently from the
type-value of its siblings.  When the parent node knows all the possible types
for each argument it can match against all its possible signatures and resolve
to one signature (or raise an error).  Going up on the first-pass recursion
will propagage up all the possibilities.  Going down on the second pass will
propagate down the final signature-binding choices.

Previous explanation, combine best of both:

Suppose we pass all the possible return values to the parent.  Each sibling
does that.  Then, it can calculate all its possible return values and pass
those to its parent.  At some point it reaches the top again, and a function
knows whether or not some unique return value has matched.  If so, then we can
go back down the tree again and fix the return values, which fix the argument
values, and so forth.  All this stuff can be pasted onto the token class
instances as necessary.  This is more expensive, but it doesn't seem
exponential or anything.  Just another pass or two.

Update: for the gist see below and section in the code explaining basics.
Also, move toward full-sig comparison model and explanations.  - On way up the
tree, collect all the possible signature types, including *all possible*
conversions which might give different return values, and save them with each
node.  Include all possible because going up the tree we don't know what might
possibly be needed.

- On way back down the tree (or down the subtree if done partially) resolve the
  possible types to a single type.

- Resolution is by removing impossible types, and running a ranking function on
  the remaining ones.  Remaining ties raise an exception.

Parameterized types and signatures
----------------------------------

Types are represented in the ``PrattParser`` by subclasses of the
``TypeObject`` class.  The subclasses themselves represent **type templates**,
and their instantiations represent **type instances** or **actual types**.
Each type template has a separate subclass created to represent it.  The Pratt
parser class stores all defined type templates in a table, indexed by a type
name.  A type template defines a specification that must be satisfied by any
concrete instance.  As a special case, the Python ``None`` value is also a
valid type template and a valid type instance, representing either a template
that anything matches or an actual type for items which are considered
typeless.

Type templates can be parameterized, but even types without parameters are
defined by creating a parameterless type template.  The type instances or
actual types must have bindings for all the parameters.  The types of actual
constructs in the parsed language are always actual types.  Each node in the
final parse tree needs to have an actual type as its node type (and a signature
containing only actual types).

In the implementation language each actual type (of a construct in the parsed
language) is represented by an instance of the ``TypeObject`` subclass
representing that type template.  Each such instance must define a value for
each parameters of the type template (if any).  The actual types may or may not
match the types required by the template.  Checking for a type match is
performed at the time of instantiation.  That is, the initializer for a
subclass of ``TypeObject`` takes as arguments the actual values to assign to
the parameters of the type template represented by the subclass.  If the
arguments do not match an error is raised, otherwise an instance is created.

A collection of type templates defining the required argument types and return
type for a function will be called the function's **type specification** or a
**type spec**.  A collection of actual types for the arguments and return types
of a function will be called the function's **type signature** or a **type
sig**.  A type sig either matches a type spec or not (either exactly or via the
use of defined conversions).  These are represented in the program as instances
of the class ``TypeSpec`` and the class ``TypeSpec`` (both derived from the
class ``FunctionTypes``).

Recall that function overloading is implemented with respect to the type spec
that is passed to the ``PrattParser`` routine for parsing the function.  The
same head handler function or tail handler function is always used when a
function is overloaded, but a list of all the defined type signatures is
maintained.  The final nodes in the ``TokenNode`` parse tree will each contain
an actual type signature.

Implementation
^^^^^^^^^^^^^^

In the implementation a head is defined for literal tokens by ``define_token``.
The method takes an argument ``val_type``.  Note that now whenever the
``val_type`` is set for the *node* it should be for an *instance* of the type
specifier.  Perhaps it should be called ``val_type_actual``, or else just set
the full ``TypeSpec`` and specify that the can only contain instances.  Then,
all the literals have instances set for them as ``val_type_actual``.  Going up
the parse tree, the higher nodes look down at the ``val_type_actual`` values of
their children to obtain the actual types of the type specifiers.

.. topic:: Example of defining types.

   The following example illustrates the definition of types and parameterized
   types in a very simple implementation of a language for matrix expressions.

   First, define two unparameterized types::

      t_real = pp.define_type("Real")
      t_int = pp.define_type("Int")
      
   The first argument to ``define_type`` is an arbitary string label for the
   type.  For mnemonic purposes the string label can be chosen to correspond to
   the type label in the parsed language, but it need not be.  The returned
   values are subclasses of ``TypeObject``.

   Now an ``m`` by ``n`` parameterized matrix type holding any type of elements
   can be defined as a templated type::

      t_matrix = pp.define_type("Mat", (None, t_int, t_int))

   The second argument to ``define_type`` is a tuple containing the template
   parameters, which are also type specifiers.  The ``None`` type of the first
   parameter matches any type, for matrix elements of arbitrary types.  The
   ``t_int`` type parameters are for the shape parameters m and n of the
   matrix.

   Using the above type definition, the type signature for matrix
   multiplication can be parameterized to ensure at parse-time that both matrix
   arguments are conformable for multiplication::

      mmult_sig = TypeSpec(t_matrix,  # return type
                         (t_matrix,  # arg 1
                          t_matrix), # arg 2
                          test_fun=conformable_test_fun)  # a test to apply

   Now suppose the infix operator ``*`` is defined for matrix multiplication,
   and that the type signature ``mmult_sig`` is passed as a keyword argument
   defining the signature.  When a matrix multiplication is parsed in the
   implemented language, whatever syntax is used, the actual arguments to the
   matrix multiplication become known (they are the actual types of the
   children in the parse tree, known in the bottom-up type resolution).

   To test whether the ``mmult_sig`` signature matches on the arguments we
   first test whether or not the basic types of each argument match
   (perhaps performing conversions [??? complications due to multiple
   possible ???]).

   Next, the function ``test_fun`` is run.  It is passed the current token
   node, the children of which are the operator arguments.  The
   children/operands have already had all their possible final signatures
   assigned (uniquely if overloading on return types is disallowed).  The
   ``TypeObject`` for each child should contain the m and n values for the
   matrix operands.  (If a matrix literal was read, for example, or an explicit
   type definition was made in the object language.) So conformability can be
   checked for the multiplication operation.

   TODO: consider whether the variable kind of indexing above, using a
   dict, to pass to the test function or the number indexing kind of thing
   below (for parameterized types) is best.

   TODO: consider defining a list or a tuple of ``TypeObject`` instances in
   place of a single ``TypeObject`` parameter to represent an "or"
   operation, accepting any of the types::

      t_real = pp.define_type("Real")
      t_int = pp.define_type("Int")
      t_mat_elem = pp.define_type("MatElem", [(t_int, t_real, t_complex)])

   So the gist would be: - Use Python ``*args`` convention for indexing
   when necessary to index.
   
   - Any type argument to the initializer of a ``TypeObject`` can be passed
     either the type's string label or the actual ``TypeObject`` instance.

   - Any type argument to the initializer of a ``TypeObject`` can alternately
     be passed a list or a tuple of instances or type labels instead, which
     represent an "or" over all the types in the list or tuple.

   - Consider: when an "or" is needed in type specifications, consider defining
     a class or function ``Or`` to take the arguments.  Cleaner and clearer
     interface than just using some implicit mechanism.

Partial instantiation of parameterized types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Parameterized types which take a ``None`` argument as a type parameter
are defined to match any type in that slot.  A partial instantiation of a
parameterized type can bind type of some of those ``None`` wildcard
types. ::

   t_real = TypeObject("Real")
   t_matrix = TypeObject("Mat", (None, t_int, t_int))
   t_real_matrix = t_matrix.set_param_type((1, 0), t_real)

The current syntax above uses indexing of the arguments with integer
indices for the arguments of the original TypeObject (the first argument
to ``set_param_type`` is a tuple indexing first the parameter position
and then the index within the parameter value.

Comparing type signatures
^^^^^^^^^^^^^^^^^^^^^^^^^

We have both actual type signatures, and defined type signatures.  They are
both represented as a ``FunctionType`` object.  We need to be able to check
that the ``ActualTypes`` for the actual arguments matches the defined
``TypeSpec`` for the function (perhaps performing conversion).  We also need to
choose which type signature to use if multiple conversions are possible.

Juxtaposition operators (jop)
-----------------------------

In mathematics it is common to define implicit operators between two objects.
The most common example is implicit multiplication between variables when they
are written next to each other, or juxtaposed.  We call this the
**juxtaposition operator** or **jop**.  The juxtaposition operator is a special
type of operator which can be defined.  It is not physically present, but in
some contexts it is **inferred**.

A juxtaposition operator (here with whitespace) allows expressions like
this::

   x = 2 pi y + 4 f(x)

An expression potentially consists of an operator between every pair of tokens.
The parser must determine when to infer a jop by using things like the kinds of
tokens and any type information which is available.  It must also implement
the correct precedence for the operator.

There are various ways that one might consider implementing a juxtaposition
operator.  You could build juxtaposition into the grammar itself and then just
implement that grammar.  That can be inconvenient to express and implement
(introducing many special cases) and difficult to extend.  In a Pratt parser
you would need to define special head handlers for any possible left operand of
a juxtaposition operator, with the logic to determine whether or not to infer
the operator.  You could attempt to hack the lexer to recognize such situation
and insert an operator, but the lexer would only have access to lower-level
information in making the decision.  At the higher, parsing level you can use
lookahead and inject a special token whenever such a situation is recognized.
The latter approach is essentially the approach taken here.

The juxtaposition operator is implemented by modifying the definition of
``recursive_parse``.  First, we need to make some assumptions about when a jop
can possibly be inferred and when it cannot be.  These rules are assumed for
juxtaposition operators:

1. A jop is always an infix operator, never a prefix operator or postfix
   operator.  Prefix and postfix jops do not really make sense, anyway.

2. A jop must obey precedence rules just as if it were an explicit infix
   operator.

3. By default some ignored character (usually whitespace) must occur at the
   point where the jop is inferred.  This option can be turned off for special
   cases (such as when single-letter variables are always used in expressions
   like ``2xy - 4x!``).  Some separation is required by default because
   multi-character variable names can easily collide.  For example, if ``p``,
   ``i``, and ``pi`` are defined identifiers then ``pi`` is unambigouously the
   combined identifier (the lexer determines that), but the user may intend
   ``p*i``.  So, assuming whitespace is ignored, ``p i`` would be required by
   default in order to infer an operator.

4. A jop must behave as an ordinary token when it is inferred, such as allowing
   multiple tail handler functions based on preconditions (heads would never be
   called, see below).  One exception is that if no preconditions match then a
   jop which would otherwise be inferred is simply not inferred.

5. A jop can only occur to the left of a token with a head and no tail.  The head
   is needed since the head of the jop will be called to get its right operand.
   A tail is ruled out since then the token has been defined as an infix or
   postfix operator.  This essentially means that a jop will never be inferred
   to the immediate left of an explicit infix or postfix operator.  This
   matches the common mathematical usage, where ``2 - 4`` does never equals ``2
   * (-4)``.  Some examples::

      4! x  # OK if '!' is only postfix and 'x' is never infix or prefix.
      4 x!  # OK if 'x' is never infix or prefix.
      4 -x  # Not OK (except if '-' is only postfix and whitespace is off).
      (x) y # OK.
      x (y) # OK if '(' is not an infix or postfix operator.

   Consider when function evaluations ``f(x)`` are used in conjunction with a
   jop and when parens also defined as a grouping operation.  The final example
   above shows that in that case function evaluations *must* be defined by
   lookahead to "(" from the function identifier rather than by defining a tail
   for "(".

6. A jop can only be inferred at what would otherwise be the end of a
   subexpression.  This follows from 5 above, since there would be no tail
   handler to call in order to continue evaluating the subexpression.  Unless a
   jop is defined any case where a jop is inferred would always an error
   condition.  That is because the prec of 0 on the next token (by 5) will act
   like an end-token and cause the parsing to hang before the real end-token is
   reached.  So a jop extends the language without invalidating any previously
   valid expressions.

.. note::

  Using a jop might complicate some uses of lookbehind.  If using both the
  possible interactions should be considered.

If types are being used then at the point where a jop is inferred you know the
type information for the left operand (at least a list of possible types, if
overloading on return is being used).  That information can be incorporated
into the preconditions for a jop (by 4 above no jop is inferred if its
preconditions fail).  Like with ordinary overloading, though, you do not know
the type of the (potential) right operand.  You can only look at the lookahead
tokens.  On the other hand, the jop will only be inferred in what would
otherwise be an error condition (by 6).  So you can just check the type of the
right operand in the tail for the jop and raise an error if necessary.

Lookbehind
----------

The parser can use lookahead information from the lexer in defining
preconditions, etc.  In some cases lookbehind information, at the previous
``processed_left`` values for the current subexpression, could be useful.  This
is a simple modification, which has been implemented.  In the
``recursive_parse`` function, whenever the ``processed_left`` variable is
assigned a new value, the value is also appended to a list called
``lookbehind``.  That list is passed to all tail handler functions in addition
to the ``processed_left`` value.

By looking at the previously processed result you have access to more
information such as resolved type information (not just token label
information).  The lookbehind tokens have already been processed.  Of course
you already can look at the ``left`` variable in a tail handler and see the
type of the subexpression for, say, the type of the left operand of an operator.

This is not a feature which will be commonly used, but it may have use cases.
Note, though, that when references are used the previous values will be
modified versions of what they were when they were first appended.

Possible but unimplemented generalizations
------------------------------------------

The following subsections discuss some possible generalizations which are not
currently implemented.

Modifiable token precedence values
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Is it possible to allow tokens to change their prec according to their
conditions? What if you just redefined the ``prec`` function to return a value
that could change depending on conditions.  You could even have pass a function
``token_subclass`` to evaluate the prec, perhaps.  This would modify the prec
calculation for the token subclass, but not for any other token subclass.

This might be useful for jop, so you could turn an identifier into a binding
operator of the right precedence if it were preceeded by another identifier.
Would need to be done for vars, functions, and numeric literals.  You could
look at type info for the left one...

Or you could just look for the conditions in ``recursive_parse``.

NOTE change below indented paras to a "not implemented but could be" type of
thing.  All tokens with the same label must now have the same prec, the static
one saved with the class (and the last one set).  Include discussion of how
things have to look the same from the peek token as they do from current.  The
modifiable prec stuff was too much effort, and when more than lookahead is
being use it *also* has to take that info into account.

Gist: - The prec values are associated with tail handlers.  - Can be
implemented with a peek and pushback and a few other rules.  - Not currently
implemented.

A limitation of single-token lexer lookahead with lookahead dispatching is that
all tail handlers for the same token must have the same prec.  This is because
when the prec can vary with the tail it requires two tokens of lexer lookahead
to find the tail of the token one peek ahead (and hence also find the prec).
More specifically, in the main ``recursive_parse`` loop we need to find the
prec of the peek(1) token.  If lookahead dispatching is being done that
requires a peek(2) in order to get the peek(1) of the first peek token (in
order to resolve the tail of the peek(1) token and hence find its prec).

The current program can use lexer lookahead set to one or two tokens (with two
the default).  If one token lookahead is used then it will always use the same
prec for every tail of a token (stored statically in the token subclass
definition).  The last-defined value will be used, overwriting any previous
ones.  When two-token lexer lookahead is used the prec values are associated
with individual tail handlers and not with the token itself (and recall that
several tail handlers can be associated with a token when lookahead dispatching
is used).  If no tail is found the default 0 prec is returned.

As far as implementing the prec stuff, as noted the prec is treated as an
attribute of the tail handlers.  The prec value is simply pasted onto the led
functions before they are placed in the dictionary where they are stored.  As
long as we can access the correct lookahead-keyed tail function we can also get
the prec associated with it (which is hence similarly lookahead-keyed).

::

      # TODO: this function should now be replacable with any function
      # at all which follows the rule.  Continue refactoring in that
      # direction.  Also, move below text to documentation of how to
      # write such a dispatching function.

      # In doing the dispatching this function can "look but not touch,"
      # so, for example, it can use peek but not next.
      # 
      # To use non-static prec values there is another restriction.  This
      # function must also be relative to pos_in_lex as the current
      # token.  So, for example, it cannot use the default arguments to
      # peek methods.  To use peek() it should modify the numerical
      # argument by replacing peek(k) with peek(pos_in_lex+k), or else
      # very carefully use lex.token_buffer directly.  It also cannot use
      # the most-recent lookbehind since that has not been calculated for
      # the peek token.
      #
      # This function must work position-independently for pos_in_lex=0
      # and pos_in_lex=1.  This is so that in the following step, after
      # next() is called, the tail which is looked up for the peek/next
      # token is the same as was looked up previously (and hence has the
      # same prec pasted onto it).
      #
      # The function returned with pos_in_lex=1 is needed by self.prec
      # to find a tail handler in order to look up the prec saved with it.
      # If this method can possibly fail in that case then it should
      # ALWAYS raise RevertToStaticBp whenever pos_in_lex=1, so that
      # self.prec can instead return a static value.  This has the
      # effect of forcing all tail handlers for the kind of token to have
      # the same value.  For example, when only one token of lookahead is
      # available and dispatching is based on lookahead then peek(2) will
      # fail, and so RevertToStaticBp should be raised.  In that example,
      # it would not be possible for a token to have a prec=5 when the
      # lookahead token is lpar and prec=0 when the lookahead token is
      # rpar.  The same value (the last set) will be used.  Not a severe
      # restriction, but it should be noted.

General multi-token lookahead
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Ordinary multiple-token ordinary lookahead, looking into the token stream of
the lexer, is already implemented and allowed.  You can use any lookahead in
defining preconditions as long as the lexer was defined with sufficient
lookahead.

The limitation of the above is that you can look ahead and see the tokens but
you do not know which handler function will be dispatched for that token at the
time when the token is the current token.  This means that you cannot get the
prec value for the token, since it is considered to be an attribute of the
handler functions.  In most cases this is not necessary, but in some cases it
might be useful.

If greater lookahead is going to be incorporated into the ``recursive_parse``
routine's while loop you need to know the prec values, because it needs to look
ahead (currently one level) to get the prec for its while loop.  Other
precondition calculations may also require the prec values.

The interactions with the jop feature would need to be considered.

This is currently not well-defined and is not implemented.  It may be something
to consider in a future version.

General position-dependent handling functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Instead of head and tail we could just have a collection of generic handler
functions associated with each function.  These functions would be passed one
argument giving the position in the current subexpression (e.g., ``"[0]"`` for
the head and ``"[1:]"`` for the tail).  We might have something like::

       denote(pos_selector, <rest_of_args>)

which, if ``HEAD`` and ``TAIL`` are defined constants, can be called as::

       denote(HEAD, ...)

or::

       denote(TAIL, ...)

This is not done because head and tail handlers are usually distinct in their
code and semantics, and because the same effect can be achieved by keeping a
list of look-behind expressions.  Any head or tail handlers can look at the
look-behind list and infer the exact position of their subexpression in the
expression-evaluation recursion level that is calling them.

Subexpression lookahead
^^^^^^^^^^^^^^^^^^^^^^^

Preconditioning on lookahead one subexpression and one token would be one way
to resolve things like ternary operations where the first operator is also an
operator by itself: ``x ?  y`` versus ``x ? y : z``.  Similarly, an if-then
with optional else can be resolved that way: ``if <test> then <action>`` versus
``if <test> then <action> else <other-action>``.  The tail handler for
processing the first operator can be chosen dependent on the token type two
tokens ahead.

Perhaps the major advantage of this lookahead is that operator overloading can
be made dependent on the types (or other properties) of the fully resolved
operands, not just the left operand and the raw lookahead tokens.

This would be a useful feature, but it has some downsides.  It might require
backtracking, and would have to be implemented carefully to avoid as
much backtracking as possible.  It has the potential to interact with other
features, such as the jop feature.  So the implementation would need to be
carefully considered.

More complex types
^^^^^^^^^^^^^^^^^^

Generally we might want:

 - types and subtypes, with equivalence defined
 - parameterized types
 - maybe multiple types, but some of this is included
   in overloading

Suppose we consider more complex type signatures, like, for example,
``Array(int)    Array(Array(int))    VectorSpace(scalar_type, add_fun,
smult_fun)`` For finite vector spaces we might also want the parameters m and n
to be specified as parameters so, for example, we can check addition and
multiplication.

These declarations have their own grammar.  If they are part of the language
itself then they will have subtrees associated with them.  The top node of such
a subtree represents the full type.

If a declaration like that is in a definition then we can just set the type to
be the subtree, perhaps (or the corresponding AST).  As a semantic action we
would remember the declaration.

Then, on type-checking, something like f(x) would appear.  We would know the
subtree for the ``val_type`` of an argument to f from the function declaration,
and the subtree for the type itself from the type declaration assigning that
type to variable x.  We just need to compare the subtrees.

Why keep them as subtrees? We could perhaps make them back into strings and
compare those, but that is extra work and there may be some advantage to
keeping the tree form.

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

