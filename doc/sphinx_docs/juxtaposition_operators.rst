Juxtaposition operators (jops)
=============================

In mathematics it is common to define implicit operators between two objects.
The most common example is implicit multiplication between variables when they
are written next to each other, i.e., juxtaposed.  We call this the
**juxtaposition operator** or **jop**.  The juxtaposition operator is a special
type of operator which can be defined.  It is not physically present, but in
some contexts it can be **inferred**.

A juxtaposition operator (here with whitespace) allows expressions like
this::

   x = 2 pi y + 4 f(x)

When jops are allowed an expression can potentially have an implicit operator
between every pair of tokens.  The parser must determine when to infer a jop
(and when not to) by using information such as the kinds of tokens which are
juxtaposed.  When available, type information can also be used.

The jop mechanism must also implement the correct precedence for any implied
operator it infers.  The inferred operator should behave exactly as if the
equivalent operator token had actually appeared in the expression.  It should
also allow for operator overloading if the language itself allows for operator
overloading.

.. topic:: Ways to implement a juxtaposition operator

   There are various ways that one might consider implementing a juxtaposition
   operator.  A few possibilities are briefly discussed here.
   
   Juxtaposition could be built into the grammar itself, and then that grammar
   could be implemeted.  That can, however, make the grammar inconvenient to
   express and make implementations difficult (introducing many special cases).
   It can also make it difficult to add new operators to extend the grammar.
   In a Pratt parser you would need to define special head handlers for any
   possible left operand of a juxtaposition operator, with the logic to
   determine whether or not to infer the operator.
   
   Another approach is to attempt to hack the lexer to recognize when to infer
   a jop, and then insert and return a token any inferred operators.  The
   downside is that the lexer only has access to lower-level information in
   making the decision of when to make the inference.
   
   At the higher, parsing level lookahead can be used to recognize when a jop
   should be inferred.  Then a special token can be injected into the token
   stream whenever such a situation is recognized.  This approach is
   essentially the approach taken in the Typped parser.

Assumptions for inferring jops
------------------------------

The juxtaposition operator is implemented by modifying the definition of
``recursive_parse``.  First, we need to make some assumptions about when a jop
can possibly be inferred and when it cannot be.  These rules are assumed for
juxtaposition operators:

1. A jop is always a binary infix operator, never a prefix operator or postfix
   operator.  Prefix and postfix jops do not really make sense, anyway.

2. A jop must obey precedence rules just as if it were an explicit infix
   operator.

3. By default some ignored character (usually whitespace) must occur at the
   point where the jop is inferred.  This option can be turned off for special
   cases (such as when single-letter variables are always used in expressions
   like ``2xy - 4x!``).  The default is to require some separation because
   otherwise multi-character variable names can easily collide.  For example,
   if ``p``, ``i``, and ``pi`` were all defined identifiers then the string
   ``pi`` would would be recognized by the lexer as the single token for
   ``pi``, but the user may have intended ``p*i`` with default multiplication.
   So, assuming that whitespace is ignored, ``p i`` would be required by
   default in order to infer an operator.

4. A jop must behave as an ordinary token when it is inferred, such as allowing
   multiple tail handler functions based on preconditions (a head handler for a
   jop would never be called, see below).  One exception to this rule is that
   if no preconditions match then, instead of raising an exception, a jop which
   would otherwise be inferred is simply not inferred.

5. A jop can only occur to the left of a token with a head handler and no tail
   handler.  The head is needed since the jop will call it in order to get its
   right operand.  A tail is ruled out since that implies that the token has
   been defined as an infix or postfix operator.  This essentially means that a
   jop will never be inferred to the immediate left of an explicit infix or
   postfix operator.  This matches the common mathematical usage, where ``2 -
   4`` never equals ``2 * (-4)``.  Some examples::

      4! x  # OK if '!' is only postfix and 'x' is never infix or prefix.
      4 x!  # OK if 'x' is never an infix or prefix operator.
      4 -x  # Not OK (except if '-' is only postfix and whitespace is ignored).
      (x) y # OK.
      x (y) # OK if '(' is not an infix or postfix operator.

   Consider how the final example above relates to the case when a function
   evaluation such as ``f(x)`` is used in conjunction with a jop, and when
   parens are also defined as a grouping operation.  In that case the ``(``
   token will have a head handler function.  If spaces are not required to
   infer a jop and a space is not required after the function name then, in the
   absence of other disambiguating information, standard functions cannot be
   defined via a tail handler for the ``"`` token (which is a common way to do
   it).  The result would be ambiguous.  This can be avoided, though, or
   standard functions can be defined as an identifier with a preconditioned
   lookahead to the ``(`` token.

6. A jop can only be inferred at what would otherwise be the end of a
   subexpression.  This actually follows from 5 above, since there would be no
   tail handler to call in order to continue evaluating the subexpression.  So
   any case where a jop is inferred would otherwise be an error condition if no
   jop were defined.  This is because the prec of 0 on the next token (by 5)
   will act like an end-token and cause the parsing to hang before the actual
   end-token is reached.  So a jop extends the language without invalidating
   any previously-valid expressions.

.. note::

  Using a jop might complicate some uses of lookbehind.  If using both the
  possible interactions should be considered.

Disambiguating jops
-------------------

Juxtaposition operators are convenient, but it is easy to create ambiguous
situations with them.  If whitespace is required for a jop then every
whitespace token in the parsed text will be tested for the conditions to infer
a jop.  If no whitespace is required to infer a jop then the conditions will be
checked between every pair of tokens.

The conditions above work in simple situations, but in more complex situations
it can become necessary to set the jop's preconditions to exclude ambiguous
cases.  For example, the jop's precondition can look at the token label of the
next token in the token stream.

If types are being used with no overloading on function return values then type
information about the two surrounding tokens can be used in the preconditions
of the jop.  This tends to be better information for inferring a jop or not
because it is based on the full, evaluated subexpressions rather than just the
individual tokens.

Using type information from the left operand works because at the point when a
jop is inferred you already know the type information for the left operand (or
at least a list of possible types, if overloading on return is being used).  It
is already evaluated, and stored in the token tree rooted at `left`.  So you
just look at `left.type_sig` or a similar attribute.

That information can be incorporated into the preconditions for a jop (by 4
above no jop is inferred if its preconditions fail).

Using the type information for the right operand is a little more involved.  At
the point when the conditions for a jop are being evaluated you do *not* know
the type of the (potential) right operand.  You can only look at the lookahead
tokens in the token stream.  On the other hand, a jop will only be inferred in
what would otherwise be an error condition (by 6).  So you can just assume a
jop (there is only one jop token to choose from, though its function sigs can
be overloaded) and check the type of the right operand inside the tail handler
function registered for the jop (after the jop's tail handler gets the right
operand with `recursive_parse`).  If it does not match the requirement you can
raise an exception.

TODO: Have a way that a handler function can register an "expected right operand
type" and then have a routine that the jop's handler function can call which
checks the last registered expectation (which needs to be cleared at the
appropriate time).

TODO: is this discussion more general than just jops?  Doesn't it apply to
general preconditions functions for inferring actual operators, too?  No, it
doesn't, because the actual tokens do not need to be inferred or not inferred.
The token will always have a handler function called (assuming some
preconditions fun matches, otherwise error) and the handler function will
always get the token with recursive_parse.  The type system will evaluate the
parse tree after that, and *choose* the overloaded typesig based on the actual
types.  That is all the parser is in charge of; the eval fun is associated with
the type sig and any other actions are up to the user.  This *should* be
discussed somewhere, though, as a discussion of how type information
overloading works.  To reiterate, if we want overloading on "*" so that it only
applies between numbers then we have a syntax error otherwise, since the "*" is
explicit in the token stream.  If it has multiple definitions as an infix
operator then you would have to deal with those inside the handler.

Note that as far as overloading the juxtaposition operator you can only
overload the jop based on the type of the left operand (and any other
information in the precondition function).  The tail handler for the jop must
then implement any further desired overloading based on the right operand, as
described above.  

Note that when overloading by return type is being used you do not have unique
type information for any parse subtree (subexpression) obtained from the
`recursive_parse` function because it may not yet be resolved.  That is not
implemented because it would require some sort of backtracking.  You can,
however, make use of the list of *possible* types at the current state of type
resolution.

