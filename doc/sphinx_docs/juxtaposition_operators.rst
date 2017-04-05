Juxtaposition operators (jops)
==============================

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
and when not to by using information such as the kinds of tokens which are
juxtaposed.  When available, type information can also be used.

The jop mechanism must also implement the correct precedence for any implied
operator it represents.  The inferred operator should behave exactly as if the
equivalent operator token had actually appeared in the expression.  It should
also allow for operator overloading if the language itself allows for operator
overloading.

.. topic:: Ways to implement a juxtaposition operator

   There are various ways that one might consider implementing a juxtaposition
   operator.  A few possibilities are briefly discussed here.
   
   Juxtaposition could be built into the formal grammar itself, and then that
   grammar could be implemented.  That can, however, make the grammar
   inconvenient to express and make implementations difficult (introducing many
   special cases).  It can also make it difficult to add new operators to
   extend the grammar.  In a Pratt parser you would need to define special head
   handlers for any possible left operand of a juxtaposition operator, with the
   logic to determine whether or not to infer the operator.

   In a Pratt parser it would be fairly simple to implicitly modify the grammar
   by defining all the syntax elements which can participate in a jop to be
   either prefix or postfix operators.  For example, a postfix operator using
   lookbehind on the type of the previous subexpression.  This adds a lot of
   operators, which is contrary to the usual practices.  It seems especially
   unusual to apply it to types like real numbers.  Questions of possible
   ambiguities need to be considered.  This is not the approach taken here with
   jops, but it might be worth considering in some cases.
   
   Another approach is to attempt to hack the lexer to recognize when to infer
   a jop, and then insert and return a token representing any inferred
   operators.  The downside is that the lexer only has access to lower-level
   information for making the decision as to when to infer a jop.
   
   At the higher, parsing level lookahead can be used to recognize when a jop
   should be inferred.  Then a special token can be injected into the token
   stream whenever such a situation is recognized.  This approach is
   essentially the approach taken in the Typped parser implementation of jops.

Assumptions for inferring jops
------------------------------

The juxtaposition operator is implemented by modifying the definition of the
function ``recursive_parse``.  First, we need to make some assumptions about
when a jop can possibly be inferred and when it cannot be.  These rules are
assumed for juxtaposition operators:

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
   if no preconditions match for the jop token then, instead of raising an
   exception, a jop which would otherwise be inferred is simply not inferred.

5. A jop can only occur to the left of a token with a head handler and no tail
   handler.  The head is needed since the jop will call it in order to get its
   right operand.  A tail is ruled out since that implies that the token has
   been defined as an infix or postfix operator.  This essentially means that a
   jop will never be inferred to the immediate left of an explicit infix or
   postfix operator.  This matches the common mathematical usage, where ``2 -
   4`` never equals ``2 * (-4)``.  Some examples, where the space character
   is a possible jop::

      4! x  # OK if '!' is only postfix and 'x' is never infix or prefix.
      4 x!  # OK if 'x' is never an infix or prefix operator.
      4 -x  # Not OK (except if '-' is only postfix and whitespace is ignored).
      (x) y # OK.
      x (y) # OK if '(' is not an infix or postfix operator in context.

   Consider how the final example above relates to the case when a function
   evaluation such as ``f(x)`` is used in conjunction with a jop, and when
   parens are also defined as a grouping operation.  The result can be
   ambiguous unless spaces are required for jops and spaces are disallowed
   in function calls.  If ``x`` is explicitly declared as a function name
   token distinct from variables then the situation can also be disambiguated.
   
6. A jop can only be inferred at what would otherwise be the end of a
   subexpression.  This actually follows from 5 above, since there would be no
   tail handler to call in order to continue evaluating the subexpression.  So
   any case where a jop is inferred would otherwise be an error condition if no
   jop were defined.  This is because the prec of 0 on the next token (by rule
   5) will act like an end-token and cause the parsing to hang before the
   actual end-token is reached.  So a jop extends the language without
   invalidating any previously-valid expressions.

As far as rule 4, in the Typped parser jops are represented by a special type
of token that can be defined with the ``def_jop_token`` method.  It is returned
whenever a jop is inferred.  The ``def_jop`` method then makes use of this
token and functions in almost the same way as the ``PrattParser`` methods for
actual infix operators.  All the jop handlers and their preconditions are
registered with this jop token.

.. note::

  Using a jop might complicate some uses of lookbehind.  If using both the
  possible interactions should be considered.

Disambiguating jops
-------------------

Juxtaposition operators are convenient, but it is easy to create ambiguous
situations with them.  If whitespace is required for a jop (usually excluding
the return character) then every string of such whitespace in the parsed text
will be tested for the conditions to infer a jop.  If no whitespace is required
to infer a jop then the conditions need to be checked between every pair of
tokens.

The conditions above work in simple situations, but in more complex situations
it can become necessary to set the jop token's preconditions to exclude
ambiguous cases.  For example, jop token can have a precondition that looks at
the token label of the previous token in the token stream as well as at the
token label of the next token.  So the kinds of tokens which are potential
operands can be taken into account.

If types are being used without overloading on function return values then type
information about the two surrounding tokens can be used in the preconditions
of the jop.  This tends to be better information for inferring a jop or not
because it is based on the full, evaluated subexpressions rather than just the
individual tokens.

Using type information from the left operand is easy because at the point when
a jop is inferred you already know the type information for the left operand
(or at least a list of possible types if overloading on return is being used).
That subexpression has already been evaluated and stored in the token tree
rooted at `left`.  So you just look at `left.type_sig` or a similar attribute.
This information can be incorporated into the preconditions for a jop (since by
4 above no jop is inferred if its preconditions fail).

Using the type information for the right operand is a little more involved.  At
the point when the conditions for a jop are being evaluated you do *not* know
the type of the (potential) right operand.  You can only look at the lookahead
tokens in the token stream.  On the other hand, a jop will only be inferred in
what would otherwise be an error condition (by rule 6).  That is, the right
operand does not have a tail handler anyway.  So you can just provisionally
assume a jop and infer it.  Then inside the tail-handler of the jop you check
the type of the right operand after the tail-handler calls ``recursive_parse``.
If it does not match the requirement you can then raise the appropriate
exception.  (At some point this functionality may be included as an option to
the ``def_jop`` method.)

The juxtaposition operator can be overloaded just like ordinary infix
operators.  But as far as overloading based on the types of the operands you
can only overload the jop based on the type of the left operand.  The tail
handler for the jop must then implement any further desired overloading based
on the right operand.

Note that when overloading by return type is being used you are not guaranteed
to have unique, resolved type information for the parse subtrees
(subexpressions) returned from the `recursive_parse` function because the types
may not yet be resolved.  Overloading on return types cannot be resolved purely
bottom-up and generally requires another pass back down the full parse tree.
You can, however, make use of the list of *possible* types at the current state
of type resolution.

