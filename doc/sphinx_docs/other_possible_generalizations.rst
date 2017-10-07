Appendix A: Possible (but currently unimplemented) generalizations
==================================================================

The following subsections discuss some possible generalizations which are not
currently implemented.

Modifiable token precedence values
----------------------------------

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

Currently the ``PrattParser`` class has the restriction that every token must
have a fixed precedence.  Even under different preconditions the precedence
must be the same.  This is not a huge restriction, but there is the question of
how it could be removed.  A reasonable generalization is to define precedence
values to be attributes of tail handler functions (recall that they have no
effect on head handler parsing).  So assume each tail handler function can have
a different precedence.  If follows from this that different preconditions would
then have handlers with different precedences.

The implementation problem arises in the ``recursive_parse`` function.  In the
loop for tail handlers there is always a lookahead to the precedence of the
next token (which is compared to the subexpression precedence).  When the
precedence can vary with tail handlers we need to determine which precedence to
use.  In the main ``recursive_parse`` loop we need to find the precedence of
the peek(1) token.  But that precedence can vary according to the conditions at
the time when the peek token itself is processed.

There are ways to get a reasonable version of this, subject to some
limitations.  You essentially step ahead the lexer to approximate the time when
the peek token would be processed, look up the handler, and take the precedence
from that.  The definitions would need to be made precise, and the limitations
discussed.  Since that adds a lot of complexity for a feature that would be
little-used it is not currently implemented.  Interactions with other features,
such as jops, would also need to be considered.

General position-dependent handling functions
---------------------------------------------

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
code and semantics, and because the same effect can be achieved by using
lookbehind (which is implemented).  Any head or tail handlers can look at the
lookbehind list and infer their exact position in their subexpression (at the
current recursion level).

Subexpression lookahead
-----------------------

In this generalization you would be able to use lookahead on the next
subexpression, not just the next token.  Preconditioning on a subexpression
lookahead and then a token lookahead would be one way (probably not the best
way) to resolve things like ternary operations where the first operator is also
an operator by itself: ``x ? y`` versus ``x ? y : z``.  Similarly, an if-then
with optional else could be resolved that way: ``if <test> then <action>`` versus
``if <test> then <action> else <other-action>``.  The tail handler for
processing the first operator can be chosen dependent on the token type two
tokens ahead.

The real advantage of this kind of lookahead is that operator overloading can
be more easily made dependent on the types (or other properties) of the fully
resolved operands, not just the left operand and the raw lookahead tokens.

This would be a useful feature, but it would obviously be more expensive since
full subexpressions would have to be provisionally parsed.  It has the
potential to interact with other features, such as the jop feature.  So the
implementation would need to be carefully considered.  Some form of this is
likely to be implemented at some point, if only to make overloading on types
easier to implement.

You can currently do something similar inside a handler function if you really
must.  You can save the state of the lexer, call ``recursive_parse`` to get a
subexpression, and then restore the state of the lexer.

Constructs with multiple possible trigger tokens
------------------------------------------------

The current implementation follows the Pratt parsing scheme of having handlers
associated with triggering tokens, as well as the head or tail position.  In
general, the looking at the current kind of token could instead be implemented
as part of the preconditions functions.

This would be less efficient in the sense that many more preconditions
functions would potentially need to be sequentially run find the handler for a
given token.  (The current implementation uses a tree to quickly go to a
smaller set of constructs, based on the head or tail property and the token
label.) There are, however, optimizations which could be applied to make this
more efficient.  Preconditions functions would become more complex, since every
construct which is based on the current kind of token (formerly the triggering
token) would need to explicitly specify which kinds of tokens in some way.

The current setup follows Pratt parsing more closely, and the equivalent to the
above is to declare multiple constructs, one for each kind of triggering token.
A null-string token could also be used to do something similar.

More complex types
------------------

Generally we might want:

 - Types and subtypes, with reasonable notions of type equivalence defined.
 - Parameterized types or templates.
 - Automatic conversions.

