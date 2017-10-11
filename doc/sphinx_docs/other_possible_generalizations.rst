Appendix A: Possible (but currently unimplemented) generalizations
==================================================================

The following subsections discuss some possible generalizations which are not
currently implemented.

Modifiable token precedence values
----------------------------------

Currently the ``PrattParser`` class has the restriction that every token must
have a fixed precedence.  Even under different preconditions the precedence
must be the same.  This is not a huge restriction, but there is the question of
how it could be removed.  A reasonable generalization is to define precedence
values to be attributes of tail handler functions or, in the Typped case,
attributes of constructs that hold tail handler functions.  (Recall
that precedences have no effect on head handler parsing.)

Assume each tail handler function can have an arbitrary precedence.  In this
case different preconditions can cause handlers functions with different
precedences to be dispatched.

The implementation problem arises in the ``recursive_parse`` function.  In the
loop for tail handlers there is always a lookahead to the precedence of the
next token (which is compared to the subexpression precedence).  So we need to
find the precedence of the `peek(1)` token.  Precedence can vary according to
which tail handler (construct) *would be* dispatched, so we need to determine
that.  But which constuct "wins" the dispatching can vary according to the
conditions at the time when that peek token itself is processed.

There are ways to get a reasonable version of this, subject to some
limitations.  You essentially step the lexer ahead to approximate the time when
the peek token would be processed, look up the handler/construct, get its
precedence value, and then step the lexer back.  This adds complexity and an
extra `go_back` operation per subexpression for a feature that would probably
be little-used.  It is not currently implemented (but there is currently
commented-out code that could be adapted to do it).  Interactions with other
features, such as jops, would also need to be considered.

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

 - Heirarchies of types and subtypes, with reasonable notions of type equivalence
   defined.
 - Unions.
 - Automatic conversions.
 - Parameterized types or templates.

Floating point precedence values
--------------------------------

The builtin functions that set right associativity currently assume that the
precedence values are ints.  The examples also use ints.  As explained
below, this is not strictly required.  Limited precision floating point values
already work in the current framework, provided a different value is subtracted
to get right associativity.

Notice that in the while loop of `recursive_parse` the comparison is::

   while lex.peek().prec() > subexp_prec:

Assume that precedence values are ints.  Consider a tail handler for, say,
infix `*` with a precedence of 5 and left associativity.  Say we process
`2*2*2`.   The head handler for the full expression calls `recursive_parse` to
process the first `*` (with `2` as the `processed_left` value).

For left associativity the subexpression precedence of 5 is passed to this
`recursive_parse` call.  When the loop in that call of `recursive_parse` peeks
at the second `*` token, with a precedence of 5, it breaks and returns because
5 > 5 is false.

If instead the subexpression precedence had been 4, for right associativity,
the peek would again see the second `*` token with a precedence 5, but since 5
> 4 loop would continue.  It continues until it sees a token with precedence
strictly greater than 4, and then it breaks.

Notice that in the latter case the behavior with respect to peeking a token
with token precedence of 4 is still the same as in the first case.  The
subexpression precedence for right associativity just needs to be less than 5
and greater than or equal to the next lowest precedence value (which in this
case is 4 because we assumed ints).

Precedences are only used in comparisons, and the only arithmetic on
precedences is subtracting from a precedence value to get a subexpression
precedence that is smaller, but not too small.  This means that we could
equally well have used 5 - 0.1 as the subexpression precedence in the latter
case of right associativity.

In general any kind of objects can be used for precedences, provided
comparisons work correctly for them and there is a way to get a slightly
smaller value that is still greater than or equal to the next smaller
precedence value.  In particular, precedences can be floating point numbers
restricted in precision to some number of digits.  If we restrict to three
digits of precision then precedences like 4.333 and 2.111 are allowed.  To get
the slightly lower value for right associativity just subtract 0.00001 instead
of 1.

This kind of thing is easy to implement, and has been tested, but is it a good
idea?  As of now the Typped builtins that set right associativity assume
precedences are ints.  In tweaking precedences during development sometimes
floats might be useful.  There is a slight loss of efficiency in the comparison
operations when floats are involved, but probably not enough to be a problem.
If it is implemented later it would still be backward compatible with using
ints.

As a possible alternative, just after precedence values are defined and passed
to ``def_construct`` they could always be multiplied by, say, 1000 and then
rounded to an int.  Then internally the representation would be as ints but to
the user they would look like limited-precision floats.  Subtracting one for
right associativity still works, and all parse-time comparisons are of ints.
The any error messages would need to convert the values back, however.
A exception could be raised if the rounding changed the value or if the
resulting int would overflow.

