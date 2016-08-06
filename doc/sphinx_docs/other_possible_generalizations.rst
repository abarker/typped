Appendix I: Possible (but unimplemented) generalizations
========================================================

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
-----------------------------

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
code and semantics, and because the same effect can be achieved by keeping a
list of look-behind expressions.  Any head or tail handlers can look at the
look-behind list and infer the exact position of their subexpression in the
expression-evaluation recursion level that is calling them.

Subexpression lookahead
-----------------------

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
------------------

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

