
.. warning::

   This page is rough and needs editing.  Some of the details may have changed.

Low-level implementation details
================================

In a top-down parser the parse tree is essentially constructed bottom-up, on
the way back up the recursion.  So the leaves are the first nodes created and
they can have their types checked.  Each node farther up has the types of its
children/arguments as well as its own type checked at the time when its subtree
of the parse tree is constructed.

Based on the above, each constructed tree is guaranteed to be resolved for
types when it is first constructed --- provided that overloading is only on
function arguments.  Overloading on return types requires another pass down the
parse tree (not necessarily the full tree, but it can be in a worst case).  As
soon as a node with a unique signature is created the types in the subtree are
resolved.

Type signatures can be declared whenever a construct is defined (or redefined
for overloading).  It is passed in kwargs to the ``def_construct`` routine
whenever a construct is defined.  If the construct already exists then
overloading is assumed, and the type signature is unioned with any existing
ones.

In the Pratt parser the ``recursive_parse`` routine is run to do the actual
parsing.  As each token is read in that routine the constructs for that token
are tested to see it their preconditions match.  The winning construct (with
the highest priority among the matching ones) provides the handler function to
process the construct.  The dispatched handler does not just run the handler,
though.  It first runs the handler, then it runs ``process_in_tree`` on the
returned subtree, and then it runs ``process_and_check_node`` on the processed
subtree.

The type signature information is stored with the ``SyntaxConstruct`` instance
associated with the winning construct.  The ``process_and_check_node`` call
checks that the types of the children of the root of the returned subtree match
the associated type signature.  (Children futher down were already processed,
since we're on the way back up the recursion.)  When a unique match is found
the signature of a node is set as an attribute.  Going up the tree, the next
node can now look at the return type of those signatures to check that the
arguments of its node match its signature.

TODO: rewrite below or delete

Overloading on return values
----------------------------

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

where the int values are from literal tokens at the leaves of the parse tree.
When we reach the bottom of the tree and start going up we cannot immediately
choose the signature of ``h`` to use.  They both match arguments to ``g``.  But
only one argument to ``g`` also matches the argument to ``f`` since we can rule
out the second signature of ``f``.

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

