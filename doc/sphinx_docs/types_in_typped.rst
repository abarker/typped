Implementing typed languages in Typped
======================================

The Typped package provides a fairly general type-definition and type-checking
mechanism.  It is optional, however, and can be ignored for untyped languages.
When they are defined the types will be automatically checked at parse-time
(according to any options which are set).  The type system also allows for
operator overloading, including optional overloading on return types.
Overloading currently has the mild restriction that all overloads defined for
an operator must share the same precedence value.

Each kind of token (corresponding to a token label) can have a type associated
with it.  The type is then associated with the construct which has that token
as the subtree root in the parse tree.  Type-checking is done on the parse tree
as it is constructed, making sure that the types of the children of a node (if
any) match the declared argument types, and that the types of the nodes
themselves match their declared value type.

Only simple types are currently implemented (though the mechanism can easily be
made more general).  The type either matches or it does not, so the types are
essentially just string labels and are equivalent iff the labels are the same.
At some point type heirarchies and type conversions may be added.

Type signatures
---------------

Types are defined using the `def_type` method of a `PrattParser`.  For example,
if `parser` is a `PrattParser` instance then a type is declared as follows:

.. code-block:: python

   t_int = parser.def_type("t_int")

After that, the Python variable `t_int` can be used as a type.

The basic type specification for a node in the parse tree is the saved as an
instance of the `TypeSig` class.  This class stores the type of the value at
the node as `val_type`, and stores the types of the children nodes as a tuple
`arg_types`.

The built-in parsing methods of the `PrattParser` class take arguments which
correspond to the `val_type` and `arg_types` specifications, so many users will
not need to explicitly use the `TypeSig` class.  It is simple to use, though.
For example, using the type `t_int` defined above, we might have:

.. code-block:: python

   sig = TypeSig(t_int, [t_int, t_int])

This specifies a value type at a parse-tree node of `t_int`, and two children
which also both have type `t_int`.  This specification would, for example, be
used for a function of two variables which takes two integers and returns
another integer.  It would be set for the token which ends up as the root of
the parse subtree representing the expression.

If the `arg_types` list is an individual type rather than a tuple then any
number of arguments/children are allowed, and they must all have that type.
For example, this would be a function taking any number if `t_int` arguments
and returning a `t_int`:

.. code-block:: python

   sig = TypeSig(t_int, t_int)

The value `None` can be passed to the initializer of `TypeSig` in place of a
defined type.  As a `val_type` it matches any type at the node, and in place of
the `arg_types` tuple it declares that no checking is to be done on the
children.  The `None` value can also be used inside the `arg_types` list to
specify arguments that are not type-checked.  Also, any argument to `TypeSig`
which is omitted will be given the default value of `None`.  So, the following
all specify that no type-checking at all is to be done:

.. code-block:: python

   TypeSig() == TypeSig(None) == TypeSig(None, None)

On the other hand, this specification will only check that the number of arguments
equals three:

.. code-block:: python

   sig = TypeSig(None, None*3)

Implementation details
----------------------

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

Comparing types and type signatures
-----------------------------------

To compare types we need a mechanism to do the following.

1. Tell when types are the same.

2. Tell when one type is an instance of another (heirarchical types).

3. Indicate what types can be converted to what other types.

3. Tell when one type should be converted to another (assuming it can be) and
   how to do it (or what to do) if there is more than one way.

The `TypeObject` class has methods for this.

We have both actual type signatures, and defined type signatures.  They are
both represented as a ``FunctionType`` object.  We need to be able to check
that the ``ActualTypes`` for the actual arguments matches the defined
``TypeSpec`` for the function (perhaps performing conversion).  We also need to
choose which type signature to use if multiple conversions are possible.

For type signatures we need to be able to do the following.

1. Tell when an actual type signature matches a formal one (in overloaded context 
   this is necessary).

2. Tell which one to use (or what to do) if more than one defined signatures
   match.

More implementation details
---------------------------

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

