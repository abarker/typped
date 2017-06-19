Implementing typed languages
============================

The Typped package provides a fairly general type-definition and type-checking
mechanism.  It is optional, however, and can be ignored for untyped languages.
When types are defined and set up properly (using either the built-in methods
of the ``PrattParser`` class or custom methods) the language will be
automatically type-checked at parse-time.

Only simple types are currently implemented (though the mechanism can easily be
made more general).  As of now type are essentially just string labels which
are equivalent iff the labels are the same.  At some point type heirarchies and
type conversions may be added.

Each construct registered with a parser instance can have type information
associated with it.  This type signature is then checked against the actual
types in the parse subtree returned by the handler function for the construct.

The type system allows for operator overloading, including optional overloading
on return types.  The default is to allow overloading only on argument types.
Overloading currently has the mild restriction that all overloads defined for
an infix operator must share the same precedence value.  To disable all
overloading, set ``overload_on_arg_types=False`` in the ``PrattParser``
initializer.  To allow overloading on return types as well as argument types,
set ``overload_on_ret_types=True`` in the initializer.

Overloading is specified by re-defining a construct for a token which already
has a construct of that name.  If the construct name is the same but
the type of the arguments are different then an overloaded type specification
is created.  If overloading on return types is allowed then the types are also
considered different if the return type is different.

Operator overloading can be used for constructs which parse the same, i.e.,
using the same handler function, but can have multiple possible type
signatures.  Type resolution is then based on the actual arguments found at
parse-time.

Type information for a construct (e.g., an function evaluation subexpression)
should generally be associated with the token which ends up as the the subtree
root in the expression/parse tree.  Type-checking is done on the nodes of the
parse tree as it is constructed, making sure that the types of the children of
a node (if any) match the declared argument types, and that the types of the
nodes themselves match their declared value type.

Type signatures
---------------

Types are defined using the ``def_type`` method of a ``PrattParser``.  For example,
if ``parser`` is a ``PrattParser`` instance then a type is declared as follows:

.. code-block:: python

   t_int = parser.def_type("t_int")

After that, the Python variable ``t_int`` can be used as a type.  The
individual types are instances of the class ``TypeObject``.

The type specification for a node in the parse tree (which is also a token
instance) is stored in an instance of the ``TypeSig`` class.  This data
structure stores the value type of the node as an attribute ``val_type``, and
stores the types of the children nodes (i.e., any function arguments) as a
tuple ``arg_types``.

The built-in parsing methods of the ``PrattParser`` class take arguments which
correspond to the ``val_type`` and ``arg_types`` specifications, so many users
will not need to explicitly use the ``TypeSig`` class.  That information is
internally stored as a ``TypeSig`` instance, though, and the basic conventions
(such as for wildcards with ``None``) also apply to the argument and value type
parameters.

Using the type ``t_int`` defined above, we might have:

.. code-block:: python

   sig = TypeSig(t_int, [t_int, t_int])

This specifies a function (in the abstract sense) which takes two integer
arguments and returns an integer.  In terms of the corresponding parse-tree
node, it specifies that the node itself has a value of type ``t_int``, and that
it has two child nodes which also both have values of type ``t_int``.  This
might be used, for example, for a function of two variables such as ``gcd``
which takes two integers and returns another integer.  Type signatures are
associated with the handler function of the token which ends up as the root of
the parse-subtree representing the expression.

If the ``arg_types`` list (or iterable) is instead an individual type (i.e., a
``TypeObject`` instance) rather than a list then any number of
arguments/children are allowed, and they must all have that type.  For example,
this would be the type specification for a function taking any number if
``t_int`` arguments and returning a ``t_int``:

.. code-block:: python

   sig = TypeSig(t_int, t_int)

For more control over variable numbers of arguments, the last group of
types in an ``arg_types`` list can be wrapped in a ``Varargs`` instance.
Then those particular arguments will be repeated any number of times
to match the number of actual arguments.  For example:

.. code-block:: python

   sig = TypeSig(t_int, [t_int, Varargs(t_int, t_float)])

The ``Varargs`` initializer can also take a keyword argument ``exact_repeat``
which, if set false, truncates any repeated arguments if necessary to match
the number of actual arguments.  The default is to raise an exception.

Wildcards, which match any type, are also allowed in type specifications.  They
can be defined by passing the value ``None`` as the type object instead of a
predefined type.  As a ``val_type`` this wildcard matches any type at the node,
and in place of the ``arg_types`` list it declares that no checking is to be
done on the children.  The following three forms are equivalent, and specify
that no type checking will be done (i.e., everything matches):

.. code-block:: python

   TypeSig(None, None) == TypeSig(None) == TypeSig()

The ``None`` wildcards can also be used inside the ``arg_types`` list to
specify arguments which are not type-checked.  This allows the number of
arguments to be checked, and possibly some but not all arguments.  For example,
this signature specifies a function which takes exactly one argument but is
otherwise unchecked:

.. code-block:: python

   TypeSig(None, [None])

This one takes exactly three arguments, all unchecked:

.. code-block:: python

   TypeSig(None, [None, None, None]) == TypeSig(None, [None]*3)

Finally, this type specification is for a function that takes two arguments,
with the first a ``t_int`` and the second unchecked, returning a value that
is unchecked:

.. code-block:: python

   TypeSig(None, [t_int, None])

Overview of type-checking
-------------------------

Type checking is automatically performed just after the head or tail handler
function of a construct has been called and has returned a subtree.  The
subtree is checked for types before it is returned to the ``recursive_parse``
routine.  The ``val_types`` of the root node's children are compared to the
``arg_types`` defined for the construct.

Setting the ``val_type`` of a construct *defines* the type of the root node of
the subtree returned by the construct's handler function.  So, for example,
setting the ``val_type`` for a token-literal construct defines the type of that
token (in that preconditions context).  Setting the ``val_type`` of an infix
operator construct defines the type which is returned by the operator.

Setting the ``arg_types`` of a construct specifies what the ``val_types`` of
the children of the returned root node should be.  These are automatically
checked, resolving overloading if possible.

In some cases the handler might need to influence the type-checking or node
processing.  The attribute ``process_and_check_kwargs`` of the root node can be
set by a handler function before the node is returned.  It should be passed a
dict containing keyword arguments and values of the ``process_and_check_node``
function.

See the built-in methods of the ``PrattParser`` class for examples of how to
define general constructs which check types.

Implementation details
----------------------

See this page for low-level implementation details:

   :doc:`type_checking_implementation_details`

