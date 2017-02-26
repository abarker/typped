Implementing typed languages
============================

The Typped package provides a fairly general type-definition and type-checking
mechanism.  It is optional, however, and can be ignored for untyped languages.
When types are defined and set up properly (using either the built-in methods
of the ``PrattParser`` class or custom methods) the language will be
automatically type-checked at parse-time.

The type system also allows for operator overloading, including optional
overloading on return types.  Overloading currently has the mild restriction
that all overloads defined for an infix operator must share the same precedence
value.  Overloading is done by re-defining a handler for a token which already
has a handler, but with a new type.  The default is to allow overloading only
on argument types.  To disable all overloading, set
``overload_on_arg_types=False`` in the ``PrattParser`` initializer.  To allow
overloading on return types as well as argument types, set
``overload_on_ret_types=True`` in the initializer.

Each handler function, head or tail, can have type information associated with
it.  Recall that handler functions are one-to-one with tuples of the form
``(head_or_tail, token_label, precond_label)``, where ``head_or_tail`` is one
of the constants ``HEAD`` or ``TAIL``.  So to have two distinct types you need
to make sure there are two distinct handlers for the separate cases.  This
usually happens by necessity anyway, since they must parse differently.  The
built-in parsing methods of the ``PrattParser`` class hide most of these details.

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

Only simple types are currently implemented (though the mechanism can easily be
made more general).  As of now type are essentially just string labels which
are equivalent iff the labels are the same.  At some point type heirarchies and
type conversions may be added.

Type signatures
---------------

Types are defined using the ``def_type`` method of a ``PrattParser``.  For example,
if ``parser`` is a ``PrattParser`` instance then a type is declared as follows:

.. code-block:: python

   t_int = parser.def_type("t_int")

After that, the Python variable ``t_int`` can be used as a type.  The
individual types are instances of the class ``TypeObject``.

The basic type specification for a node in the parse tree is the represented as
an instance of the ``TypeSig`` class.  This data structure stores the value
type of the node as an attribute ``val_type``, and stores the types of the
children nodes (i.e., the function arguments) as a tuple ``arg_types``.

The built-in parsing methods of the ``PrattParser`` class take arguments which
correspond to the ``val_type`` and ``arg_types`` specifications, so many users
will not need to use the ``TypeSig`` class explicitly.  This information is
internally stored as a ``TypeSig`` instance, though, and the conventions (such
as for wildcards with ``None``) also apply to the argument and value type
specifications.

These type specifications are fairly easy to use.  For example, using the type
``t_int`` defined above, we might have:

.. code-block:: python

   sig = TypeSig(t_int, [t_int, t_int])

This specifies a value type at a parse-tree node of ``t_int``, and two children
which also both have type ``t_int``.  This specification would, for example, be
used for a function of two variables such as ``gcd``, which takes two integers
and returns another integer.  This type signature would be associated with the
handler function of the token which ends up as the root of the parse-subtree
representing the expression.

If the ``arg_types`` list (or iterable) is an individual type (a ``TypeObject``
instance) rather than a list then any number of arguments/children are allowed,
and they must all have that type.  For example, this would be the type
specification for a function taking any number if ``t_int`` arguments and
returning a ``t_int``:

.. code-block:: python

   sig = TypeSig(t_int, t_int)

Wildcards are also allowed, using ``None`` as the type object.  The value
``None`` can be passed to the initializer of ``TypeSig`` in place of a
predefined type.  As a ``val_type`` this wildcard matches any type at the node,
and in place of the ``arg_types`` list it declares that no checking is to be
done on the children.  The following three forms are equivalent, and specify
that no type checking will be done (i.e., everything matches):

.. code-block:: python

   TypeSig(None, None) == TypeSig(None) == TypeSig()

The ``None`` wildcards can also be used inside the
``arg_types`` list to specify arguments which are not type-checked.  This allows
the number of arguments to be checked, and possibly some but not all arguments.
For example, this signature specifies a function which takes exactly one argument
but is otherwise unchecked:

.. code-block:: python

   TypeSig(None, [None])

This one takes exactly three arguments, all unchecked:

.. code-block:: python

   TypeSig(None, [None, None, None]) == TypeSig(None, None*3)

Finally, this type specification is for a function that takes two arguments,
with the first a ``t_int`` and the second unchecked, returning a value that
is unchecked:

.. code-block:: python

   TypeSig(None, [t_int, None])

Implementation details
----------------------

Type checking is performed when the handler functions first produce subtrees
of the final expression tree.  The function ``process_and_check_node`` is
called at the end of handler functions, before returning the token which has been
processed into the root of the subtree.

See this page for low-level implementation details:

   :doc:`type_checking_implementation_details`

