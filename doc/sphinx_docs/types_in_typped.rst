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

The type system also allows for operator overloading, including optional
overloading on return types.  The default is to allow overloading only on
argument types.  Overloading currently has the mild restriction that all
overloads defined for an infix operator must share the same precedence value.
To disable all overloading, set ``overload_on_arg_types=False`` in the
``PrattParser`` initializer.  To allow overloading on return types as well as
argument types, set ``overload_on_ret_types=True`` in the initializer.

Overloading is specified by re-defining a handler for a token which already has
a handler.  If the preconditions function is the same but the type of the
arguments are different then an overloaded type specification is created.  If
overloading on return types is allowed then the types are also considered
different if the return type is different.

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

If the ``arg_types`` list (or iterable) is an individual type (i.e., a
``TypeObject`` instance) rather than a list then any number of
arguments/children are allowed, and they must all have that type.  For example,
this would be the type specification for a function taking any number if
``t_int`` arguments and returning a ``t_int``:

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

   TypeSig(None, [None, None, None]) == TypeSig(None, [None]*3)

Finally, this type specification is for a function that takes two arguments,
with the first a ``t_int`` and the second unchecked, returning a value that
is unchecked:

.. code-block:: python

   TypeSig(None, [t_int, None])

Defining handlers which check types
-----------------------------------

Type checking is performed in the head and tail handler functions just before
they return their partially-processed result (which is set to the new
``processed_left`` in ``recursive_parse``).  When writing custom
handlers for languages which use type checking there are a few rules for
how to incorporate type-checking.

1. Just before returning a subtree of the final expression tree handler
   functions should call ``process_and_check_node``.  The first argument it
   takes is always the handler function itself (i.e., the "def" name of the
   handler function, which points to the function object).  The type signatures
   associated with a handler function are stored as attributes of the function
   object, and this reference is used to look them up.  That is usually all
   that is required, but ``process_and_check_node`` also takes some optional
   arguments ``typesig_override``, ``check_override_sig``, ``in_tree``, and
   ``repeat_args``, which are documented in the API for ``TokenNode`` objects.

2. When handler functions are registered with a token using the
   ``modify_token`` method of a ``PrattParser`` instance that function should
   also be passed the value type as the keyword argument ``val_type`` and the
   argument types as the keyword argument ``arg_types``.   That method creates
   a ``TypeSig`` object, so ``val_type`` should be a type, i.e., a
   ``TypeObject`` instance, and ``arg_types`` should be a list or iterable of
   types.  The value ``None`` is allowed as a type, and the convention that a
   single type passed as the argument list repeats it as many times as required
   also holds.

See the built-in methods of the ``PrattParser`` class for examples.

Implementation details
----------------------

See this page for low-level implementation details:

   :doc:`type_checking_implementation_details`

