Implementing typed languages
============================

The Typped package provides a fairly general type-definition and type-checking
mechanism.  It is optional, however, and can be ignored for untyped languages.
Types can be used for detecting syntax errors (either dynamically or
statically), for overloading operators, for validating data, etc.

When types are defined and set up properly (using either the built-in methods
of the ``PrattParser`` class or custom methods) the resulting expression trees
will be automatically type-checked at parse-time.  To disable type-checking
altogether the argument ``skip_type_checking=True`` can be passed to the
initializer of the ``PrattParser`` instance.

Only simple types are currently implemented (though the mechanism can easily be
made more general).  As of now type are essentially just string labels which
are equivalent iff the labels are the same.  At some point type heirarchies,
type conversions, etc., may be added.

Each construct registered with a parser instance can have type information
associated with it.  Generally this is specified by giving the type of the root
node in the expression subtree for the parsed construct as well as the types of
the children of that node.  This type signature is then checked against the
actual types in the parse subtree returned by the handler function for the
construct.

The type system allows for operator overloading, including optional overloading
on return types.  The default is to allow overloading only on argument/child
types.  Overloading currently has the mild restriction that all overloads
defined for an infix operator must share the same precedence value.  To disable
all overloading, set ``overload_on_arg_types=False`` in the ``PrattParser``
initializer.  To allow overloading on return types as well as argument types,
set ``overload_on_ret_types=True`` in the initializer.  The default is to allow
overloading on argument types but not on return types.

Operator overloading is used for constructs which parse the same (i.e., via the
same handler function and with the same triggering conditions) but which can
have multiple possible type signatures.  Type resolution is then based on the
actual arguments found at parse-time.  Overloading of a construct with a new
type signature (and possibly with a corresponding evaluation function and/or an
AST data item) is done by explicitly calling the ``overload`` method of a
construct instance.

The nodes in the final parsed expression tree have an attribute ``type_sig``
giving the actual types, and an attribute ``original_sig`` giving the matching
original signature.  The evaluation functions and AST data for a construct are
stored keyed by the original signature which was passed in at the time when
that overload was defined.

Type information for a construct (e.g., a function evaluation subexpression)
should generally be defined relative to the token which ends up as the subtree
root in the expression/parse tree for the construct.  Type-resolution and
checking is done as soon as possible on the nodes of the parse tree as it is
constructed, bottom-up.  Checking makes sure that the types of the children of
a node (if any) match the declared argument types, and that the types of the
nodes themselves match their declared value type.  Note that if overloading on
return types is allowed then a second downward pass can also be required to
resolve the signatures.

Declaring types
---------------

Types themselves are instances of the ``TypeObject`` class.  They are declared
by calling the ``def_type`` method of a ``PrattParser`` instance::

   my_int = parser.def_type("t_int")
   my_float = parser.def_type("t_float")

As of now these types are essentially just string labels, and type equivalence
is based on string equivalence.  At some point the code may be extended to
allow more complex type objects (see the module ``pratt_types``.)

Type objects can be passed to functions which take ``val_type``
and ``arg_types`` arguments.  These include the builtin parsing methods
as well as ``def_construct`` calls  for defining custom constructs.  They
also include ``overload`` calls, which add a new type signature to an
existing construct. ::

   floor_construct = parser.def_stdfun("k_floor", "k_lpar", "k_rpar", "k_comma"
                                       val_type=my_int, arg_types=[my_float])
   floor_construct.overload(val_type=my_int, arg_types=[my_int])

Type signatures
---------------

A type signature is an instance of the ``TypeSig`` class.  It is basically a
container class for a return or value type, called the ``val_type``, and a list
or tuple of argument types, called the ``arg_types``.

The built-in parsing methods of the ``PrattParser`` class take arguments which
correspond to the ``val_type`` and ``arg_types`` specifications, which are then
used to create a ``TypeSig`` object.  Many users will not need to explicitly
use the ``TypeSig`` class.  The basic conventions for wildcards, etc., apply to
the ``val_types`` and ``arg_types`` arguments since they are passed to the
initializer for a ``TypeSig`` object.

The type specifications for a nodes (token instances) in parsed expression
trees are stored as instances of the ``TypeSig`` class.  This data structure
stores the value type of the node as an attribute ``val_type``, and stores the
types of the children nodes (i.e., any function arguments) as a tuple
``arg_types``.

Using the type ``t_int`` defined earlier, we might have:

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

Generally, if one argument is passed to initialize a ``TypeSig`` it is assumed
to be the ``val_type``:

.. code-block:: python

   TypeSig("t_int", None) == TypeSig("t_int")
   
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

Note that while paramaters like ``val_types`` and ``arg_types`` which are
passed to a ``TypeSig`` interpret ``None`` as matching anything, passing
``None`` to a function parameter that expects an explicit ``TypeSig`` instance
is interpreted as an undefined signature.

Overview of type-checking
-------------------------

Type checking is automatically performed just after the head or tail handler
function of a construct has been called and has returned a subtree.  The
subtree is checked for types before it is returned to the ``recursive_parse``
routine.  The ``val_types`` of the root node's children are compared to the
``arg_types`` defined for the construct.  (Overloading on return types also
requires a second pass down the tree later.)

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

