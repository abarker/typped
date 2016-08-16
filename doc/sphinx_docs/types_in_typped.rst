Implementing typed languages in Typped
======================================

Basic type-checking
-------------------

TODO:
   Need to give a big-picture overview.  Each kind of token can potentially
   have a type associated with it.  Types defined with the `def_type` method.
   Types have a value-type and optionally a list of argument types.  Each
   non-terminal node in the parse tree is checked to see if the children of
   the node satisfy the specified argument types of the parent node (if any
   were specified).

   Notions of type equivalence and type conversion must be defined for
   particular languages by the implementer/designer.  The Typped system
   provides a way to declare that one type is valid as an instance of another
   one.  It should be a resolvable heirarchy, but that is not checked.  Can
   also define that one type can be converted to another one.  That information
   is noted in the individual nodes of the parse tree (the formal type, the
   actual type, and any conversion operations needed).

The Typped package provides a fairly general type-definition and type-checking
mechanism.  It is optional, however, and can be ignored for untyped languages.
When they are defined the types will be automatically checked at parse-time
(according to any options which are set).  The type system also allows for
operator overloading, including optional overloading on return types.

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

Implementation details
----------------------

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

Templates and type parameters
-----------------------------

TODO:
   Is this a completely separate thing, which should have its own section?
   Probably.

   BUT, types are related because the parameters of templates are usually
   types.  So even if the mechanism is for general parameters, make sure
   it works for types.

   Have a `def_template` method that takes a subtree with certain
   variables/nodes marked for replacement or modification.  Returns a Python
   function which can be called to give a copy of the subtree with the relevant
   nodes modified or replaced.  This Python function can then be called inside
   the handler functions for the instantiation of templated constructs.

   Look over this, maybe, discussion of overloading vs. specialization.
   http://www.gotw.ca/publications/mill17.htm

   To use a template:

   1. When a head or tail handler finds a template definition, call
         `def_template`, and get a function to eval the template (or register
         it with a class to save one, etc.)

   2. When encountering the template instantiation in a head or tail handler,
         call the function above to return a subtree to use to replace the
         instantion subtree.  Be sure to call the `process_and_check_node`
         method to do type checking.

   Note that logic substitution functions are basically just templates.  But,
   you don't always want to replace or evaluate them.  You just want to keep
   their definitions so you could evaluate them or replace them.  Then have an
   operation of a wff to do so, so you could do a proof of properties with
   substitution functions.

TODO:
   For templates, maybe you parse the template's subtree but save it, and then
   when you get an instantiation you plug in all the parameters with their
   actual types or values!

   The implementer must provide the way to parse the `template` namespace down
   to code which contains substitutable parameters!!!  Then the rest should be
   fairly easy...

   Consider arbitrary levels, too, maybe... if easy.

**Templates** are essentially a meta-construct in languages which allow for
code to be generated at compile time (for static typing) or runtime (for
dynamic typing) according to a previously-defined pattern.  They add a level of
indirection or abstraction.

Templates are meta-functions which take **type parameters** and/or **value
parameters** as arguments.  The type parameters represent types, while the
value parameters can represent any other kind if data which is known at the
time of template specialization and which the implemented language chooses to
allow.  (The Typped system does not restrict what kinds of parameters can be
passed to parameters, that is language-specific.)

The process of converting a defined template into code with the template
parameters bound to actual values is known as **template instantiation**.
Essentially, the template meta-function is called with actual arguments for its
parameters and it returns some code (i.e., an AST subtree).  **Implicit
instantiation** occurs whenever the compiler or interpreter chooses to do it,
while **explicit instantiation** occurs at the point where a program statement
requests it.  (The type parameters and value parameters to use in instantiating
a template may be either deduced by the compiler or interpreter using **type
deduction** or else explicitly specified in the language as an **explicit type
specification**.)

The particular code you get when you instantiate a template is called a
**specialization** of the template.  **Implicit specialization** occurs
whenever the compiler or interpreter chooses to do it.  **Explicit
specialization** occurs when a program statement requests it.  An explicit
specialization can also provide code which, for that case, will override the
general or primary code defined for the template.

**Partial specialization** is the binding of only some of the template
parameters.  This essentially produces a partial meta-function from the
template meta-function, resulting in another template with fewer parameters.
Partial specialization can also provide code which will override the general or
primary template body code in those particular cases.

In some situations certain kinds of arguments to a template function might not
be appropriate, even with specialization.  Some languages have **template
constraints** to limit the situations when a template can be instantiated.
This is like type-checking the arguments of the meta-function representing the
template, restricting their possible values.  Some languages such as D also
have meta-language constructs such as if-then blocks which can be used inside
the template and which are evaluated at compile time.

The D language has a nice templating system (which was based on simplifying C++
templates).  While the language shortcuts for using templates, under the hood
it implements templating using **template namespaces**.  These have an explicit
`template` keyword.  Any templated code can appear inside the namespace.

TODO
   Check that the code below is valid... doesn't all highlight and I added
   extra template parameters (and name changes).

.. code-block:: d

   template myTemplate(T1, T2, V1) {
       T1 myTemplatedFun(T2 myVar1, T2 myVar2) {
           return V1 * myVar;
       }
   }

Since this kind of thing is sufficient to implement all the features that
Typped provides explicit for, the discussion will be limited to that.  It is up
to the language implementer to provide whatever additional syntactic sugar
might be desired.  The construct above would be fairly easy to parse with
Typped, so that will be assumed.

At the time when any particular template instantiation occurs the parameterized
types for that specialization must be known (or else an error has occurred).
The language implementation must handle that detail, whether by explicit
declarations in the language, by inference, or both.  The values of the type
parameters similarly need to be known values at the time of any particular
template specialization.  How these values are known depends on the language
implementation.  So, the Typped system needs to provide:

1. Type inference for objects with declared types.

2. A way to overload the type inference system.

3. A way for the compiler or runtime to pass arbitrary values to the value
   parameters.

Some languages provide for **partial specialization** or **partial
instantiation** of templated types, to create a new templated type with some of
the parameters bound but not all of them.  This is essentially a partial
function made from the templating function and its parameters by binding some
of the parameters.

.. topic:: Composite types

   Composite types in a language are types which are constructed from other
   types.  The definition chains end with the primitive data types of language
   (much like the literals at the leaves of a parse tree).  An example in
   C is the use of `typedef` to create a type `point` from a `struct` of two
   `int` values.  In C++ classes create types whenever they are defined (or
   the template is instantiated, if it is templated).

   Composite types are a feature of the implemented language, so Typped does
   not directly handle them.  In implementing this language feature using
   Typped you should always create a new type in Typped (using the `def_type`
   method) whenever such a type definition is parsed (or instantiated, if it is
   a templated definition, or run, if typing is dynamic rather than static).

xxxxx Update below, move, or delete!!!!! xxxxx
----------------------------------------------

Types are represented in the ``PrattParser`` by subclasses of the
``TypeObject`` class.  The subclasses themselves represent **type templates**,
and their instantiations represent **type instances** or **actual types**.
Each type template has a separate subclass created to represent it.  The Pratt
parser class stores all defined type templates in a table, indexed by a type
name.  A type template defines a specification that must be satisfied by any
concrete instance.  As a special case, the Python ``None`` value is also a
valid type template and a valid type instance, representing either a template
that anything matches or an actual type for items which are considered
typeless.

Type templates can be parameterized, but even types without parameters are
defined by creating a parameterless type template.  The type instances or
actual types must have bindings for all the parameters.  The types of actual
constructs in the parsed language are always actual types.  Each node in the
final parse tree needs to have an actual type as its node type (and a signature
containing only actual types).

In the implementation language each actual type (of a construct in the parsed
language) is represented by an instance of the ``TypeObject`` subclass
representing that type template.  Each such instance must define a value for
each parameters of the type template (if any).  The actual types may or may not
match the types required by the template.  Checking for a type match is
performed at the time of instantiation.  That is, the initializer for a
subclass of ``TypeObject`` takes as arguments the actual values to assign to
the parameters of the type template represented by the subclass.  If the
arguments do not match an error is raised, otherwise an instance is created.

A collection of type templates defining the required argument types and return
type for a function will be called the function's **type specification** or a
**type spec**.  A collection of actual types for the arguments and return types
of a function will be called the function's **type signature** or a **type
sig**.  A type sig either matches a type spec or not (either exactly or via the
use of defined conversions).  These are represented in the program as instances
of the class ``TypeSpec`` and the class ``TypeSpec`` (both derived from the
class ``FunctionTypes``).

Recall that function overloading is implemented with respect to the type spec
that is passed to the ``PrattParser`` routine for parsing the function.  The
same head handler function or tail handler function is always used when a
function is overloaded, but a list of all the defined type signatures is
maintained.  The final nodes in the ``TokenNode`` parse tree will each contain
an actual type signature.

Implementation
--------------

In the implementation a head is defined for literal tokens by ``define_token``.
The method takes an argument ``val_type``.  Note that now whenever the
``val_type`` is set for the *node* it should be for an *instance* of the type
specifier.  Perhaps it should be called ``val_type_actual``, or else just set
the full ``TypeSpec`` and specify that the can only contain instances.  Then,
all the literals have instances set for them as ``val_type_actual``.  Going up
the parse tree, the higher nodes look down at the ``val_type_actual`` values of
their children to obtain the actual types of the type specifiers.

.. topic:: Example of defining types.

   The following example illustrates the definition of types and parameterized
   types in a very simple implementation of a language for matrix expressions.

   First, define two unparameterized types::

      t_real = pp.define_type("Real")
      t_int = pp.define_type("Int")
      
   The first argument to ``define_type`` is an arbitary string label for the
   type.  For mnemonic purposes the string label can be chosen to correspond to
   the type label in the parsed language, but it need not be.  The returned
   values are subclasses of ``TypeObject``.

   Now an ``m`` by ``n`` parameterized matrix type holding any type of elements
   can be defined as a templated type::

      t_matrix = pp.define_type("Mat", (None, t_int, t_int))

   The second argument to ``define_type`` is a tuple containing the template
   parameters, which are also type specifiers.  The ``None`` type of the first
   parameter matches any type, for matrix elements of arbitrary types.  The
   ``t_int`` type parameters are for the shape parameters m and n of the
   matrix.

   Using the above type definition, the type signature for matrix
   multiplication can be parameterized to ensure at parse-time that both matrix
   arguments are conformable for multiplication::

      mmult_sig = TypeSpec(t_matrix,  # return type
                         (t_matrix,  # arg 1
                          t_matrix), # arg 2
                          test_fun=conformable_test_fun)  # a test to apply

   Now suppose the infix operator ``*`` is defined for matrix multiplication,
   and that the type signature ``mmult_sig`` is passed as a keyword argument
   defining the signature.  When a matrix multiplication is parsed in the
   implemented language, whatever syntax is used, the actual arguments to the
   matrix multiplication become known (they are the actual types of the
   children in the parse tree, known in the bottom-up type resolution).

   To test whether the ``mmult_sig`` signature matches on the arguments we
   first test whether or not the basic types of each argument match
   (perhaps performing conversions [??? complications due to multiple
   possible ???]).

   Next, the function ``test_fun`` is run.  It is passed the current token
   node, the children of which are the operator arguments.  The
   children/operands have already had all their possible final signatures
   assigned (uniquely if overloading on return types is disallowed).  The
   ``TypeObject`` for each child should contain the m and n values for the
   matrix operands.  (If a matrix literal was read, for example, or an explicit
   type definition was made in the object language.) So conformability can be
   checked for the multiplication operation.

   TODO: consider whether the variable kind of indexing above, using a
   dict, to pass to the test function or the number indexing kind of thing
   below (for parameterized types) is best.

   TODO: consider defining a list or a tuple of ``TypeObject`` instances in
   place of a single ``TypeObject`` parameter to represent an "or"
   operation, accepting any of the types::

      t_real = pp.define_type("Real")
      t_int = pp.define_type("Int")
      t_mat_elem = pp.define_type("MatElem", [(t_int, t_real, t_complex)])

   So the gist would be: - Use Python ``*args`` convention for indexing
   when necessary to index.
   
   - Any type argument to the initializer of a ``TypeObject`` can be passed
     either the type's string label or the actual ``TypeObject`` instance.

   - Any type argument to the initializer of a ``TypeObject`` can alternately
     be passed a list or a tuple of instances or type labels instead, which
     represent an "or" over all the types in the list or tuple.

   - Consider: when an "or" is needed in type specifications, consider defining
     a class or function ``Or`` to take the arguments.  Cleaner and clearer
     interface than just using some implicit mechanism.

Partial instantiation of parameterized types
--------------------------------------------

TODO:
   Is this something that should be done with templates?  Or should you also
   be able to pass parameters to a type directly?

Parameterized types which take a ``None`` argument as a type parameter
are defined to match any type in that slot.  A partial instantiation of a
parameterized type can bind type of some of those ``None`` wildcard
types. ::

   t_real = TypeObject("Real")
   t_matrix = TypeObject("Mat", (None, t_int, t_int))
   t_real_matrix = t_matrix.set_param_type((1, 0), t_real)

The current syntax above uses indexing of the arguments with integer
indices for the arguments of the original TypeObject (the first argument
to ``set_param_type`` is a tuple indexing first the parameter position
and then the index within the parameter value.
