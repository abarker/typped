Templates and type parameters
=============================

NOTE:
   Templates are related to types because the parameters of templates are usually
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
   (much like the literal tokens at the leaves of a parse tree).  An example in
   C is the use of `typedef` to create a type `point` from a `struct` of two
   `int` values.  In C++ classes create types whenever they are defined (or the
   template is instantiated, if it is templated).

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
all the literal tokens have instances set for them as ``val_type_actual``.
Going up the parse tree, the higher nodes look down at the ``val_type_actual``
values of their children to obtain the actual types of the type specifiers.

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

