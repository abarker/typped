# -*- coding: utf-8 -*-
"""

This module contains classes defining types and methods which can be called by
the `PrattParser` class when checking types.  It defines the class `TypeSig`
and the class `ParameterizedType`, as well as a dict class for storing
parameterized types.

All checking of type equivalence has been abstracted into this module, via
utility routines (often static methods of classes).

Terminology:

- Function **parameters** or **formal arguments** are the identifiers which appear
  in the function definition (and the function signature).  The type
  specifications for parameters will be called **formal types**, since the
  term parameter is used differently below.

- Function **arguments" or **actual arguments** are the values which are actually
  passed to the function on a function call.  Their types will be referred to
  as **actual types**.

Note that the formal type of a parameter can possibly match more than one
actual type.  At least one must match, however.

On the other hand,

- **Parameterized types** are type specifications which take arguments, such as
  something like the type definition `Matrix<T>` in a C++-like language, which
  can be called with, say, `Matrix<Float>` in the program text.  The variable
  `T` is a **parameter** of the parameterized type.  In the terminology used
  here, all types are parameterized types, except that some do not actually
  take parameters (and so they equal their expanded types).  Formal types can
  be parameterized, but actual types must be instantiated in all parameters.

- **Expanded types** are parameterized types and types with `None` alone as a
  wildcard argument which have been expanded to have the correct number of
  arguments and to fill in values for the template variables.

The each instance of the `PrattParser` class holds all of its defined types in
a `ParameterizedTypeDict` class, defined in this module.

This module defines two types of objects: type signatures and the type objects
themselves.  Type signatures are basically tuples of type objects, one for
the return type and one for each argument.

"""

from __future__ import print_function, division, absolute_import
import sys
from enum_wrapper import Enum

#
# Formal and actual type specs for functions.
#

class TypeSig(object):
    """The formal type specification for a function; may have parameterized types, etc.
    Set at function definition.

    This object is essentially just a tuple `(val_type, arg_types)`, where
    `val_type` is hashable and `arg_types` is a list.  Using a separate class
    allows for additional information to be stored with the tuple and produces
    better error messages.  The class also provides a convenient place to
    localize some routines which operate on type signatures and lists of type
    signatures.
    
    For the purposes of equality comparison these objects are equivalent to the
    tuple form.  Equality is exact equality and **does not** hold for
    instantiated parameterized types.  For that, use `is_valid_actual_sig`.
    Equality also ignores any attributes (other than `val_type` and
    `arg_types`) which might be added to or modified in a `TypeSig` instance.

    Note that `None` is a wildcard which matches any type, and `None` for the
    `arg_types` list or tuple matches any arguments and any number of arguments
    (it is expanded during parsing to as many `None` arguments as are
    required).  Note that `TypeSig() == TypeSig(None) == TypeSig(None, None)`.
    To specify an object like a literal which takes no arguments an empty tuple
    should be used for `arg_types`, as in `TypeSig(None, ())`, with the
    `val_type` argument set to whatever type if it is not a wildcard."""

    def __init__(self, val_type=None, arg_types=None, test_fun=None):
        """The argument `val_type` should be a hashable type in the language or
        else `None`.  The argument `arg_types` should be a list, tuple, or
        other iterable of such hashable types (including `None`) or else simply
        `None` (it will be converted to a tuple).  The `None` value is treated
        as a wildcard that matches any corresponding type; `None` alone for
        `arg_types` allows any number of arguments of any type."""
        # TODO test_fun is unset as a class var and unused.  What is it supposed
        # to do???
        if isinstance(arg_types, str):
            raise ParserException("The `arg_types` argument must"
                    " be `None` or an iterable returning types (e.g., a list"
                    " or tuple of types).")
        self.val_type = val_type
        if arg_types:
            self.arg_types = tuple(arg_types)
        else:
            self.arg_types = arg_types
        self.original_sig = None # This is set when wildcards are expanded.
        self.eval_fun = None # Optional eval fun associated with this signature.

    @staticmethod
    def get_all_matching_sigs(sig_list, list_of_child_sig_lists, tnode=None,
                              repeat_args=False, raise_err_on_empty=True):
        """Return the list of all the signatures on `sig_list` whose arguments
        match some choice of child/argument signatures from `list_of_child_sig_lists`.

        The `sig_list` argument should be a list of `TypeSig` instances.

        The `list_of_child_sig_lists` argument should be a list of lists, where
        each sublist is a list of all the possible signatures for a child node.
        The sublists should be in the same order as the children/arguments.
       
        This is the only method of this class which is actually called from
        the `PrattParser` class (except for `__init__` other magic methods
        like equality testing)."""
        num_args = len(list_of_child_sig_lists)

        # Remove duplicates from the list.  This is PROBABLY no longer needed
        # because they are removed in in register_handler_fun, using
        # append_sig_to_list_replacing_if_identical.
        #sig_list = TypeSig.remove_duplicate_sigs(sig_list)

        # Expand the signatures with wildcards to match the number of args.
        sig_list = TypeSig.expand_sigs_with_None_args(num_args, sig_list)

        # Filter the signatures by the number of arguments.
        sig_list = TypeSig.get_sigs_matching_num_args(num_args, sig_list,
                                         repeat_args, tnode, raise_err_on_empty)

        # Now filter by sigs for which the actual value of the child type matches
        # (as a type, not equality) the required formal type return type.
        sig_list = TypeSig.get_sigs_matching_child_types(sig_list,
                             list_of_child_sig_lists, tnode, raise_err_on_empty)
        return sig_list

    #@staticmethod
    #def remove_duplicate_sigs(sig_list):
    #    """Return the list of signatures in `sig_list`, removing any duplicates."""
    #    return list(set(sig_list))

    @staticmethod
    def expand_sigs_with_None_args(num_args, sig_list):
        """Expand the signatures on list `sig_list` so that any `None` argument not
        inside a tuple or list is converted to a tuple of `None` having
        `num_args` of argument.  Each expanded version has the original signature
        saved with it as an attribute called `original_sig`."""
        all_sigs_expanded = []
        for sig in sig_list:
            if sig.arg_types is None:
                new_sig = TypeSig(sig.val_type, (None,)*num_args)
            else:
                new_sig = sig
            new_sig.original_sig = sig
            all_sigs_expanded.append(new_sig)
        return all_sigs_expanded

    @staticmethod
    def get_sigs_matching_num_args(num_args, sig_list, repeat_args=False,
                                        tnode=None, raise_err_on_empty=True):
        """Return a list of signatures from `sig_list` which match `num_args`
        as the number of arguments.  Expand as necessary by repeating the
        arguments over and over if `repeat_args` is `True`.  The optional
        token-tree node `tnode` is only used for improved error reporting."""
        sigs_matching_numargs = []
        for sig in sig_list:
            sig_args_len = len(sig.arg_types)
            if sig_args_len != num_args:
                if not repeat_args: continue
                if num_actual_args % sig_args_len != 0: continue
                num_repeats = num_actual_args // sig_args_len
                sig = TypeSig(sig.val_type, sig.arg_types * num_repeats)
            sigs_matching_numargs.append(sig)

        if raise_err_on_empty and not sigs_matching_numargs:
            msg = "Number of arguments does not match any signature."
            if tnode: tnode._raise_type_mismatch_error([], msg)
            else: raise TypeError(msg)
        return sigs_matching_numargs

    @staticmethod
    def get_sigs_matching_child_types(sig_list, list_of_child_sig_lists,
                                          tnode=None, raise_err_on_empty=True):
        """Return a list of all signatures in sig_list which have arguments that
        match the return types of the child signatures in `list_of_child_sig_lists`.
        The latter should be a list containing a list of all the signatures for
        each child, in order.  The number of arguments is assumed to already match
        (see `get_sigs_matching_num_args`).  The optional token-tree node `tnode`
        is only used for improved error reporting."""
        matching_sigs = []
        # Loop over each possible sig, testing for matches.
        for sig in sig_list:
            # Handle the case of literals, with no children/arguments.
            if len(sig.arg_types) == 0:
                matching_sigs.append(sig)
                continue
            # Handle the case with one or more children/arguments.
            mismatch_with_sig = False
            for child_sig_list, arg_type in zip(
                                    list_of_child_sig_lists, sig.arg_types):
                some_child_retval_matches = False
                for child_sig in child_sig_list:
                    if arg_type == None:
                        some_child_retval_matches = True
                        break
                    # TODO BUG, need to change below line to use
                    # is_valid_actual_type, but that causes error because they have
                    # string values in them!  Apparently not being registered or
                    # looked up in dict by label?  Maybe mod when register?  Or,
                    # maybe disallow strings and force an actual type var name to
                    # be set in the actual application code (see later tests with
                    # types in test_pratt_parser.py).
                    if child_sig.val_type == arg_type:
                        some_child_retval_matches = True
                        break
                if not some_child_retval_matches:
                    mismatch_with_sig = True
                    break
            if not mismatch_with_sig: matching_sigs.append(sig)

        if raise_err_on_empty and not matching_sigs:
            msg = "Actual argument types do not match any signature."
            if tnode: tnode._raise_type_mismatch_error([], msg)
            else: raise TypeError(msg)

        return matching_sigs

    @staticmethod
    def get_child_sigs_matching_return_arg_type(child, return_type, matching_sigs):
        """Called with a child node and the expected return type for that
        child, and a list of matching sigs for the child.  Returns all the
        `child.matching_sigs` which also match in return type.  This is used in
        pass two of the type-checking, and `matching_sigs` is the
        `child.matching_sigs` attribute which was set already on pass one."""
        return [s for s in matching_sigs 
                if s.val_type == return_type or return_type is None]

    @staticmethod
    def append_sig_to_list_replacing_if_identical(sig_list, sig):
        """Return `sig_list` either with `sig` appended or with any identical
        signature (according only to type equality, not attributes) replaced
        if one is there.  This is called in `register_handler_funs`."""
        try:
            sig_list.remove(sig)
        except ValueError:
            pass
        sig_list.append(sig)
        return sig_list

    def is_valid_actual_sig(self, sig):
        """Test if the signature passed in is a valid actual signature matching
        this signature as a formal signature.  Note the difference between this
        and equality!  This is the one which should be called to determine
        signature equivalence based on type equivalences and instantiations
        of parameters."""
        return (self.val_type == sig.val_type and self.arg_types == sig.arg_types)

    def __getitem__(self, index):
        """Indexing works like a tuple."""
        if index == 0: return self.val_type
        elif index == 1: return self.arg_types
        else: raise IndexError

    
    # TODO this is explicit and better than __eq__ but doesn't work with sets...
    #def is_identical(self, sig):
    #    return (self.val_type == sig.val_type and self.arg_types == sig.arg_types)
    def __eq__(self, sig):
        """Note that equality is *only* based on `val_type` and `arg_type` being
        *identical*.  It ignores other attributes, and does not consider more
        sophisticated notions of equality."""
        return (self.val_type == sig.val_type and self.arg_types == sig.arg_types)
    def __ne__(self, sig):
        return not self.__eq__(sig)

    def __repr__(self): 
        return "TypeSig('{0}', {1})".format(self.val_type, self.arg_types)
    def __hash__(self):
        """Needed to index dicts and for use in Python sets."""
        return hash((self.val_type, self.arg_types))


#
# Type templates, representing individual types.
#

class TypeObject(object):
    """The base class for type objects.  Each formal, parameterized type is
    represented by a subclasses of this class.  Formal types can be
    parameterized or not.  Instances of those subclasses represent the actual
    types (which may or may not match the required argument signature, that is
    checked by `TypeSig` objects).

    Subclasses of this class are automatically generated via the
    `create_type_template` function.  The `ParameterizedTypeDict` class calls that
    function to create the classes (representing types) that it stores.
    """
    def __init__(self):
        pass
    def __repr__(self): 
        return "TypeObject()"

    @staticmethod
    def expand_type(num_args):
        """Expand this type object to fill in any parameter values with the
        passed-in values (the latter still TODO)."""

def create_type_template():
    """Return a subclass of `TypeObject` to represent a type template.  This
    routine can be redefined for particular applications, but should not be
    called directly.  Use the `create_typeobject_subclass` method of
    `ParameterizedTypeDict` instead (which creates a subclass, adds some
    attributes, and saves it in a dict).  End users should use the
    `def_type` method of a `PrattParser` instance, which calls
    `create_typeobject_subclass`.  The `template_subclass_fun` keyword argument
    in the initializer `ParameterizedTypeDict` can be used to change the function
    which is called."""

    # TODO: Consider.  Should a ParameterizedType be a callable object, which takes
    # arguments which possibly instantiate some of the parameters?????  Does this
    # work nicely with using the TypeSig(...) call in the definitions of functions
    # and arguments in the PrattParser?

    class ParameterizedType(TypeObject):
        """Instances of this object are used to represent types in the parsed
        language."""
        type_name = None # Set by the method that calls create_type_template.
        type_param_types = None # Set by the method that calls create_type_template.

        def __init__(self, type_param_vals=None, conversions=None):
            """Instantiate an actual type from the parameterized type represented
            by the subclass."""
            self.type_params = type_param_vals
            if conversions: self.conversions = conversions
            else: self.conversions = {} # Dict keyed by to_type values.

        def def_conversion(self, to_type, priority=0, tree_data=None):
            """Define an automatic conversion to be applied to the `TypeObject`
            instance, to convert it to type `to_type`.  The highest-priority
            conversion which matches the type spec will always be the one which is
            chosen.  Exact match has priority zero, so priorities will usually be
            negative.  Raises `TypeError` if there is still ambiguity (i.e., a
            tie).  The `tree_data` parameter is an arbitrary object which is
            associated with the conversion and will be accessible from the
            `TypeSig` stored in the parse tree.  It might be a Python function to
            actually do the conversion, for example.  Or it might be a node to add
            to the AST to represent the conversion.  No actual conversions are
            performed; those are considered semantic actions for the user to
            implement."""
            # TODO define for parameterized template version or instantiated versions???
            # TODO may need to redefine priority mechanism, since across a
            # full signature matching there may be problems with the greedy
            # approach to choosing... you find all possible signatures, but
            # then you have to rank the full signatures across all types in
            # them.
            self.conversions[to_type] = (priority, tree_data)
        def undef_conversion(self, to_type):
            try: del self.conversions[to_type]
            except KeyError: return

        def is_valid_actual_type(self, type_obj):
            """Test whether `type_obj` is a valid actual argument of this type
            object, treating this one as a formal type."""
            if self.type_name != typeobject.type_name: return False
            if len(self.parameters) != len(typeobject.parameters): return False
            return all(self.parameters[i] == typeobject.parameters[i]
                       for i in range(self.parameters))

        def __eq__(self, typeobject):
            """Note that this defines equality between types.  The `==` symbol
            is defined as exact match only.  Use `is_valid_actual_type` for
            comparing formal to actual."""
            if self.type_name != typeobject.type_name: return False
            if len(self.parameters) != len(typeobject.parameters): return False
            return all(self.parameters[i] == typeobject.parameters[i]
                       for i in range(self.parameters))

        def __ne__(self, typeobject):
            return not self.__eq__(typeobject)
        def __hash__(self):
            """Needed to index dicts and for use in Python sets."""
            # TODO, hash on only the first few vars, maybe
            return hash((self.type_name, self.type_param_types))
        def __repr__(self): 
            return self.__name__ + "({0}, {1})".format(self.type_name, self.type_params)

    return ParameterizedType


class ParameterizedTypeDict(object):
    """A symbol table holding subclasses of the `TypeObject` class, one for each
    defined type in the language.  Each `PrattParser` instance has such a table,
    which holds the objects which represent each type defined in the language
    that the particular parser parses."""
    aliases = {} # A static dict mapping defined aliases to TypeObjects. TODO
    def __init__(self, template_subclass_fun=create_type_template):
        """Initialize the symbol table.  The parameter `token_subclassing_fun`
        can be passed a function to be used to generate token subclasses,
        taking a token label as an argument.  The default is
        `create_token_subclass`."""
        self.template_subclass_fun = template_subclass_fun
        self.type_template_dict = {}

    def has_key(self, type_name):
        """Test whether a `TypeObject` subclass for `type_name` has been stored."""
        return type_name in self.type_template_dict

    def get_typeobject_subclass(self, type_name):
        """Look up the subclasses of base class `TypeObject` corresponding to
        `type_name` in the symbol table and return it.  Raises a
        `TypeException` if no subclass is found for the token label."""
        if type_name in self.type_template_dict:
            TokenSubclass = self.type_template_dict[type_name]
        return TokenSubclass

    def create_typeobject_subclass(self, type_name, type_param_types=None):
        """Create a subclass for a type named `type_name` and store it
        in the symbol table.  Return the new subclass.  Raises a `TypeException`
        if a subclass for `type_name` has already been created."""
        # TODO: type_param_types field is only set and never used anywhere.
        # What is it supposed to be doing?  How does it differ from the field
        # type_params which is set for the same class objects in __init__?
        if type_name in self.type_template_dict:
            raise TypeException("In create_type_template, already created a"
                                " type with type_name '{0}'.".format(type_name))
        # Create a new TypeObject subclass for type_name and add some attributes.
        TypeObjectSubclass = self.template_subclass_fun()
        TypeObjectSubclass.type_name = type_name
        TypeObjectSubclass.type_param_types = type_param_types

        # Create an informative name for the subclass for debugging purposes.
        if not type_param_types:
            type_param_type_names = "unparameterized"
        else:
            type_param_type_names = [ t.type_name for t in type_param_types ]
        param_names_str = "-".join(type_param_type_names)
        TypeObjectSubclass.__name__ = ("typeobject_subclass-" + type_name 
                                       + param_names_str)

        # Store the newly-created subclass in the token_dict and return it, too.
        self.type_template_dict[type_name] = TypeObjectSubclass
        return TypeObjectSubclass

    def undef_typeobject_subclass(self, type_name):
        """Un-define the token with label type_name.  The TokenNode subclass
        previously associated with that label is removed from the dictionary."""
        try: del self.type_template_dict[type_name]
        except KeyError: return # Not saved in dict, ignore.

if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_pratt_types.py") # TODO no test file

