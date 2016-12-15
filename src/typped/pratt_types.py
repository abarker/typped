# -*- coding: utf-8 -*-
"""

This module contains classes defining types and methods which can be called by
the `PrattParser` class when checking types.  It defines the class `TypeSig`
and the class `TypeObject`, as well as a dict class for storing
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

TODO fix terminology

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
a `TypeTable` class, defined in this module.

"""

from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run("../../test/test_pratt_parser.py", pytest_args="-v")

import sys
from .shared_settings_and_exceptions import ParserException

#
# Formal and actual type specs for functions.
#

# TODO: If types have classes with formal args and instances with actual we may
# want the same for TypeSig objects (like it used to be...).  But that seems
# like a lot of machinery for what are now string labels and tuples.  It does
# have room to get fancier, I suppose, and the classes are convenient
# namespaces for related methods.
#
# Annoying complexity, but also solves an annoying problem of how to store the
# actual versus the formal (instance has both, one static and one set in
# instances, and can check it, etc.).  Don't really need to keep these in a
# dict, like the TypeObjectSubclass objects representing types.  These are just
# stored with the handlers, etc., and then and do not even have labels (i.e.,
# dict keys).  Throwaway classes.
#
# All the static funs can stay with the base (convert TypeSig to TypeSigBase)
# and then have a factory fun spit out TypeSig objects (which are classes)
#
# Back to prev thinking:
#
# TypeSig is *not* quite the same because we want user to be able to specify
# TypeSig(.....) so maybe above isn't the right approach.... don't want another
# dict and another label system.  Let the tokens keep track of which are actual
# and formal... just a container with methods...
#
# Another note: looking ahead to templates, going to need to have an option to
# avoid code bloat when using them as subst_funs.  Does it make sense for TypeObject
# to take parameter?

class TypeSig(object):

    """The formal type specification for a function.  Generally set at function
    definition.  The "functions" themselves can be any syntactic construct that
    produces a node in the final parse tree, with of the node representing the
    arguments.

    A `TypeSig` instance is essentially just a tuple `(val_type, arg_types)`,
    where `val_type` is a `TypeObjectSubclass` and the `arg_types` is a tuple
    of them.  Using a separate class instead of a tuple allows for additional
    information to be stored with the data and produces better error messages.
    The class also provides a convenient place to localize some routines which
    operate on type signatures and lists of type signatures.

    For the purposes of equality comparison these objects are equivalent to the
    tuple form.  Equality is exact equality and **does not** hold for formal
    signatures and their corresponding actual signatures.  For that, use
    `matches_formal_sig`.  Equality also ignores any attributes (other than
    `val_type` and `arg_types`) which might be added to or modified in a
    `TypeSig` instance.

    Note that `None` is a wildcard which matches any type argument, and `None`
    for the `arg_types` list or tuple matches any arguments and any number of
    arguments (it is expanded during parsing to as many `None` arguments as are
    required).  Note that `TypeSig() == TypeSig(None) == TypeSig(None, None)`.
    To specify an object like a literal which takes no arguments an empty tuple
    should be used for `arg_types`, as in `TypeSig(None, ())`, with the
    `val_type` argument set to whatever type if it is not a wildcard.

    A single `TypeObject` as an `arg_types` argument (i.e., not an iterable) is
    expanded to be a tuple of that type of objects, of any required length.
    For example, a function might take an arbitrary number of arguments but
    they must all have `Int` type.

    The values type and the argument types can be accessed by indexing the
    0 and 1 element of an instance, respectively, or by using the `val_type`
    and `arg_types` attributes."""

    def __init__(self, val_type=None, arg_types=None, test_fun=None,
                                             eval_fun=None, ast_label=None):
        """Initialize a type signature object.

        The argument `val_type` should be either `None`, or a `TypeObject`
        instance.

        The argument `arg_types` should be a list, tuple, or other iterable of
        `None` values and/or `TypeObject` instances.

        The `None` value is treated as a wildcard that matches any
        corresponding type; `None` alone for `arg_types` allows any number of
        arguments of any type.

        The `eval_fun` and `ast_label` arguments are just copied as attributes
        of the newly-created signature."""

        # TODO test_fun is not set or used as of now, but it is supposed to be
        # an optional user-defined function which tests whether the parsed
        # subexpression subtree which was found in the parsed program text
        # actually matches the declared type in the function spec.  Decide if
        # useful, else delete.

        self.eval_fun = eval_fun
        self.ast_label = ast_label

        #
        # Convert val_type argument to TypeObject instance.
        #

        if isinstance(val_type, TypeObject):
            pass # May need to do something at some point, not as of now.
        elif val_type is None:
            val_type = TypeObject(None)
            pass
        else:
            raise TypeModuleException("`TypeSig` initialized with invalid `val_type`"
                     " of '{0}', of Python type {1}.  Must be a `TypeObject` instance."
                                .format(val_type, type(val_type)))

        #
        # Convert arg_type to tuple of TypeObject instances or a single wildcard one.
        #

        if arg_types is None: # None representing arbitrary objects.
            arg_types = TypeObject(None)
        elif isinstance(arg_types, TypeObject): # Single TypeObject, expands as needed.
            pass
        elif not arg_types: # Matches (), [], and anything else that bools to False
            arg_types = () # Below case catches this, but this clearer.
        else:
            arg_types = list(arg_types)
            for i in range(len(arg_types)):
                if isinstance(arg_types[i], TypeObject): # Ignores None values.
                    pass # May take some action at some point, not now...
                elif arg_types[i] is None:
                    arg_types[i] = TypeObject(None)
                    pass
                else:
                    raise TypeModuleException("`TypeSig` initialized with invalid"
                                " `val_type` of '{0}', of Python type {1}.  Must be"
                                " a `TypeObject` instances."
                                .format(val_type, type(val_type)))
            arg_types = tuple(arg_types)

        self.val_type = val_type
        self.arg_types = arg_types


    @staticmethod
    def get_all_matching_expanded_sigs(sig_list, list_of_child_sig_lists, tnode=None,
                              repeat_args=False, raise_err_on_empty=True):
        """Return the list of all the signatures on `sig_list` whose arguments
        match some choice of child/argument signatures from
        `list_of_child_sig_lists`  Note that this does not look at the return
        types, and returns all signatures with matching arguments.

        This is the main method of this class which is actually called from the
        `PrattParser` to find all the argument-matching signatures.  (If
        overloading on return types is used then `PrattParser` also calls
        `get_child_sigs_matching_return_arg_type` on the second pass, to check
        return matches.)

        The `sig_list` argument should be a list of `TypeSig` instances
        representing formal types.

        The `list_of_child_sig_lists` argument should be a list of lists, where
        each sublist is a list of all the possible signatures for a child node.
        The sublists should be in the same order as the children/arguments.

        The returned list is a list of expanded formal type signatures (i.e.,
        with `None` arguments expanded and with a fixed number of arguments).
        The original, unexpanded formal signature which matched is saved as an
        attribute `original_formal_sig` of each matching expanded formal
        signature which is returned."""
        num_args = len(list_of_child_sig_lists)

        # Expand the signatures with wildcards to match the number of args.
        sig_list = TypeSig.get_sigs_expanded_for_num_actual_args(num_args, sig_list)

        # Filter the signatures by the number of arguments.
        sig_list = TypeSig.get_sigs_matching_num_args(num_args, sig_list,
                                         repeat_args, tnode, raise_err_on_empty)

        # Now filter by sigs for which the actual value of the child type matches
        # (as a type, not equality) the required formal type return type.
        sig_list = TypeSig.filter_sigs_matching_child_types(sig_list,
                             list_of_child_sig_lists, tnode, raise_err_on_empty)
        return sig_list

    #@staticmethod
    #def remove_duplicate_sigs(sig_list):
    #    """Return the list of signatures in `sig_list`, removing any duplicates."""
    #    return list(set(sig_list))

    @staticmethod
    def get_sigs_expanded_for_num_actual_args(num_args, sig_list):
        """Expand the signatures on list `sig_list` so that any `None` argument
        not inside a tuple or list is converted to a tuple of `None` having
        `num_args` of argument.

        This routine also expands argument signatures consisting of a single
        `TypeObject` to take any number of arguments of that type.

        Each expanded version has the original, unexpanded signature saved with
        it as an attribute called `original_formal_sig`."""
        all_sigs_expanded = []
        for sig in sig_list:
            if isinstance(sig.arg_types, TypeObject): # TypeObject(None) here also.
                new_sig = TypeSig(sig.val_type, (sig.arg_types,)*num_args,
                                  eval_fun=sig.eval_fun, ast_label=sig.ast_label)
            else:
                new_sig = sig
            new_sig.original_formal_sig = sig # Save formal sig as an attribute of expanded.
            all_sigs_expanded.append(new_sig)
        return all_sigs_expanded

    @staticmethod
    def get_sigs_matching_num_args(num_args, sig_list, repeat_args=False,
                                        tnode=None, raise_err_on_empty=True):
        """Return a list of signatures from `sig_list` which match `num_args`
        as the number of arguments.  Expand as necessary by repeating the
        arguments over and over if `repeat_args` is `True`.  The optional
        token-tree node `tnode` is only used for improved error reporting.

        This routine assumes that a `formal_sig` attribute has been set for all
        passed-in signatures (for example, as set by the method
        `expand_sigs_with_None_args`."""
        sigs_matching_numargs = []
        for sig in sig_list:
            sig_args_len = len(sig.arg_types)
            if sig_args_len == num_args:
                new_sig = sig
            else:
                if not repeat_args:
                    continue
                if num_actual_args % sig_args_len != 0:
                    continue
                num_repeats = num_actual_args // sig_args_len
                # NOTE repeating adds refs, not copies; OK for now but keep in mind.
                new_sig = TypeSig(sig.val_type, sig.arg_types * num_repeats,
                                  eval_fun=sig.eval_fun, ast_label=sig.ast_label)
            new_sig.original_formal_sig = sig.original_formal_sig # Copy over the formal sig.
            sigs_matching_numargs.append(new_sig)

        if raise_err_on_empty and not sigs_matching_numargs:
            msg = "The number of arguments ({0}) does not match any signature.".format(
                    num_args)
            if tnode:
                tnode._raise_type_mismatch_error([], msg)
            else:
                raise TypeErrorInParsedLanguage(msg)
        return sigs_matching_numargs

    @staticmethod
    def filter_sigs_matching_child_types(sig_list, list_of_child_sig_lists,
                                          tnode=None, raise_err_on_empty=True):

        """Return a list of all signatures in `sig_list` which have arguments
        that match (in order) the return type of some child signature in the
        corresponding sublist of `list_of_child_sig_lists`.  The latter should
        be a list containing lists of all the expanded signatures for each
        child, in corresponding order.

        The number of arguments is assumed to already match (see
        `get_sigs_matching_num_args`).  This just filters the list, removing the
        non-matches.

        The optional token-tree node `tnode` is only used for improved error
        reporting."""

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
                    if arg_type == TypeObject(None):
                        some_child_retval_matches = True
                        break
                    if child_sig.val_type.matches_formal_type(arg_type):
                        some_child_retval_matches = True
                        break
                if not some_child_retval_matches:
                    mismatch_with_sig = True
                    break
            if not mismatch_with_sig:
                matching_sigs.append(sig)

        if raise_err_on_empty and not matching_sigs:
            msg = "Actual argument types do not match any signature."
            if tnode:
                tnode._raise_type_mismatch_error([], msg)
            else:
                raise TypeErrorInParsedLanguage(msg)

        return matching_sigs

    @staticmethod
    def get_child_sigs_matching_return_arg_type(child, return_type, matching_sigs):
        """Called with a child node, the expected return type for that
        child, and a list of matching sigs for the child.  Returns all the
        `child.matching_sigs` which also match in return type.

        This is used in pass two of the type-checking.  When called, the
        `matching_sigs` parameter is passed the `child.matching_sigs` attribute
        which was already set on pass one of the checking.  If the result
        is unique the return type of the child is known, and so the full
        signature can be resolved and assigned."""
        # TODO TODO: This needs to use actual_matches_formal routine instead!
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

    def matches_formal_sig(self, formal_sig):
        """Test if this signature as an actual signature matches `formal_sig`
        as a formal signature.  Note the difference between this and equality!
        This is the one which should be called to determine signature
        equivalence based on type equivalences and possible conversions.

        NOT CURRENTLY USED, low-level `matches_formal_type` is used instead."""
        if not isinstance(formal_sig, self.__class__):
            raise TypeError(
                    "Comparing {0} with some other kind of object.".format(__class__))
        if not self.val_type.matches_formal_type(formal_sig.val_type):
            return False
        if len(self.arg_types) != len(formal_sig.arg_types):
            return False
        for actual, formal in zip(self.arg_types, formal_sig.arg_types):
            if not actual.matches_formal_type(formal):
                return False
        return True

    def __getitem__(self, index):
        """Indexing works like a tuple."""
        if index == 0:
            return self.val_type
        elif index == 1:
            return self.arg_types
        else:
            raise IndexError

    def __eq__(self, sig):
        """Note that equality is *only* based on `val_type` and `arg_type`
        being *identical*.  It ignores all other attributes, and does not
        consider more sophisticated notions of equality."""
        if not isinstance(sig, self.__class__):
            raise TypeError(
                    "Comparing {0} with some other kind of object.".format(__class__))
        return (self.val_type == sig.val_type and self.arg_types == sig.arg_types)
    def __ne__(self, sig):
        return not self == sig

    def __repr__(self):
        if isinstance(self.arg_types, TypeObject):
            arglist = self.arg_types
        else:
            arglist = [t.short_repr() for t in self.arg_types]
            arglist = "[" + ", ".join(arglist) + "]"
        return "TypeSig({0}, {1})".format(self.val_type.short_repr(), arglist)
    def __hash__(self):
        """Needed to index dicts and for use in Python sets."""
        return hash((self.val_type, self.arg_types))


#
# Type objects, representing individual types.
#

# NOTE: Consider if there is any advantage to having types themselves take
# parameters other than the string labels.

def actual_matches_formal(actual_type, formal_type):
    # TODO: this could be a method of a full class, which handles all the
    # type equivalence, alias, and conversion stuff.....
    # Alternately, it would fit in as part of the existing TypeTable class.
    """The default function to check whether actual types match formal types.
    Currently uses straight equality, nothing else."""
    return actual_type == formal_type # Later consider fancier matching.

# TODO would be nice to have only one wildcard object, but may need __new__.
NONE = (None,) # A representation for a type label of None, so == comparisons OK.

class TypeObject(object):
    """Instances of this class represent types."""

    def __init__(self, type_label, actual_matches_formal_fun=actual_matches_formal):
        """Instantiate a type object or a wildcare object with `None` argument."""
        super(TypeObject, self).__init__() # Call base class __init__.
        if type_label is None:
            self.type_label = NONE
            self.is_wildcard = True
        else:
            self.type_label = type_label
            self.is_wildcard = False
        self.conversions = {} # Dict keyed by to_type values.
        self.actual_matches_formal_fun = actual_matches_formal_fun

    def def_conversion(self, to_type, priority=0, tree_data=None):
        """Define an automatic conversion to be applied to the `TypeObjectBase`
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
        # TODO may need to redefine priority mechanism, since across a
        # full signature matching there may be problems with the greedy
        # approach to choosing... you find all possible signatures, but
        # then you have to rank the full signatures across all types in
        # them.
        self.conversions[to_type] = (priority, tree_data)

    def undef_conversion(self, to_type):
        try: del self.conversions[to_type]
        except KeyError: return

    def matches_formal_type(self, formal_type):
        """Test whether this object as an actual type matches `formal_type` as
        a formal type.  All checks for type matches use this method.

        A hook is provided to pass in a different function to do the
        comparison.  Set the `TypeObject` initializer keyword
        `actual_matches_formal_fun` to the function on the creation of each
        type.  It should take two `TypeObject` arguments: the actual one
        followed by the formal one.  Different functions can be passed to
        different type objects, or a generic one can be passed to all of them.

        See also the related method `matches_formal_sig` of `TypeSig` which
        checks each value and argument in a signature"""
        return self.actual_matches_formal_fun(self, formal_type)

    def __eq__(self, type_obj):
        """Note that this defines equality between types.  The `==` symbol
        is defined as exact match only.  Use `matches_formal_type` for
        comparing actual to formal."""
        if not isinstance(type_obj, TypeObject):
            return False
        return self.type_label == type_obj.type_label
    def __ne__(self, type_object):
        return not self == type_object

    #def __hash__(self):
    #    """Needed to index dicts and for use in Python sets."""
    #    # TODO, may need fancier hash at some point, not needed at all now, though.
    #    return hash(self.type_label)
    def __repr__(self):
        str_label = self.type_label
        if self.type_label == NONE: str_label = "None"
        return "TypeObject({0})" .format(str_label)
    def short_repr(self):
        str_label = self.type_label
        if self.type_label == NONE: str_label = "None"
        return str_label

#
# A dict-like class for holding type objects
#

class TypeTable(object):
    """A type table holding instances of the `TypeObject` class, one for each
    defined type in the language.  Each `PrattParser` instance has such a table,
    which holds the objects which represent each type defined in the language
    that the particular parser parses."""
    aliases = {} # A static dict mapping defined aliases. TODO

    def __init__(self, parser_instance):
        """Initialize the type table.  The `parser_instance` parameter should be the
        `PrattParser` instance which owns this type table.  All type tables must
        be associated with a parser instance (the types are part of the language
        it parses)."""
        self.parser_instance = parser_instance
        self.type_object_dict = {}

    def has_key(self, type_label):
        """Test whether a `TypeObject` instance for `type_label` has been stored."""
        return type_label in self.type_object_dict
    __contains__ = has_key # For testing with the "in" keyword.

    def get_typeobject(self, type_label):
        """Look up the `TypeObject` instance corresponding
        to `type_label` in the type table and return it.  Raises a
        `TypeException` if no instance is found for the token label."""
        if type_label in self.type_object_dict:
            type_object = self.type_object_dict[type_label]
        return type_object
    __getitem__ = get_typeobject

    def create_typeobject(self, type_label):
        """Create a `TypeObject` instance for a type with string label `type_label`
        and store it in the type table.  Return the new instance.  Raises a
        `TypeException` if an instance for `type_label` has already been
        created."""
        if type_label in self.type_object_dict:
            raise TypeModuleException("Already created a type with type_label '{0}'."
                                .format(type_label))

        # Create a new TypeObject instance for the formal type label.
        #type_object = TypeObject(type_label, self.parser_instance.type_table) # TODO
        type_object = TypeObject(type_label)

        # Store the newly-created instance in the token_dict and then return it.
        self.type_object_dict[type_label] = type_object
        return type_object

    def undef_typeobject(self, type_label):
        """Un-define the token with label type_label.  The `TypeObject` instance
        previously associated with that label is removed from the dictionary."""
        try:
            del self.type_object_dict[type_label]
        except KeyError:
            return # Not saved in dict, ignore.

class TypeErrorInParsedLanguage(ParserException):
    """Raised when the there is a type error in the language being parsed,
    as opposed to a `TypeError` in the Python code."""
    pass

class TypeModuleException(ParserException):
    """An exception in the code of the `pratt_types` module."""
    pass

