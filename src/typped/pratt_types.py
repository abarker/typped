# -*- coding: utf-8 -*-
"""

Terminology:

- Function "parameters" or "formal arguments" are the identifiers which appear
  in the function definition (and the function signature).  The type
  specifications for parameters will be called "formal types."

- Function "arguments" or "actual arguments" are the values which are actually
  passed to the function on a function call.  Their types will be referred to
  as "actual types."

Note that the formal type of a parameter can possibly match more than one
actual type.  At least one must match, however.

"""

from __future__ import print_function, division, absolute_import
import sys
from enum_wrapper import Enum


class TypeObject(object):
    """Subclasses of this object represent parameterized types.  Instances of
    those subclasses represent the actual types.

    Each *parameterized* type is represented as a subclass of the `TypeObject`
    class.  Each type itself is an instance of such a subclass, with the
    parameters filled in with actual arguments (which may or may not match the
    required arguments, that is checked elsewhere)."""
    def __init__(self):
        pass
    def __repr__(self): 
        return "TypeObject()"


class FunctionTypes(object):
    """This object is essentially just a tuple `(val_type, arg_types)`, where
    `val_type` is hashable and `arg_types ` is a list.  This container is the
    base class for both the `ActualTypes` and `TypeSig` classes.  The first
    one holds actual types (always instantiated if parameterized) in its slots
    while the second one represents the formal types and can have parameterized
    types in its slots.
    
    For the purposes of comparison these objects are equivalent to the tuple
    form.  Making a separate class allows for additional information to be
    stored with the tuple and produces better error messages.  The class also
    provides a convenient place to localize some routines which operate on type
    signatures and lists of type signatures.
    
    Note that `None` is a wildcard which matches any type, and `None` for the
    `arg_types` list/tuple matches any arguments and any number of
    arguments."""
    def __init__(self, val_type=None, arg_types=None, test_fun=None):
        """The argument `val_type` should be a hashable type in the language or
        else `None`.  The argument `arg_types` should be a list, tuple, or
        other iterable of such hashable types (including `None`) or else simply
        `None`.  The `None` value is treated as a wildcard that matches any
        corresponding type; `None` alone for `arg_types` allows any number of
        arguments of any type."""
        # TODO test_fun is unset as a class var and unused.  What is it supposed
        # to do???
        if isinstance(arg_types, str):
            raise ParserException("The `arg_types` argument must"
                    " be `None` or an iterable returning types (e.g., a list"
                    " or tuple of types).")
        self.val_type = val_type
        if arg_types: self.arg_types = tuple(arg_types)
        else: self.arg_types = arg_types

    @staticmethod
    def get_all_matching_sigs(sig_list, list_of_child_sig_lists, tnode=None,
                              repeat_args=False, raise_err_on_empty=True):
        """Return the list of all the signatures on `sig_list` whose arguments
        match some choice of child signatures from `list_of_child_sig_lists`.
        The latter list should be a list of lists, where each sublist is a
        list of all the possible signatures for a child node.  The sublists
        should be in the same order as the children/arguments."""
        num_args = len(list_of_child_sig_lists)
        sig_list = FunctionTypes.remove_duplicate_sigs(sig_list)
        sig_list = FunctionTypes.expand_sigs_with_None_args(num_args, sig_list)
        sig_list = FunctionTypes.get_sigs_matching_num_args(num_args, sig_list,
                                         repeat_args, tnode, raise_err_on_empty)
        sig_list = FunctionTypes.get_sigs_matching_child_types(sig_list,
                             list_of_child_sig_lists, tnode, raise_err_on_empty)
        return sig_list

    @staticmethod
    def remove_duplicate_sigs(sig_list):
        """Return the list of signatures in `sig_list`, removing any duplicates."""
        return list(set(sig_list))

    @staticmethod
    def expand_sigs_with_None_args(num_args, sig_list):
        """The signatures on list `sig_list` so that any `None` argument not
        inside a tuple or list is converted to a tuple of `None` having
        `num_args` of argument."""
        all_sigs_expanded = []
        for sig in sig_list:
            if sig.arg_types is None:
                new_sig = TypeSig(sig.val_type, (None,)*num_args)
            else: new_sig = sig
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
                    if child_sig.val_type == arg_type or arg_type == None:
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

    def __getitem__(self, index):
        if index == 0: return self.val_type
        elif index == 1: return self.arg_types
        else: raise IndexError
    def __eq__(self, sig):
        return (self.val_type == sig.val_type and self.arg_types == sig.arg_types)
    def __ne__(self, sig):
        return not self.__eq__(sig)
    def __repr__(self): 
        return "TypeSig('{0}', {1})".format(self.val_type, self.arg_types)
    def __hash__(self):
        """Needed to index dicts and for use in Python sets."""
        return hash((self.val_type, self.arg_types))


class ActualTypes(FunctionTypes):
    """The actual type signature for a function, holding actual types.  Set from
    the types of the function call arguments."""
    def __init__(self, val_type=None, arg_types=None, test_fun=None):
        super(ActualTypes, self).__init__(val_type, arg_types, test_fun)


class TypeSig(FunctionTypes):
    """The formal type specification for a function; may have parameterized types, etc.
    Set at function definition."""
    def __init__(self, val_type=None, arg_types=None, test_fun=None):
        super(TypeSig, self).__init__(val_type, arg_types, test_fun)


def create_type_template():
    """Return a subclass of `TypeObject` to represent a type template.  This
    routine can be redefined for particular applications, but should not be
    called directly.  Use the `create_typeobject_subclass` method of
    `TypeTemplateDict` instead (which creates a subclass, adds some
    attributes, and saves it in a dict).  End users should use the
    `def_type` method of a `PrattParser` instance, which calls
    `create_typeobject_subclass`.  The `template_subclass_fun` keyword argument
    in the initializer `TypeTemplateDict` can be used to change the function
    which is called."""

    # TODO: Consider.  Should a TypeTemplate be a callable object, which takes
    # arguments which possibly instantiate some of the parameters?????  Does this
    # work nicely with using the TypeSig(...) call in the definitions of functions
    # and arguments in the PrattParser?

    class TypeTemplate(TypeObject):
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
        def __eq__(self, typeobject):
            """Define `==` as exact match only."""
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

    return TypeTemplate


class TypeTemplateDict(object):
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

