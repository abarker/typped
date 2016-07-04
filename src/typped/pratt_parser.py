# -*- coding: utf-8 -*-
"""

A general Pratt parser module.

To use the `PrattParser` class you need to do these things:

    1. Create an instance of the PrattParser class.  Example::

          parser = PrattParser()
 
    2. Define each token that will appear in the language, including a string
       label and a regex for to recognize that kind of token.  If necessary,
       the appropriate `on_ties` values can be set to break ties in case of equal
       match lengths (the lexer will always take the longest match over all the
       defined tokens, with ties broken by any `on_ties` values, defaulting to zero).
       Examples::

          parser.def_token("k_number", r"\d+")
          parser.def_token("k_lpar", r"\(")
          parser.def_token("k_ast", r"\*")
          parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    
    3. Define the syntactical elements of the language that you are parsing.
       Any necessary token labels must have already been defined in the previous
       step.  The predefined syntax-definition methods of `PrattParser` take as
       arguments token labels, type information, etc.  They also take a string
       as a label for the resulting AST node (but this label can be used in any
       way desired, including in preconditions).  Examples::

          parser.def_literal("l_number", "k_number", val_type="t_number")
          parser.def_infix_op("k_add", "l_plus", 10, Assoc.left,
                           val_type="number", arg_types=["t_number","t_number"])

       If the predefined methods are not sufficient you might need to create a
       subclass of `PrattParser` to provide additional methods.  Note that
       literals must be defined as syntax, in addition to being defined as
       tokens.  If typing is to be used then any type information should also be
       set for the syntax elements.

    4. Pass the parser a string of text to parse and save the resulting token
      tree. ::

          result_tree = parser.parse("x + (4 + 3)*5")
          print(result_tree.tree_repr())

    5. You can optionally evaluate the resulting tree (if evaluate functions
       were supplied as kwargs for the appropriate methods) or convert it to an
       AST with a different type of nodes. ::

          eval_result = result_tree.evaluate_subtree()
          ast = result_tree.convert_to_AST(TokenNode_to_AST_converter_fun)

In reading the code, the correspondence between the naming convention used here
and Pratt's original naming conventions is given in this table:

+----------------------------------+--------------------------+
| this code                        | Pratt's terminology      |
+==================================+==========================+
| token precedence                 | left binding power, lbp  |
+----------------------------------+--------------------------+
| subexpression precedence         | right binding power, rbp |
+----------------------------------+--------------------------+
| head handler function            | null denotation, nud     |
+----------------------------------+--------------------------+
| tail handler function            | left denotation, led     |
+----------------------------------+--------------------------+

"""

from __future__ import print_function, division, absolute_import
import re
import sys
import types
import collections
import inspect
from enum_wrapper import Enum
from lexer import (Lexer, TokenNode, TokenSubclassDict,
                   BufferIndexError, LexerException)

from pratt_types import TypeTemplateDict, TypeSig, ActualTypes

# TODO: Do preconditions really need labels?  Can the users just be left to
# manage their own preconditions, or maybe a separate class can be defined to
# manage them in a dict, maybe with some predefined ones?  Seems like it could
# be separated out, and reduce complexity.  Is equality testing of
# preconditions ever truly required?  If not, why not just use the function
# objects and leave the user to manage their functions however they want.

#
# AST stuff
#

# This AST stuff should all be independent of the rest of the module, so it
# can later be moved out.  Different applications will define it differently.
# For implementing the AST nodes, use a shallow inheritance tree:
# a base node with basic, default operations and specialized nodes for the
# different possible types of AST nodes (maybe even an extra level for
# functions???).

class AST_Node(object):
    """The base class for nodes in the abstract syntax tree."""
    def __init__(self):
        self.children_types = []
        self.children = []
        self.parent = None
    def append_children(self, *token_nodes):
        """Append all the arguments as children, also setting their parent to self."""
        for t in token_nodes:
            self.children.append(t)
            t.parent = self
    def old_repr(self):
        """This is for backward compatibility in some test cases."""
        if self.token_label == "k_number":
            return "[literal {0}]".format(self.value)
        if self.token_label == "k_lpar":
            return "[k_lpar {0} k_rpar]".format(self.children[0])
        else:
            str_val = "[" + str(self.value)
            for a in self.children: str_val += " " + str(a)
            str_val += "]"
            return str_val
    def value_repr(self):
        return str(self.value)
    def label_repr(self):
        return str(self.token_label)
    def summary_repr(self):
        return "<" + str(self.token_label) + "," + str(self.value) + ">"
    def tree_repr(self, indent=""):
        string = indent + self.summary_repr() + "\n"
        for c in self.children:
            string += c.tree_repr(indent=indent+" "*4)
        return string
    def string_repr(self):
        string = self.value_repr()
        if self.children:
            string += "["
            string += ", ".join(c.string_repr() for c in self.children)
            string += "]"
        return string
    __repr__ = old_repr

# TODO will AST nodes really need to be subclasses defined in a table, or
# can the AST_node constructor (or some predefined method) just take a
# TokenNode as an argument and spit out the corresponding AST node?
# The init function itself could make subclasses of itself, store them
# in a dict if desired, and return an instance -- but that seems weird.

class AST_NodeDict(object):
    """The purpose of this class is to save the subclasses associated with
    various AST labels.  These are subclasses of the `AST_Node` class.  A
    token with an `ast_label` attribute can be passed to the `get_AST_node`
    method to return the subclass for the attribute.

    Generally, these AST nodes are used to replace the `TokenNode` classes
    which are initially generated in the parse tree.  The user may want some
    other format.  The conversion may be trivial or more complex.
    
    Currently implemented as a dict of generated classes for tokens, indexed by
    the token label."""
    def __init__(self):
        """Initialize the symbol table."""
        self.ast_node_dict = {} 

    def get_AST_subclass(self, ast_label):
        """Return the AST node subtype representing `ast_label`, defining it if 
        necessary."""
        if ast_label in self.ast_node_dict:
            AST_Subclass = self.ast_node_dict[ast_label]
        else: # AST subclass has not been created.
            class AST_Subclass(AST_Node):
                def __init__(self, string_form=None):
                    """Argument string_form, if not `None`, should be a list of
                    n+1 strings, where n is the number of children.  These strings
                    will be printed before, between, and after the children nodes
                    when the string form is produced."""
                    super(AST_Subclass, self).__init__() # Call base class __init__.
                    self.string_form = string_form
            AST_Subclass.__name__ = ast_label
            AST_Subclass.label = ast_label
            self.ast_node_dict[ast_label] = AST_Subclass
        return AST_Subclass

    def get_AST_node(self, token):
        class_type = self.get_AST_subclass(token.ast_label)
        instance = class_type()
        instance.value = token.value
        instance.token_label = token.token_label
        return instance

#
# TokenNode
#

""" The `TokenNode` base class is defined in `lexer.py`.  It contains some of
the basic, general methods that apply to tokens and nodes in token trees.
Methods particular to an application need to be defined in a subclass.  The
function `create_token_subclass` returns a subclass of `TokenNode` which
represents tokens with a given token label.  The `PrattParser` class sets this
function to be used by its `TokenSubclassDict` instance in order to
create a token subclass for each kind of token.  Many methods particular to the
`PrattParser` application are added to the subclass."""

def create_token_subclass():
    """This function is called from the `create_token_subclass` method of
    `TokenSubclassDict` when it needs to create a new subclass to begin
    with.  It should not be called directly.
    
    Create and return a new token subclass which will be modified and used
    to represent a particular kind of token.  Specifically, each scanned token
    matching the regex defined for tokens with a given token label is
    represented as an instance of the subclass created by calling this function
    (with further attributes, such as the token label, added to it).
    
    Using a separate subclass for each token label allows for attributes
    specific to a kind of token (including head and tail handler methods) to
    later be added to the class itself without conflicts.  This function
    returns a bare-bones subclass without any head or tail functions, etc."""

    class TokenSubclass(TokenNode):
        handler_funs = {} # Handler functions, i.e., head and tail handlers.
        preconditions_dict = {} # Registered preconditions for this kind of token.
        static_prec = 0 # The prec value for this kind of token, with default zero.
        token_label = None # Set to the actual value later by create_token_subclass.
        def __init__(self, value):
            """Initialize an instance of the subclass for a token of the kind
            labeled with `token_label`.  The `value` is the actual parsed
            string from the text.  This instance represents the token."""
            super(TokenSubclass, self).__init__() # Call base class __init__.
            self.value = value # Set from lex.token_generator; static value=None.
            self.val_type = None # The type of the token (after parsing).
            self.type_sig = None # The full type signature after parsing.

        @classmethod
        def prec(self):
            """Return the precedence for the token.  This is currently a static
            value for each type of token.  Later it may be dynamic value
            associated with the particular tail function which is selected in a
            given context."""
            return self.static_prec

        @classmethod
        def register_precond_fun(self, precond_label, precond_fun, parser_global=False):
            """Save the preconditions function `precond_fun` in a dict keyed by
            `precond_label`.  If there is already a dict entry for the label,
            then the new function overwrites the old one (but they should
            compute the same thing if they have the same label, by definition
            of the uniqueness of the precondition labels).  The priority is
            also replaced.  Using labels allows equality between preconditions
            functions to be easily defined and tested, and allows for
            commonly-used precondition functions to be predefined."""
            if parser_global:
                self.parser_instance.preconditions_dict[precond_label] = precond_fun
            else:
                self.preconditions_dict[precond_label] = precond_fun

        @classmethod
        def unregister_precond_fun(self, precond_label, parser_global=False):
            """Un-registers the precondition label and its associated function."""
            try:
                if parser_global:
                    del self.parser_instance.preconditions_dict[precond_label]
                else:
                    del self.preconditions_dict[precond_label]
            except KeyError:
                raise ParserException("Attempt to unregister a preconditions "
                        "function which is not registered.  The label is '{0}'"
                        "and the token label is '{1}'."
                        .format(precond_label, self.token_label))

        @classmethod
        def lookup_precond_fun(self, precond_label):
            """Look up the preconditions function with label "precond_label."
            First checks the dict local to the token subclass, and if that
            fails it checks the parser-global dict."""
            try:
                precond_fun = self.preconditions_dict[precond_label]
            except KeyError:
                try:
                    precond_fun = self.parser_instance.preconditions_dict[precond_label]
                except KeyError:
                    raise ParserException("In function lookup_preconditions: the "
                        " preconditions label '{0}' has not been registered."
                        .format(precond_label))
            return precond_fun

        @classmethod
        def register_handler_fun(self, head_or_tail, handler_fun,
                             precond_label=None, precond_fun=None, precond_priority=0,
                             type_sig=None):
            """Register a handler function (either head or tail) with the given
            properties.
            
            If no precondition label or function is provided a dummy
            precondition that always returns `True` will be used.  If
            `precond_priority` is set it will apply to that dummy function, but
            this will completely override anything with a lower priority.

            If `precond_label` is set but `precond_fun` is not then a
            preconditions function will be assumed to have already been
            registered for that label.  If `precond_fun` is also provided then
            it will be registered as a preconditions function with the given
            label.
            
            Data is saved on lists keyed by the value of `head_or_tail`
            (either `HEAD` or `TAIL`) in a dict, along with any specified
            precondition information.  The lists are sorted by priority."""
            # See if there is already a handler for the case, in which case the
            # type sig is assumed to be overloaded.  There should only ever be
            # one previous handler, at most, since only overloaded type info is
            # retained, nothing else (with the precondition only the first
            # would get called, anyway).
            try:
                prev_handler_list = [ s[3] for s in self.handler_funs[head_or_tail]
                                                 if s[0] == precond_label ]
                assert len(prev_handler_list) <= 1 # Multiple defs shouldn't happen.
            except KeyError:
                prev_handler_list = None

            if prev_handler_list:
                prev_type_sigs = prev_handler_list[0].type_sigs
            else:
                prev_type_sigs = []

            if prev_handler_list and not self.parser_instance.overload_on_arg_types:
                raise TypeError("Value of self.overload_on_arg_types is False but "
                       "attempt to redefine and possibly set multiple signatures for "
                       "the {0} function for token with label '{1}' with "
                       "preconditions label '{2}'."
                       .format(head_or_tail, self.token_label, precond_label))

            # Type info is stored as an attribute of handler funs.
            # For overloading, append the type_sig to pref_type_sigs, saving them all.
            prev_type_sigs.append(type_sig)
            handler_fun.type_sigs = prev_type_sigs

            if precond_fun:
                self.register_precond_fun(precond_label, precond_fun)

            if precond_label:
                precond_fun = self.lookup_precond_fun(precond_label)
            else:
                # If neither precond_fun nor precond_label, use dummy True precond
                def true_fun(lex, lookbehind): return True
                true_fun.priority = precond_priority
                precond_fun = true_fun
                # Default precondition label is a tuple so that no string matches it.
                precond_label = ("always-true-default-precondition",)

            # Get the current list of head or tail handlers for the node, containing the
            # [precond_label, precond_fun, precond_priority, handler_fun] sublists
            # which have already been registered.
            sorted_handler_list = self.handler_funs.get(head_or_tail, [])

            # Remove any existing handler with the same precondition label.
            for index in reversed(range(len(sorted_handler_list))):
                if sorted_handler_list[index][0] == precond_label:
                    del sorted_handler_list[index]

            # Insert the data in the selected head or tail list, in priority-sorted order.
            data_list = [precond_label, precond_fun, precond_priority, handler_fun]
            if not sorted_handler_list:
                sorted_handler_list.append(data_list)
                self.handler_funs[head_or_tail] = sorted_handler_list
            else:
                for index in range(len(sorted_handler_list)):
                    if (precond_priority == sorted_handler_list[index][2] and
                            self.parser_instance.raise_exception_on_precondition_ties):
                        raise ParserException("Two preconditions for the token"
                                " subclass named '{0}' for token with label '{1}' have"
                                " the same priority, {2}.  Their precondition labels"
                                " are '{3}' and '{4}'.  If precondition labels are"
                                " the same there may be a redefinition. Set the flag"
                                " False if you actually want to allow precondition"
                                " ties." .format(self.__name__, self.token_label,
                                    precond_priority, precond_label,
                                    sorted_handler_list[index][0]))
                    if precond_priority >= sorted_handler_list[index][2]:
                        sorted_handler_list.insert(index, data_list)
                        break
            return

        @classmethod
        def unregister_handler_fun(self, head_or_tail,
                                   precond_label=None, type_sig=None,
                                   all_handlers=False):
            """Unregister the previously-registered handler function (head or
            tail).  If `all_handlers` is set then all head or tail handlers (as
            selected by `head_or_tail`) are unregistered.  overloads are
            unregistered.  No error is raised if a matching handler function is
            not found."""
            if all_handlers:
                try: del self.handler_funs[head_or_tail]
                except KeyError: pass
                return

            # Item format for sorted_handler_list is:
            #     [precond_label, precond_fun, precond_priority, handler_fun]
            sorted_handler_list = self.handler_funs.get(head_or_tail, [])
            if not sorted_handler_list: return

            for i in reversed(range(len(sorted_handler_list))):
                item = sorted_handler_list[i]
                if item[0] != precond_label: continue
                handler_fun = item[3]
                new_handler_sigs = [s for s in handler_fun.type_sigs if s != type_sig]
                if not new_handler_sigs:
                    del sorted_handler_list[i] # No type sigs at all, remove item.
                    continue
                handler_fun.type_sigs = new_handler_sigs
            return

        def lookup_handler_fun(self, head_or_tail, lex=None, lookbehind=None,
                               precond_label=None):
            """Look up and return the handler function for the given
            subexpression position in `head_or_tail`, based on the current state.
            Either the `lex` parameter or the `precond_label` parameter must be
            set.  If `lex` is set it will be passed to the precondition
            functions as an argument, and similarly for `lookbehind`.  This
            method evaluates each preconditions function in the sorted list for
            the kind of handler and this kind of token, returning the handler
            function associated with the first one which evaluates to `True`.
            Raises `NoHandlerFunctionDefined` if no handler function is found.
            
            If the parameter `precond_label` is set this method returns the
            handler function which *would be* returned, assuming that that were
            the label of the "winning" precondition function."""
            try:
                sorted_handler_list = self.handler_funs[head_or_tail]
            except KeyError:
                raise ParserException("No {0} handler functions at all are defined"
                        " for tokens with token label '{1}'.  Current and token"
                        " value is '{2}'."
                        .format(head_or_tail, self.token_label, self.value))

            if precond_label:
                for pre_fun_label, pre_fun, pre_prior, handler in sorted_handler_list:
                    if pre_fun_label == precond_label:
                        return pre_fun

            for pre_fun_label, pre_fun, pre_prior, handler in sorted_handler_list:
                if pre_fun(lex, lookbehind):
                    return handler

            raise NoHandlerFunctionDefined("No {0} handler function matched the "
                    "preconditions for token with token label '{1}' and value '{2}'."
                    .format(head_or_tail, self.token_label, self.value))

        def dispatch_and_call_handler(self, head_or_tail, lex, 
                                      left=None, lookbehind=None):
            """Look up and call the handler function, passing along the arguments."""
            if head_or_tail == HEAD:
                fun = self.lookup_handler_fun(HEAD, lex)
                return fun(self, lex)
            elif head_or_tail == TAIL:
                fun = self.lookup_handler_fun(TAIL, lex, lookbehind=lookbehind)
                return fun(self, lex, left)
            else: 
                raise ParserException("Bad first argument to dispatch_and_call_handler"
                        " function: must be HEAD or TAIL or equivalent.")

        def process_and_check_node(self, fun_object, ast_label, eval_fun,
                                   typesig_override=None, in_tree=True,
                                   repeat_args=False):
            """This routine should always be called from inside the individual
            head and tail functions just before they return a value.  It sets
            some attributes and checks that the actual types match a defined
            type signature for the function.
            
            The `fun_object` argument should be a reference to the function
            that called this routine.  This is needed to access signature data
            which is pasted onto the function object as attributes.  Inside a
            handler function this function object is referenced simply by the
            name of the function.
            
            The `typesig_override` argument is a `ActualTypes` other than `None`
            then it will be *assigned* to the node as its signature after all
            checking, overriding any other settings.  This is useful for
            handling things like parentheses and brackets which inherit the
            type of their child (assuming they are kept as nodes in the parse
            tree and not eliminated).
            
            If `in_tree` is `False` then the node for this token will not
            appear in the final token tree: its children will replace it, in
            order, in its parent node's list of children.  This does not
            (currently) work for the root node, which has no parent.
            
            Setting `in_tree` to `False` can be useful for things like unary
            plus and parentheses which are not wanted in the final tree.
            
            If `repeat_args` is true then the argument types for defined type
            signatures will be expanded to match the actual number of
            arguments, if possible, by repeating them an arbitary number of
            times."""

            self.ast_label = ast_label
            self.in_tree = in_tree
            if eval_fun: self.add_evaluate_subtree_method(eval_fun)
           
            # Process the children to implement in_tree, if set.
            modified_children = []
            for child in self.children:
                if child.in_tree: modified_children.append(child)
                else: modified_children += child.children
            self.children = modified_children

            # Get all the sigs for the node while we have access to fun_object.
            all_sigs = fun_object.type_sigs

            # Perform the type-checking unless the skip option is set.
            if not self.parser_instance.skip_type_checking:
                if not self.parser_instance.overload_on_ret_types: # One-pass.
                    self._check_types(all_sigs, repeat_args)
                else: # Two-pass.
                    self._check_types(all_sigs, repeat_args, first_pass_of_two=True)
                    # If we have a *unique* matching sig, run pass two on the
                    # subtree.  In this case, since the signature is fixed by
                    # argument types (regardless of where the top-down pass
                    # starts from) we can in this case resolve the types in the
                    # subtree early.
                    if len(self.matching_sigs) == 1:
                        self.check_types_in_tree_second_pass()

            if typesig_override:
                self.typesig = typesig_override
                self.val_type = typesig_override.val_type
            return

        def _check_types(self, all_sigs, repeat_args, first_pass_of_two=False):
            """Utility function called from `process_and_check_node` to check
            the actual types against their signatures.  It assumes a single
            pass unless `first_pass_of_two` is set.  The `all_sigs` argument is
            a list (or iterable) of all the possible signatures for the
            node."""
            
            if not first_pass_of_two:
                # Ordinary case, each child c has a unique c.type_sig already set.
                list_of_child_sig_lists = [ [c.type_sig] for c in self.children ]
            else:
                # First pass case, multiple sigs in child's self.matching_sigs list.
                list_of_child_sig_lists = [ c.matching_sigs for c in self.children ]

            # Reduce to only the signatures that the types of the children match.
            self.matching_sigs = TypeSig.get_all_matching_sigs(
                                      all_sigs, list_of_child_sig_lists,
                                      tnode=self, repeat_args=repeat_args)

            if not first_pass_of_two:
                if len(self.matching_sigs) != 1:
                    self._raise_type_mismatch_error(self.matching_sigs,
                            "Actual argument types match multiple signatures.")

                # Found a unique signature; set the node's val_type to its val_type.
                self.type_sig = self.matching_sigs[0] # Save sig for semantic actions.
                self.val_type = self.type_sig.val_type # Set the node type.
                delattr(self, "matching_sigs")
            return

        def check_types_in_tree_second_pass(self, root=False):
            """Recursively run the second pass on the token subtree with the
            `self` node as the root.  Currently still needs to be explicitly
            called for the root of the final parse tree, from the `PrattParser`
            method `parse`, as well as from the checking routines here to do
            partial checks on subtrees which are resolvable."""
            unresolved_children = [ 
                    c for c in self.children if hasattr(c, "matching_sigs") ]
            self._check_types_pass_two() # Call first on self to do top-down.
            for child in unresolved_children: # Recurse on unprocessed children.
                child.check_types_in_tree_second_pass()
            # Delete childrens' matching_sigs lists after they are no longer needed.
            # This also acts as an indicator that the node has been resolved.
            for child in unresolved_children: delattr(child, "matching_sigs")
            if root: delattr(self, "matching_sigs")

        def _check_types_pass_two(self):
            """A second pass is only used when overloading on return types is
            allowed.  It is a top-down pass where each node chooses a unique
            final signature for each of its children.  It depends on the
            node attribute `self.matching_sigs` having been set in the first
            pass."""
            # On FIRST pass: on the way *up* the tree get all the signature
            # types for a node which match in arguments for *some* possible
            # return-type choice of the children.  Same as the one-pass
            # version, but now sets of possibilities are allowed and state is
            # saved for the second pass to use: the list of matching sigs is
            # saved with the node in self.matched_sigs.
            #
            # Summary: first pass, bottom-up, find all sigs that match possible
            # val_types of the node's children, across all arguments.
            #
            # After the first pass the root should have a unique sig; if not
            # there is ambiguity.  (Each node saved a set of sigs that is
            # satisfiable by some realizable choice of child sigs, and parents
            # can force children to assume any of their possible types).
            # 
            # On SECOND pass: On the way *down* the tree, parents choose one
            # sig as final for each child and set it in that child's node as
            # the new self.matching_sigs.  This should always be a unique sig;
            # otherwise there is ambibuity.  
            #
            # Summary: second pass, top-down, root is unique and parents assign
            # and set the (unique) signature for each of their children.
            #
            # Note that this algorithm works just as well if the second pass is
            # run on each subtree where the root has a unique signature, and
            # the recursion is only down to subtrees with roots having a unique
            # signature.  This yields partial results and some error conditions
            # sooner, and is what is implemented here.
            if len(self.matching_sigs) != 1: # The root case needs this.
                self._raise_type_mismatch_error(self.matching_sigs,
                        "Ambiguous type resolution (second pass).  Possible type "
                        "assignments for the children/arguments of the token node match"
                        " {0} possible node signatures: {1}.  Uniqueness is required."
                        .format(len(self.matching_sigs), self.matching_sigs))

            # We have a unique signature; set the node's type attributes
            self.type_sig = self.matching_sigs[0] # Save signature for semantic actions.
            self.val_type = self.type_sig.val_type # Set the type for the node.

            def get_child_sigs_matching_return_arg_type(child, return_type):
                return [ s for s in child.matching_sigs 
                        if s.val_type == return_type or return_type is None ]

            # Update the matching_sigs attribute for each child (should be singleton).
            for count, child in enumerate(self.children):
                if not hasattr(child, "matching_sigs"): continue # Already resolved.
                matched_sigs = get_child_sigs_matching_return_arg_type(
                                                 child, self.type_sig.arg_types[count])
                # From the first pass, we know at least one child sig matches.
                assert len(matched_sigs) != 0
                #if len(matched_sigs) == 0:
                #    child._raise_type_mismatch_error(matched_sigs,
                #        "Token node has no signatures with return type matching type of "
                #        "parent (pass two). Parent expects type '{0}'.  Defined "
                #        "signatures are: {1}.".format(self.val_type, child.matching_sigs))
                if len(matched_sigs) > 1:
                    # Recursion could catch this on the next step, but better err msg.
                    child._raise_type_mismatch_error(matched_sigs,
                        "Token node has multiple signatures with return type matching "
                        "type of parent (pass two). Parent expects type '{0}'.  Defined "
                        "signatures are: {1}.".format(self.val_type, child.matching_sigs))
                child.matching_sigs = matched_sigs
            return

        def _raise_type_mismatch_error(self, matching_sigs, basic_msg):
            """Raise an error, printing a helpful diagnostic message."""
            diagnostic = ("  Current token node has value '{0}' and label '{1}'.  The"
                         " children/arguments have labels and values of {2} and "
                         "types {3}.  The matching signatures "
                         "are {4}.".format(self.value, self.token_label,
                             tuple(c.summary_repr() for c in self.children),
                             tuple(c.val_type for c in self.children), matching_sigs))
            raise TypeError(basic_msg + diagnostic)

        #
        # Evaluations and semantic actions.
        #

        # TODO TODO TODO The evaluations need to depend on the final type that
        # is resolved.  The eval funs are passed in with a type, and should be
        # stored with that handler function which is set (like the type info
        # itself).  This evaluate interface needs to be re-worked slightly to
        # accomodate that, and the code below that uses the
        # add_evaluate_subtree_method needs to be modified.
        def add_evaluate_subtree_method(self, eval_fun):
            """Add a method called `evaluate_subtree` to the instance of the class.
            The function `eval_fun` passed in should take self as its first
            argument.  It should return the result of evaluating the node,
            calculated from its own attributes and from the results of calling
            `c.evaluate_subtree()` for each child `c`.  For example, an addition
            node would return the results of adding the evaluations from the
            children.  If this is defined for all the nodes in a tree it can be
            called from the root to evaluate the full tree."""
            self.evaluate_subtree = types.MethodType(self, eval_fun)

        def semantic_action(self):
            # TODO decide how to implement and when to call.  Should probably
            # be relative to typesigs/handlers rather than nodes in particular.
            pass

        #
        # Some representations that apply to the subclasses.
        #

        def summary_repr_with_types(self):
            return ("<" + str(self.token_label) + 
                    "," + str(self.value) + 
                    "," + str(self.val_type) + ">")

        def tree_repr_with_types(self, indent=""):
            string = indent + self.summary_repr_with_types() + "\n"
            for c in self.children:
                string += c.tree_repr_with_types(indent=indent+" "*4)
            return string

        def string_repr_with_types(self):
            string = self.summary_repr_with_types()
            if self.children:
                string += "("
                string += ",".join(c.string_repr_with_types() for c in self.children)
                string += ")"
            return string

    return TokenSubclass # Return from create_token_subclass function.

#
# Parser
#

class Assoc(Enum):
    """An enumeration of the kinds of association for infix operators."""
    left = 0
    right = 1

# Some convenient constants, which double as strings to use in error messages.
HEAD = "head"
TAIL = "tail"

class PrattParser(object):
    """A parser object.  Each parser object contains its own symbol table for tokens
    and its own lexer."""
    DEFAULT_BEGIN_TOKEN_LABEL = "k_begin" # Default label for begin token.
    DEFAULT_END_TOKEN_LABEL = "k_end" # Default label for end token.

    def __init__(self, num_lookahead_tokens=2,
                       lexer = None,
                       default_begin_end_tokens=True,
                       type_table = None,
                       skip_type_checking=False,
                       overload_on_arg_types=True,
                       overload_on_ret_types=False,
                       multi_expression=False):
        """Initialize the parser.  If a Lexer is passed in the parser will use
        that lexer and its symbol table, otherwise a new one is created.  No
        default begin and end functions will be set if a lexer is passed in,
        regardless of the value of `default_begin_end_tokens`.  Otherwise,
        default begin and end tokens will be defined unless
        `default_begin_end_tokens` is set false (note that creating them by
        default is the opposite of the default behavior for the lower-level
        Lexer class).
        
        Setting `skip_type_checking=True` is slightly faster if typing is not
        being used at all.  Setting `overload_on_ret_types` requires an extra
        walk of the token tree, and implies overloading on argument types.
        
        If `multi_expression` is set then multiple expressions will be parsed
        from the token stream until it reaches the end, and a list of token
        trees will be returned, one for each expression."""

        if lexer: # Lexer passed in.
            self.lex = lexer
            self.symbol_table = lexer.symbol_table
        else: # No Lexer passed in.
            self.symbol_table = TokenSubclassDict(
                                   token_subclassing_fun=create_token_subclass)
            self.lex = Lexer(self.symbol_table,
                                         num_lookahead_tokens=num_lookahead_tokens,
                                         default_begin_end_tokens=False)
            # Set the begin and end tokens unless the user specified not to.
            if default_begin_end_tokens:
                self.def_begin_end_tokens(self.DEFAULT_BEGIN_TOKEN_LABEL,
                                          self.DEFAULT_END_TOKEN_LABEL)
        if type_table:
            self.type_table = type_table
        else:
            self.type_table = TypeTemplateDict()
        self.num_lookahead_tokens = num_lookahead_tokens
        self.lex.parser_instance = self # To access parser from a lex argument alone.
        preconditions_dict = {} # Registered parser-global preconditions functions.
        # If exceptions are not raised on ties below, the last-set one has precedence.
        self.raise_exception_on_precondition_ties = True
        self.jop_token_subclass = None
        self.jop_token_label = None
        self.multi_expression = False # Whether to parse multiple expressions.
        # Type-checking options below; these can be changed between calls to `parse`.
        self.skip_type_checking = skip_type_checking # Skip all type checks, faster.
        self.overload_on_arg_types = overload_on_arg_types # Raise error on mult defs?
        self.overload_on_ret_types = overload_on_ret_types # Requires extra processing.
        if overload_on_ret_types:
            self.overload_on_arg_types = True

    #
    # Methods dealing with tokens.
    #

    # TODO these need undefine methods

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False):
        """A convenience function; calls the Lexer `def_token` method."""
        self.lex.def_token(
                token_label, regex_string, on_ties=on_ties, ignore=ignore)

    def def_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once.  Each element
        of the passed-in list should be a tuple containing the arguments to the
        ordinary `def_token` method.  Calls the equivalent `Lexer` function."""
        self.lex.def_tokens(tuple_list)

    def def_ignored_tokens(self, tuple_list):
        """A convenience function, to define multiple ignored tokens at once.
        Each element of the passed-in list should be a tuple containing the arguments
        to the ordinary `def_token` method with `ignore=True`.  Calls the equivalent
        `Lexer` function."""
        self.lex.def_ignored_tokens(tuple_list)

    def undef_token(self, token_label):
        """A convenience function; calls the Lexer `undef_token` method.  Should
        not be used for the begin or end token."""
        self.lex.undef_token(token_label)

    def def_begin_end_tokens(self, begin_token_label, end_token_label):
        """Calls the `Lexer` to def_begin_end_tokens.  The subclasses are
        then given initial head and tail functions for use in the Pratt parser.
        To use the `PrattParser` this method must be called, not the method of
        `Lexer` with the same name (since it also creates head and tail handler
        functions that raise exceptions for better error messages).  The
        default is to call this method automatically on initialization, with
        the default token labels for the begin and end tokens.  If
        `default_begin_end_tokens` is set false on initalization then the user
        must call this function (setting whatever token labels are desired).
        Returns a tuple containing the new begin and end `TokenNode`
        subclasses."""
        # Call lexer to create and register the begin and end tokens.
        self.lex.def_begin_end_tokens(begin_token_label, end_token_label)
        # define the begin token
        self.begin_token_label = begin_token_label
        def begin_head(self, lex):
            raise ParserException("Called head of begin token.")
        def begin_tail(self, lex, left):
            raise ParserException("Called tail of begin token.")
        self.begin_token_subclass = self.modify_token_subclass(
                               begin_token_label, head=begin_head, tail=begin_tail)
        # define the end token
        self.end_token_label = end_token_label
        def end_head(self, lex):
            raise ParserException("Called head of end token.")
        def end_tail(self, lex, left):
            raise ParserException("Called tail of end token.")
        self.end_token_subclass = self.modify_token_subclass(
                                  end_token_label, head=end_head, tail=end_tail)
        return self.begin_token_subclass, self.end_token_subclass

    def def_jop_token(self, jop_token_label, ignored_token_label):
        """Define a token for the juxtaposition operator.  This token is not
        stored in the lexer's symbol table and has no regex pattern.  An
        instance is inserted in `recursive_parse` when it is inferred to be
        present based based on type information in the definition of the
        juxtaposition operator.  This method must be called before a
        juxtaposition operator can be used.  The parameter `jop_token_label` is the
        label for the newly-created token representing the juxtaposition
        operator.  The `ignored_token_label` parameter is the label of an
        ignored token which must be present for a jop to be inferred.  Some
        token is required; usually it will be a token for spaces and tabs."""

        if self.jop_token_subclass:
            raise ParserException("A jop token is already defined.  It must be "
                                  "undefined before defining an new one.")
        self.jop_token_label = jop_token_label
        self.jop_ignored_token_label = ignored_token_label
        self.jop_token_subclass = self.symbol_table.create_token_subclass(
                                                           jop_token_label)
        return self.jop_token_subclass

    def modify_token_subclass(self, token_label, prec=None, head=None, tail=None, 
                       precond_label=None, precond_fun=None,
                       precond_priority=0, val_type=None, arg_types=None):
        """Look up the subclass of base class `TokenNode` corresponding to the
        label `token_label` (in the symbol table) and modify it.  A token with
        that label must already be in the symbol table, or an exception will be
        raised.  Return the modified class. Sets any given head or tail functions
        as attributes of the class.  If `tail` is set then the prec will also be
        set unless `prec` is `None`.  For a head `prec` is ignored.  If `tail` is
        set and `prec` is `None` then the prec value defaults to zero.  If `head`
        or `tail` is set and `precond_label` is also set then the head or tail
        function will be associated with the preconditions function for that
        label.  If `precond_fun` is also set then it will first be registered
        with the label `precond_label` (which must be present in that case)."""
        if isinstance(arg_types, str):
            raise ParserException("The arg_types argument to token_subclass must"
                    " be None or an iterable returning type labels (e.g., a list"
                    " or tuple).")
        if tail and prec is None: prec = 0

        if self.symbol_table.has_key(token_label):
            TokenSubclass = self.symbol_table.get_token_subclass(token_label)
        else:
            raise ParserException("In call to mod_token_subclass: subclass for"
                    " token labeled '{0}' has not been defined.  Maybe try"
                    " calling `def_token` first.".format(token_label))
            # This used to just create a subclass, but that can mask errors.
            #TokenSubclass = self.symbol_table.create_token_subclass(token_label)

        # Save a reference to the PrattParser, so nodes can access it if they need to.
        TokenSubclass.parser_instance = self # maybe weakref later

        if tail: TokenSubclass.static_prec = prec # Ignore prec for heads; it will stay 0.

        if arg_types is None:
            type_sig = TypeSig(val_type, None)
        else: 
            type_sig = TypeSig(val_type, arg_types) 

        if head:
            TokenSubclass.register_handler_fun(HEAD, head,
                               precond_label=precond_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig)
        if tail:
            TokenSubclass.register_handler_fun(TAIL, tail,
                               precond_label=precond_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig)
        return TokenSubclass

    def undef_handler(self, token_label, head_or_tail, precond_label=None,
                         val_type=None, arg_types=None, all_handlers=False):
        """Undefine a head or tail function with the given `token_label`,
        `precond_label` and type signature.  The `head_or_tail` value should be
        `HEAD` or `TAIL`.  If `all_precond` is set then all heads and tails for all
        preconditions will be undefined.  If `all_overloads` then all
        overloaded type signatures will be undefined.  The token itself is
        never undefined; use the `undef_token` method for that."""
        TokenSubclass = self.symbol_table.get_token_subclass(token_label)
        TokenSubclass.unregister_handler_fun(head_or_tail,
                                         precond_label=precond_label,
                                         type_sig=TypeSig(val_type, arg_types),
                                         all_handlers=all_handlers)

    #
    # Methods dealing with types.
    #

    def def_type(self, type_name, type_params=None):
        """Define a type associated with the name `type_name`."""
        return self.type_table.create_typeobject_subclass(type_name, type_params)

    def undef_type(self, type_name):
        self.type_table.undef_typeobject_subclass(type_name)

    #
    # Methods defining syntax elements.
    #

    def def_parser_global_precondition(self):
        pass
        # TODO decide if this is a good idea, implement if so, delete otherwise.
        # Is the right level for this, or should it be module level or only
        # token subclass level?  Note that the dict and TokenSubclass method
        # is already implemented.
        #
        # Note that precondition priority is a property of the handler functions
        # using them, not the precondition funs.  If error not set to be raised
        # on ties then last-set one has priority on ties.

    # TODO these can each have a corresponding undefine method; should be easy
    # with undef_handler method.

    def def_literal(self, ast_label, token_label, val_type=None, eval_fun=None):
        """Defines the token with label `token_label` to be a literal in the
        syntax of the language being parsed.  This method adds a head handler
        function to the token.  Literals are the leaves of the parse tree; they
        are things like numbers and variable names in a numerical expression.
        They always occur as the first (and only) token in a subexpression
        being evaluated by `recursive_parse`, so they need a head handler but not
        a tail handler."""
        def head_handler_literal(self, lex):
            self.process_and_check_node(head_handler_literal, ast_label, eval_fun)
            return self
        self.modify_token_subclass(token_label, head=head_handler_literal,
                                                            val_type=val_type)

    def def_multi_infix_op(self, ast_label, operator_token_labels, prec,
                                    assoc, repeat=False, in_tree=True,
                                    val_type=None, arg_types=None, eval_fun=None):
        # TODO only this type currently supports "in_tree" kwarg.  General and easy
        # mechanism, though.  Test more and add to other methods.
        # Does in-tree keep the first one? how is it defined for this thing?
        # Comma operator is example of in_tree=False, but how does it handle
        # the root??
        """Takes a list of operator token labels and defines a multi-infix
        operator.  If `repeat=True` it will accept any number of repetitions of
        the list of operators (but type-checking for that is not implemented
        yet).  For a single operator, repeating just has the effect of putting
        the arguments in a flat argument/child list instead of as nested binary
        operations based on left or right association.  Any argument-checking
        is done after any node removal, which may affect the types that should
        be passed-in in the list arg_types of parent constructs."""
        recurse_bp = prec
        if assoc == Assoc.right: recurse_bp = prec - 1
        def tail_handler(self, lex, left):
            self.append_children(left, PrattParser.recursive_parse(lex, recurse_bp))
            while True:
                for op in operator_token_labels[1:]:
                    PrattParser.match_next(lex, op)
                    self.append_children(PrattParser.recursive_parse(lex, recurse_bp))
                if not repeat: break
                # Peek ahead and see if we need to loop another time.
                if lex.peek().token_label != operator_token_labels[0]: break
                PrattParser.match_next(lex, operator_token_labels[0])
                self.append_children(PrattParser.recursive_parse(lex, recurse_bp))
            self.process_and_check_node(tail_handler,
                         ast_label, eval_fun, in_tree=in_tree, repeat_args=repeat)
            return self
        self.modify_token_subclass(operator_token_labels[0], prec=prec,
                                tail=tail_handler, val_type=val_type, arg_types=arg_types)

    def def_infix_op(self, ast_label, operator_token_label, prec,
                              assoc, in_tree=True,
                              val_type=None, arg_types=None, eval_fun=None):
        """This just calls the more general method `def_multi_infix_op`."""
        self.def_multi_infix_op(ast_label, [operator_token_label], prec,
                              assoc, in_tree=in_tree,
                              val_type=val_type, arg_types=arg_types, eval_fun=eval_fun)

    def def_prefix_op(self, ast_label, operator_token_label, prec,
                               val_type=None, arg_types=None, eval_fun=None):
        """Define a prefix operator."""
        def head_handler(self, lex):
            self.append_children(PrattParser.recursive_parse(lex, prec))
            self.process_and_check_node(head_handler, ast_label, eval_fun)
            return self
        self.modify_token_subclass(operator_token_label, head=head_handler,
                                val_type=val_type, arg_types=arg_types)

    def def_postfix_op(self, ast_label, operator_token_label, prec,
                                allow_ignored_before=True,
                                val_type=None, arg_types=None, eval_fun=None):
        """Define a postfix operator.  If `allow_ignored_before` is false then
        no ignored token (usually whitespace) can appear immediately before the
        operator."""
        def tail_handler(self, lex, left):
            if not allow_ignored_before: PrattParser.no_ignored_before(lex)
            self.append_children(left)
            self.process_and_check_node(tail_handler, ast_label, eval_fun)
            return self
        self.modify_token_subclass(operator_token_label, prec=prec, tail=tail_handler, 
                                val_type=val_type, arg_types=arg_types)

    def def_bracket_pair(self, ast_label, lbrac_token_label, rbrac_token_label,
                                                           prec, eval_fun=None):
        """Define a matching bracket grouping operation.  The returned type is
        set to the type of its single child (i.e., the type of the contents of the
        brackets)."""
        # Define a head for the left bracket of the pair.
        def head_handler(self, lex):
            self.append_children(PrattParser.recursive_parse(lex, prec))
            PrattParser.match_next(lex, rbrac_token_label)
            self.process_and_check_node(head_handler, ast_label, eval_fun,
                        typesig_override=ActualTypes(self.children[0].val_type, None))
            return self
        self.modify_token_subclass(lbrac_token_label, head=head_handler)

    def def_stdfun_lookahead(self, ast_label, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label,
                      val_type=None, arg_types=None, eval_fun=None):
        """This definition of stdfun uses lookahead."""
        def preconditions(lex, lookbehind):
            """Must be followed by a token with label 'lpar_token_label', with no
            whitespace in-between."""
            peek_tok = lex.peek()
            if peek_tok.ignored_before():
                return False
            if lex.peek().token_label != lpar_token_label:
                return False
            return True
        precond_label = "lpar after, no whitespace between" # Should be a unique label.

        def head_handler(self, lex):
            PrattParser.match_next(lex, lpar_token_label) # We know this will match.
            while lex.peek().token_label != rpar_token_label:
                self.append_children(PrattParser.recursive_parse(lex, 0))
                if lex.peek().token_label == comma_token_label: 
                    # TODO single utility fun or flag for this if?
                    PrattParser.match_next(lex, comma_token_label)
                else: break
            PrattParser.match_next(lex, rpar_token_label)
            self.process_and_check_node(head_handler, ast_label, eval_fun)
            return self
        subclass = self.modify_token_subclass(fname_token_label, prec=0,
                         head=head_handler, precond_label=precond_label,
                         precond_fun=preconditions, precond_priority=1,
                         val_type=val_type, arg_types=arg_types)

    def def_stdfun_lpar_tail(self, ast_label, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label, prec_of_lpar,
                      val_type=None, arg_types=None, eval_fun=None):
        """This is an alternate version of stdfun that defines lpar as an infix
        operator (with a tail).  This function works in the usual cases but
        current version without preconditions may have problems distinguishing
        "b (" from "b(" when a multiplication jop is set."""
        # Could also recognize alternate symbols to divide args (like
        # using "|" in probability and ";" in some cases).
        def tail_handler(self, lex, left):
            PrattParser.no_ignored_before(lex) # Nothing between fun name and lpar_token.
            while lex.peek().token_label != rpar_token_label:
                left.append_children(PrattParser.recursive_parse(lex, prec_of_lpar))
                if lex.peek().token_label == comma_token_label: 
                    # TODO single utility fun or flag for this if?
                    PrattParser.match_next(lex, comma_token_label)
                else: break
            PrattParser.match_next(lex, rpar_token_label)
            left.process_and_check_node(tail_handler, ast_label, eval_fun)
            return left
        self.modify_token_subclass(lpar_token_label,
                                         prec=prec_of_lpar, tail=tail_handler,
                                         val_type=val_type, arg_types=arg_types)

    def def_jop(self, ast_label, prec, assoc, precond_label=None,
                                      precond_fun=None, precond_priority=None,
                                      val_type=None, arg_types=None, eval_fun=None):
        """The function `precond_fun` is called to determine whether or not to
        infer a juxtaposition operator between the previously-parsed
        subexpression result and the next token.  This function will be passed
        the lexer as well as the lookbehind list as arguments.  Note that the
        `jop_precond` function has access to the type information for the
        potential left operand but not for the potential right operand.  If
        this function returns `True` then a jop is inferred and the parse
        proceeds assuming there is a jop token in the token stream.
        
        If `backtrack_on_parse_error` is `True` then any parse error in
        evaluating the tail of the jop will cause the algorithm to backtrack and
        proceed without inferring a jop.  If `backtrack_on_type_error` is
        `True` then any type errors in processing the jop will cause the
        algorithm to backtrack and proceed without inferring a jop.  Note that
        careful definition of `jop_precond` can greatly reduce or eliminate the
        need to backtrack.  Note also that if the juxtaposition operator always
        resolves to a single type signature based on its argument types then,
        even if overloading on return types is in effect, the jop can be
        effectively inferred based on type signature information.""" 

        recurse_bp = prec
        if assoc == Assoc.right: recurse_bp = prec - 1
        def tail_handler(self, lex, left):
            self.append_children(left, PrattParser.recursive_parse(lex, recurse_bp))
            self.process_and_check_node(tail_handler, ast_label, eval_fun)
            return self
        self.modify_token_subclass(self.jop_token_label, prec=prec, tail=tail_handler,
                            precond_label=precond_label, precond_fun=precond_fun,
                            precond_priority=precond_priority,
                            val_type=val_type, arg_types=arg_types)
        
    #
    # Static methods for use inside head and tail functions.
    #

    @staticmethod
    def match_next(lex, token_label_to_match, discard_non_matches=False):
        """A utility function that asserts the value of the next token label
        in `lex` and also consumes the token from the lexer.  If the token
        label does not match an exception is raised."""
        if token_label_to_match != lex.peek().token_label:
            raise ParserException("Match function expected {0} but found {1}."
                             .format(token_label_to_match, lex.peek().token_label))
        lex.next() # Eat the token that was matched.
        return

    @staticmethod
    def in_ignored_tokens(lex, token_label_to_match):
        """A utility function to test if a particular token label is among the
        tokens ignored before the current token.  Returns a boolean value."""
        ignored_token_labels = [t.token_label for t in lex.peek().ignored_before_list]
        if token_label_to_match in ignored_token_labels:
            return True
        return False

    @staticmethod
    def no_ignored_after(lex):
        # TODO maybe make just a boolean fun, for localized error handling.
        """Test if any tokens were ignored between current token and lookahead."""
        if lex.peek().ignored_before(): raise ParserException(
                "Expected nothing between {0} and previous symbol."
                .format(lex.peek().value))

    @staticmethod
    def no_ignored_before(lex):
        # TODO maybe make just a boolean fun, for localized error handling.
        """Test if any tokens were ignored between previous token and current."""
        if lex.token.ignored_before(): raise ParserException(
                "Expected nothing between {0} and previous symbol."
                .format(lex.token))

    #
    # The main parse routines.
    #

    def parse(self, program):
        """The main routine for parsing a full program or expression.  Users of
        the class should call this method to perform the parsing operations
        (after defining a grammar, of course).  Returns a token tree or a list
        of token trees if `multi_expression` is set."""

        parse_tree_list = []
        while True:
            self.lex.set_text(program)
            output = PrattParser.recursive_parse(self.lex, 0)
            parse_tree_list.append(output)

            # Finalize type-checking for root when overloading on return types.
            if self.overload_on_ret_types: 
                output.check_types_in_tree_second_pass(root=True)

            # See if we reached the end of the token stream.
            if self.lex.is_end_token(self.lex.peek()): break
            if self.multi_expression: continue
            else: raise ParserException("Parsing never reached end of expression,"
                    " stopped at current token with label '{0}' and value "
                    "'{1}' before a token with label '{2}' and value '{3}'."
                    .format(self.lex.token.token_label, self.lex.token.value, 
                            self.lex.peek().token_label, self.lex.peek().value))

        if self.multi_expression: return parse_tree_list
        else: return output

    @staticmethod
    def infer_jop_conditions(lex):
        """Test whether or not a juxtaposition operator should be inferred in
        the `recursive_parse` function."""

        # Fail if jop undefined.
        if not lex.parser_instance.jop_token_subclass: return False
        # Fail if at end of expression.
        if lex.is_end_token(lex.peek()): return False
        # Fail if the ignored token for jop is not present (TODO optional but default.)
        if (lex.parser_instance.jop_ignored_token_label 
                not in lex.peek().ignored_before_labels()): return False
        # Fail if peek has a tail, since that may be lower-prec operator.
        if lex.peek().prec() > 0: return False

        return True

    @staticmethod
    def recursive_parse(lex, subexp_prec):

        """Parse a subexpression as defined by token precedences. Return the
        result of the evaluation.  Recursively builds up the final result in
        `processed_left`, which is the tree for the part of the full expression
        to the left of the current token.  This is a static method so that it
        can be called from head and tail functions.  Note that the functions
        `head_dispatcher` and `tail_dispatcher` which are called in the code
        often recursively call `recursive_parse` again.  Each recursive call
        inside the function processes a subexpression, sub-subexpression, etc.
        (as implicitly defined by the token precedences).  The list
        `lookbehind` saves all the previously evaluated subexpressions at this
        level of recursion (i.e., at the top level in the same subexpression)
        and passes it to the `tail_dispatcher` method of the tokens, in case
        that routine wants to make use of it.  For example, the ordinal
        position of the token in the top level of the subexpression can be
        calculated from the length of `lookbehind`."""

        # NOTE that with a good, efficient pushback function the modifiable
        # prec for different handler functions might be doable: just do a next
        # then evaluate the prec, then pushback.

        curr_token = lex.next()
        processed_left = curr_token.dispatch_and_call_handler(HEAD, lex)
        lookbehind = [processed_left]

        while True:

            # The main loop, except for the special case when a jop is defined.
            while lex.peek().prec() > subexp_prec:
                curr_token = lex.next()
                processed_left = curr_token.dispatch_and_call_handler(
                                       TAIL, lex, processed_left, lookbehind)
                lookbehind.append(processed_left)

            # Broke out of main loop, determine whether or not to infer a jop.
            if not PrattParser.infer_jop_conditions(lex): break

            # Infer a jop, but only if 1) its prec would satisfy the while loop
            # above as an ordinary token and 2) it has a head handler defined
            # in the current conditions.
            if lex.parser_instance.jop_token_subclass.prec() > subexp_prec:
                # Infer a jop: create a subclass instance for the jop token.
                jop_instance = lex.parser_instance.jop_token_subclass(None)
                try: processed_left = jop_instance.dispatch_and_call_handler(
                                       TAIL, lex, processed_left, lookbehind)
                except NoHandlerFunctionDefined:
                    break # No precondition matches, assume no jop.
                lookbehind.append(processed_left)
            else:
                break

        return processed_left

#
# Exceptions
#

class ParserException(Exception):
    """General parser errors."""
    pass

class TypeError(ParserException):
    """Error in type matching."""
    pass

class NoHandlerFunctionDefined(ParserException):
    """Raised by dispatcher function if it fails to find a handler function
    (head or tail, whichever it was looking for)."""
    pass

#
# Run tests below when invoked as a script.
#

if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_pratt_parser.py", pytest_args="-v")

