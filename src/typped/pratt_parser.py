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

          parser.def_literal("k_number", val_type="t_number", ast_label="a_number")
          parser.def_infix_op("k_plus", 10, Assoc.left,
                           val_type="number", arg_types=["t_number","t_number"],
                           ast_label="a_add")

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

          eval_result = result_tree.eval_subtree()
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

# TODO:  make a real package, importing stuff into the __init__.py, etc. and maybe
# using setup.py and pip -e, but first can just add to AUTOENV.

from __future__ import print_function, division, absolute_import
import types
from collections import OrderedDict, namedtuple
from enum_wrapper import Enum
from lexer import Lexer, TokenNode, TokenSubclassDict, LexerException, BufferIndexError

from pratt_types import TypeTemplateDict, TypeSig, ActualTypes

# NOTE that you could use the evaluate function stuff to also do the conversion to AST.

# TODO: Do preconditions really need labels?  Can the users just be left to
# manage their own preconditions, or maybe a separate class can be defined to
# manage them in a dict, maybe with some predefined ones?  Seems like it could
# be separated out, and reduce complexity.  Is equality testing of
# preconditions ever truly required?  If not, why not just use the function
# objects and leave the user to manage their functions however they want.

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
                             type_sig=None, eval_fun=None):
            # TODO: save the eval_fun with the signature, in a way that
            # process_and_check_node can recover and set for instance (after
            # finding matching one).  Make sure that unregister_handler_fun
            # still works, too.
            #

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

            handler_fun.eval_fun = eval_fun # TODO change this to work for overloaded

            # See if there is already a preconditions function with this
            # precond_label associated with the token (in which case the type
            # sig is assumed to be overloaded).  There should only ever be one
            # previous preconditions function with the label, at most, since
            # only overloaded type info is retained, nothing else (the way the
            # preconditions are evaluated only the first would ever get called,
            # anyway).
            #

            # Remember that you can only sort based on precondition priorities.
            # You do not get to choose the particular signature that will
            # appear in the program text stream.  BUT, we DO want the chosen
            # eval_fun to depend on that.  The actual parsing method will NOT
            # depend on the different signatures, since we need to parse it
            # first before we learn its signature (could be done on the fly,
            # but strange and hard to see how useful it would be).  So as it is
            # it WORKS, but needs cleaning up (especially preconditions stuff,
            # maybe also go to OrderedDict.  NOTE the difference between the
            # parsing function and the evaluation function (if there is one)!
            #
            # For a simpler possible implementation, consider hashing the
            # precond label with the type_sig.  Then you would only have one
            # sig saved with each key in the dict and could store eval_fun
            # there, too.  BUT it is uncertain what kind of interplay that will
            # have with fancier type signatures.  For now, store eval_fun with
            # the type sigs -- need to extract it right at the moment when a
            # unique type sig has been narrowed down to.  Advantage is you
            # could delete things in finer detail, a precond_label and type_sig
            # combo; but either alone might be harder (though you can just search
            # the keys).  Disadvantage is that type equality of fancy types
            # might or might not work out nicely.  Maybe do first part, then
            # wait to see how that turns out?

            # Ideas for above refactoring.  First refactor below to OrderedDict
            # better in both cases, full thing is unsure. 
            # function?  Seems like we should have a dict keyed by both the
            # precond_label AND the type_sig (using return values if that is
            # allowed, otherwise not).  I.e., types unique if that tuple is
            # unique.  I think type_sig is hashable, so should probably work...
            # THEN the eval function will work just by storing it with the
            # handler itself.
            #
            # To transistion, first turn the prev_handlers_for_precond_list
            # into a sorted OrderedDict, still using *only* the precond_label.
            # See recipe here (just re-sort after adding things, fix later if
            # needed):
            # https://docs.python.org/3/library/collections.html#ordereddict-examples-and-recipes

            # TODO: update to use below line or similar, or delete.
            #type_sig_and_eval_tuple = (type_sig, eval_fun)

            try:
                prev_handlers_for_precond_list = [s.handler_fun
                            for s in self.handler_funs[head_or_tail]
                            if s.precond_label == precond_label]
                # Multiple defs shouldn't happen; list is used just to check.
                assert len(prev_handlers_for_precond_list) <= 1
            except KeyError:
                prev_handlers_for_precond_list = None

            if prev_handlers_for_precond_list:
                prev_type_sigs_for_precond = prev_handlers_for_precond_list[0].type_sigs
            else:
                prev_type_sigs_for_precond = []

            if (prev_handlers_for_precond_list
                              and not self.parser_instance.overload_on_arg_types):
                raise ParserTypeError("Value of self.overload_on_arg_types is False "
                       "but attempt to redefine and possibly set multiple signatures "
                       "for the {0} function for token with label '{1}' with "
                       "preconditions label '{2}'."
                       .format(head_or_tail, self.token_label, precond_label))

            # Type info is stored as an attribute of handler funs.  For
            # overloading, append the type_sig to prev_type_sigs_for_precond,
            # saving them all.
            prev_type_sigs_for_precond.append(type_sig)
            handler_fun.type_sigs = prev_type_sigs_for_precond

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
            # Assume this is a redefinition.
            for index in reversed(range(len(sorted_handler_list))):
                if sorted_handler_list[index].precond_label == precond_label:
                    del sorted_handler_list[index]

            # Insert the data in the selected head or tail list, in priority-sorted order.
            # The namedtuple HandlerData is used to hold it.
            data_list = HandlerData(precond_label, precond_fun, precond_priority,
                                    handler_fun)
            if not sorted_handler_list:
                sorted_handler_list.append(data_list)
                self.handler_funs[head_or_tail] = sorted_handler_list
            else:
                for index in range(len(sorted_handler_list)):
                    if (precond_priority == sorted_handler_list[index].precond_priority
                        and self.parser_instance.raise_exception_on_precondition_ties):
                        raise ParserException("Two preconditions for the token"
                                " subclass named '{0}' for token with label '{1}' have"
                                " the same priority, {2}.  Their precondition labels"
                                " are '{3}' and '{4}'.  If precondition labels are"
                                " the same there may be a redefinition. Set the flag"
                                " False if you actually want to allow precondition"
                                " ties." .format(self.__name__, self.token_label,
                                    precond_priority, precond_label,
                                    sorted_handler_list[index].precond_label))
                    if precond_priority >= sorted_handler_list[index].precond_priority:
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
                if item.precond_label != precond_label: continue
                handler_fun = item.handler_fun
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
            Raises `NoHandlerFunctionDefined` if no handler function can be found.
            
            If the parameter `precond_label` is set then this method returns the
            handler function which *would be* returned, assuming that that were
            the label of the "winning" precondition function."""

            try:
                sorted_handler_list = self.handler_funs[head_or_tail]
            except KeyError:
                raise NoHandlerFunctionDefined(
                        "No {0} handler functions at all are defined"
                        " for tokens with token label '{1}'.  The token's"
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
                    "token with token label '{1}' and value '{2}' in the current "
                    "preconditions."
                    .format(head_or_tail, self.token_label, self.value))

        def dispatch_and_call_handler(self, head_or_tail, lex, 
                                      left=None, lookbehind=None, call=True):
            """Look up and call the handler function, passing along the arguments.
            
            If `call` is set false then the handler will not be called.  This
            is used to test if there is a handler of the specified type, by
            catching the `NoHandlerFunctionDefined` exception."""
            if head_or_tail == HEAD:
                fun = self.lookup_handler_fun(HEAD, lex)
                if call: return fun(self, lex)
            elif head_or_tail == TAIL:
                fun = self.lookup_handler_fun(TAIL, lex, lookbehind=lookbehind)
                if call: return fun(self, lex, left)
            else: 
                raise ParserException("Bad first argument to dispatch_and_call_handler"
                        " function: must be HEAD or TAIL or equivalent.")

        def process_and_check_node(self, fun_object,
                                   typesig_override=None, in_tree=True,
                                   repeat_args=False, ast_label=None):
            """This routine should always be called from inside the individual
            head and tail handler functions, just before they return a value.
            It sets some attributes and checks that the actual types match some
            defined type signature for the function.
            
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
            times.
            
            The `ast_label` argument an optional string label of an AST node
            type (subclass of `AST_Node`) which is simply set as an attribute
            of the corresponding `TokenNode` in the parse tree.  It can
            optionally be set and used when converting a parse tree to a
            (preliminary) AST.  For example, the infix asterisk, the function
            `mult`, and the juxtaposition operator might all be assigned the
            same AST node (for the general multiplication operation)."""

            self.ast_label = ast_label
            self.in_tree = in_tree
            #if eval_fun:
            #    # This sets for the CLASS, not the instance, we need for instance...
            #    #self.add_eval_subtree_method(eval_fun)
            #    self.eval_fun = eval_fun # DEBUG, testing....
            self.eval_fun = fun_object.eval_fun
           
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
                list_of_child_sig_lists = [[c.type_sig] for c in self.children]
            else:
                # First pass case, multiple sigs in child's self.matching_sigs list.
                list_of_child_sig_lists = [c.matching_sigs for c in self.children]

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
                    c for c in self.children if hasattr(c, "matching_sigs")]
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
            raise ParserTypeError(basic_msg + diagnostic)

        #
        # Evaluations and semantic actions.
        #

        # TODO TODO TODO The evaluations need to depend on the final type that
        # is resolved.  The eval funs are passed in with a type, and should be
        # stored with that handler function which is set (like the type info
        # itself).  This evaluate interface needs to be re-worked slightly to
        # accomodate that, and the code below that uses the
        # add_eval_subtree_method needs to be modified.
        def eval_subtree(self): # DEBUG, testing.
            print("id of eval fun", id(self.eval_fun))
            return self.eval_fun(self) # Run the function saved with the instance.

        def add_eval_subtree_method(self, eval_fun):
            """Add a method called `eval_subtree` to the instance of the class.
            The function `eval_fun` passed in should take self as its first
            argument.  It should return the result of evaluating the node,
            calculated from its own attributes and from the results of calling
            `c.eval_subtree()` for each child `c`.  For example, an addition
            node would return the results of adding the evaluations from the
            children.  If this is defined for all the nodes in a tree it can be
            called from the root to evaluate the full tree."""
            self.eval_subtree = types.MethodType(eval_fun, self)

        def semantic_action(self):
            # TODO decide how to implement and when to call.  Should probably
            # be relative to typesigs/handlers rather than nodes in particular.
            pass

        #
        # Some helper functions for use in handler functions.
        #

        def match_next(self, token_label_to_match, peeklevel=1,
                       raise_on_fail=False, raise_on_true=False, consume=True):
            """A utility function that tests whether the value of the next token label
            in `lex` equals a given token label, and consumes the token from the lexer
            if there is a match.  Returns a boolean.  The parameter `peeklevel` is
            passed to the peek function for how far to look; the default is one.
            
            If `raise_on_fail` set true then a `ParserException` will be raised
            if the match fails.  If `consume` is false then no tokens will be
            consumed."""
            lex = self.lex
            retval = False
            if token_label_to_match == lex.peek(peeklevel).token_label:
                retval = True
            if consume and retval:
                lex.next() # Eat the token that was matched.

            if retval and raise_on_true:
                    raise ParserException(
                        "Function match_next with peeklevel={0} found unexpected "
                        "token {1}."
                        .format(peeklevel, str(lex.peek(peeklevel))))
            if not retval and raise_on_fail:
                    raise ParserException(
                        "Function match_next with peeklevel={0} expected token "
                        " with label '{1}' but found token {2}."
                        .format(peeklevel, token_label_to_match,
                                str(lex.peek(peeklevel))))
            return retval

        def in_ignored_tokens(self, token_label_to_match,
                              raise_on_fail=False, raise_on_true=False):
            """A utility function to test if a particular token label is among the
            tokens ignored before the current token.  Returns a boolean value."""
            lex = self.lex
            retval = False
            ignored_token_labels = [t.token_label for t in lex.peek().ignored_before_list]
            if token_label_to_match in ignored_token_labels:
                retval = True

            if retval and raise_on_true:
                    raise ParserException(
                        "Function in_ignored_tokens found unexpected token with "
                        "label '{0}' before the current token {1}."
                        .format(token_label_to_match, str(lex.token)))
            if not retval and raise_on_fail:
                    raise ParserException(
                        "Function in_ignored_tokens expected token with label "
                        "'{0}' before the current token {1}, but it was not found."
                        .format(token_label_to_match, str(lex.token)))
            return retval

        def no_ignored_after(self, raise_on_fail=False, raise_on_true=False):
            """Boolean function to test if any tokens were ignored between current token
            and lookahead."""
            lex = self.lex
            retval = True
            if lex.peek().ignored_before():
                retval = False

            if retval and raise_on_true:
                    raise ParserException(
                        "Function no_ignored_after expected tokens between the current "
                        "token {0} and the following token {1}, but there were none."
                        .format(str(lex.token), str(lex.peek())))
            if not retval and raise_on_fail:
                raise ParserException(
                        "Function no_ignored_after expected nothing between the "
                        "current token {0} and the following token {1}, but there "
                        "were ignored tokens."
                        .format(str(lex.token), str(lex.peek())))
            else:
                return False
            return retval

        def no_ignored_before(self, raise_on_fail=False, raise_on_true=False):
            """Boolean function to test if any tokens were ignored between previous token
            and current token."""
            lex = self.lex
            retval = True
            if lex.token.ignored_before():
                retval = False

            if retval and raise_on_true:
                    raise ParserException(
                        "Function no_ignored_before expected ignored tokens before "
                        " the current token {0}, but none were found."
                        .format(str(lex.token)))
            if not retval and raise_on_fail:
                raise ParserException(
                        "Function no_ignored_before expected no ignored tokens "
                        "before the current token {0}, but at least one was found."
                        .format(str(lex.token)))
            return retval

        #
        # The main recursive_parse function.
        #

        def recursive_parse(self, subexp_prec):
            """Parse a subexpression as defined by token precedences. Return
            the result of the evaluation.  Recursively builds up the final
            result in `processed_left`, which is the tree for the part of the
            full expression to the left of the current token.  This is a static
            method so that it can be called from head and tail functions.  Note
            that the function `dispatch_and_call_handler` which is called in
            the code often recursively call `recursive_parse` again.  Each
            recursive call inside the function processes a subexpression,
            sub-subexpression, etc.  (as implicitly defined by the token
            precedences).  The list `lookbehind` saves all the previously
            evaluated subexpressions at this level of recursion (i.e., at the
            top level in the same subexpression) and passes it to the
            `tail_dispatcher` method of the tokens, in case that routine wants
            to make use of it.  For example, the ordinal position of the token
            in the top level of the subexpression can be calculated from the
            length of `lookbehind`.
            
            This function is made a method of `TokenSubclass` so that handler
            functions can easily call it by using `tok.recursive_parse`, and
            also so that it can access the lexer without it needing to be
            passed as an argument."""
            # NOTE that with a good, efficient pushback function the modifiable
            # prec for different handler functions might be doable: just do a
            # next then evaluate the prec, then pushback.

            lex = self.lex
            curr_token = lex.next()
            processed_left = curr_token.dispatch_and_call_handler(HEAD, lex)
            lookbehind = [processed_left]

            while True:

                #
                # The main loop, except for the special case when a jop is defined.
                #

                while lex.peek().prec() > subexp_prec:
                    curr_token = lex.next()
                    processed_left = curr_token.dispatch_and_call_handler(
                                           TAIL, lex, processed_left, lookbehind)
                    lookbehind.append(processed_left)

                #
                # Broke out of main loop, determine whether or not to infer a jop.
                #

                # TODO: Looking at the parser instance works for now, but as it
                # is will break when different parsers share a common lexer.
                # At least some of the jop stuff could move to the Lexer.  The
                # parsers could also write their jop stuff to the lexer... consider.

                # Not if jop undefined.
                if not lex.parser_instance.jop_token_subclass: break
                # Not if at end of expression.
                if lex.is_end_token(lex.peek()): break
                # Not if the ignored token for jop is set but not present.
                if lex.parser_instance.jop_ignored_token_label and (
                              lex.parser_instance.jop_ignored_token_label 
                              not in lex.peek().ignored_before_labels()): 
                    break

                # Infer a jop, but only if 1) its prec would satisfy the while loop
                # above as an ordinary token, 2) the next token has a head
                # handler defined in the conditions when the jop will run its head
                # handler, and 3) the next token similarly has no tail handler.
                if lex.parser_instance.jop_token_subclass.prec() > subexp_prec:

                    # Provisionally infer a jop; create a subclass instance for its token.
                    jop_instance = lex.parser_instance.jop_token_subclass(None)

                    # This is a little inefficient, but we need to be sure that
                    # when the tail handler of the jop is called and it reads a
                    # token that that token has a head handler defined for it *in
                    # that precondition context*.  Otherwise, no jop will be
                    # inferred.  We also make sure that it has no tail handler in
                    # the context, since then it would be a lower-precedence (lower
                    # precedence because we broke out of the loop above) infix or
                    # postfix operator, and no jop is inferred before another operator).
                    curr_token = lex.next()
                    try:
                        # Dispatch without calling here, since calling will consume
                        # another token; also, deeper-level recursions could cause false
                        # results to come up the recursion chain.
                        curr_token.dispatch_and_call_handler(
                                HEAD, lex, processed_left, lookbehind, call=False)
                        try: # Found head handler, now make sure it has no tail handler.
                            curr_token.dispatch_and_call_handler(
                                    TAIL, lex, processed_left, lookbehind, call=False)
                        except NoHandlerFunctionDefined: pass
                        else: break
                    except NoHandlerFunctionDefined:
                        break # No precondition matches, assume no jop.
                    finally:
                        lex.go_back(1)

                    # Finally, we can infer a jop.
                    processed_left = jop_instance.dispatch_and_call_handler(
                                           TAIL, lex, processed_left, lookbehind)
                    lookbehind.append(processed_left)
                else:
                    break

            return processed_left


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
        self.preconditions_dict = {} # Registered parser-global preconditions functions.
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
        self.lex.def_token(token_label, regex_string, on_ties=on_ties, ignore=ignore)

    def def_ignored_token(self, token_label, regex_string, on_ties=0):
        """A convenience function to define a token with `ignored=True`."""
        self.lex.def_ignored_token(token_label, regex_string, on_ties=on_ties)

    def def_multi_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once.  Each element
        of the passed-in list should be a tuple containing the arguments to the
        ordinary `def_token` method.  Calls the equivalent `Lexer` function."""
        self.lex.def_multi_tokens(tuple_list)

    def def_multi_ignored_tokens(self, tuple_list):
        """A convenience function, to define multiple ignored tokens at once.
        Each element of the passed-in list should be a tuple containing the arguments
        to the ordinary `def_token` method with `ignore=True`.  Calls the equivalent
        `Lexer` function."""
        self.lex.def_multi_ignored_tokens(tuple_list)

    def undef_token(self, token_label):
        """A convenience function; calls the Lexer `undef_token` method.  Should
        not be used for the begin or end token."""
        self.lex.undef_token(token_label)

    def def_begin_end_tokens(self, begin_token_label, end_token_label):
        """Calls the `Lexer` to def_begin_end_tokens.  The subclasses are then
        given initial head and tail functions for use in the Pratt parser.  To
        use the `PrattParser` this method must be called, not the method of
        `Lexer` with the same name (since it also creates head and tail handler
        functions that raise exceptions for better error messages).  The
        default is to call this method automatically on initialization, with
        the default token labels for the begin and end tokens.  If the flag
        `default_begin_end_tokens` is set false on `PrattParser` initalization
        then the user must call this function (setting whatever token labels
        are desired).  Returns a tuple containing the new begin and end
        `TokenNode` subclasses."""
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
        self.jop_token_subclass.lex = self.lex
        return self.jop_token_subclass

    def modify_token_subclass(self, token_label, prec=None, head=None, tail=None, 
                       precond_label=None, precond_fun=None,
                       precond_priority=0, val_type=None, arg_types=None,
                       eval_fun=None):
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
            print("DEBUG registered head handler eval_fun with id", id(eval_fun))
            TokenSubclass.register_handler_fun(HEAD, head,
                               precond_label=precond_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig,
                               eval_fun=eval_fun)
        if tail:
            tail.eval_fun = eval_fun
            print("DEBUG registered tail handler eval_fun with id", id(eval_fun))
            TokenSubclass.register_handler_fun(TAIL, tail,
                               precond_label=precond_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig,
                               eval_fun=eval_fun)
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

    def def_literal(self, token_label, val_type=None, eval_fun=None, ast_label=None):
        """Defines the token with label `token_label` to be a literal in the
        syntax of the language being parsed.  This method adds a head handler
        function to the token.  Literals are the leaves of the parse tree; they
        are things like numbers and variable names in a numerical expression.
        They always occur as the first (and only) token in a subexpression
        being evaluated by `recursive_parse`, so they need a head handler but not
        a tail handler."""
        def head_handler_literal(tok, lex):
            tok.process_and_check_node(head_handler_literal, ast_label=ast_label)
            return tok
        self.modify_token_subclass(token_label, head=head_handler_literal,
                                                val_type=val_type, eval_fun=eval_fun)

    def def_multi_literals(self, tuple_list):
        """An interface to the `def_literal` method which takes a list of
        tuples.  The `def_literal` method will be called for each tuple, unpacked
        in the order in the tuple.  Unspecified optional arguments get their default
        values."""
        multi_funcall(self.def_literal, tuple_list)

    def def_infix_multi_op(self, operator_token_labels, prec,
                                    assoc, repeat=False, in_tree=True,
                                    val_type=None, arg_types=None, eval_fun=None,
                                    ast_label=None):
        # TODO only this type currently supports "in_tree" kwarg.  General and easy
        # mechanism, though.  Test more and add to other methods.
        # Does in-tree keep the first one? how is it defined for this thing?
        # Comma operator is example of in_tree=False, but how does it handle
        # the root??
        # TODO: How about in-tree that works at root iff the node only has one child?
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
        def tail_handler(tok, lex, left):
            tok.append_children(left, tok.recursive_parse(recurse_bp))
            while True:
                for op in operator_token_labels[1:]:
                    tok.match_next(op, raise_on_fail=True)
                    tok.append_children(tok.recursive_parse(recurse_bp))
                if not repeat: break
                # Peek ahead and see if we need to loop another time.
                if lex.peek().token_label != operator_token_labels[0]: break
                tok.match_next(operator_token_labels[0], raise_on_fail=True)
                tok.append_children(tok.recursive_parse(recurse_bp))
            tok.process_and_check_node(tail_handler, in_tree=in_tree,
                                        repeat_args=repeat, ast_label=ast_label)
            return tok
        self.modify_token_subclass(operator_token_labels[0], prec=prec,
                                tail=tail_handler, val_type=val_type, arg_types=arg_types,
                                eval_fun=eval_fun)

    def def_infix_op(self, operator_token_label, prec, assoc, in_tree=True,
                     val_type=None, arg_types=None, eval_fun=None, ast_label=None):
        """This just calls the more general method `def_multi_infix_op`."""
        self.def_infix_multi_op([operator_token_label], prec,
                              assoc, in_tree=in_tree,
                              val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                              ast_label=ast_label)

    def def_prefix_op(self, operator_token_label, prec, val_type=None, arg_types=None,
                      eval_fun=None, ast_label=None):
        """Define a prefix operator."""
        def head_handler(tok, lex):
            tok.append_children(tok.recursive_parse(prec))
            tok.process_and_check_node(head_handler, ast_label=ast_label)
            return tok
        self.modify_token_subclass(operator_token_label, head=head_handler,
                            val_type=val_type, arg_types=arg_types, eval_fun=eval_fun)

    def def_postfix_op(self, operator_token_label, prec, allow_ignored_before=True,
                       val_type=None, arg_types=None, eval_fun=None,
                       ast_label=None):
        """Define a postfix operator.  If `allow_ignored_before` is false then
        no ignored token (usually whitespace) can appear immediately before the
        operator."""
        def tail_handler(tok, lex, left):
            if not allow_ignored_before:
                tok.no_ignored_before(raise_on_fail=True)
            tok.append_children(left)
            tok.process_and_check_node(tail_handler, ast_label=ast_label)
            return tok
        self.modify_token_subclass(operator_token_label, prec=prec, tail=tail_handler, 
                            val_type=val_type, arg_types=arg_types, eval_fun=eval_fun)

    def def_bracket_pair(self, lbrac_token_label, rbrac_token_label,
                                               eval_fun=None, ast_label=None):
        """Define a matching bracket grouping operation.  The returned type is
        set to the type of its single child (i.e., the type of the contents of the
        brackets).  Defines a head handler for the left bracket token, so effectively
        gets the highest evaluation precedence."""
        # Define a head for the left bracket of the pair.
        def head_handler(tok, lex):
            tok.append_children(tok.recursive_parse(0))
            tok.match_next(rbrac_token_label, raise_on_fail=True)
            tok.process_and_check_node(head_handler,
                        typesig_override=ActualTypes(tok.children[0].val_type, None),
                        ast_label=ast_label)
            return tok
        self.modify_token_subclass(lbrac_token_label, head=head_handler,
                                   eval_fun=eval_fun)

    def def_stdfun(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label,
                      val_type=None, arg_types=None, eval_fun=None, ast_label=None,
                      num_args=None):
        """This definition of stdfun uses lookahead.  This will take
        arbitrarily many arguments if `arg_types` is `None`.  To check the
        number of arguments when types are not used, set `arg_types` to, for
        example, `[None]*3` for three arguments.
        
        The `num_args` parameter is optional for specifying the number of
        arguments when typing is not being used.  If it is set to a nonnegative
        number then it will automatically set `arg_types` to the corresponding
        list of `None` values; if `arg_types` is set then it is ignored."""
        if num_args is not None and arg_types is None: arg_types = [None]*num_args

        def preconditions(lex, lookbehind):
            """Must be followed by a token with label 'lpar_token_label', with no
            whitespace in-between."""
            peek_tok = lex.peek()
            if peek_tok.ignored_before(): return False
            if peek_tok.token_label != lpar_token_label: return False
            return True
        precond_label = "lpar after, no whitespace between" # Should be a unique label.

        def head_handler(tok, lex):
            # Below match is for a precondition, so it will match and consume.
            tok.match_next(lpar_token_label, raise_on_fail=True)
            # Read comma-separated subexpressions until the peek is rpar_token_label.
            while not tok.match_next(rpar_token_label, consume=False):
                tok.append_children(tok.recursive_parse(0))
                if not tok.match_next(comma_token_label):
                    break
                else:
                    tok.match_next(rpar_token_label, raise_on_true=True)
            tok.match_next(rpar_token_label, raise_on_fail=True)
            tok.process_and_check_node(head_handler, ast_label=ast_label)
            return tok
        self.modify_token_subclass(fname_token_label, prec=0,
                         head=head_handler, precond_label=precond_label,
                         precond_fun=preconditions, precond_priority=1,
                         val_type=val_type, arg_types=arg_types, eval_fun=eval_fun)

    def def_stdfun_lpar_tail(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label, prec_of_lpar,
                      val_type=None, arg_types=None, eval_fun=None, ast_label=None,
                      num_args=None):
        """This is an alternate version of stdfun that defines lpar as an infix
        operator (with a tail).  This function works in the usual cases but
        the current version without preconditions may have problems distinguishing
        "b (" from "b(" when a multiplication jop is set.  The lookahead version
        `def_stdfun` is usually preferred."""
        if num_args is not None and arg_types is None: arg_types = [None]*num_args

        def tail_handler(tok, lex, left):
            # Nothing between fun name and lpar_token.
            tok.no_ignored_before(raise_on_fail=True)
            while not tok.match_next(rpar_token_label, consume=False):
                left.append_children(tok.recursive_parse(prec_of_lpar))
                if not tok.match_next(comma_token_label):
                    break
                else:
                    tok.match_next(rpar_token_label, raise_on_true=True)
            tok.match_next(rpar_token_label, raise_on_fail=True)
            left.process_and_check_node(tail_handler, ast_label=ast_label)
            return left
        self.modify_token_subclass(lpar_token_label,
                                         prec=prec_of_lpar, tail=tail_handler,
                                         val_type=val_type, arg_types=arg_types,
                                         eval_fun=eval_fun)

    def def_jop(self, prec, assoc,
                      precond_label=None, precond_fun=None, precond_priority=None,
                      val_type=None, arg_types=None, eval_fun=None,
                      ast_label=None):
        """The function `precond_fun` is called to determine whether or not to
        infer a juxtaposition operator between the previously-parsed
        subexpression result and the next token.  This function will be passed
        the lexer as well as the lookbehind list as arguments.  Note that the
        `jop_precond` function has access to the type information for the
        potential left operand but not for the potential right operand.  If
        this function returns `True` then a jop is inferred and the parse
        proceeds assuming there is a jop token in the token stream.
        
        Note that if the juxtaposition operator always resolves to a single
        type signature based on its argument types then, even if overloading on
        return types is in effect, the jop can be effectively inferred based on
        type signature information.""" 

        recurse_bp = prec
        if assoc == Assoc.right: recurse_bp = prec - 1
        def tail_handler(tok, lex, left):
            right_operand = tok.recursive_parse(recurse_bp)
            tok.append_children(left, right_operand)
            tok.process_and_check_node(tail_handler, ast_label=ast_label)
            return tok
        self.modify_token_subclass(self.jop_token_label, prec=prec, tail=tail_handler,
                            precond_label=precond_label, precond_fun=precond_fun,
                            precond_priority=precond_priority,
                            val_type=val_type, arg_types=arg_types, eval_fun=eval_fun)

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
            begin_token = self.lex.peek() # Get first token to access recursive_parse.
            output = begin_token.recursive_parse(0)
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


def multi_funcall(function, tuple_list):
   """A convenience function that takes a list of tuples and a method name and
   calls `function` with the values in the tuple as arguments.  The parameter
   `num_args` is the number of arguments, and `defaults` is a list of all the
   default values assigned to parameters.  The parameter `tuple_list` is the
   list of tuples of arguments."""
   for t in tuple_list:
       try:
          function(*t)
       except TypeError:
           raise ParserException(
                   "Bad multi-definition of {0}: Omitted required arguments."
                   "\nError on this tuple: {1}".format(function.__name__, t))


def sort_handler_dict(d):
    """Return the sorted `OrderedDict` version of the dict `d` passed in,
    sorted by the precondition priority.  Not currently used."""
    # https://docs.python.org/3/library/collections.html#ordereddict-examples-and-recipes
    return OrderedDict(sorted(d.items(), key=lambda t: t[2])) 

HandlerData = namedtuple("HandlerData",
              ["precond_label", "precond_fun", "precond_priority", "handler_fun"])

#
# Exceptions
#

class ParserException(Exception):
    """General parser errors."""
    pass

class ParserTypeError(ParserException):
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

