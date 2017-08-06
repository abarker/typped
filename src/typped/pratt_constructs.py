# -*- coding: utf-8 -*-

"""

This module holds the `Construct` class and the `ConstructTable`
class definitions.  They are basically just data containers, which are
accessed by the `PrattParser` instances.  Each `PrattParser` instance
has a `ConstructTable` instance.  The `ConstructTable` instances
hold `Construct` instances.

Constructs themselves are just handler functions for a pratt parser
which are also associated with a triggering token label, a preconditions
function, and other data.  See the documentation on preconditioned
dispatching pratt parsers.

The actual dispatching of handlers is done by the `dispatch_handler` routine of
the `ConstructTable`.  It runs the preconditions functions in order, and
returns a handler function for the winning construct.  This handler function
has its arguments bound, since they are known at the time the handler is
chosen.  This bound handler function first runs the registered handler; it
then does type checking and other options before returning the subtree.

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_ebnf_classes_and_operators.py",
                              "../../test/test_example_calculator.py",
                              "../../test/test_parser_called_from_parser.py",
                              "../../test/test_pratt_parser.py"
                              ], pytest_args="-v")

import functools
from collections import OrderedDict
from .pratt_types import TypeSig, TypeErrorInParsedLanguage
from .shared_settings_and_exceptions import (HEAD, TAIL, NoHandlerFunctionDefined,
                                             ParserException)

class Construct(object):
    """A syntax construct in the language.  Usually corresponds to a subtree of
    the final parse tree.  Essentially a data frame for a handler function
    containing extra context information such as the preconditions function,
    etc.  Each construct for a head (tail) and particular token label must have
    a unique label (overloading is assumed if a label is re-used).

    A construct is possibly triggered when a hander needs to be dispatched for
    its kind of token.  The preconditions functions for all such constructs are
    run in priority-sorted order.  A construct is triggered if its
    preconditions function is the first to evaluate to true.  It then provides
    the handler function to be called.

    Constructs are also saved as attributes of the handler function itself, so
    it can easily be accessed from inside the handler function when it runs.
    (Note that the trigger token is not always the token that ends up at the
    root of the returned subtree, which is where typesig checking is done
    from.)

    A list of typesigs must be stored because overloaded functions use the same
    handler function (it is unknown which signature will apply until after the
    arguments/subtree is parsed to resolve the overload).  Some additional
    information (eval functions and ast labels) is stored in dicts keyed by
    typesigs."""
    __slots__ = ["parser_instance", "construct_label", "trigger_head_or_tail",
                 "trigger_token_label", "handler_fun", "precond_fun",
                 "precond_priority", "original_sigs", "ast_data_dict",
                 "eval_fun_dict", "key_on_values"]
    # At some point precedence (lbp) values might be incorporated into a
    # constructs, but for now all constructs for the same token would need the
    # same precedence.  The info seems to go here, though, at least for future
    # generalization -- reword that section of docs to incorporate constructs.

    def __init__(self, parser_instance,
                       construct_label,
                       trigger_head_or_tail=None,
                       trigger_token_label=None,
                       handler_fun=None,
                       precond_fun=None,
                       precond_priority=0,
                       original_sig=None,
                       eval_fun_dict = None,
                       ast_data_dict = None,
                       key_on_values=False):

        """Initialize a `Construct` instance associated with the parser
        `parser_instance`.  Users should usually use the `register_construct`
        method of a `PrattParser` instance instead of instantiating a
        `Construct` directly.  That routine calls the `register_construct`
        method of a `ConstructTable` instance, which in turn instantiates a
        `Construct`.

        For a new construct the string `construct_label` should be unique among
        constructs associated with the parser.  The `register_construct` method
        of `ConstructTable` (parsers store constructs in instances of that
        class) assumes overloading if the label is the same as an existing one
        in the table.

        The `trigger_head_or_tail` argument should be either `HEAD` or `TAIL`
        (which are currently defined as the strings `"head"` and `"tail"`).

        The `trigger_token_label` is the label of the triggering token.

        The `handler_fun` argument should be passed the handler function.  It
        will be used with preconditions function `precond_fun`, at priority
        `precond_priority`.

        The formal type signature, unexpanded since that is done at parse-time,
        can be passed as `original_sig`.

        An dictionary of evaluation functions and arbitrary data can be passed
        as `eval_fun_dict` and `ast_data_dict`.  Otherwise new empty instances
        are created.  Dicts are passed when redefining a construct (i.e.,
        overloading) in order to save the previous data.

        If `key_on_values` is set true then the string values of parsed tokens
        are also used as part of the key for saving and looking up AST data and
        evaluation functions."""

        self.parser_instance = parser_instance
        self.construct_label = construct_label

        self.trigger_head_or_tail = trigger_head_or_tail
        self.trigger_token_label = trigger_token_label

        self.handler_fun = handler_fun
        self.precond_fun = precond_fun
        self.precond_priority = precond_priority

        if original_sig is None:
            self.original_sigs = []
        else:
            self.original_sigs = original_sig

        # Dicts keyed on original sig (and looked up with one that matches actual sig.)
        if ast_data_dict is None:
            ast_data_dict = {}
        self.ast_data_dict = ast_data_dict
        if eval_fun_dict is None:
            eval_fun_dict = {}
        self.eval_fun_dict = eval_fun_dict
        self.key_on_values = key_on_values

    @staticmethod
    def run(construct, tok, lex, processed_left=None, lookbehind=None):
        """Run the handler associated with the construct.  Check the returned parse
        subtree with `process_and_check_node`.  Return the subtree if it passes type
        checking.

        This is a static method because the arguments will be bound using
        `functools.partial` before it is dispatched to be called."""

        handler_fun = construct.handler_fun

        if construct.trigger_head_or_tail == HEAD:
            subtree = handler_fun(tok, lex)
        else:
            subtree = handler_fun(tok, lex, processed_left)

        #
        # Process any in_tree=False declarations (remove item from the final tree).
        #

        subtree.process_not_in_tree() # Could have option to skip this...

        #
        # Do type checking on the expression tree before returning it.
        #

        if construct.parser_instance.skip_type_checking:
            return subtree

        if hasattr(subtree, "process_and_check_kwargs"):
            subtree.process_and_check_node(construct, **subtree.process_and_check_kwargs)
        else:
            subtree.process_and_check_node(construct)

        return subtree

    def save_eval_fun(self, eval_fun, type_sig=TypeSig(None), value_key=None):
        """Save data in the `eval_fun_dict` and `ast_data_dict`, keyed by the
        `TypeSig` instance `typesig` and also by the `arg_types` of that
        typesig.  If `key_on_values` is true for the construct then the
        value `value_key` is also used in the hashing."""
        if not self.key_on_values:
            value_key = None

        # Save in dicts hashed with full signature (full overload with return).
        dict_key = (type_sig, value_key)
        self.eval_fun_dict[dict_key] = eval_fun
        # Also save in dicts hashed only on args (overloading only on args).
        dict_key = (type_sig.arg_types, value_key)
        self.eval_fun_dict[dict_key] = eval_fun
        # Also save in dicts hashed only on precond label (no overloading).
        dict_key = value_key
        self.eval_fun_dict[dict_key] = eval_fun

    def save_ast_data(self, ast_data, type_sig=TypeSig(None), value_key=None):
        """Save data in the `eval_fun_dict` and `ast_data_dict`, keyed by the
        `TypeSig` instance `typesig` and also by the `arg_types` of that
        typesig.  If `key_on_values` is true for the construct then the
        value `value_key` is also used in the hashing."""
        if not self.key_on_values:
            value_key = None

        # Save in dicts hashed with full signature (full overload with return).
        dict_key = (type_sig, value_key)
        self.ast_data_dict[dict_key] = ast_data
        # Also save in dicts hashed only on args (overloading only on args).
        dict_key = (type_sig.arg_types, value_key)
        self.ast_data_dict[dict_key] = ast_data
        # Also save in dicts hashed only on precond label (no overloading).
        dict_key = value_key
        self.ast_data_dict[dict_key] = ast_data

    def get_eval_fun(self, orig_sig, value_key=None):
        """Return the evaluation function saved by `_save_eval_fun`.
        Must be called after parsing because the `construct_label` attribute must
        be set on the token instance.  Keyed on `is_head`, `construct_label`, and
        original signature."""
        if not self.key_on_values:
            value_key = None
        if self.parser_instance.overload_on_ret_types:
            dict_key = (orig_sig, value_key)
        elif self.parser_instance.overload_on_arg_types:
            dict_key = (orig_sig.arg_types, value_key)
        else:
            dict_key = value_key
        return self.eval_fun_dict.get(dict_key, None)

    def get_ast_data(self, orig_sig, value_key=None):
        """Return the ast data saved by `_save_ast_data`.
        Must be called after parsing because the `construct_label` attribute must
        be set on the token instance.  Keyed on `is_head`, `construct_label`, and
        original signature."""
        if not self.key_on_values:
            value_key = None
        if self.parser_instance.overload_on_ret_types:
            dict_key = (orig_sig, value_key)
        elif self.parser_instance.overload_on_arg_types:
            dict_key = (orig_sig.arg_types, value_key)
        else:
            dict_key = value_key
        return self.ast_data_dict.get(dict_key, None)


class ConstructTable(object):
    """A dict holding `Construct` objects, with related methods.  Each
    `PrattParser` instance has a `ConstructTable` instance to hold its constructs."""
    def __init__(self, parser_instance=None):
        self.parser_instance = parser_instance
        self.construct_dict = {}
        self.construct_dict[HEAD] = {}
        self.construct_dict[TAIL] = {}

    def register_construct(self, head_or_tail, trigger_token_label, construct_label,
                           handler_fun, precond_fun, precond_priority=0,
                           type_sig=TypeSig(None, None),
                           eval_fun=None, ast_data=None, value_key=None,
                           parser_instance=None): # TODO may not need parser instance arg later...
        """Register a construct (either head or tail) with the subclass for
        this kind of token, setting the given properties.  This method is only
        ever called from the `def_construct` method of a `PrattParser`
        instance.

        The `head_or_tail` argument must be `HEAD` or `TAIL`.

        The `type_sig` argument must be a valid `TypeSig` instance.  Every
        unique type signature is saved, and non-unique ones have their
        previously-associated data overwritten.

        If `value_key` is set then it will be used as part of the key on evaluation
        functions and AST data.  The `key_on_values` attribute of the `Construct`
        instance is set based on this being set, and must always be the same for a
        given construct."""
        self.parser_instance = parser_instance
        key_on_values = False
        if value_key:
            key_on_values = True

        # Todo: Consider this possible optimization in looking up handler
        # functions, instead of always linear search.  Some very commonly
        # used preconditions can be hashed on and will often give a unique
        # result.  Instead of using just `head_or_tail` to hash the handler
        # funs and then linear search, what about a tuple of several common
        # conditions:
        #
        #    (head_or_tail, peek_token_label, pstate_stack_top)
        # Then you need to check for those values and the None values,
        # so three hashes:
        #    (head_or_tail, None, pstate_stack_top)
        #    (head_or_tail, peek_token_label, None)
        #    (head_or_tail, None, None)
        #
        # Then in `lookup_construct` go through all three of these sorted
        # sublists in parallel, from the beginning, taking by top priority
        # and running the precond funs.  Almost like a real `case`
        # statement with jumps.  To get the optimized version you would
        # need to specially register that the token uses the precondition,
        # and give a value for it to look for (i.e., to be hashed under).
        # Probably should pass precond assertions list to the
        # `modify_token` routine, maybe something like:
        #    precond_optimize_assert=(None,None,"wff")
        # For those which register the optimization you still need a precond
        # function itself, at least for the unique name, but you can leave off
        # testing the asserted conditions because they won't be considered if
        # the hash doesn't match.
        #
        # Note that the fallback behavior is the same as before (very
        # slight performance penalty, a few hashes, especially with careful coding).
        # On the other hand, how long will the list of handlers ever get?  Is
        # it worth it?  Probably for doing recursive descent it is.
        #
        # Be sure to also update the unregister method if implemented.

        # Set up the construct_dict structure if necessary.
        token_construct_dict = self.construct_dict[head_or_tail]
        if trigger_token_label not in token_construct_dict:
            token_construct_dict[trigger_token_label] = OrderedDict()
        sorted_construct_dict = token_construct_dict[trigger_token_label]

        # Get and save any previous type sig info, ast data, and evaluation
        # functions (for overloaded sigs corresponding to a single
        # construct_label).
        prev_construct = sorted_construct_dict.get(construct_label, None)
        if prev_construct is None:
            prev_sigs = []
            prev_ast_data = {}
            prev_eval_funs = {}
        else:
            if key_on_values != prev_construct.key_on_values:
                raise ParserException("If any overload of a `Construct` instance "
                        "supplies a `value_key` then all its overloads must also.")
            prev_sigs = prev_construct.original_sigs
            prev_ast_data = prev_construct.ast_data_dict
            prev_eval_funs = prev_construct.eval_fun_dict

        if prev_construct and not self.parser_instance.overload_on_arg_types:
            raise TypeErrorInParsedLanguage("Value of overload_on_arg_types"
                   " is False but attempt to redefine and possibly set multiple"
                   " signatures for the {0} function for token with label '{1}'"
                   " with preconditions label '{2}'."
                   .format(head_or_tail, self.token_label, construct_label))

        # For overloading, append the type_sig to prev_type_sigs_for_precond,
        # saving them all.  A static method of TypeSig currently does it
        # since it depends on the definition of TypeSig equality.
        TypeSig.append_sig_to_list_replacing_if_identical(prev_sigs, type_sig)

        # Set up the new construct.
        new_construct = Construct(self.parser_instance,
                                  construct_label=construct_label,
                                  trigger_head_or_tail=head_or_tail,
                                  trigger_token_label=trigger_token_label,
                                  handler_fun=handler_fun,
                                  precond_fun=precond_fun,
                                  precond_priority=precond_priority,
                                  original_sig=prev_sigs,
                                  ast_data_dict=prev_ast_data,
                                  eval_fun_dict=prev_eval_funs,
                                  key_on_values=key_on_values)
        sorted_construct_dict[construct_label] = new_construct

        # Re-sort the OrderedDict, since we added an item.
        resorted_handler_dict = sort_handler_dict(sorted_construct_dict)
        self.construct_dict[head_or_tail][trigger_token_label] = resorted_handler_dict

        # Save the eval_fun and ast_data.  Note the construct's `key_on_values`
        # setting will be used.  Note this could be saved directly to new_construct
        # above instead of going through the ConstructTable method.
        self.save_eval_fun(head_or_tail, trigger_token_label, construct_label,
                                                type_sig, eval_fun, value_key)
        self.save_ast_data(head_or_tail, trigger_token_label, construct_label,
                                                type_sig, ast_data, value_key)

        # Make sure we don't get multiple definitions with the same
        # priority when the new one is inserted.
        #
        # TODO: Note that WE MAY NOT WANT TO DO THIS AT ALL, or just give a
        # warning.  See case of preconds for quantifiers in logic example
        # which use MUTUALLY EXCLUSIVE handlers which have the same priority.
        # Code below works but isn't being run... consider what it should
        # do and delete if no use.
        pre_check_for_same_priority = False # CODE WORKS BUT SHOULD IT EVER RUN?
        if pre_check_for_same_priority:
            for p_label, data_item in resorted_handler_dict.items():
                if p_label == construct_label:
                    continue
                if (data_item.precond_priority == precond_priority and
                            self.parser_instance.raise_exception_on_precondition_ties):
                    raise ParserException("Two preconditions for the token"
                            " subclass named '{0}' for token with label '{1}' have"
                            " the same priority, {2}.  Their precondition labels"
                            " are '{3}' and '{4}'.  If precondition labels are"
                            " the same there may be a redefinition. Set the flag"
                            " False if you actually want to allow precondition"
                            " ties." .format(self.__name__, self.token_label,
                                precond_priority, construct_label, p_label))
        return new_construct

    def unregister_construct(self, head_or_tail, trigger_token_label,
                             construct_label=None, type_sig=None, all_handlers=False):
        """Unregister the previously-registered construct (head or
        tail).  If `construct_label` is not set then all head or tail
        handlers (as selected by `head_or_tail`) are unregistered.  If
        `type_sig` is not present then all overloads are also unregistered.
        No error is raised if a matching handler function is not found."""
        # TODO Untested method,  NEEDS REWRITING since constructs used now and dict
        # structure added trigger_token_label...

        if construct_label is None:
            if head_or_tail in self.construct_dict:
                self.construct_dict[head_or_tail] = OrderedDict()
                self.handler_sigs[head_or_tail] = {}
            return

        # Tuple format for sorted_handler_list is:
        #     (precond_fun, precond_priority, handler_fun)
        sorted_handler_dict = self.construct_dict[head_or_tail]
        if not sorted_handler_dict:
            return

        if not construct_label in sorted_handler_dict:
            return

        if type_sig is None:
            del sorted_handler_dict[construct_label]
            return

        sig_list = sorted_handler_dict[construct_label]

        for i in reversed(range(len(sorted_handler_dict))):
            item = sorted_handler_dict[i]
            handler_fun = item.handler_fun
            new_handler_sigs = [s for s in handler_fun.type_sigs if s != type_sig]
            if not new_handler_sigs:
                del sorted_handler_dict[i] # No type sigs at all, remove item.
                continue
            handler_fun.type_sigs = new_handler_sigs

    def lookup_construct(self, head_or_tail, trigger_token_instance,
                         lex=None, lookbehind=None, construct_label=None):
        """Look up and return the construct for the given subexpression
        position in `head_or_tail`, based on the current state.

        Either the `lex` parameter or the `construct_label` parameter must be
        set.  If `lex` is set it will be passed to the precondition
        functions as an argument, and similarly for `lookbehind`.

        This method evaluates each preconditions function in the sorted
        dict for this kind of token and the specified kind of construct
        (head or tail), returning the construct associated with the first
        one which evaluates to `True`.  Raises `NoHandlerFunctionDefined`
        if no handler function can be found.

        If the parameter `construct_label` is set then this method returns
        the construct which *would be* returned, assuming that that were
        the label of the "winning" construct.  Not currently used.

        This function also sets the attribute `construct_label` of this token
        instance to the label of the winning precondition function."""
        trigger_token_label = trigger_token_instance.token_label

        token_construct_dict = self.construct_dict[head_or_tail]
        if not trigger_token_label in token_construct_dict:
            raise NoHandlerFunctionDefined(
                    "No {0} handler functions at all are defined"
                    " for tokens with token label '{1}'.  The token's"
                    " value is '{2}'."
                    .format(head_or_tail, trigger_token_label,
                            trigger_token_instance.value))
        sorted_construct_dict = token_construct_dict[trigger_token_label]

        if construct_label: # This condition is not currently used in the code.
            for pre_fun_label, construct in sorted_construct_dict.items():
                if pre_fun_label == construct_label:
                    return construct.precond_fun

        # Sequentially run sorted precondition functions until one is true.
        for pre_label, construct in sorted_construct_dict.items():
            if construct.precond_fun(lex, lookbehind):
                # Note construct_label is saved as a user-accesible attribute here.
                trigger_token_instance.construct_label = construct.construct_label
                return construct

        raise NoHandlerFunctionDefined("No {0} handler function matched the token "
                "with value '{1}' and label '{2}' in the current preconditions."
                .format(head_or_tail, trigger_token_instance.value,
                        trigger_token_label))

    def dispatch_handler(self, head_or_tail, trigger_token_instance,
                         lex, left=None, lookbehind=None):
        """Look up and return the handler function for the token, with its arguments
        bound."""
        if head_or_tail == HEAD:
            construct = self.lookup_construct(HEAD, trigger_token_instance, lex)
            handler = functools.partial(construct.run, construct, trigger_token_instance, lex)
        elif head_or_tail == TAIL:
            construct = self.lookup_construct(TAIL, trigger_token_instance, lex, lookbehind=lookbehind)
            handler = functools.partial(construct.run, construct, trigger_token_instance, lex, left)
        else:
            raise ParserException("Bad first argument to dispatch_handler"
                    " function: must be HEAD or TAIL or the equivalent.")
        return handler

    def save_eval_fun(self, head_or_tail, trigger_token_label, construct_label,
                                                  type_sig, eval_fun, value_key=None):
        """This is a utility function that saves data in the `eval_fun_dict`
        and `ast_data_dict` associated with token `token_subclass`, keyed by
        the `TypeSig` instance `typesig` and also by the `arg_types` of that
        typesig.  This is used so overloaded instances can have different
        evaluations and AST data."""
        # TODO: better error-checking,
        construct = self.construct_dict[head_or_tail][trigger_token_label][construct_label]
        construct.save_eval_fun(eval_fun, type_sig, value_key)

    def save_ast_data(self, head_or_tail, trigger_token_label, construct_label,
                                                  type_sig, ast_data, value_key=None):
        """This is a utility function that saves data in the `eval_fun_dict`
        and `ast_data_dict` associated with token `token_subclass`, keyed by
        the `TypeSig` instance `typesig` and also by the `arg_types` of that
        typesig.  This is used so overloaded instances can have different
        evaluations and AST data."""
        # TODO: better error-checking,
        construct = self.construct_dict[head_or_tail][trigger_token_label][construct_label]
        construct.save_ast_data(ast_data, type_sig, value_key)

    def get_eval_fun(self, orig_sig, trigger_token_instance):
        """Return the evaluation function saved by `_save_eval_fun`.
        Must be called after parsing because the `construct_label` and `is_head`
        attributes must be set on the token instance."""
        construct_label = trigger_token_instance.construct_label # Attribute set during parsing.
        if trigger_token_instance.is_head:
            construct = self.construct_dict[HEAD][trigger_token_instance.token_label][construct_label]
        else:
            construct = self.construct_dict[TAIL][trigger_token_instance.token_label][construct_label]
        return construct.get_eval_fun(orig_sig, trigger_token_instance.value)

    def get_ast_data(self, orig_sig, trigger_token_instance):
        """Return the ast data saved by `_save_ast_data_and_ast_data`.
        Must be called after parsing because the `construct_label` and `is_head`
        attributes must be set on the token instance."""
        construct_label = trigger_token_instance.construct_label # Attribute set during parsing.
        if trigger_token_instance.is_head:
            construct = self.construct_dict[HEAD][trigger_token_instance.token_label][construct_label]
        else:
            construct = self.construct_dict[TAIL][trigger_token_instance.token_label][construct_label]
        return construct.get_ast_data(orig_sig, trigger_token_instance.value)

#
# Utility functions.
#

def sort_handler_dict(d):
    """Return the sorted `OrderedDict` version of the dict `d` passed in,
    sorted by the precondition priority in the items.  Used in
    `PrattParser.register_handler_fun` to keep handlers sorted by priority."""
    # https://docs.python.org/3/library/collections.html#ordereddict-examples-and-recipes
    return OrderedDict(sorted(
           d.items(), key=lambda item: item[1].precond_priority, reverse=True))


