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

# TODO: Consider allowing empty constructs to be defined from init.  A minor
# change, though, which can wait (if it is ever implemented).
#
# TODO: The final level of ConstructTable trees does not need to be an
# OrderedDict anymore.  They can just be ordinary lists.  Construct labels are
# now purely informational attributes of constructs.

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
from collections import OrderedDict, defaultdict
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

    A list of typesigs must be stored because overloaded functions use the same
    handler function (it is unknown which signature will apply until after the
    arguments/subtree is parsed to resolve the overload).  Some additional
    information (eval functions and ast labels) is stored in dicts keyed by
    typesigs.

    This is a low-level container class which blindly creates, saves, and
    retrieves data.  Higher-level constraints on constructs (such as preventing
    redefinition with a new `head_or_tail` or `trigger_token_label`) are handled
    in the `ConstructTable` class or the `def_construct` method of `PrattParser`.
    """
    __slots__ = ["parser_instance", "construct_label", "trigger_head_or_tail",
                 "trigger_token_label", "handler_fun", "precond_fun",
                 "precond_priority", "original_sigs", "ast_data_dict",
                 "eval_fun_dict", "key_on_token_values", "is_empty"]
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
                       key_on_token_values=False):

        """Initialize a `Construct` instance associated with the parser
        `parser_instance`.  Users should usually use the `def_construct`
        method of a `PrattParser` instance instead of instantiating a
        `Construct` directly.  That routine calls the `register_construct`
        method of a `ConstructTable` instance, which in turn instantiates a
        `Construct`.

        The string `construct_label` is a string label that is used to label a
        construct.  It is optional and can provide extra debugging information
        and string-label access to constructs.  When the
        `overload_on_matching_construct_def` option is set for the parser it is
        also used as a label for preconditions functions to determine, equality
        of preconditions functions.  Note that the `precond_fun` is part of the
        definition of equality of constructs (along with `trigger_head_or_tail`
        and `trigger_token_label`).  With that option set equality of
        constructs determines when a new construct is created versus when
        overwriting or overloading occurs.

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

        If `key_on_token_values` is set true then the string values of parsed tokens
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
            self.original_sigs = [TypeSig(None)]
        else:
            self.original_sigs = [original_sig]

        self.ast_data_dict = defaultdict(dict)
        self.eval_fun_dict = defaultdict(dict)
        self.key_on_token_values = key_on_token_values
        self.is_empty = False

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

    def _get_dict_keys(self, type_sig, token_value_key):
        """Return the dict key tuple based on the overload settings of the parser
        instance."""
        # TODO: Should None be a TypeSig(None) or a real None here?
        if type_sig is None:
            type_sig = TypeSig(None)
        # Ignore if keying turned of for construct. Global setting may not be needed...
        if not self.key_on_token_values:
            token_value_key = None

        if self.parser_instance.overload_on_ret_types:
            dict_keys = (type_sig, token_value_key)
        elif self.parser_instance.overload_on_arg_types:
            dict_keys = (type_sig.arg_types, token_value_key)
        else:
            dict_keys = (None, token_value_key)
        return dict_keys

    def save_eval_fun(self, eval_fun, type_sig=None, token_value_key=None):
        """Save data in the `eval_fun_dict` and `ast_data_dict`, keyed by the
        `TypeSig` instance `typesig` and also by the `arg_types` of that
        typesig.  If `key_on_token_values` is true for the construct then the
        value `token_value_key` is also used in the hashing."""
        dict_keys = self._get_dict_keys(type_sig, token_value_key)
        self.eval_fun_dict[dict_keys[0]][dict_keys[1]] = eval_fun

    def save_ast_data(self, ast_data, type_sig=None, token_value_key=None):
        """Save data in the `eval_fun_dict` and `ast_data_dict`, keyed by the
        `TypeSig` instance `typesig` and also by the `arg_types` of that
        typesig.  If `key_on_token_values` is true for the construct then the
        value `token_value_key` is also used in the hashing."""
        dict_keys = self._get_dict_keys(type_sig, token_value_key)
        self.ast_data_dict[dict_keys[0]][dict_keys[1]] = ast_data

    def get_eval_fun(self, orig_sig, token_value_key=None):
        """Return an evaluation function saved by `save_eval_fun`.  The
        `orig_sig` argument is the original signature it was saved under, not
        the expanded signature."""
        dict_keys = self._get_dict_keys(orig_sig, token_value_key)
        return self.eval_fun_dict.get(dict_keys[0], {}).get(dict_keys[1], None)

    def get_ast_data(self, orig_sig, token_value_key=None):
        """Return the ast data saved by `save_ast_data`.  The `orig_sig`
        argument is the original signature it was saved under, not the expanded
        signature."""
        dict_keys = self._get_dict_keys(orig_sig, token_value_key)
        return self.eval_fun_dict.get(dict_keys[0], {}).get(dict_keys[1], None)

    def overload(self, val_type=None, arg_types=None,
                 eval_fun=None, ast_data=None, token_value_key=None,
                 num_args=None):
        """Overload the construct to allow a different type signature with
        a different evaluation function and AST data element.

        To overload only on the number of argments when any types are allowed
        set `num_args` to the number.  Note that in that case `val_type` and
        `arg_types` are ignored."""
        if self.parser_instance.skip_type_checking:
            raise ParserException("Overloading on types requires that type checking be "
                          " enabled.  The parser instance has skip_type_checking=True.")
        if self.key_on_token_values and not token_value_key:
            # NOTE: Keying on values could be per type sig, vs. global for construct.
            raise ParserException("If `key_on_token_values` is set for a `Construct`"
                    " instance then all its overloads must supply a `token_value_key`"
                    " argument.")
        if not self.parser_instance.overload_on_arg_types:
            raise TypeErrorInParsedLanguage("Value of overload_on_arg_types"
                   " is False but attempt to redefine and possibly set multiple"
                   " signatures for the {0} function triggered by tokens with the"
                   " label '{1}' with preconditions label '{2}'."
                   .format(head_or_tail, trigger_token_label, construct_label))

        if num_args is not None:
            type_sig = TypeSig(None, [None] * num_args)
        else:
            type_sig = TypeSig(val_type, arg_types)
        TypeSig.append_sig_to_list_replacing_if_identical(self.original_sigs,
                                                          type_sig)
        self.save_eval_fun(eval_fun, type_sig, token_value_key)
        self.save_ast_data(ast_data, type_sig, token_value_key)
        return self

    def unregister_overload(self, type_sig, token_value_key=None):
        """Unregister an overload.  If the last overload is deleted then the
        ``original_sigs`` attribute will be left empty.  The construct will
        still be in the any ``ConstructTable`` that it was part of, though.
        Called from the `unregister_construct` of `ConstructTable`.

        If `token_value_key` is set then only that token value is removed, unless
        it is the last one in which case the whole signature is removed."""
        # TODO: Untested method.
        dict_keys = self._get_dict_keys(type_sig, token_value_key)

        # Remove the data associated with the type sig.
        for eval_or_data_dict in [self.eval_fun_dict, self.ast_data_dict]:
            if dict_keys[0] in eval_or_data_dict:
                if token_value_key is None:
                    del eval_or_data_dict[dict_keys[0]]
                else:
                    values_dict = eval_or_data_dict[dict_keys[0]]
                    if dict_key[1] in values_dict:
                        del values_dict[token_value_key]
                    if not eval_or_data_dict.get(dict_keys[0]):
                        del eval_or_data_dict[dict_keys[0]]

        # If all associated data is now gone, remove the type sig itself.
        if (dict_keys[0] not in self.eval_fun_dict
                and dict_keys[0] not in self.ast_data_dict):
            self.original_sigs = [s for s in self.original_sigs if s != type_sig]

        if not self.original_sigs:
            self.is_empty = True

class ConstructTable(object):
    """A dict holding `Construct` objects, with related methods.  Each
    `PrattParser` instance has a `ConstructTable` instance to hold its
    constructs."""
    def __init__(self, parser_instance):
        """Initialize a `ConstructTable` associated with the `PrattParser`
        instance `parser_instance`."""
        self.parser_instance = parser_instance
        # Dict to look up dicts of constructs by [head_or_tail][trigger_token_label]
        # with the final dicts keyed by [construct_label].
        self.construct_lookup_dict = {}
        self.construct_lookup_dict[HEAD] = {}
        self.construct_lookup_dict[TAIL] = {}

    def register_construct(self, head_or_tail, trigger_token_label, handler_fun,
                           precond_fun, precond_priority, construct_label,
                           type_sig, eval_fun, ast_data, token_value_key):
        """Register a construct (either head or tail) with the subclass for
        this kind of token, setting the given properties.  This method is only
        ever called from the `def_construct` method of a `PrattParser`
        instance.

        The `head_or_tail` argument must be `HEAD` or `TAIL`.

        The `type_sig` argument must be a valid `TypeSig` instance.  Every
        unique type signature is saved, and non-unique ones have their
        previously-associated data overwritten.

        If `token_value_key` is set then it will be used as part of the key on
        evaluation functions and AST data.  The `key_on_token_values` attribute of
        the `Construct` instance is set based on this being set, and must
        always be the same for a given construct."""

        key_on_token_values = False
        if token_value_key:
            key_on_token_values = True

        # Todo maybe later, if profiling shows it might be worth it: Consider
        # possible optimizations in looking up handler functions, instead of
        # always linear search after splitting on `head_or_tail` and
        # `trigger_token_label` values.  Some other commonly used precondition
        # could potentially also be used to reduce the possibilities.  But, by
        # definition, the `head_or_tail` and `trigger_token_label` values are
        # mutually exclusive in selecting constructs.  For others you also have
        # the possibility of a "don't care" value, which increases the
        # complexity.
        #
        # We could increase the tree depth and also hash on, say,
        # `token_value`, `peek_token_label`, `pstate_stack_top`, etc.  That
        # complicates the tree, though, and when a property does not matter
        # then you need to go down all the branches at that split and yet end
        # with a list that is sorted.
        #
        # A simpler optimization might be to just do the linear search but skip
        # calling any preconditions function for a construct which does not
        # match a declared requirement (among some predefined set).  Suppose a
        # decorator is used on preconditions functions (which returns a wrapper
        # that contains the extra info):
        #
        # @precond_fun(peek_token_label="k_lpar")
        # def my_fun(...):
        #    return True
        #
        # Alternately, a kwarg to the construct initializer could be passed
        # along with the precond fun:
        #
        #    precond_optimize_assert=[(peek_token_label,"k_wff")]
        #
        # This could set an attribute of the construct when it is initialized.
        # The attribute would give the required value or set of values.  Then
        # in the loop you can look at that attribute of the constructs and skip
        # with `continue` if the test fails.  But is looking up and comparing
        # several attributes per construct to avoid calling some precond
        # functions sufficiently faster than just linearly running the preconds
        # funs to justify the complexity?  Probably not worth it.
        #
        # Alternative, with the same setup as now for mutually-exclusive
        # properties but with this kind of thing done for each priority-sorted
        # sublist above some threshold size (properties are things like "peek token
        # label value" or "string value on top of pstate stack").
        # 1) A list of dicts, one for each extra property to test on.
        # 2) Each dict for a property has items keyed by the property values
        #    (which are inserted into it as it is built up).  Each dict items contains
        #    as its value a two-tuple containing 0) a priority-sorted lists of
        #    constructs in the subset with that property value as well as 2) a set
        #    of the constructs with that value for the property.
        # 3) To look up a construct you get the ordereddict for each property,
        #    looking up keyed by the property's value in the current precond context.
        # 4) Take the intersection of these ordereddicts.  Algorithm: start with one
        #    (smallest size is best) and sequentially (by ordering priority) compare
        #    for membership in all the others, using the sets that are also saved.
        #    Take first one that is contained in all of them.
        # Premature optimization for now.  What sizes of sets involved would make
        # it worth the overhead is another question.

        # Set up the construct_lookup_dict structure if necessary.
        head_or_tail_construct_dict = self.construct_lookup_dict[head_or_tail]
        if trigger_token_label not in head_or_tail_construct_dict:
            head_or_tail_construct_dict[trigger_token_label] = OrderedDict()
        sorted_construct_dict = head_or_tail_construct_dict[trigger_token_label]

        construct = Construct(self.parser_instance,
                              construct_label=construct_label,
                              trigger_head_or_tail=head_or_tail,
                              trigger_token_label=trigger_token_label,
                              handler_fun=handler_fun,
                              precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              original_sig=type_sig,
                              key_on_token_values=key_on_token_values)
        sorted_construct_dict[construct_label] = construct

        # Re-sort the OrderedDict, since we added an item.  (Could be a little
        # more efficient as a binary tree insertion, putting it in where it belongs,
        # but the builtin Python sort is in C so it might still win.)
        sorted_construct_dict = sort_construct_dict(sorted_construct_dict)
        self.construct_lookup_dict[
                       head_or_tail][trigger_token_label] = sorted_construct_dict

        # Save the eval_fun and ast_data.  Note the construct's `key_on_token_values`
        # setting will be used.  Note this could be saved directly to new_construct
        # above instead of going through the ConstructTable method.
        construct.save_eval_fun(eval_fun, type_sig, token_value_key)
        construct.save_ast_data(ast_data, type_sig, token_value_key)

        # Make sure we don't get multiple definitions with the same priority if
        # that checking is enabled.
        if self.parser_instance.raise_on_equal_priority_preconds:
            for p_label, data_item in sorted_construct_dict.items():
                if p_label == construct_label:
                    continue
                if data_item.precond_priority == precond_priority:
                    trigger_token = self.parser_instance.get_token(trigger_token_label)
                    raise ParserException("Two preconditions for the token"
                            " subclass named '{0}' for token with label '{1}' have"
                            " the same priority, {2}.  Their precondition labels"
                            " are '{3}' and '{4}'.  If precondition labels are"
                            " the same there may be a redefinition. Set the flag"
                            " raise_on_equal_priority_preconds=False if you"
                            " actually want to allow precondition ties."
                            .format(trigger_token.__name__, trigger_token.token_label,
                                             precond_priority, construct_label, p_label))
        return construct

    def unregister_construct(self, head_or_tail, trigger_token_label,
                             construct_label=None, type_sig=None, token_value_key=None):
        """Unregister the previously-registered construct.

        If `construct_label` is not set then all head or tail handlers matching
        `head_or_tail` and `trigger_token_label` are unregistered.

        The `type_sig` argument must be a valid `TypeSig` instance or else
        `None`.  If `type_sig` is `None` then all overloads are unregistered;
        otherwise only the particular signature is unregistered.

        No error is raised if a matching construct function is not found."""
        # TODO Untested method.

        token_label_keyed_dict = self.construct_lookup_dict[head_or_tail]
        if not token_label_keyed_dict:
            return

        if trigger_token_label not in token_label_keyed_dict:
            return
        construct_label_keyed_dict = token_label_keyed_dict[trigger_token_label]

        if construct_label is None:
            token_label_keyed_dict[trigger_token_label] = OrderedDict()
            return

        if not construct_label in token_label_keyed_dict:
            return
        ordered_construct_dict = token_label_keyed_dict.get(construct_label, None)

        if type_sig is None:
            del token_label_keyed_dict[construct_label]
            return

        construct = token_label_keyed_dict[construct_label]
        construct._unregister_overload(type_sig, token_value_key)
        if not construct.original_sigs:
            del token_label_keyed_dict[construct_label]

    def lookup_winning_construct(self, head_or_tail, trigger_token_instance,
                                 lex=None, lookbehind=None, construct_label=None):
        """Look up and return the "winning" construct for the given head or tail
        position, based on the current state.

        Either the `lex` parameter or the `construct_label` parameter must be
        set.  If `lex` is set it will be passed to the precondition
        functions as an argument, and similarly for `lookbehind`.

        This method evaluates each preconditions function in the sorted
        dict for this kind of token and the specified kind of construct
        (head or tail), returning the construct associated with the first
        one which evaluates to `True`.  Raises `NoHandlerFunctionDefined`
        if no handler function can be found.

        If the parameter `construct_label` is set then this method returns the
        construct which *would be* returned, assuming that that were the label
        of the "winning" construct.  Not currently used, and may be deleted at
        some point.

        This function also sets the attribute `construct_label` of this token
        instance to the label of the winning precondition function."""
        trigger_token_label = trigger_token_instance.token_label

        head_or_tail_construct_dict = self.construct_lookup_dict[head_or_tail]
        if not trigger_token_label in head_or_tail_construct_dict:
            raise NoHandlerFunctionDefined(
                    "No {0} handler functions at all are defined"
                    " for tokens with token label '{1}'.  The token's"
                    " value is '{2}'."
                    .format(head_or_tail, trigger_token_label,
                            trigger_token_instance.value))
        sorted_construct_dict = head_or_tail_construct_dict[trigger_token_label]

        if construct_label: # This option is not currently used in the code.
            for pre_fun_label, construct in sorted_construct_dict.items():
                if pre_fun_label == construct_label:
                    return construct.precond_fun

        # Sequentially run sorted precondition functions until one is true.
        for pre_label, construct in sorted_construct_dict.items():
            if construct.is_empty: # Ignore empty constructs.
                continue
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
        """Look up and return a wrapper for the "winning" handler function for the
        token, with its arguments bound."""
        if head_or_tail == HEAD:
            construct = self.lookup_winning_construct(HEAD, trigger_token_instance, lex)
            handler = functools.partial(
                            construct.run, construct, trigger_token_instance, lex)
        elif head_or_tail == TAIL:
            construct = self.lookup_winning_construct(TAIL, trigger_token_instance,
                                              lex, lookbehind=lookbehind)
            handler = functools.partial(
                            construct.run, construct, trigger_token_instance, lex, left)
        else:
            raise ParserException("Bad first argument to dispatch_handler"
                    " function: must be HEAD or TAIL or the equivalent.")
        return handler

    def lookup_construct(self, head_or_tail, trigger_token_label, construct_label):
        """Look up and return the construct in the `ConstructTable` instance."""
        construct = self.construct_lookup_dict[
                               head_or_tail][trigger_token_label][construct_label]
        return construct

    def lookup_construct_for_parsed_token(self, trigger_token_instance):
        """Look up the construct for the parsed token instance `trigger_token_instance`.
        Must be called after parsing because the `construct_label` and `is_head`
        attributes are set on the token instance at parse-time."""
        construct_label = trigger_token_instance.construct_label # Attr set during parsing.
        if trigger_token_instance.is_head: # Attr set during parsing.
            construct = self.construct_lookup_dict[
                             HEAD][trigger_token_instance.token_label][construct_label]
        else:
            construct = self.construct_lookup_dict[
                             TAIL][trigger_token_instance.token_label][construct_label]
        return construct

#
# Utility functions.
#

def sort_construct_dict(d):
    """Return the sorted `OrderedDict` version of the dict `d` passed in,
    sorted by the precondition priority in the items.  Used in
    `PrattParser.register_handler_fun` to keep handlers sorted by priority."""
    # https://docs.python.org/3/library/collections.html#ordereddict-examples-and-recipes
    return OrderedDict(sorted(
           d.items(), key=lambda item: item[1].precond_priority, reverse=True))


