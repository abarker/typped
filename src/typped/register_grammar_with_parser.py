# -*- coding: utf-8 -*-
"""

This module contains the functions which handle the recursive descent parsing
of the rules of a `Grammar` object instance. Grammar objects are defined in
`ebnf_classes_and_operators` module.

In ths module only the function `register_rule_handlers_with_parser` is
formally exposed to be called externally (and it is only called from the
`ebnf_classes_and_operators` module).

The first argument to `register_rule_handlers_with_parser` is a parser
instance.  The function modifies the parser to parse the grammar rule which is
also passed in.

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_ebnf_classes_and_operators.py",
                             ], pytest_args="-v")

from collections import defaultdict
import functools
from .shared_settings_and_exceptions import (HEAD, TAIL,
                                     ParserException, CalledEndTokenHandler)

#from .lexer import (Lexer, TokenNode, TokenTable, LexerException, BufferIndexError,
#                    multi_funcall)
#from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage

#
# Production rule methods.
#

def register_rule_handlers_with_parser(parser, nonterm_label, grammar):
    """Register production rules for all the cases of the nonterminal
    `nonterm_label`, as defined in the `Grammar` object `grammar`.

    This is run to initially process a caselist.  It partitions the caselist
    for the `nonterm_label` nonterminal/pstate into separate caselists for
    cases which start with the same thing and may require backtracking.  It
    does the following:

    1. Look up the caselist for `nonterm_label` in the grammar object.

    2. Loop over that caselist, making a sub-caselists for cases which start
    with the same thing (i.e., which have the same first set).  Start with the
    first case, and proceed until the initial caselist is fully processed and
    converted to a collection of sub-caselists.  (All production rule starts
    are currently considered the same, but when the first-set is computed then
    they will be grouped with others having the same first-set.)

    These sub-caselists which start with a token are stored in a dict
    `token_literal_start_cases`, keyed by the token label of the beginning token.
    Those which start with a nonterminal are saved in a list
    `nonterm_start_cases`.

    3. For each sub-caselist created above, call
    `def_handlers_for_first_case_of_nonterminal` with that caselist.

    """
    # TODO: finish implementing first-sets and make the sub-caselists be groups
    # that start with the same first-set.  Then make that a precond on it.  To
    # start with, ONLY consider the first literal token in a case as far as
    # calculating first sets (and make comment that users should note this).  Later
    # possibly add support for using several sequential token literals as an expanded
    # start pattern.

    if not parser.null_string_token_subclass:
        parser.def_null_string_token() # Define null-string if necessary.

    caselist = grammar[nonterm_label]
    caselist = list(caselist) # Only need ordinary Python lists here.

    # Partition the caselist into sub-caselists.
    token_literal_start_cases, nonterm_start_cases = partition_caselist(caselist)

    # Register null-string handlers for each unique first item of some case.
    # All rules currently treated as non-unique.
    null_token = parser.null_string_token_subclass

    # Use null-string with a peek in front of actual tokens, also, to avoid
    # special-curr_case handling in later code.
    for token_label, caselist in token_literal_start_cases.items():
        # Register handler with a precondition on the token label.
        # --> Here and elsewhere, assuming the token_label and pstate uniquely
        # identify in both cases, but may need more preconds (later). Todo
        if caselist:
            def_handlers_for_first_case_of_nonterminal(parser, nonterm_label,
                                                   null_token, caselist, token_label)

    caselist = nonterm_start_cases # All rules currently handled the same.
    # Register handler with the null-string token preconditioned on
    # nonterm_label being on the top of the pstate_stack.
    if caselist:
        def_handlers_for_first_case_of_nonterminal(parser, nonterm_label,
                                                   null_token, caselist)

def def_handlers_for_first_case_of_nonterminal(parser, nonterm_label, null_token,
                                               caselist, peek_token_label=None):
                   # Below params not used yet.... need to know cases of production...
                   #val_type=None, arg_types=None, eval_fun=None,
                   #ast_label=None):

    """Define the head and tail handlers for the null-string token that is
    called to parse a production rule (i.e., called when precondition that
    the state label `nonterm_label` is on the top of the `pstate_stack` is
    satisfied).

    These handlers handle all the production rule cases of the caselist for
    the nonterminal, backtracking on failure and trying the next case, etc.
    They act very much like the usual recursive descent function for
    parsing a production rule, except that to make a recursive call to
    handle a sub-production they push that production label onto the
    `pstate_stack` and then call `recursive_parse`.  Then the handler for
    that production will be called, doing a search over its cases, etc.,
    returning the value to the calling level.

    The label of the starting nonterminal is assumed to have initially been
    pushed on the `pstate_stack` attribute of the parser whenever
    productions are being used."""
    # Todo: later consider limiting the depth of the recursions by not
    # allowing a null-string token handler to be called recursively
    # unless something has been consumed from the lexer (curtailment,
    # but no memoization, see e.g. Frost et. al 2007).

    def preconditions(lex, lookbehind, peek_token_label=peek_token_label):
        """This function is only registered and used if `peek_token_label`
        is not `None`."""
        pstate_stack = lex.token_table.parser_instance.pstate_stack
        if pstate_stack[-1] != nonterm_label:
            return False
        if peek_token_label and lex.peek().token_label != peek_token_label:
            return False
        return True

    head_handler = generic_head_handler_function_factory(nonterm_label, caselist)

    tail_handler = generic_tail_handler_function_factory(nonterm_label, caselist)

    # Register the handler for the first item of the first case.
    construct_label = "construct_for_nonterminal_symbol_{0}".format(nonterm_label)
    precond_priority = 10000
    parser.def_construct(HEAD, head_handler, null_token.token_label, prec=0,
                 construct_label=construct_label,
                 precond_fun=preconditions, precond_priority=precond_priority)
                 #val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                 #ast_label=ast_label)

    prec = 10 # TODO: Temporary hardcode, will need to vary.
    parser.def_construct(TAIL, tail_handler, null_token.token_label, prec=prec,
                 construct_label=construct_label,
                 precond_fun=preconditions, precond_priority=precond_priority)

def recursive_parse_nonterm_handler(tok, subexp_prec): #, itemlist, index):
    """The equivalent of `recursive_parse` which is called from handlers for
    nonterminals (triggered by null-string tokens)."""
    parser_instance = tok.parser_instance
    lex = tok.token_table.lex
    curr_token, head_handler = tok.get_null_string_token_and_handler(
                                                     HEAD, lex, subexp_prec)
    if not curr_token:
        curr_token = lex.next()
        head_handler = curr_token.dispatch_handler(HEAD, lex)
    curr_token.is_head = True # To look up eval_fun and ast_data later.

    processed_left = head_handler()
    lookbehind = [processed_left]

    while lex.peek().prec() > subexp_prec:
        ns_token, tail_handler = tok.get_null_string_token_and_handler(
                        TAIL, lex, subexp_prec, processed_left, lookbehind)
        if not ns_token:
            curr_token = lex.next()
            tail_handler = curr_token.dispatch_handler(
                                 TAIL, lex, processed_left, lookbehind)

        processed_left = tail_handler()
        lookbehind.append(processed_left)

    return processed_left

def generic_head_handler_function_factory(nonterm_label, caselist):
    """Return a generic head handler with `nonterm_label` and `caselist` bound
    in the closure."""
    # Note partial replaces positional arguments from the left.
    return functools.partial(generic_handler, nonterm_label, caselist)

def generic_handler(nonterm_label, caselist, tok, lex):
    """The head handler assigned to the first token of the first case for
    a caselist.  It tries all the other cases if it fails."""
    # The "called as head vs. tail" thing won't matter, because
    # productions are always called as the whole case, by the
    # production label.  So, all cases will have the *same* method of
    # calling....  If first case was called as head then call all later
    # cases as heads; same with tails.
    pstate_stack = tok.parser_instance.pstate_stack
    first_call_of_start_state = False

    # TODO Save the label of the nonterminal that parsed the token.
    # Document that value of null-string token (was None) is set to
    # nonterm_label.  Pass value up in modifications where full tree
    # not shown.
    tok.value = nonterm_label

    #def indent(): # DEBUG fun
    #    return " " * ((len(pstate_stack)-1) * 3)

    num_cases = len(caselist)
    last_case = False

    lex_token_count = lex.all_token_count
    lex_saved_begin_state = lex.get_current_state() # Return here on failure.
    lex_peek_label = lex.peek().token_label # ONLY saved for backtrack check

    # Loop through the cases until one succeeds or all fail.
    for case_count, case in enumerate(caselist):
        assert lex.peek().token_label == lex_peek_label
        tok.children = [] # Reset the children of this null-state token.
        if case_count == num_cases - 1:
            last_case = True

        # Loop through the items testing for match; backtrack on exception.
        try:
            for item in case:
                # Item is a Token.
                if item.kind_of_item == "token":
                    item_token = item.value
                    item_token_label = item_token.token_label
                    if not lex.match_next(item_token_label, consume=False):
                        raise BranchFail("Expected '{0}' token not found.".
                                         format(item_token_label))

                    # Actual tree nodes must be processed by literal handler.
                    # Todo: document/improve this stuff, nice to push known label
                    pstate_stack.append(None) # Avoid recursion to this handler.
                    next_tok = recursive_parse_nonterm_handler(tok, 0) # Get the token.
                    pstate_stack.pop()

                    if next_tok.children:
                        raise ParserException("In parsing the nonterminal {0}"
                                " a call to recursive_parse_nonterm_handler returned a"
                                " subexpression tree rather than the expected"
                                " single token with label {1}.  Subexpression"
                                " was {2}"
                                .format(nonterm_label, item_token_label,
                                                                  next_tok))
                    tok.append_children(next_tok) # Make the token a child.

                # Case item is a nonterminal (i.e., recursive call).
                elif item.kind_of_item == "nonterminal":
                    item_nonterm_label = item.value
                    pstate_stack.append(item_nonterm_label)
                    try:
                        next_subexp = recursive_parse_nonterm_handler(tok, 0)
                    except (BranchFail, CalledEndTokenHandler):
                        raise
                    finally:
                        pstate_stack.pop()
                    tok.append_children(next_subexp)

                # Unknown case item.
                else:
                    raise ParserException("No item recognized.")

            assert tok.token_label == "k_null-string" # DEBUG

            # If parser.top_level_production is set, check that all
            # the tokens were consumed from the lexer.
            if (tok.parser_instance.top_level_production
                    and len(tok.parser_instance.pstate_stack) == 1
                    and not lex.peek().is_end_token()):
                raise BranchFail("Parsing did not reach end of expression.")
            return tok

        except (BranchFail, CalledEndTokenHandler) as e:
            # Backtrack; need to restore to previous saved state.
            lex.go_back_to_state(lex_saved_begin_state)
            if last_case: # Give up, all cases failed.
                raise BranchFail("All production rule cases failed.")
#return head_handler
             #val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
             #ast_label=ast_label)

def generic_tail_handler_function_factory(nonterm_label, caselist):
    """Return a generic tail handler function."""

    def tail_handler(tok, lex, left):
        """Just push the state, relay the call, and pop afterwards."""
        # TODO Below not used or implemented at all yet, just a copied-over stub.
        #
        # TODO: since routines head and tail are the same, maybe just define a
        # generic handler but look at a closure variable or bind the var as
        # a partial function for head vs. tail.....
        #
        # Note that for tail-handler that we presumably want *this* token as the
        # subtree root.
        # TODO: Assoc needs to be fixed in closure at least, and find recurse_bp
        raise ParserException("ERROR: Tail functions not implemented yet for"
                              " null-string tokens.")

        if assoc not in ["left", "right"]:
            raise ParserException('Argument assoc must be "left" or "right".')
        recurse_bp = prec
        if assoc == "right":
            recurse_bp = prec - 1

        pstate_stack = tok.parser_instance.pstate_stack
        pstate_stack.append("pstate_label")
        processed = recursive_parse_nonterm_handler(tok, tok.subexp_prec,
                            processed_left=left, lookbehind=tok.lookbehind)
        pstate_stack.pop()
        return processed
    return tail_handler


def partition_caselist(caselist):
    """Partition a caselist into sub-caselists.  Case lists here are ordinary
    Python lists of `Case` instances, not full `CaseList` objects (which are
    just needed for the overloaded grammar processing).

    Return a defaultdict of caselists holding the ones which start with token
    literal, keyed by the token labels.  Also return a list of caselists for
    those that start with a nonterminal."""

    token_literal_start_cases = defaultdict(list) # Cases starting with a token literal.
    nonterm_start_cases = [] # Cases starting with a nonterminal.

    while caselist: # Items deleted from caselist after processing; run until empty.

        # Repeatedly cycle through caselist, comparing the first case to later
        # ones, copying ones with a common start trigger to the relevant
        # sub-caselist, and finally deleting from caselist all that were copied on
        # that cycle.
        first_saved = False
        first_case = caselist[0]
        first_item = first_case[0]
        first_item_val = first_item.value
        first_item_kind = first_item.kind_of_item
        del_list = [0] # Cases to delete from caselist (after copying to other lists).

        if first_item_kind == "token":
            # Note that *sequences* of tokens could also be handled with
            # some preconditioned lookahead, but may not be more efficient.
            first_item_token_label = first_item_val.token_label
            if not first_saved:
                token_literal_start_cases[first_item_token_label].append(first_case)
                first_saved = True
            for i, curr_case in enumerate(caselist[1:]):
                curr_first_item = curr_case[0]
                curr_first_item_val = curr_first_item.value
                curr_first_item_kind = curr_first_item.kind_of_item
                if curr_first_item_kind != "token":
                    continue
                # We know now that both are starting case-items are tokens.
                curr_first_token_label = curr_first_item_val.token_label
                # The token labels must also be the same.
                if (curr_first_token_label == first_item_token_label):
                    token_literal_start_cases[first_item_token_label].append(curr_case)
                    del_list.append(i)
                else:
                    continue

        elif first_item_kind == "nonterminal":
            if not first_saved:
                nonterm_start_cases.append(first_case)
                first_saved = True
            for i, curr_case in enumerate(caselist[1:]):
                curr_first_item = curr_case[0]
                curr_first_item_val = curr_first_item.value
                curr_first_item_kind = curr_first_item.kind_of_item
                if curr_first_item_kind != "nonterminal":
                    continue
                # We know now that both are starting case-items are nonterminals.
                # LATER these can be given precomputed lookahead preconditions.
                token_label = first_item_val
                nonterm_start_cases.append(curr_case)
                del_list.append(i)

        else:
            raise ParserException("Unrecognized kind of item in CaseList"
                    "processed by def_production_rule.  The item is: "
                    "{0}.".format(first_item))

        for i in reversed(del_list):
            del caselist[i]
    return token_literal_start_cases, nonterm_start_cases

class BranchFail(ParserException):
    """Used in backtracking when parsing production rules.  Raised when a case of
    a rule fails."""
    pass

