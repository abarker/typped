# -*- coding: utf-8 -*-
"""

This module contains the functions which handle the recursive descent parsing
of the rules of a `Grammar` object instance. Grammar objects are defined in
`production_rules` module.  Only the function `def_production_rules_for_nonterminal`
is called externally.  The first argument takes a parser instance, and it is
modified to parse the grammar which is also passed in.

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_production_rules.py",
                              ], pytest_args="-v")

# TODO: Fix syntax checking and remove unneeded imports below.

import sys
import types
import copy
import functools
from collections import OrderedDict, namedtuple, defaultdict

from .shared_settings_and_exceptions import (ParserException, CalledEndTokenHandler)
from .lexer import (Lexer, TokenNode, TokenTable, LexerException, BufferIndexError,
                    multi_funcall)
from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage

#
# Production rule methods.
#

# TODO (general): can preconditions see their token, too, as arg?
# Convenient info to have.... could have here as closure, but maybe better
# to pass it in explicitly if possible.  Is it always available where these
# are called from?

def def_production_rules_for_nonterminal(parser, nonterm_label, grammar):
    """Register production rules for all the cases of the nonterminal
    `nonterm_label`, as defined in the `Grammar` object `grammar`.

    To initially process a caselist this algorithm is run.  It partitions
    the caselist for the `nonterm_label` nonterminal into separate caselists.

    1. Look up the caselist for `nonterm_label` in the grammar object.

    2. Loop over the caselist, removing and making a sub-caselist out of
    the case at the start of the caselist combined with all the other cases
    that start with the same thing.  (All production rule starts are currently
    considered the same, but when the first-set is computed then they will be
    grouped with others having the same first-set.)

    3. For each partial caselist created above, call
    `def_handlers_for_first_case_of_nonterminal` with that caselist.

    """
    # TODO: finish first-sets and make the sub-caselists be groups that
    # start with the same first-set.  Then make that a precond on it.

    # TODO: maybe move this routine back into production_rules module.
    # It is really just preprocessing, and this module is too large.
    # The one use of self can be pushed down to called routine instead.
    # Wait until more mature, though, since linked changes still to be
    # made.

    #print("\nRegistering production rule with label:", nonterm_label)
    if not parser.null_string_token_subclass:
        parser.def_null_string_token() # Define null-string if necessary.

    caselist = grammar[nonterm_label]
    caselist = list(caselist) # Only need ordinary Python lists here.
    #print("\nThe caselist for the rule is", caselist)

    token_start_cases = defaultdict(list) # Cases starting with a token.
    nonterm_start_cases = [] # Cases starting with a nonterminal.

    while caselist:
        # Repeatedly cycle through caselist, comparing first case to later ones,
        # saving common-start ones and then deleting all that were saved on that
        # cycle from caselist.
        #print("PPPPPPPPPPPPP caselist at top of while =", caselist)
        first_saved = False
        first_case = caselist[0]
        first_item = first_case[0]
        first_item_val = first_item.value
        first_item_kind = first_item.kind_of_item
        del_list = [0] # Cases to delete; they are saved on another list.
        if first_item_kind == "token":
            # Note that *sequences* of tokens could also be handled with
            # some preconditioned lookahead, but may not be more efficient.
            first_item_token_label = first_item_val.token_label
            if not first_saved:
                #print("WWWWWW appending (first) token case", first_case)
                token_start_cases[first_item_token_label].append(first_case)
                first_saved = True
            for i, curr_case in enumerate(caselist[1:]):
                curr_first_item = curr_case[0]
                curr_first_item_val = curr_first_item.value
                curr_first_item_kind = curr_first_item.kind_of_item
                if curr_first_item_kind != "token":
                    continue
                # We know now that both are starting case-items are tokens.
                curr_first_token_label = curr_first_item_val.token_label
                #print("YYYYYY token case in while, first token is", first_item_token_label)
                #print("YYYYYY token case in while, curr token is", curr_first_token_label)
                # The token labels must also be the same.
                if (curr_first_token_label == first_item_token_label):
                    #print("WWWWWW appending (later) token case", curr_case)
                    token_start_cases[first_item_token_label].append(curr_case)
                    del_list.append(i)
                else:
                    continue # Each different kind of token gets its own handler.
        elif first_item_kind == "nonterminal":
            if not first_saved:
                #print("WWWWWW appending rule case", first_case)
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
                #print("WWWWWW appending rule case", first_case)
                nonterm_start_cases.append(curr_case)
                del_list.append(i)
        else:
            raise ParserException("Unrecognized kind of item in CaseList"
                    "processed by def_production_rule.  The item is: "
                    "{0}.".format(first_item))

        for i in reversed(del_list):
            del caselist[i]

    #print("\nThe token start cases are", token_start_cases)
    #print("The rule start cases are", nonterm_start_cases, "\n")

    # Register null-string handlers for each unique first item of some case.
    # All rules currently treated as non-unique.
    null_token = parser.null_string_token_subclass

    # Use null-string with a peek in front of actual tokens, also, to avoid
    # special-curr_case handling in later code.
    for token_label, caselist in token_start_cases.items():
        #print("ZZZZZZZZZZ just before registering token, caselist =", caselist)
        # Register handler with a precondition on the token label.
        # --> Here and elsewhere, assuming the token_label and pstate uniquely
        # identify in both cases, but may need more preconds (later). Todo
        #print("XXXXXXXXXX registering peek token of", token_label)
        if caselist:
            def_handlers_for_first_case_of_nonterminal(parser, nonterm_label, null_token,
                                                     caselist, token_label)

    caselist = nonterm_start_cases # All rules currently handled the same.
    #print("ZZZZZZZZZZ just before registering rule, caselist =", caselist)
    # Register handler with the null-string token preconditioned on
    # nonterm_label being on the top of the pstate_stack.
    if caselist:
        def_handlers_for_first_case_of_nonterminal(parser,
                                                nonterm_label, null_token, caselist)

def def_handlers_for_first_case_of_nonterminal(parser, nonterm_label, null_token,
                                               caselist, peek_token_label=None):
                   # Below not used yet.... need to know cases of production...
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

    #print("case list passed to first case start is", caselist)
    def preconditions(lex, lookbehind, peek_token_label=peek_token_label):
        """This function is only registered and used if `peek_token_label`
        is not `None`."""
        pstate_stack = lex.token_table.parser_instance.pstate_stack
        if pstate_stack[-1] != nonterm_label:
            return False
        ##print("QQQQQQQQQQQQQQ testing precondition token hit on", peek_token_label)
        if peek_token_label and lex.peek().token_label != peek_token_label:
            return False
        ##print("QQQQQQQQQQQQQQ got a precondition token hit on", peek_token_label)
        return True
    # The label below label NEEDS to be different for each nonterminal AND for
    # each peek token label.  Otherwise, the routine can fail because the
    # conditions can be wrongly considered identical to previously-defined
    # ones, and overwrite them (found and fixed such a bug, hard to track).
    precond_label = ("precond for production {0} peek {1}"
                                    .format(nonterm_label, peek_token_label))

    def head_handler(tok, lex):
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

        #print()
        #print(indent() + "=======> Running head handler for token", tok)
        #print(indent() + "stack is:", pstate_stack, "\n")
        #print(indent() + "caselist passed in is:", caselist, "\n")

        lex_token_count = lex.all_token_count
        lex_saved_begin_state = lex.get_current_state()
        lex_peek_label = lex.peek().token_label # ONLY saved for backtrack check

        # Loop through the cases until the first succeeds or all fail.
        for case_count, case in enumerate(caselist):
            #print()
            #print(indent() + "Handling case number", case_count, "of", nonterm_label)
            #print(indent() + "Case is:", case)
            assert lex.peek().token_label == lex_peek_label
            tok.children = [] # Reset the children of this null-state token.
            if case_count == num_cases - 1:
                last_case = True

            # Loop through the items testing for match; backtrack on exception.
            try:
                for item in case:
                    #print("\n" + indent() + "Handling case item in loop:", item)

                    # Item is a Token.
                    if item.kind_of_item == "token":
                        item_token = item.value
                        item_token_label = item_token.token_label
                        if not lex.match_next(item_token_label, consume=False):
                            #print()
                            #print(indent() + "FAIL token match for expected",
                            #        item_token_label)
                            #print(indent() + "the actual token is", lex.peek())
                            raise BranchFail("Expected token not found.")
                        #print(indent() + "SUCCESS token match for", item_token_label)

                        # Actual tree nodes must be processed by literal handler.
                        # Todo: document/improve this stuff, nice to push known label
                        pstate_stack.append(None) # Avoid recursion to this handler.
                        next_tok = tok.recursive_parse(0) # Get the token.
                        pstate_stack.pop()

                        #print(indent()+"==> next_tok subexpr is", next_tok)
                        if next_tok.children:
                            raise ParserException("In parsing the nonterminal {0}"
                                    " a call to recursive_parse returned a"
                                    " subexpression tree rather than the expected"
                                    " single token with label {1}.  Subexpression"
                                    " was {2}"
                                    .format(nonterm_label, item_token_label,
                                                                      next_tok))
                        tok.append_children(next_tok) # Make the token a child.

                    # Case item is a nonterminal (i.e., recursive call).
                    elif item.kind_of_item == "nonterminal":
                        item_nonterm_label = item.value
                        #print(indent() + "Handling a production, pushing state:",
                        #                         item_nonterm_label)
                        pstate_stack.append(item_nonterm_label)
                        try:
                            next_subexp = tok.recursive_parse(0)
                        except (BranchFail, CalledEndTokenHandler):
                            #print(indent() + "FAIL production rule recursion.")
                            raise
                        finally:
                            pstate_stack.pop()
                        #tok.append_children(next_subexp) # Keep rule toks in tree.
                        #print(indent() + "SUCCESS in production rule recursion.")
                        #print(indent() + "Appending children:", next_subexp.children)
                        tok.append_children(next_subexp)

                    # Unknown case item.
                    else:
                        #print(indent() + "unrecognized item is", item)
                        raise ParserException("No item recognized.")

                assert tok.token_label == "k_null-string" # DEBUG
                #print(indent() + "Returning tok with children", tok.children)
                #print()
                tok.process_and_check_node(head_handler)

                # If parser.top_level_production is set, check that all
                # the tokens were consumed from the lexer.
                if (tok.parser_instance.top_level_production
                        and len(tok.parser_instance.pstate_stack) == 1
                        and not lex.peek().is_end_token()):
                    raise BranchFail("Parsing did not reach end of expression.")
                return tok

            except (BranchFail, CalledEndTokenHandler) as e:
                # Backtrack; need to restore to previous saved state.
                ##print() # DEBUG
                #print(indent() + "============> backtracking at token", tok)
                #print(indent() + "\nFAIL is:\n", tok.tree_repr(indent=indent()))
                #print(indent() + "Exception is:", e)
                lex.go_back_to_state(lex_saved_begin_state)
                if last_case: # Give up, all cases failed.
                    raise BranchFail("All rule cases failed.")

    def tail_handler(tok, lex, left):
        """Just push the state, relay the call, and pop afterwards."""
        # TODO: since routines head and tail are the same, maybe just define a
        # generic handler but look at a closure variable or bind the var as
        # a partial function for head vs. tail.....
        #
        # TODO Below not used or implemented at all yet, just a copied-over stub.
        #
        # Note that for tail-handler that we presumably want *this* token as the
        # subtree root.
        raise ParserException("ERROR: No tail functions defined yet for"
                " null-string tokens.")
        pstate_stack = tok.parser_instance.pstate_stack
        pstate_stack.append("pstate_label")
        processed = tok.recursive_parse(tok.subexp_prec,
                            processed_left=left, lookbehind=tok.lookbehind)
        pstate_stack.pop()
        return processed

    # Register the handler for the first item of the first case.
    prec = 0 # TEMPORARY
    precond_priority = 10000
    return parser.modify_token_subclass(null_token.token_label, prec=prec,
                 head=head_handler, tail=tail_handler, precond_label=precond_label,
                 precond_fun=preconditions, precond_priority=precond_priority)
                 #val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                 #ast_label=ast_label)


class BranchFail(ParserException):
    """Used in backtracking when parsing production rules.  Raised when a case of
    a rule fails."""
    pass

