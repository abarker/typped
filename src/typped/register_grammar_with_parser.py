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

Note that because the processing uses the parser instance variables `pstate_stack`
and `pstate_processing_in_progress` this parsing is not thread-safe for
multiple parses on the same parser (but the parser in general probably is not,
either).

Rules and guidelines
--------------------

* No left recursion.  Some token must be consumed before any recursive call.
  This is not OK.  The left recursion will never stop::

      arglist = Rule("arglist") + Tok("k_comma") + Rule("arg") | Rule("arg")

  This is OK:

      arglist = Rule("arg") + Tok("k_comma") + Rule("arglist") | Rule("arg")

* Cases are evaluated in sequential order, so if cases have the same prefix
  put the longer cases first.  This modified version of the OK rule above will
  never select the second case.  It will always parse a single "arg" rule and be
  satisfied::

      arglist = Rule("arg") | Rule("arg") + Tok("k_comma") + Rule("arglist")

Precedences
-----------

Precedences not handled right for now...

Currently head or tail handlers are registered only for the first item of first
case, but really ALL literal tokens and nonterminals should have a handler
defined -- conditioned on the pstate value to avoid conflicts.  They can check
types if the types are defined correctly.  For literal tokens we could just
call def_literal (or just read the thing with `next` in if it must be there).

Assume for now that precedences only apply to token literals.  Two possible
ways to consider:

   num_expr      = k_number + Rule("operator") + num_expr | k_number
   operator      = Tok("k_ast")[10] | Tok("k_plus")[20]

   num_expr         = k_number + Rule("op_and_right_arg") | k_number
   op_and_right_arg = Tok("k_ast")[10] + num_expr | Tok("k_plus")[20] + num_expr

parse("5 + 3 * 2")

When the handler for a nonterminal calls `recursive_parse` it always does it as
`recursive_parse(tok.extra_data.subexp_prec)` to simply forward the current
subexpression precedence to the next call.

When a nonterminal handler processes a case the it goes through the items,
calling recursive_parse to get each one.  If they have `prec=0` then they are
parsed via their head handlers, which just return the subtree (for either the
token literal or the rule).  All those values are just appended to the children
list.

When a token with a precedence >0 is called it is parsed with the tail handler.
The tail handler does exactly the same thing as the head handler *except* that
it 1) takes the `left` argument and makes it it own left child/operand, and 2)
calls `recursive_parse` to get the right child/operand.

But when the parser encounters a nonterminal it is not sure if it will be a
head or tail token...

------------------

The handler for a nonterminal does this:

1. Sequentially call recursive-parse for each item.  Check that the returned
   thing matches what it is supposed to match.


for item in item_list:
    next_from_text = recursive_parse(0)
    if not compare_item_with_tree(item, next_from_text):
       raise Fail

Consider peekahead prec for nonterminals:  The precedence is the same
as the peek token, i.e., the next non-virtual token that is actually in
the lexer.  Easy to do, since prec() is a function (or can be a property).

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_ebnf_classes_and_operators.py",
                              "../../test/test_example_expression_grammar.py",
                             ], pytest_args="-v")

from collections import defaultdict
import functools
from .shared_settings_and_exceptions import (HEAD, TAIL,
                                     ParserException, CalledEndTokenHandler)
from .pratt_types import TypeSig
from .pratt_constructs import Construct

#from .lexer import (Lexer, TokenNode, TokenTable, LexerException, BufferIndexError,
#                    multi_funcall)
#from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage

#
# Production rule methods.
#

DEBUG = False

HUGE_PRIORITY = 1000000000000 # Currently preconds for grammar-based stuff run first.

# TODO: Optimizations to speed runtime and memory.
# 1) Implement first-sets in the recursion.  Code is set up for it when it
#    is calculated in Grammar.  Just register nonterminal-starting caselists
#    like token-literal starting ones are currently done.  Modify partitioning fun.
# 2) Consider memoization on the recursive calls, which repeat a lot of
#    work re-doing parts of cases which have already been done.
# 3) Optimize the selection of handlers for null-string tokens in ConstructTable.  There
#    is one such token, and it triggers *all* of the rules, sequentially running
#    preconds.  Could a) add split to tree in ConstructTable, b) do general optimization
#    described in ConstructTable, c) make a shortcut dict from nonterm
#    labels and peeks directly to the corresponding sorted list of precond funs,
#    or d) have a different kind of null-string tokens for each nonterminal (with
#    different labels, or somehow split on the values of tokens, too).

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

    # TODO: finish implementing first-sets.  Then just need to modify the
    # partition_caselist function to include the nonterminal-starting cases
    # with first-sets tokens in the returned dict (keyed by token labels)
    # instead of in the list of things without a peek token.  Only consider
    # single-token first sets for now.

    # TODO: Precedences not currently handled correctly.
    if DEBUG: print("\n==== Start registering rule:", nonterm_label)

    if not parser.null_string_token_subclass:
        parser.def_null_string_token() # Define null-string if necessary.
    null_token = parser.null_string_token_subclass

    caselist = grammar[nonterm_label]
    caselist = list(caselist) # Only need ordinary Python lists here.

    # Partition the caselist into sub-caselists.
    cases_with_peek_token_dict, cases_without_peek_token_list = partition_caselist(
                                                                          caselist)

    # For first-set items register handlers for null-string with a precond on
    # both the state on the pstate_stack and the peek token label.
    for token_label, caselist in cases_with_peek_token_dict.items():
        if DEBUG: print("token literal start caselist @@@@@@@@ ", token_label, caselist)
        # Register handler with a precondition on the token label.
        if caselist:
            def_null_string_handler_for_first_item_of_caselist(parser, nonterm_label,
                           null_token, caselist, peek_token_label=token_label)

    # For things without a first-set element register handlers for null-string
    # with a precond only on the state on top of the pstate_stack.
    caselist = cases_without_peek_token_list # A list; nonterms don't need peek_token.
    if DEBUG: print("nonterm start caselist &&&&&&&& ", caselist)
    if cases_without_peek_token_list:
        def_null_string_handler_for_first_item_of_caselist(parser, nonterm_label,
                                       null_token, cases_without_peek_token_list)

def def_null_string_handler_for_first_item_of_caselist(parser, nonterm_label,
                                  null_token, caselist, peek_token_label=None):
                   # Below params not used yet.... need to know cases of production...
                   #val_type=None, arg_types=None, eval_fun=None,
                   #ast_label=None):

    """Define the head and tail handlers for the null-string token that is
    called to parse a production rule (i.e., it is called when the precondition
    that the state label `nonterm_label` is on the top of the `pstate_stack` is
    satisfied).

    These handlers handle all the production rule cases of the caselist for the
    nonterminal `nonterminal`, backtracking on failure and trying the next
    case, etc.  They act very much like the usual recursive descent function
    for parsing a production rule, except that to make a recursive call to
    handle a sub-production they push that production label onto the
    `pstate_stack` and then call `recursive_parse`.  Then the handler for that
    production will be called, doing a search over its cases, etc., returning
    the value to the calling level.

    The `peek_token_label` is used to set a precondition on cases which start
    with a particular kind of token.  This can also be used to implement
    first-sets.

    The label of the starting nonterminal is assumed to have initially been
    pushed on the `pstate_stack` in order to do grammar-based processing."""
    # Todo: later consider limiting the depth of the recursions by not allowing
    # a null-string token handler to be called recursively unless something has
    # been consumed from the lexer (curtailment, but no memoization, see e.g.
    # Frost et. al 2007).

    # TODO either set var below in PrattParser when settled or set pstate_stack
    # here, too.  Or maybe from grammar-processing routine that calls this one.
    parser.pstate_processing_in_progress = False # Preconds look at this, don't fire when true.


    first_case_first_item_precond = get_precond_funs(nonterm_label, peek_token_label)

    #prec_of_first_item = caselist[0][0].prec
    prec_of_first_item = 0 # Assume only heads for now.

    if prec_of_first_item == 0:
        # Register the handler for the first item of the first case.
        head_handler = nonterminal_handler_factory(nonterm_label, caselist)
        construct_label = "head construct_for_nonterminal_{0}".format(nonterm_label)
        parser.def_construct(HEAD, head_handler, null_token.token_label, prec=0,
                     construct_label=construct_label,
                     precond_fun=first_case_first_item_precond,
                     precond_priority=HUGE_PRIORITY)
                     #val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                     #ast_label=ast_label)

        # Set up a failure case where none of the preconditions match in
        # the context of the pstate.
        parser.def_construct(HEAD, always_raise_branch_fail_head_handler, null_token.token_label,
                     prec=0, construct_label="all_fail_construct",
                     precond_fun=lower_priority_fail_if_stack_nonempty_precond,
                     precond_priority=HUGE_PRIORITY-1)

    else:
        tail_handler = generic_tail_handler_function_factory(nonterm_label, caselist)
        construct_label = "tail construct_for_nonterminal_{0}".format(nonterm_label)
        prec = prec_of_first_item
        parser.def_construct(TAIL, tail_handler, null_token.token_label, prec=prec,
                     construct_label=construct_label,
                     precond_fun=first_case_first_item_precond,
                     precond_priority=precond_priority)

    ## Create a construct for every token-literal to hold its associated data.
    ## ACTUALLY constructs_by_label_and_nonterm_label MUST BE SOMEWHERE ACCESSIBLE.
    #constructs_by_label_and_nonterm_label = {}
    #for case_count, case in enumerate(caselist):
    #    token_label_set = set()
    #    for item_count, item in enumerate(case):
    #        if item.kind_of_item == "token":
    #            token_label = item.value.token_label
    #            if token_label in token_label_set:
    #                continue # Only add the token literal once per nonterminal label.
    #            token_label_set.add(token_label)
    #            construct_label = "construct_for_token_{0}_in_nonterminal_{1}".format(
    #                                                       token_label, nonterm_label)
    #            c = None # = Construct(...) # SET AND FILL IN.
    #            constructs_by_label_and_nonterm_label[(token_label, nonterm_label)] = c

def nonterminal_handler_factory(nonterm_label, caselist):
    """Return a generic head handler with `nonterm_label` and `caselist` bound
    in the closure."""
    # Note partial replaces positional arguments from the left.
    return functools.partial(generic_nonterminal_handler, nonterm_label, caselist)

def generic_nonterminal_handler(nonterm_label, caselist, tok, lex):
    """The head handler assigned to the first token of the first case for
    a caselist.  It tries all the other cases if it fails."""
    # The "called as head vs. tail" thing won't matter, because
    # productions are always called as the whole case, by the
    # production label.  So, all cases will have the *same* method of
    # calling....  If first case was called as head then call all later
    # cases as heads; same with tails.
    if DEBUG: print("\nRunning handler for nonterm label", nonterm_label,
                    "caselist is\n", caselist)
    pstate_stack = tok.parser_instance.pstate_stack
    first_call_of_start_state = False

    # TODO Save the label of the nonterminal that parsed the token.
    # Document that value of null-string token (was None) is set to
    # nonterm_label.  Pass value up in modifications where full tree
    # not shown.
    tok.value = nonterm_label

    #def indent(): # DEBUG fun
    #    return " " * ((len(pstate_stack)-1) * 3)

    lex_token_count = lex.all_token_count
    lex_saved_begin_state = lex.get_current_state() # Return here on failure.
    lex_peek_label = lex.peek().token_label # ONLY saved for backtrack check

    # Loop through the cases until one succeeds or all fail.
    num_cases = len(caselist)
    last_case = False
    for case_count, case in enumerate(caselist):
        if DEBUG: print(nonterm_label, "case number", case_count)
        if DEBUG: print(" ", case)
        tok.children = [] # Reset the children of this null-state token.
        if case_count == num_cases - 1:
            last_case = True

        # Loop through the items testing for match; backtrack on exception.
        try:
            parse_tree = parse_case(case, lex, pstate_stack, tok, nonterm_label)
            break
        except (BranchFail, CalledEndTokenHandler) as e:
            # Backtrack; need to restore to previous saved state.
            lex.go_back_to_state(lex_saved_begin_state)
            if last_case: # Give up, all cases failed.
                raise BranchFail("All production rule cases failed.")

    return parse_tree

def next_token_literal_with_type_assignment(lex):
    """Read a token and set its types."""
    # TODO: May later want to define a full Construct object for each token in the grammar,
    # or even for each (token_label, nonterminal) in the grammar.  Then eval funs,
    # etc. can also be put there.
    tok = lex.next()
    sig = TypeSig(None) # TODO: Save in a dict somewhere along with prec to look up.
    #tok.all_possible_sigs = [TypeSig(None)]
    #tok._check_types_one_pass() # Just set the types directly below.
    tok.is_head = True # To lookup eval funs, later maybe just set eval fun when option added.
    tok.original_sig = sig
    tok.actual_sig = sig
    tok.expanded_formal_sig = sig
    return tok

def parse_case(case, lex, pstate_stack, tok, nonterm_label):
    """Try to parse the particular case represented by the `ItemList` passed as
    the `case` parameter.  Raise an exception on failure."""
    # Get the current subexpression precedence to pass through to the
    # calls to recursive_parse.
    subexp_prec = tok.extra_data.subexp_prec
    if DEBUG: print("      parsing individual case:", nonterm_label, case)
    if DEBUG: print("      pstate_stack", pstate_stack)

    for item in case:
        # Item is a nonterminal (i.e., recursive call).
        if item.kind_of_item == "nonterminal":
            item_nonterm_label = item.value
            if DEBUG: print("      recursing to parsing rule:", item_nonterm_label)

            pstate_stack.append(item_nonterm_label)
            try:
                if DEBUG: print("      pstate_stack before recurse", pstate_stack)
                next_subexp = tok.recursive_parse(subexp_prec, only_head=True)
                if DEBUG: print("      SUCCESS in recursion parse", next_subexp)
                if DEBUG: print("      SUCCESS with pstate_stack", pstate_stack)
            except (BranchFail, CalledEndTokenHandler):
                raise
            finally:
                pstate_stack.pop()
            tok.append_children(next_subexp)

        # Item is a Token.
        elif item.kind_of_item == "token":
            item_token = item.value
            item_token_label = item_token.token_label
            if DEBUG: print("      item expects a:", item_token_label)
            if DEBUG: print("      considering a match with token", lex.peek())
            if not lex.match_next(item_token_label, consume=False):
                if DEBUG: print("      FAIL to match expected")
                raise BranchFail("Expected '{0}' token not found.".
                                 format(item_token_label))
            if DEBUG: print("      matched a token with token label", lex.peek().token_label,
                      "and value", lex.peek().value)

            # Set the pstate_processing_in_progress flag to avoid getting a
            # null-string token for a nonterminal when trying to get a token
            # literal (violates the precond for a nonterminal).
            tok.parser_instance.pstate_processing_in_progress = True
            #next_tok = tok.recursive_parse(subexp_prec, only_head=True) # Get the token.
            next_tok = next_token_literal_with_type_assignment(lex)
            if DEBUG: print("      ===> returned this token from recursive parse", next_tok)
            #next_tok = lex.next() # Get the token. Fails, won't type check...
            tok.parser_instance.pstate_processing_in_progress = False

            if next_tok.children:
                raise ParserException("In parsing the nonterminal '{0}'"
                        " a call to recursive_parse_nonterm_handlers returned a"
                        " subexpression tree rather than the expected"
                        " single token with label '{1}'.  Subexpression"
                        " was '{2}'"
                        .format(nonterm_label, item_token_label, next_tok))
            tok.append_children(next_tok) # Make the token a child.

        # Unknown item.
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
        processed = tok.recursive_parse(tok.subexp_prec, # Now attrs of tok.extra_data
                            processed_left=left, extra_data=tok.extra_data)
        pstate_stack.pop()
        return processed
    return tail_handler

def partition_caselist(caselist):
    """A utility routine to partition a caselist into sub-caselists.  Case
    lists here are ordinary Python lists of `Case` instances, not full
    `CaseList` objects (which are really only needed for the
    overloaded-operator grammar processing).

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

#
# Precondition functions and fixed handlers used in registering null-string handlers.
#

def always_raise_branch_fail_head_handler(tok, lex):
    """This is a lower-priority head handler that is triggered by null-string
    tokens when all their actual precondition functions fail.  It signals
    branch failure in the search tree."""
    if DEBUG: print(" X X X X X X X X X X X X X ALL FAIL PRECOND")
    raise BranchFail

def lower_priority_fail_if_stack_nonempty_precond(tok, lex):
    """A precondition to fail if no other stack-based precondition function
    matches when `nonterm_label` is at the top of the `pstate_stack`."""
    if DEBUG: print("fail precond!!!")
    pstate_stack = lex.token_table.parser_instance.pstate_stack
    already_processing = lex.token_table.parser_instance.pstate_processing_in_progress
    if not pstate_stack:
        return False
    #if pstate_stack[-1] != nonterm_label:
    #    if DEBUG: print("   failed lower priority already proc")
    #    return False
    if already_processing:
        if DEBUG: print("   failed lower priority already proc")
        return False # This avoids accidental recursion.
    return True

def get_precond_funs(nonterm_label, peek_token_label):
    """Get the precondition functions for registering null-space tokens.  The
    `nonterm_label` and `peek_token_label` values are saved in the function
    closure."""

    def first_case_first_item_precond(tok, lex):
        """A precondition that `nonterm_label` is at the top of the `pstate_stack`.
        If `peek_token_label` is set then it must also match the label of the peek
        token."""
        if DEBUG: print("running precond for null-string token, rule",
                nonterm_label, "peek", peek_token_label)
        pstate_stack = lex.token_table.parser_instance.pstate_stack
        already_processing = lex.token_table.parser_instance.pstate_processing_in_progress
        if not pstate_stack:
            return False
        if pstate_stack[-1] != nonterm_label:
            if DEBUG: print("   failed precond pstate")
            return False
        if already_processing:
            if DEBUG: print("   failed precond already processing")
            return False # This avoids accidental recursion in token processing.
        if peek_token_label and lex.peek().token_label != peek_token_label:
            if DEBUG: print("   failed precond peek, real peek is", lex.peek())
            return False
        if DEBUG: print("   precond success")
        return True

    return first_case_first_item_precond

#
# Exceptions.
#

class BranchFail(ParserException):
    """Used in backtracking when parsing production rules.  Raised when a case of
    a rule fails."""
    pass

