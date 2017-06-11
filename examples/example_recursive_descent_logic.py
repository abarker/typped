# -*- coding: utf-8 -*-
"""

Beginning experiments working out how the null-state tokens can be used to
do recursive descent parsing from a grammar.  Currently only parses basic
-A(c,c) type stuff.  Work out pushing and popping states, defining the
needed null-state tokens, etc.

EBNF
----

EBNF symbols, from wikipedia just below.  Note angle braces are optional, and
there are various bracket-pairs which modify precedence (such as for parens for
grouping and Pascal-style comments).  Note I'm leaving out concatenation commas
and terminators.  Compare this standard EBNF with W3C EBNF (more like regexes)
   https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form
   https://www.w3.org/TR/REC-xml/#sec-notation

    * repetition-symbol
    - except-symbol
    | definition-separator-symbol
    = defining-symbol

    (* comment *)
    ( group )
    [ optional ]
    { repeated-0-or-more }
    { repeated-1-or-more }-
    3 * xx      repeat xx 3 times

Examples (from same page):

    aa = 'A';
    bb = 3 * aa, 'B';
    cc = 3 * [aa], 'C';
    dd = {aa}, 'D';
    ee = aa, {aa}, 'E';
    ff = 3 * aa, 3 * [aa], 'F';
    gg = {3 * aa}, 'G';

    Terminal strings defined by these rules are as follows:

    aa: A
    bb: AAAB
    cc: C AC AAC AAAC
    dd: D AD AAD AAAD AAAAD etc.
    ee: AE AAE AAAE AAAAE AAAAAE etc.
    ff: AAAF AAAAF AAAAAF AAAAAAF
    gg: G AAAG AAAAAAG etc.

Basic grammar in EBNF is:

    <wff> = <atomic_formula>
          | '-' <wff>
          | '(' <wff> ')'
          | <wff> '->' <wff>

    <atomic_formula> = <predicate_letter> '(' <termlist> ')'

    <termlist> = <term> ',' <termlist> | <term>

    <term> = <function_letter> '(' <termlist> ')'
           | <variable>
           | <constant>

    <variable> = TOKEN
    <constant> = TOKEN
    <predicate_letter> = TOKEN
    <function_letter> = TOKEN

Note that this has left recursion in it when <wff> calls <wff> first.  That can
be removed (as per Wikipedia) by some standard replacements:

    <wff> = <atomic_formula> <wff_prime>
          | '-' <wff_prime>
          | '(' <wff_prime> ')'

    <wff_prime> = '->' <wff>

    <atomic_formula> = <predicate_letter> '(' <termlist> ')'

    <termlist> = <term> ',' <termlist> | <term>

    <term> = <function_letter> '(' <termlist> ')'
           | <variable>
           | <constant>

    <variable> = TOKEN
    <constant> = TOKEN
    <predicate_letter> = TOKEN
    <function_letter> = TOKEN

CONSIDER
--------

What if the only thing that null-state tokens do is run their relevant handler,
generally to just change the pstate.  When getting `next` and looking up
handler you just loop until no null-state match.  As long as one matches it is
called and loop goes on.  This would be especially good for looking at the left
or lookbehind, so for two productions in a row you could look back to the first
one as a precondition on the second one, and see the actual subexpression type.

This AVOIDS the problem of having to either set the precedence for such tokens
such as based on the upcoming actual token prececence (or else mess up the whole
precedence mechanism for infix operators).

REMEMBER, that just setting the state to the one you want causes that
production to activate on the next recursive_parse  call (if set up correctly),
and IT will get the subexpression/subtree.  THEN the tokens following within
their own case of their production can look behind them in the subexpression as
one of their preconditions.  Handles cases of same beginning production with
different other non-production tokens after.

What happens when two productions occur in a row at the beginning of a case?

Curtailment works even better... you can detect cycles in the initial
null-string token stage without affecting anything else.

Case-handling rules (broken still):

    0) Each case may need to be a separate production or at least have a
    separate substate and replace the stack state with that.

    1) Productions which start with a regular token can be completely processed by
    the handler for that token.  It can push on the states and get what it needs.
    Not especially automated, though...

    2) Cases must end with a regular token in order to pop the state.

How do we handle:
    X = Y | Z
when we basically need to know lookahead info?  Almost need to preprocess or else
potentially do some backtracking.  For an LL(1) grammar it is unambiguous, but not
from the production alone.  Basically, do the substitution of all the sub-productions
until they all start with a terminal symbol.  This is called Griebach normal form
when epsilon productions are allowed:

https://en.wikipedia.org/wiki/Greibach_normal_form

    In formal language theory, a context-free grammar is in Greibach normal
    form (GNF) if the right-hand sides of all production rules start with a
    terminal symbol, optionally followed by some variables. A non-strict form
    allows one exception to this format restriction for allowing the empty word
    (epsilon, ε) to be a member of the described language. The normal form was
    established by Sheila Greibach and it bears her name.

    More precisely, a context-free grammar is in Greibach normal form, if all
    production rules are of the form:

        A → a A1 A2 ⋯ An
    or
        S → ε

    where A is a nonterminal symbol, a is a terminal symbol, A1 A2 … An is a
    (possibly empty) sequence of nonterminal symbols not including the start
    symbol, S is the start symbol, and ε is the empty word.[1]

    Observe that the grammar does not have left recursions.

    Every context-free grammar can be transformed into an equivalent grammar in
    Greibach normal form.[2] Various constructions exist. Some do not permit
    the second form of rule and cannot transform context-free grammars that can
    generate the empty word. For one such construction the size of the
    constructed grammar is O(n4) in the general case and O(n3) if no derivation
    of the original grammar consists of a single nonterminal symbol, where n is
    the size of the original grammar.[3] This conversion can be used to prove
    that every context-free language can be accepted by a real-time pushdown
    automaton, i.e., the automaton reads a letter from its input every step.

    Given a grammar in GNF and a derivable string in the grammar with length n,
    any top-down parser will halt at depth n.

https://en.wikipedia.org/wiki/Chomsky_normal_form
    In formal language theory, a context-free grammar G is said to be in Chomsky
    normal form (first described by Noam Chomsky)[1] if all of its production rules
    are of the form:[2]:92–93,106

        A → BC,   or
        A → a,   or
        S → ε,

    where A, B, and C are nonterminal symbols, a is a terminal symbol (a symbol
    that represents a constant value), S is the start symbol, and ε denotes the
    empty string. Also, neither B nor C may be the start symbol, and the third
    production rule can only appear if ε is in L(G), namely, the language
    produced by the context-free grammar G.

    Every grammar in Chomsky normal form is context-free, and conversely, every
    context-free grammar can be transformed into an equivalent one which is in
    Chomsky normal form and has a size no larger than the square of the
    original grammar's size.

Greibach can be parsed by Pratt parser without any null-string tokens?  No.
NOTE, that somebody on stackexchange says Greibach can be parsed by recursive
descent, but it might require backtracking (but space complexity is linear).
   http://cs.stackexchange.com/questions/10468/the-importance-of-normal-forms-like-chomsky-normal-form-for-cfgs
That is because then you have aBCD on rhs you might still have several B, C, and D
choices... you have to go down to an A->a production or fail for alternatives...
Can be parsed with null-string tokens that aren't real tokens... at least close.

CYK algorithm uses dynamic programming for language in Chomsky normal form.

https://en.wikipedia.org/wiki/Recursive_descent_parser
    A predictive parser is a recursive descent parser that does not require
    backtracking.

http://www.cs.engr.uky.edu/~lewis/essays/compilers/ll-lang.html
    Definition. A context free grammar is an s-grammar if and only if every
    production's right hand side begins with a terminal symbol and this
    terminal is different for any two productions with the same left-hand side.

TODO: The LAST token of a case should pop the state for that production, so the
others in the case see the correct state in the precond testing.

ALTERNATIVE, back to real tokens for productions: what if the first null-string
token is called as a head and each following token is called as a tail, with
null-string tokens being

Ideas, implementation with null-string tokens
---------------------------------------------

For future, consider "curtailment"'-'type algorithm: You cannot call a null'-'string
production again if the current lexer character is the same as the last time.

To start, push the wff production onto the stack, before calling parse.
The handler for the wff production will pop it off, and push on the next
one to call (if one) before calling recursive_parse.  One of the three
handler choices is chosen according to preconditions.
#
Say the last is chosen.  We registered a handler for "(" with wff state
as a precondition.  That will then read in the "(", push the state for
wff_prime (one version if multiple) and call recursive_parse.  Then
consumes the final ")".  Returns the new `left`.
#
Say the first.  A handler for null-string tokens has been defined in the
wff state with no preconds, at lower priority than the ones with
preconds.  (Can there only be one?  No lookahead beyond?  What kind of
parser/grammar is that called?).  That handler pushes atomic_formula and
calls recursive_parse, then pushes wff_prime and calls recursive_parse.
Returns the new `left`.

TODO: the operator precedences apparently WILL work IF each "or"
component is given a unique name/precedence value.  Separating the "or"
components, each production gets the precedence of its leftmost token.
But they need to be able to have different precedences set when they
are defined with def_production.  But seems like an easy redefine to
make it that... and not too bad a requirement, since recursive descent
can't do it normally, anyway.

A head or a tail handler of a null-string token is called according its
position, just like ordinary tokens.  It calls recursive_parse in
exactly the same way that it was called, just relaying really.

NOTE that the Pratt way is in some way using one head handler and then
tail-handling the whole thing with appropriate case statements... In the
Pratt version the null-strings are in some sense delegated the task of
reading the tail...

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

#pytest_helper.script_run(self_test=True, pytest_args="-v")
#pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import math
import operator
import typped as pp

# Some naming conventions.
#
# Lexer:
#    tokens:    k_float
# Syntax:
#    types:     t_int
# AST labels:
#    a_number

def define_logic_parser():
    #
    #


    class LogicParser(pp.PrattParser):
        def __init__(self, *args, **kwargs):
            super(LogicParser, self).__init__(*args, **kwargs)

    parser = LogicParser()
    return parser

def define_logic_language(parser):

    #
    # Some general tokens.
    #

    whitespace_tokens = [
            ("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
            ("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
            ]
    parser.def_multi_ignored_tokens(whitespace_tokens)

    token_list = [
            ("k_not", r"-"),
            ("k_implies", r"->"),
            ("k_and", r"and"),
            ("k_or", r"or"),
            ("k_equals", r"="),

            ("k_forall", r"forall"),
            ("k_exists", r"exists"),

            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_comma", r","),

            ("k_true", r"True"),
            ("k_false", r"False"),

            ("k_pred_letter", r"A[0-9]*"),
            ("k_fun_letter", r"f[0-9]*"),
            ("k_const_letter", r"c[0-9]*"),
            ]
    parser.def_multi_tokens(token_list)

    #
    # Basic syntax.
    #

    parser.def_literal("k_const_letter")

    #
    # Define predicate letters and functions as standard functions.
    #

    parser.def_stdfun("k_pred_letter", "k_lpar", "k_rpar", "k_comma", arg_types=None)
    parser.def_stdfun("k_fun_letter", "k_lpar", "k_rpar", "k_comma", arg_types=None)

    #
    # Define null-state token and init pstack.
    #

    parser.def_null_string_token("null-string")
    parser.pstack = [None, "wff"] # Consider [None] as the "empty pstack state"

    #
    # In the "wff" state, handle the logical not token.
    #

    def logical_not_token_precond_wff_state(lex, lookbehind):
        pstack_state = lex.token.parser_instance.pstack[-1]
        return pstack_state == "wff"
    construct_label = "logical_not_token_precond_wff_state"
    precond_priority = 0

    def head_handler(tok, lex):
        tok.parser_instance.pstack.pop()
        tok.append_children(tok.recursive_parse(0))
        tok.parser_instance.pstack.append("wff_prime")
        return tok

    parser.def_construct(pp.HEAD, head_handler, "k_not", prec=0,
                     construct_label=construct_label,
                     precond_fun=logical_not_token_precond_wff_state,
                     precond_priority=precond_priority,
                     #val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                     #ast_data=ast_data
                     )

    #
    # In the "wff_prime" state, handle the k_implies token.
    #

    def implies_token_in_wff_prime_state_precond(lex, lookbehind):
        pstack_state = lex.token.parser_instance.pstack[-1]
        return pstack_state == "wff_prime"
    construct_label = "implies_token_in_wff_prime_state_precond"
    precond_priority = 0

    def head_handler(tok, lex): # ??????????? TAIL OR HEAD??????????
        tok.append_children(tok.recursive_parse(0))
        tok.parser_instance.pstack.pop()
        return tok

    parser.def_construct(pp.HEAD, head_handler, "k_implies", prec=0, # PREC??????????
                     construct_label=construct_label,
                     precond_fun=implies_token_in_wff_prime_state_precond,
                     precond_priority=precond_priority,
                     #val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                     #ast_data=ast_data
                     )

    # TODO still need the null-state token....

def dummy_take_what_is_needed():
    #
    # Parens and brackets, highest precedence (since they have a head function).
    #

    parser.def_bracket_pair("k_lpar", "k_rpar",
                            eval_fun=lambda t: t[0].eval_subtree())
    parser.def_bracket_pair("k_lbrac", "k_rbrac",
                            eval_fun=lambda t: t[0].eval_subtree())

    #
    # Basic operators, from highest to lowest precedence.
    #

    parser.def_prefix_op("k_plus", 50,
                         eval_fun=lambda t: operator.pos(t[0].eval_subtree()))
    parser.def_infix_op("k_double_ast", 30, "right",
            eval_fun=lambda t: operator.pow(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Assign simple variable.
    #

    parser.calculator_symbol_dict = {} # Store symbol dict as a new parser attribute.
    symbol_dict = parser.calculator_symbol_dict

    symbol_dict["pi"] = math.pi # Predefine pi.
    symbol_dict["e"] = math.e # Predefine e.

    # Note that on_ties is -1, so function names will take precedence over identifiers.
    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    parser.def_literal("k_identifier",
            eval_fun=lambda t: symbol_dict.get(t.value, 0.0))

    def eval_assign(t):
        rhs = t[1].eval_subtree()
        symbol_dict[t[0].value] = rhs
        return rhs

    parser.def_infix_op("k_equals", 5, "right", ast_data="a_assign",
                        eval_fun=eval_assign)


def read_eval_print_loop(parser):
    try:
        read_input = raw_input
    except NameError:
        read_input = input

    print("Enter ^C to exit.")
    while True:
        try:
            line = read_input("> ")
        except (KeyboardInterrupt, EOFError):
            print("\nBye.")
            break
        if not line: continue

        try:
           parse_tree = parser.parse(line)
           #eval_value = parse_tree.eval_subtree()
        except (ValueError, ZeroDivisionError,
                pp.ParserException, pp.LexerException) as e:
            print(e)
            continue
        else:
            print(parse_tree.tree_repr())
            #print(eval_value)


def define_and_run_logic_repl():
    import readline
    parser = define_logic_parser()
    define_logic_language(parser)
    read_eval_print_loop(parser)

define_and_run_logic_repl()

