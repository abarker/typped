# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

#pytest_helper.script_run(self_test=True, pytest_args="-v")
#pytest_helper.auto_import()
pytest_helper.sys_path("../src/typped")

import math
import operator
import pratt_parser as pp

# Some naming conventions.
#
# Lexer:
#    tokens:    k_float
# Syntax:
#    types:     t_int
#    operators: op_add
#    otherwise: no currently defined prefix
# AST nodes:
#    a_number
# Semantics:
#    eval fun:  e_add

# TOKEN DEFINITIONS #################################################################


def define_basic_calculator_tokens(parser):
    #lexer_or_parser.def_token("whitespace", r"\s+", ignore=True) # note + NOT *

    whitespace_tokens = [
            ("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
            ("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
            ]
    parser.def_multi_ignored_tokens(whitespace_tokens)

    token_list = [
            #("k_float", r"\d+"),
            #("k_float", r"[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?"),
            # https://docs.python.org/2/library/re.html#simulating-scanf
            # But cannot use Python doc form exactly or 4-4 (without space) would fail.
            ("k_float", r"(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?"),

            ("k_imag_number", r"\d+[i]"),
            ("k_double_ast", r"(?:\*\*|\^)"), # Note ^ is defined as a synonym.
            ("k_plus", r"\+"),
            ("k_minus", r"\-"),
            ("k_fslash", r"/"),
            ("k_ast", r"\*"),
            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_comma", r","),
            ("k_bang", r"!"),
            ("k_question", r"\?"),
            ("k_colon", r"\:"),
            ("k_semicolon", r";")
            ]
    parser.def_multi_tokens(token_list)

    # NOTE that the exponentiation function could alternately be defined twice,
    # once for a ** token as the operator and once for a ^ token.  (They can
    # both be given the same AST label.)  What is done here instead is to
    # define multiple symbols for a single token (via the regex), making ^ an
    # alias for **.

def define_basic_calculator_syntax(parser):
    Assoc = pp.Assoc # Enum for association.

    #
    # Literals.
    #

    parser.def_literal("k_float", eval_fun=lambda t: float(t.value))
    #parser.def_literal("k_imag_number", ast_label="a_imag_number")
    #parser.def_literal("k_identifier", ast_label="a_variable")

    #
    # Standard functions.
    #

    parser.def_token("k_sin", r"sin")
    parser.def_stdfun("k_sin", "k_lpar", "k_rpar", "k_comma",
                      eval_fun=lambda t: math.sin(t[0].eval_subtree()))

    #
    # Operators.
    #

    # TODO: consider defining a shortcut in pp which applies some fun to all
    # the evals of children.  Just do a comprehension evaluating them all,
    # and then pass the *eval_value_list to the function!!!!!  (And return
    # the function which does that, so it can be set here.)  May make it
    # more appealing to use the group-apply methods on these things.

    prefix_operators = [ # TODO consider using or removing
            ("k_plus", 10, Assoc.left),
            ("k_minus", 10, Assoc.left),
            ]
    parser.def_prefix_op("k_plus", 100, ast_label="a_positive",
            eval_fun=lambda t: operator.pos(t[0].eval_subtree()))
    parser.def_prefix_op("k_minus", 100, ast_label="a_negative",
            eval_fun=lambda t: operator.neg(t[0].eval_subtree()))

    parser.def_infix_op("k_plus", 10, Assoc.left,
            eval_fun=lambda t: operator.add(t[0].eval_subtree(), t[1].eval_subtree()))
    # NOTE that the float def already defines prefix negative when 
    parser.def_infix_op("k_minus", 10, Assoc.left, ast_label="a_subtract",
            eval_fun=lambda t: operator.sub(t[0].eval_subtree(), t[1].eval_subtree()))

    parser.def_infix_op("k_ast", 20, Assoc.left, ast_label="a_mult",
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))
    parser.def_infix_op("k_fslash", 20, Assoc.left, ast_label="a_divide",
            eval_fun=lambda t: operator.truediv(t[0].eval_subtree(), t[1].eval_subtree()))

    parser.def_postfix_op("k_bang", 100, allow_ignored_before=False, ast_label="factorial",
            eval_fun=lambda t: math.factorial(t[0].eval_subtree()))

    parser.def_infix_op("k_double_ast", 30, Assoc.right, ast_label="a_exp",
            eval_fun=lambda t: operator.pow(t[0].eval_subtree(), t[1].eval_subtree()))

    #parser.def_infix_multi_op(["k_question", "k_colon"], 90, Assoc.right,
    #                          ast_label="a_ternary_conditional")

    #
    # Jop as synonym for multiplication.
    #

    #parser.def_jop_token("k_jop", "k_space") # Space token required to infer a jop.
    parser.def_jop_token("k_jop", None) # Space token NOT required to infer a jop. TODO
    parser.def_jop(20, Assoc.left,
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Parens and brackets.
    #

    parser.def_bracket_pair("k_lpar", "k_rpar", 0, ast_label="paren_brackets",
            eval_fun=lambda t: t[0].eval_subtree())

    #parser.define_comma_list("k_comma", 5, Assoc.right, ast_label="comma_list")
    parser.def_infix_multi_op(["k_comma"], 5, Assoc.left,
                              in_tree=False, repeat=True, ast_label="comma_list")

    parser.def_infix_multi_op(["k_semicolon"], 3, Assoc.left,
                              repeat=True, ast_label="a_statements")

    #
    # Comments (defined via an ignored token pattern).
    #

    # Note that comment_to_endline is non-greedy due to *? symbol.
    #parser.def_ignored_token("k_comment_to_EOL", r"\#[.]*", on_ties=2)
    parser.def_ignored_token("k_comment_to_EOL", r"\#[^\r\n]*$", on_ties=10)

def read_eval_print_loop(parser):
    try:
        read_input = raw_inp
    except NameError:
        read_input = input

    print("Enter ^C to exit.")
    while True:
        try:
           line = read_input("> ")
        except KeyboardInterrupt:
            print("\nBye.")
            break

        if not line: continue
        try:
           parse_tree = parser.parse(line)
           eval_value = parse_tree.eval_subtree()
           # TODO: this still doesn't catch ValueError in evaluating 3.3!
        except pp.ParserException as e:
            print(e)
            raise
        except pp.LexerException as e:
            print(e)
        except KeyboardInterrupt:
            print("Bye.")
            break
        else:
            print(parse_tree.tree_repr())
            print(eval_value)
        

def define_identifier_token(lexer_or_parser):
    # The last part of below only needs \w, but commented-out line is a good
    # example of using a pattern.
    #lexer_or_parser.def_token("k_identifier", r"[a-zA-Z_](?:[\w|\d]*)", on_ties=-1)
    # Note the on_ties is set to -1 so directly defined identifiers have precedence.
    lexer_or_parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

def define_and_run_basic_calculator():
    import readline
    parser = pp.PrattParser()
    define_basic_calculator_tokens(parser)
    define_basic_calculator_syntax(parser)
    read_eval_print_loop(parser)

define_and_run_basic_calculator()

