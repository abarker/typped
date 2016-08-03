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

def define_basic_calculator(parser):

    #
    # Some general tokens.
    #

    whitespace_tokens = [
            ("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
            ("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
            ]
    parser.def_multi_ignored_tokens(whitespace_tokens)

    token_list = [
            #("k_float", r"[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?"),
            # Above from: https://docs.python.org/2/library/re.html#simulating-scanf
            # But cannot use Python doc form exactly or 4 -4 (jop for mult) would fail.
            ("k_float", r"(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?"),

            ("k_double_ast", r"(?:\*\*|\^)"), # Note ^ is defined as a synonym.
            ("k_plus", r"\+"),
            ("k_minus", r"\-"),
            ("k_fslash", r"/"),
            ("k_ast", r"\*"),
            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_lbrac", r"\["),
            ("k_rbrac", r"\]"),
            ("k_comma", r","),
            ("k_bang", r"!"),
            ("k_equals", r"="),
            ]
    parser.def_multi_tokens(token_list)

    # NOTE that the exponentiation function could alternately be defined twice,
    # once for a ** token as the operator and once for a ^ token as the
    # operator.  What is done here instead is to define multiple symbols for a
    # single token (via the regex), making ^ an alias for **.

    Assoc = pp.Assoc # Enum for association.

    #
    # Literals.
    #

    parser.def_literal("k_float", eval_fun=lambda t: float(t.value))

    #
    # Standard functions.
    #

    # Note we use the None argument types to check the number of arguments.
    parser.def_token("k_sin", r"sin")
    parser.def_stdfun("k_sin", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.sin(t[0].eval_subtree()))
    parser.def_token("k_cos", r"cos")
    parser.def_stdfun("k_cos", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.cos(t[0].eval_subtree()))
    parser.def_token("k_ln", r"ln")
    parser.def_stdfun("k_ln", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.log(t[0].eval_subtree()))
    parser.def_token("k_sqrt", r"sqrt")
    parser.def_stdfun("k_sqrt", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.sqrt(t[0].eval_subtree()))

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

    # TODO: consider defining a shortcut in pp which applies some fun to all
    # the evals of children.  Just do a comprehension evaluating them all,
    # and then pass the *eval_value_list to the function!!!!!  (And return
    # the function which does that, so it can be set here.)  May make it
    # more appealing to use the group-apply methods on these things.

    parser.def_prefix_op("k_plus", 50,
                         eval_fun=lambda t: operator.pos(t[0].eval_subtree()))
    parser.def_prefix_op("k_minus", 50,
                         eval_fun=lambda t: operator.neg(t[0].eval_subtree()))

    parser.def_postfix_op("k_bang", 40, allow_ignored_before=False,
                          eval_fun=lambda t: math.factorial(t[0].eval_subtree()))

    parser.def_infix_op("k_double_ast", 30, Assoc.right,
            eval_fun=lambda t: operator.pow(t[0].eval_subtree(), t[1].eval_subtree()))

    parser.def_infix_op("k_ast", 20, Assoc.left,
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))
    parser.def_infix_op("k_fslash", 20, Assoc.left,
            eval_fun=lambda t: operator.truediv(t[0].eval_subtree(), t[1].eval_subtree()))

    parser.def_infix_op("k_plus", 10, Assoc.left,
            eval_fun=lambda t: operator.add(t[0].eval_subtree(), t[1].eval_subtree()))
    parser.def_infix_op("k_minus", 10, Assoc.left,
            eval_fun=lambda t: operator.sub(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Jop as synonym for multiplication.
    #

    jop_required_token = "k_space" # Set to None to not require any whitespace.
    parser.def_jop_token("k_jop", jop_required_token)
    parser.def_jop(20, Assoc.left,
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))

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

    parser.def_infix_op("k_equals", 100, Assoc.right, ast_label="a_assign",
            eval_fun=eval_assign)

    #
    # Comments, all after '#' to EOL, defined via an ignored token pattern.
    #

    parser.def_ignored_token("k_comment_to_EOL", r"\#[^\r\n]*$", on_ties=10)


def read_eval_print_loop(parser):
    try:
        read_input = raw_input
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
        except pp.ParserException as e:
            print(e)
            continue
        except pp.LexerException as e:
            print(e)
            continue
        else:
            print(parse_tree.tree_repr())
            print(eval_value)
        

def define_and_run_basic_calculator():
    import readline
    parser = pp.PrattParser()
    define_basic_calculator(parser)
    read_eval_print_loop(parser)

define_and_run_basic_calculator()

