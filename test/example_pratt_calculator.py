# -*- coding: utf-8 -*-
"""

A simple calculator example using the Typped parser.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

#pytest_helper.script_run(self_test=True, pytest_args="-v")
#pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import math
import operator
import typped as pp

def define_basic_calculator(parser):

    #
    # Some general tokens.
    #

    #whitespace_tokens = [
    #        ("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
    #        ("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
    #        ]
    #parser.def_multi_ignored_tokens(whitespace_tokens)
    parser.def_default_whitespace() # Does the same as the commented-out lines above.

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

    # Note that the token for the exponentiation operator has multiple symbols
    # defined for it in its regex (both '**' and '^').  The same thing could
    # alternately be done by keeping the tokens separate and later defining a
    # handler function for each one, both of which do exponentiation.

    #
    # Literals.
    #

    parser.def_literal("k_float", eval_fun=lambda t: float(t.value))

    #
    # Standard functions.
    #

    parser.def_token("k_sin", r"sin")
    parser.def_stdfun("k_sin", "k_lpar", "k_rpar", "k_comma", num_args=1,
                      eval_fun=lambda t: math.sin(t[0].eval_subtree()))
    parser.def_token("k_cos", r"cos")
    parser.def_stdfun("k_cos", "k_lpar", "k_rpar", "k_comma", num_args=1,
                      eval_fun=lambda t: math.cos(t[0].eval_subtree()))
    parser.def_token("k_sqrt", r"sqrt")
    parser.def_stdfun("k_sqrt", "k_lpar", "k_rpar", "k_comma", num_args=1,
                      eval_fun=lambda t: math.sqrt(t[0].eval_subtree()))

    # Note that log is overloaded because different numbers of arguments are
    # specified, and they have different eval funs.
    parser.def_token("k_log", r"log")
    parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma", num_args=1,
                      eval_fun=lambda t: math.log(t[0].eval_subtree()))
    parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma", num_args=2,
               eval_fun=lambda t: math.log(t[0].eval_subtree(), t[1].eval_subtree()))

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
    parser.def_prefix_op("k_minus", 50,
                         eval_fun=lambda t: operator.neg(t[0].eval_subtree()))

    parser.def_postfix_op("k_bang", 40, allow_ignored_before=False,
                          eval_fun=lambda t: math.factorial(t[0].eval_subtree()))

    parser.def_infix_op("k_double_ast", 30, "right",
            eval_fun=lambda t: operator.pow(t[0].eval_subtree(), t[1].eval_subtree()))

    parser.def_infix_op("k_ast", 20, "left",
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))
    parser.def_infix_op("k_fslash", 20, "left",
            eval_fun=lambda t: operator.truediv(t[0].eval_subtree(), t[1].eval_subtree()))

    parser.def_infix_op("k_plus", 10, "left",
            eval_fun=lambda t: operator.add(t[0].eval_subtree(), t[1].eval_subtree()))
    parser.def_infix_op("k_minus", 10, "left",
            eval_fun=lambda t: operator.sub(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Juxtaposition operator (jop) as synonym for multiplication.
    #

    jop_required_token = "k_space" # Can be set to None to not require any whitespace.
    parser.def_jop_token("k_jop", jop_required_token)
    parser.def_jop(20, "left",
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Assign simple variable.
    #

    parser.calculator_symbol_dict = {} # Store symbol dict as a new parser attribute.
    symbol_dict = parser.calculator_symbol_dict

    symbol_dict["pi"] = math.pi # Predefine pi.
    symbol_dict["e"] = math.e # Predefine e.

    # Note that on_ties for identifiers is set to -1, so that when string
    # lengths are equal defined function names will take precedence over
    # identifiers (which are only defined as a group regex).
    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    parser.def_literal("k_identifier",
            eval_fun=lambda t: symbol_dict.get(t.value, 0.0))

    def eval_assign(t):
        rhs = t[1].eval_subtree()
        symbol_dict[t[0].value] = rhs
        return rhs

    parser.def_infix_op("k_equals", 5, "right", ast_label="a_assign",
                        eval_fun=eval_assign)

    #
    # Comments, all after '#' to EOL, defined via an ignored token pattern.
    #

    parser.def_ignored_token("k_comment_to_EOL", r"\#[^\r\n]*$", on_ties=10)


def read_eval_print_loop(parser):
    import readline

    try:
        read_input = raw_input # Python 2.
    except NameError:
        read_input = input # Python 3.

    print("Enter ^C to exit, and 'toggletree' to toggle tree display.")

    show_tree = False # Toggled in the loop below.
    while True:
        try:
            line = read_input("> ")
        except (KeyboardInterrupt, EOFError):
            print("\nBye.")
            break
        if not line:
            continue

        if line == "toggletree":
            show_tree = not show_tree
            continue

        try:
            parse_tree = parser.parse(line)
            eval_value = parse_tree.eval_subtree()
        except pp.CalledEndTokenHandler:
            continue # Comment on empty line, don't show error message.
        except (ValueError, ZeroDivisionError,
                pp.ParserException, pp.LexerException) as e:
            print(e)
            continue
        else:
            if show_tree:
                print("\n", parse_tree.tree_repr(), sep="")
            print(eval_value)


def define_and_run_basic_calculator():
    parser = pp.PrattParser()
    define_basic_calculator(parser)
    read_eval_print_loop(parser)

if __name__ == "__main__":

    define_and_run_basic_calculator()

