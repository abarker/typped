# -*- coding: utf-8 -*-
"""

A simple calculator example using the Typped parser.  This module is described
in the general Typped documentation web page as a calculator example.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

import math
import operator
import typped as pp

def define_general_tokens_and_literals(parser):
    """Define some general tokens and literals in the calculator language.
    Other tokens such as for functions in the language will be defined
    later."""
    #
    # Tokens.
    #

    # tok("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
    # tok("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
    parser.def_default_whitespace() # Does the same as the commented-out lines above.

    tok = parser.def_token
    tok("k_plus", r"\+")
    tok("k_minus", r"\-")
    tok("k_fslash", r"/")
    tok("k_ast", r"\*")
    tok("k_lpar", r"\(")
    tok("k_rpar", r"\)")
    tok("k_lbrac", r"\[")
    tok("k_rbrac", r"\]")
    tok("k_comma", r",")
    tok("k_bang", r"!")
    tok("k_equals", r"=")

    # The token for the exponentiation operator has multiple symbols defined
    # for it in its regex (both '**' and '^').  The same thing could
    # alternately be done by keeping the tokens separate and later defining a
    # handler function for each one, both of which do exponentiation.
    tok("k_double_ast", r"(?:\*\*|\^)") # Note ^ is a synonym for **.

    # This token definition for a float is based on the regex from
    # https://docs.python.org/2/library/re.html#simulating-scanf
    #   r"[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?"
    # But if we used the Python doc form exactly then 4 -4 would be interpreted
    # as a multiplication jop for rather than correctly, as subtraction.  So
    # the [+-] part is left off and is implemented as a prefix operator instead.
    tok("k_float", r"(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?")

    #
    # Literals.
    #

    parser.def_literal("k_float", eval_fun=lambda t: float(t.value))

def define_functions_and_operators(parser):
    """Define the all the functions and operators for the calculator.
    Evaluation functions are also supplied for each one.  Parentheses and
    brackets are also defined here, since they have a precedence in the order
    of evaluations."""

    #
    # Parens and brackets, highest precedence (since they have a head function).
    #

    parser.def_bracket_pair("k_lpar", "k_rpar",
                            eval_fun=lambda t: t[0].eval_subtree())
    parser.def_bracket_pair("k_lbrac", "k_rbrac",
                            eval_fun=lambda t: t[0].eval_subtree())

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
    # specified.  The two versions have different eval funs.
    parser.def_token("k_log", r"log")
    log_fun_construct = parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma",
                          num_args=1, eval_fun=lambda t: math.log(t[0].eval_subtree()))
    log_fun_construct.overload(num_args=2,
                 eval_fun=lambda t: math.log(t[0].eval_subtree(), t[1].eval_subtree()))

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

def define_juxtaposition_operators(parser):
    """Define the juxtaposition operator (jop) as synonym for multiplication."""

    jop_required_token = "k_space" # Can be set to None to not require any whitespace.
    parser.def_jop_token("k_jop", jop_required_token)
    parser.def_jop(20, "left", # Same precedence and assoc. as ordinary multiplication.
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))

def define_assignment_operator(parser):
    """Define assignment and reading of simple variables."""

    parser.calculator_symbol_dict = {} # Store symbol dict as a new parser attribute.
    symbol_dict = parser.calculator_symbol_dict

    symbol_dict["pi"] = math.pi # Predefine pi.
    symbol_dict["e"] = math.e # Predefine e.

    # Note that on_ties for identifiers is set to -1, so that when string
    # lengths are equal defined function names will take precedence over generic
    # identifiers (which are only defined as a group regex).
    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    parser.def_literal("k_identifier", eval_fun=lambda t: symbol_dict.get(t.value, 0.0))

    def eval_assign(t):
        """Evaluate the identifier token `t` and save the value in `symbol_dict`."""
        rhs = t[1].eval_subtree()
        symbol_dict[t[0].value] = rhs
        return rhs

    parser.def_infix_op("k_equals", 5, "right",
                precond_fun=lambda tok, lex: lex.peek(-1).token_label == "k_identifier",
                eval_fun=eval_assign)

def define_comments(parser):
    """Define comments in the calculator.  Everything from '#' to EOL is a
    comment.  Defined using an ignored token pattern."""

    parser.def_ignored_token("k_comment_to_EOL", r"\#[^\r\n]*$", on_ties=10)

def define_semicolon_separator(parser):
    """Define semicolon to separate expressions, returning the value of the last one."""

    def eval_semicolon(t):
        t[0].eval_subtree()
        return t[1].eval_subtree()

    parser.def_token("k_semicolon", r";")
    parser.def_literal("k_semicolon")
    parser.def_infix_op("k_semicolon", 1, "right",
                        eval_fun=eval_semicolon)

def define_basic_calculator(parser):
    """Define the calculator language in the parser instance."""
    define_general_tokens_and_literals(parser)
    define_functions_and_operators(parser)
    define_juxtaposition_operators(parser)
    define_assignment_operator(parser)
    define_comments(parser)
    define_semicolon_separator(parser)

def read_eval_print_loop(parser):
    """Implement the REP loop."""
    import readline

    try:
        read_input = raw_input # Python 2.
    except NameError:
        read_input = input # Python 3.

    print("Enter ^C to exit, and 'toggle' to toggle tree display.")

    show_tree = False # Toggled in the loop below.
    while True:
        try:
            line = read_input("> ")
        except (KeyboardInterrupt, EOFError):
            print("\nBye.")
            break
        if not line:
            continue
        if line == "toggle":
            show_tree = not show_tree
        elif line.strip().startswith("#"): # Tries to parse empty line.
            continue

        try:
            parse_tree = parser.parse(line)
            eval_value = parse_tree.eval_subtree()
        except (ValueError, ZeroDivisionError,
                pp.ParserException, pp.LexerException) as e:
            print(e)
            continue

        if show_tree:
            print("\n", parse_tree.tree_repr(), sep="")
        print(eval_value)

def cmd_read_evaluate_print_loop(parser):
    """Use the Python library module `cmd` to do the read, evaluate, print loop."""
    import cmd
    import readline

    class CalculatorREPL(cmd.Cmd, object):
        """Simple command processor example."""
        prompt = "> "
        intro = ("Enter ^D to exit the calculator, 'help' for help. "
                 " Toggle tree display with 'toggle'.")

        def __init__(self):
            self.show_tree = False
            super(CalculatorREPL, self).__init__()

        def emptyline(self):
            pass

        def default(self, line):
            """Default action, parse and evaluate and expression."""
            if line.strip().startswith("#"): # Tries to parse empty line.
                return
            try:
                parse_tree = parser.parse(line)
                eval_value = parse_tree.eval_subtree()
            except (ValueError, ZeroDivisionError,
                    pp.ParserException, pp.LexerException) as e:
                print(e)
                return

            if self.show_tree:
                print("\n", parse_tree.tree_repr(), sep="")

            print(eval_value)

        def do_toggle(self, line):
            """Toggle printing of expression trees."""
            self.show_tree = not self.show_tree

        def do_EOF(self, line):
            """Exit on EOF."""
            print("\nBye.")
            return True

    CalculatorREPL().cmdloop()


def define_and_run_basic_calculator():
    """Get a parser, define the calculator language, and start the REP loop."""
    parser = pp.PrattParser()
    define_basic_calculator(parser)
    #read_eval_print_loop(parser)
    cmd_read_evaluate_print_loop(parser)

if __name__ == "__main__":

    define_and_run_basic_calculator()

