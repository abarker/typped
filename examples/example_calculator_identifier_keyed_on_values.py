# -*- coding: utf-8 -*-
"""

A simple calculator example using the Typped parser.  This module is described
in the general Typped documentation web page as a calculator example.

This example is almost the same as that in `example_calculator.py` except that
it uses a single identifier token for functions like `sin` and `cos`.  The
other example defines a new token for each one.  This example uses the
`value_key` keyword argument to allow for looking up the different evaluation
functions.

There are two ways to set the `value_key` (of, say, `"cos"`) for a construct.
You can call the defining builtin `def_stdfun` each time, with only the
`value_key` value changing.  That is often the easiest way.  Alternately, you
can save the returned construct and just call its `save_eval_fun` method.  Both
ways are implemented below.  To switch between them you can change the
`REDEFINE_STDFUN_FOR_KEY_VALUE` variable's value.

Note that the overloading on `log` works because the `num_args` keyword of
`def_stdfun` is actually causing the `arg_types` for the construct to be set.
When calling the construct's `save_eval_fun` method in this case you need to
explicitly provide the corresponding `TypeSig` instances.  Also, you need to
call `def_stdfun` for both the one-argument version of `log` and the
two-argument version.  This is because the type signature for parsing the
construct itself needs to be set.  Once you have a one-argument standard
function you can add `value_key` strings to it, but for a two-place standard
function you first need a new construct for two-argument standard functions.
Then you can add the `value_key` to that.

In this calculator implementation identifiers like `cos` and `sin` which
represent functions in the language can also be assigned values as variables.
This does not happen in the other version of the calculator because the tokens
for functions in the language have a higher `on_ties` value than identifiers
(which set it to -1).  Along with a jop, and assuming `cos=4` has been
executed, the string `cos (4)` evaluates to `16`, but `cos(4)` evaluates to the
cosine of 16, so the space is significant.  To prevent this some further
language restrictions would need to be implemented.  You could have separate
namespaces, or some priority mechanism.  You could define two kinds of tokens,
for example: one for keywords like `sin` and `cos` and one for identifiers with
the keywords token having a higher `on_ties` value.

"""

# TODO: Make automated testfile for this example in the test dir.

from __future__ import print_function, division, absolute_import
import pytest_helper

import math
import operator
import typped as pp

REDEFINE_STDFUN_FOR_KEY_VALUE = False

def define_general_tokens_and_literals(parser):
    """Define some general tokens and literals in the calculator language.
    Other tokens such as for functions in the language will be defined
    later."""
    #
    # Tokens.
    #

    #whitespace_tokens = [
    #        ("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
    #        ("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
    #        ]
    #parser.def_multi_ignored_tokens(whitespace_tokens)
    parser.def_default_whitespace() # Does the same as the commented-out lines above.

    token_list = [
            # This token tuple for a float is based on the regex from:
            # https://docs.python.org/2/library/re.html#simulating-scanf
            #   ("k_float", r"[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?")
            # But if we used the Python doc form exactly then 4 -4 would be interpreted
            # as a multiplication jop for rather than correctly, as subtraction.  So
            # the [+-] part is left off and is implemented as a prefix operator instead.
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

    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)")
    stdfun = parser.def_stdfun

    # The two different ways to set value_key: either call def_stdfun each time with
    # the full arguments, or else save the construct and call its save_eval_fun method.
    if REDEFINE_STDFUN_FOR_KEY_VALUE:
        stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma", num_args=1,
               eval_fun=lambda t: math.sin(t[0].eval_subtree()), value_key="sin")
        stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma", num_args=1,
               eval_fun=lambda t: math.cos(t[0].eval_subtree()), value_key="cos")
        stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma", num_args=1,
               eval_fun=lambda t: math.sqrt(t[0].eval_subtree()), value_key="sqrt")

        # Note that log is overloaded because different numbers of arguments are
        # specified.  The two versions have different eval funs.
        stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma", num_args=1,
               eval_fun=lambda t: math.log(t[0].eval_subtree()), value_key="log")
        stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma", num_args=2,
               eval_fun=lambda t: math.log(t[0].eval_subtree(), t[1].eval_subtree()),
               value_key="log")

    else:
        stdfun_construct_1 = stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma",
                               num_args=1,
                               eval_fun=lambda t: math.sin(t[0].eval_subtree()),
                               value_key="sin")
        stdfun_construct_1.save_eval_fun(lambda t: math.cos(t[0].eval_subtree()),
                                       pp.TypeSig(None, [None]), value_key="cos")
        stdfun_construct_1.save_eval_fun(lambda t: math.cos(t[0].eval_subtree()),
                                       pp.TypeSig(None, [None]), value_key="cos")
        stdfun_construct_1.save_eval_fun(lambda t: math.sqrt(t[0].eval_subtree()),
                                       pp.TypeSig(None, [None]), value_key="sqrt")

        # Note that we need to explicitly specify the type when overloading.
        stdfun_construct_1.save_eval_fun(lambda t: math.log(t[0].eval_subtree()),
                                       pp.TypeSig(None, [None]), value_key="log")
        stdfun_construct_2 = stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma",
               num_args=2,
               eval_fun=lambda t: math.log(t[0].eval_subtree(), t[1].eval_subtree()),
               value_key="log")

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

    parser.def_literal("k_identifier", eval_fun=lambda t: symbol_dict.get(t.value, 0.0))

    def eval_assign(t):
        """Evaluate the identifier token `t` and save the value in `symbol_dict`."""
        rhs = t[1].eval_subtree()
        symbol_dict[t[0].value] = rhs
        return rhs

    parser.def_infix_op("k_equals", 5, "right",
                precond_fun=lambda lex, lb: lex.peek(-1).token_label == "k_identifier",
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

