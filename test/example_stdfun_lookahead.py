# -*- coding: utf-8 -*-
"""

This is an example from the documentation.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

#pytest_helper.script_run(self_test=True, pytest_args="-v")
#pytest_helper.sys_path("../src")

import math
import operator
from typped import PrattParser

# Some naming conventions.
#
# Lexer:
#    tokens:    k_float
# Syntax:
#    types:     t_int
# AST label:
#    a_number


# TODO: another example where a regular function is defined.

def define_parser_subclass():

    class MyParser(PrattParser):
        """Subclass and add a new method to the `PrattParser` class as an example."""

        def __init__(self, *args, **kwargs):
            """Call the superclass initializer."""
            super(MyParser, self).__init__(*args, **kwargs)

        def def_stdfun_lookahead(self, fname_token_label, lpar_token_label,
                       rpar_token_label, comma_token_label, num_args,
                       precond_priority=1):
            """Define a standard function with a fixed number of arguments."""

            # Define the preconditions function and a unique label for it.
            def preconditions(lex, lookbehind):
                # Note that helper functions like `match_next` could also be used.
                peek_tok = lex.peek()
                if peek_tok.ignored_before: return False
                if peek_tok.token_label != lpar_token_label: return False
                return True
            precond_label = "lpar after, no whitespace between" # Some unique label.

            # Define the head-handler function.
            def head_handler(tok, lex):
                # Below match is for a precondition, so it will match and consume.
                lex.match_next(lpar_token_label, raise_on_fail=True)

                # Read comma-separated subexpressions as arguments.
                for i in range(num_args-1):
                    tok.append_children(tok.recursive_parse(0))
                    lex.match_next(comma_token_label, raise_on_fail=True)
                    lex.match_next(rpar_token_label, raise_on_true=True) # Error.
                if num_args != 0:
                    tok.append_children(tok.recursive_parse(0))
                lex.match_next(rpar_token_label, raise_on_fail=True)

                # Always call this function at the end of a handler function.
                tok.process_and_check_node(head_handler)
                return tok

            # Register the handler function with the token, associated with the
            # preconditions function.
            self.modify_token_subclass(fname_token_label, prec=0,
                                       head=head_handler,
                                       precond_label=precond_label,
                                       precond_fun=preconditions,
                                       precond_priority=precond_priority)
    return MyParser

def define_grammar(MyParser):
    parser = MyParser()
    parser.def_token("k_space", r"[ \t]+", ignore=True) # note + NOT *
    parser.def_token("k_newline", r"[\n\f\r\v]+", ignore=True) # note + NOT

    tokens = [("k_number", r"\d+"),
              ("k_lpar", r"\("),
              ("k_rpar", r"\)"),
              ("k_comma", r","),
              ("k_add", r"add"),
              ("k_sub", r"sub"),
             ]
    parser.def_multi_tokens(tokens)

    literals = [("k_number",),
                ("k_lpar",),
                ("k_rpar",),
               ]
    parser.def_multi_literals(literals)

    parser.def_stdfun_lookahead("k_add", "k_lpar", "k_rpar", "k_comma", 2)
    parser.def_stdfun_lookahead("k_sub", "k_lpar", "k_rpar", "k_comma", 2)

    return parser

if __name__ == "__main__":

    print()
    MyParser = define_parser_subclass()
    parser_instance = define_grammar(MyParser) # Use better names in real code.

    expr = "add(4, sub(5,6))"
    print("Parse of", expr, "\n")
    print(parser_instance.parse(expr).tree_repr(indent=3))

    expr = "add(add(2,4), sub(5,6))"
    print("Parse of", expr, "\n")
    print(parser_instance.parse(expr).tree_repr(indent=3))

