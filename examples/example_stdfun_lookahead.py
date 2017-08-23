# -*- coding: utf-8 -*-
"""

This is an example from the documentation.  It parses standard function calls
such as `f(x)` by a precondition on a head-handler for the function-name token
rather than by defining a tail-handler for `(` as an infix operator.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

import math
import operator
import typped as pp

# TODO: another example where a regular function is defined.

def define_parser_subclass():

    class MyParser(pp.PrattParser):
        """Subclass and add a new method to the `PrattParser` class as an example."""

        def __init__(self, *args, **kwargs):
            """Call the superclass initializer."""
            super(MyParser, self).__init__(*args, **kwargs)

        def def_stdfun_lookahead(self, fname_token_label, lpar_token_label,
                                 rpar_token_label, comma_token_label, num_args,
                                 precond_priority=1):
            """Define a standard function with a fixed number of arguments."""

            # Define the preconditions function.
            def preconditions(lex, lookbehind):
                peek_tok = lex.peek()
                if peek_tok.ignored_before: # No space allowed between name and lpar.
                    return False
                if peek_tok.token_label != lpar_token_label:
                    return False
                return True

            # Define the head-handler function.
            def head_handler(tok, lex):
                # Below match_next is for a precondition, so it will match and consume.
                lex.match_next(lpar_token_label, raise_on_fail=True)

                # Read comma-separated subexpressions as arguments.
                for i in range(num_args-1):
                    tok.append_children(tok.recursive_parse(0))
                    lex.match_next(comma_token_label, raise_on_fail=True)
                    lex.match_next(rpar_token_label, raise_on_success=True) # Error.
                if num_args != 0:
                    tok.append_children(tok.recursive_parse(0))
                # Consume closing paren.
                lex.match_next(rpar_token_label, raise_on_fail=True)
                return tok

            # Register the construct with the parser.
            precond_label = "function call using precondition on function name"
            self.def_construct(pp.HEAD, head_handler, fname_token_label, prec=0,
                               precond_label=precond_label,
                               precond_fun=preconditions,
                               precond_priority=precond_priority)
    return MyParser

def define_grammar(MyParser):
    parser = MyParser()
    parser.def_default_whitespace()

    tok = parser.def_token
    tok("k_number", r"\d+"),
    tok("k_lpar", r"\("),
    tok("k_rpar", r"\)"),
    tok("k_comma", r","),
    tok("k_add", r"add"),
    tok("k_sub", r"sub"),

    lit = parser.def_literal
    lit("k_number")
    lit("k_lpar")
    lit("k_rpar")

    parser.def_stdfun_lookahead("k_add", "k_lpar", "k_rpar", "k_comma", 2)
    parser.def_stdfun_lookahead("k_sub", "k_lpar", "k_rpar", "k_comma", 2)

    return parser

if __name__ == "__main__":

    print()
    MyParser = define_parser_subclass()
    parser_instance = define_grammar(MyParser)

    expr = "add(4, sub(5,6))"
    print("Parse of", expr, "\n")
    expr_tree = parser_instance.parse(expr)
    print(expr_tree.tree_repr(indent=3))

    expr = "add(add(2,4), sub(5,6))"
    print("Parse of", expr, "\n")
    print(parser_instance.parse(expr).tree_repr(indent=3))

