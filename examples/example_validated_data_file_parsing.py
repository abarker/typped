#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

This example defines a parser to read and validate a simple data file format.
The data file is assumed to have lines in this simple format::

    float "," int ":" int ["," int]*

So a line has a float, then a comma, and int and then a colon, and then one or more
integers separated by commas.

Alternate way: use comma as operator... consider others too.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper
import typped as pp

from typped import Varargs, HEAD

def setup_parser():
    parser = pp.PrattParser()

    parser.def_default_whitespace()
    parser.def_default_single_char_tokens(",:")
    parser.def_default_float_token()

    # Ints are preferred over floats of same length.
    parser.def_default_int_token(on_ties=1)

    # TODO: How do we check the type of the float which triggers the construct?
    # We don't, we ASSIGN a type to the root of the construct in the typesig of
    # the construct.  It is checked at higher level if an arg (it isn't here)
    # but is DEFINED in this case for floats in this context.  Like literals
    # define types for literals in a context...  Maybe explain better in docs.
    #
    # Whenever you define a val_type for a node you are DEFINING an association
    # between a type and a node.  Whenever you define arg_types you are asserting
    # the types of the child nodes, which will be checked as each subtree is
    # returned by a handler function.
    #
    # Add above para or similar to types documentation.

    t_float = parser.def_type("t_float")
    t_int = parser.def_type("t_int")
    parser.def_literal("k_int", val_type=t_int)

    def precond_fun(lex, lookbehind):
        return lex.token.is_first_on_line

    def handler_fun(tok, lex):
        """Handler is for the float token; read all the rest on line."""
        lex.match_next("k_comma", raise_on_fail=True)
        tok.append_children(tok.recursive_parse(subexp_prec=0))
        lex.match_next("k_colon", raise_on_fail=True)
        tok.append_children(tok.recursive_parse(subexp_prec=0))
        while lex.match_next("k_comma"):
            tok.append_children(tok.recursive_parse(subexp_prec=0))
        return tok

    parser.def_construct(HEAD, handler_fun, "k_float", "parse_line",
                         precond_fun=precond_fun,
                         val_type=t_float,
                         arg_types=[t_int, t_int, Varargs(t_int)])
    return parser

def run_parser():
    parser = setup_parser()

    with open("zzz_example_outfile.dat", "w") as f:
        print("-334.0, -54 :  4, 55", file=f)
        #print("33.4 , 5, :  4, 55", file=f) # TODO test this error case
        print("3003.4, 3 :  9, 449", file=f)

    with open("zzz_example_outfile.dat", "r") as f:
        for line in f:
            print(line)
            tree = parser.parse(line)
            print(tree.tree_repr())

run_parser()

