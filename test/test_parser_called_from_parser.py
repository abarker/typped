# -*- coding: utf-8 -*-
"""

Define a wff parser that uses a sub-parser inside it (called from handler
functions) to parse terms.

An EBNF grammar for a wff in a simple logic (with predicates left off) is:
::
    <wff> = <atomic_formula>
          | 'not' <wff>
          | '(' <wff> ')'
          | <wff> <infix_logical_operator> <wff>

    <atomic_formula> = <predicate_name> '(' <term_list> ')'

    <term> = '(' <term> ')'
           | <function_eval>
           | <variable_name>
           | <constant_name>

This test case defines a parser for terms and then calls it from a handler for
wffs.  It uses standard Pratt-style parsing for parsing in both parsers.

Note that to use a parser as an inner parser it needs to be able to parse
subexpressions which do not necessarily extend to the end-of-text end-token.
Set `partial_expressions=True` in the inner parsers so they will return partial
results.  This can also be done dynamically, by modifying the attribute of
the parser instance.

The token definition spaces for different parsers are completely distinct.  To
use two parsers together, though, the outer parser needs to have defined at
least enough tokens of the inner parser to do any necessary peeks into
expressions of that sublanguage.  The precedences, etc., of those tokens also
need to be correct or at least interact correctly (generally the head of a
subexpression starts with a precedence of 0).  So the outer parser should at
least have a token for each beginning-token of an expression in the
sublanguage.  They generally do not need to be assigned handlers (such as
by declaring them literals).

If the token spaces can be identical without causing problems then you can just
define a function to make the token definitions and then call it for each
parser.

Using jops may or may not work in combination with inner and outer parsers.
They will probably work in the inner parser, at least.  The possible problem
is that they check for head handlers before deciding to infer a jop.

TODO: Types are not yet worked as to how they should interact in the
type-checking.  Currently types not used in this example.

"""
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")
pytest_helper.sys_path("../examples")

import typped as pp
from example_parser_called_from_parser import *

def test_basic_example():
    term_parser = define_term_parser() # Define a parser for terms.
    test_term = "f(x,x33)"
    parse_tree = "\n" + term_parser.parse(test_term).tree_repr(indent=4)
    assert parse_tree == ("""
    <k_funname,'f'>
        <k_varname,'x'>
        <k_varname,'x33'>\n""")

    wff_parser = define_wff_parser(term_parser) # Define a parser for wffs.
    test_wff = "not A(f(x,x33))"
    parse_tree = "\n" + wff_parser.parse(test_wff).tree_repr(indent=4)
    assert parse_tree == ("""
    <k_not,'not'>
        <k_predname,'A'>
            <k_funname,'f'>
                <k_varname,'x'>
                <k_varname,'x33'>\n""")

    test_wff = "not A(f(x,x33)) and A44() and A9(x1, c2, f(c1))"
    print("parsing wff:", test_wff, "\n")
    print(wff_parser.parse(test_wff).tree_repr(indent=3))
    parse_tree = "\n" + wff_parser.parse(test_wff).tree_repr(indent=4)
    assert parse_tree == ("""
    <k_and,'and'>
        <k_and,'and'>
            <k_not,'not'>
                <k_predname,'A'>
                    <k_funname,'f'>
                        <k_varname,'x'>
                        <k_varname,'x33'>
            <k_predname,'A44'>
        <k_predname,'A9'>
            <k_varname,'x1'>
            <k_constname,'c2'>
            <k_funname,'f'>
                <k_constname,'c1'>\n""")


