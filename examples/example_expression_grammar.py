# -*- coding: utf-8 -*-
"""

A simple example of an expression grammar using the EBNF overload language.

"""

# TODO NOT FINISHED AT ALL, and not used.  Clean up and use or delete.  Meant
# as simple example on doc page.

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run(self_test=True, pytest_args="-vv")

pytest_helper.autoimport()
#pytest_helper.sys_path("../src")

from typped import * # Assumes typped is installed locally.

def def_expression_tokens_and_literals(parser):
    """Define some general tokens."""
    #
    # Define the tokens and literals.
    #

    # Operators.
    k_plus = parser.def_token("k_plus", r"\+")
    k_minus = parser.def_token("k_minus", r"\-")
    k_fslash = parser.def_token("k_fslash", r"/")
    k_ast = parser.def_token("k_ast", r"\*")

    # Grouping.
    k_lpar = parser.def_token("k_lpar", r"\(")
    k_rpar = parser.def_token("k_rpar", r"\)")

    # Numbers.
    k_number = parser.def_token("k_number", r"[1-9][0-9]*")

    #
    # Define the terminal symbols.
    #

    literals_list = [
            ("k_plus",),
            ("k_minus",),
            ("k_fslash",),
            ("k_ast",),
            ("k_lpar",),
            ("k_rpar",),
            ("k_number",),
            ]
    parser.def_multi_literals(literals_list)
    pytest_helper.locals_to_globals()

    parser = PrattParser()
    parser.def_default_whitespace()
    def_expression_tokens_and_literals(parser)
    g = Grammar()

    expression = ( Rule("term1") + Tok("k_plus") + Rule("term2")
                 | Rule("term3") + k_minus + Rule("term4")
                 | Rule("term5")
                 )

    term = ( Rule("factor1") + k_ast + Rule("factor2")
           | Rule("factor3") + k_fslash + Rule("factor4")
           | Rule("factor5")
           )

    factor = ( k_number
             | k_lpar + Rule("expression") + k_rpar
             )

    parser = PrattParser()
    parser.def_default_whitespace()

    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.  Basic expression parsing of these expressions is
    # checked in previous test.
    #

    g = Grammar()

    expression = ( Rule("term") + Tok("k_plus") + Rule("term")
                 | Rule("term") + k_minus + Rule("term")
                 | Rule("term")
                 )

    term = ( Rule("factor") + k_ast + Rule("factor")
           | Rule("factor") + k_fslash + Rule("factor")
           | Rule("factor")
           )

    factor = ( k_number
             | k_lpar + Rule("expression") + k_rpar
             )

    #
    # Parse some expressions.
    #

    g.compile("expression", parser, locals())

    #parser.pstate_stack = ["expression"]
    #parser.top_level_production

    simple_example1_parse = parser.parse(
                                "4*4", pstate="expression").string_tree_repr()

    assert simple_example1_parse == ("<k_null-string,'expression'>("
                                        "<k_null-string,'term'>("
                                             "<k_null-string,'factor'>("
                                                 "<k_number,'4'>),"
                                             "<k_ast,'*'>,"
                                             "<k_null-string,'factor'>("
                                                 "<k_number,'4'>)))")

    simple_example2 = "5 * (444 + 32)"
    simple_example2_parse = parser.parse(
                         simple_example2, pstate="expression").tree_repr(indent=7)

    assert simple_example2_parse == (
    """\
       <k_null-string,'expression'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_number,'5'>
               <k_ast,'*'>
               <k_null-string,'factor'>
                   <k_lpar,'('>
                   <k_null-string,'expression'>
                       <k_null-string,'term'>
                           <k_null-string,'factor'>
                               <k_number,'444'>
                       <k_plus,'+'>
                       <k_null-string,'term'>
                           <k_null-string,'factor'>
                               <k_number,'32'>
                   <k_rpar,')'>\n""")

    simple_example3 = "(5 + 99) * 388"
    simple_example3_parse = parser.parse(
                         simple_example3, pstate="expression").tree_repr(indent=7)

    assert simple_example3_parse == (
    """\
       <k_null-string,'expression'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_lpar,'('>
                   <k_null-string,'expression'>
                       <k_null-string,'term'>
                           <k_null-string,'factor'>
                               <k_number,'5'>
                       <k_plus,'+'>
                       <k_null-string,'term'>
                           <k_null-string,'factor'>
                               <k_number,'99'>
                   <k_rpar,')'>
               <k_ast,'*'>
               <k_null-string,'factor'>
                   <k_number,'388'>\n""")

    simple_example4 = "5 + 99 * 388"
    simple_example4_parse = parser.parse(
                         simple_example4, pstate="expression").tree_repr(indent=7)

    assert simple_example4_parse == (
    """\
       <k_null-string,'expression'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_number,'5'>
           <k_plus,'+'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_number,'99'>
               <k_ast,'*'>
               <k_null-string,'factor'>
                   <k_number,'388'>\n""")

    simple_example5 = "5 * 7 - 388"
    simple_example5_parse = parser.parse(
                         simple_example5, pstate="expression").tree_repr(indent=7)

    assert simple_example5_parse == (
    """\
       <k_null-string,'expression'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_number,'5'>
               <k_ast,'*'>
               <k_null-string,'factor'>
                   <k_number,'7'>
           <k_minus,'-'>
           <k_null-string,'term'>
               <k_null-string,'factor'>
                   <k_number,'388'>\n""")

