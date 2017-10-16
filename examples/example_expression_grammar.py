# -*- coding: utf-8 -*-
"""

A simple example of an expression grammar using the BNF overload language.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    # No test file for now, just run the parser's tests.
    #pytest_helper.script_run(self_test=True, pytest_args="-vv")
    pass

pytest_helper.autoimport() # Gets the unindent function.
#pytest_helper.sys_path("../src")

from typped import *

def example_from_Sphinx_recursive_descent_page():
    """This is an example from the Sphinx documentation."""
    import typped as pp
    parser = pp.PrattParser()
    parser.def_default_whitespace()
    parser.def_default_single_char_tokens()
    k_int = parser.def_default_int_token(signed=True)
    k_identifier = parser.def_default_identifier_token()
    literals = ["k_int", "k_identifier", "k_plus", "k_minus",
                "k_ast", "k_slash", "k_lpar", "k_rpar"]
    parser.def_multi_literals([lit,] for lit in literals)

    expression = ( Rule("term") + Tok("k_plus") + Rule("expression")
                 | Rule("term") + Tok("k_minus") + Rule("expression")
                 | Rule("term"))
    term       = ( Rule("factor") + Tok("k_ast") + Rule("term")
                 | Rule("factor") + Tok("k_slash") + Rule("term")
                 | Rule("factor"))
    factor     = ( Rule("constant")
                 | Rule("variable")
                 | Tok("k_lpar") + Rule("expression") + Tok("k_rpar"))
    variable   = k_identifier
    constant   = k_int

    g = pp.Grammar("expression", parser, locals())

    print("\nThe evaluated grammar is:\n")
    g.print_grammar()

    test_string = "4 + my_var * (3 - 1)"
    print("\n\nParse of the string '{0}':".format(test_string))
    tree = parser.parse("4 + my_var * (3 - 1)", pstate="expression")
    print()
    print(tree.tree_repr())

    assert tree.tree_repr() == unindent(8, """
        <k_null-string,'expression'>
            <k_null-string,'term'>
                <k_null-string,'factor'>
                    <k_null-string,'constant'>
                        <k_int,'4'>
            <k_plus,'+'>
            <k_null-string,'expression'>
                <k_null-string,'term'>
                    <k_null-string,'factor'>
                        <k_null-string,'variable'>
                            <k_identifier,'my_var'>
                    <k_ast,'*'>
                    <k_null-string,'term'>
                        <k_null-string,'factor'>
                            <k_lpar,'('>
                            <k_null-string,'expression'>
                                <k_null-string,'term'>
                                    <k_null-string,'factor'>
                                        <k_null-string,'constant'>
                                            <k_int,'3'>
                                <k_minus,'-'>
                                <k_null-string,'expression'>
                                    <k_null-string,'term'>
                                        <k_null-string,'factor'>
                                            <k_null-string,'constant'>
                                                <k_int,'1'>
                            <k_rpar,')'>

        """)

    return parser

def def_expression_tokens_and_literals(parser):
    """Define some general tokens."""
    # TODO: THIS IS OLDER STUFF, may not work, need to clean up, move, or remove.

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

if __name__ == "__main__":

    example_from_Sphinx_recursive_descent_page()
