# -*- coding: utf-8 -*-
"""

Test the code in the production_rules.py file.

"""

# TODO: install typped in pip

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run(self_test=True, pytest_args="-vv")

pytest_helper.autoimport()
#pytest_helper.sys_path("../src")

from typped import * # Assumes pip install, local for devel work.

def def_expression_tokens_and_literals(parser):
    #
    # Define the tokens.
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
    # Define the literals (terminals).
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

def test_EBNF_like_expressions():
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

    assert str(expression) == ('CaseList(ItemList(Rule("term1"), '
                                                'Tok("k_plus"), '
                                                'Rule("term2")), '
                                       'ItemList(Rule("term3"), '
                                                'Tok("k_minus"), '
                                                'Rule("term4")), '
                                       'ItemList(Rule("term5")))')

    assert str(term) == ('CaseList(ItemList(Rule("factor1"), '
                                           'Tok("k_ast"), '
                                           'Rule("factor2")), '
                                 'ItemList(Rule("factor3"), '
                                           'Tok("k_fslash"), '
                                           'Rule("factor4")), '
                                 'ItemList(Rule("factor5")))')

    assert str(factor) == ('CaseList(ItemList(Tok("k_number")), '
                                    'ItemList(Tok("k_lpar"), '
                                             'Rule("expression"), '
                                             'Tok("k_rpar")))')

def test_parsing_from_basic_expression_grammar():
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

def test_shortcut_operator_overloads_in_expression_grammar():
    parser = PrattParser()
    parser.def_default_whitespace()

    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #

    print("token number now looks like this", k_number)
    t_float = parser.def_type("t_float")
    none_sig = TypeSig(t_float, None)

    tok_str = Tok(k_number)
    assert tok_str.kind_of_item == "token"
    assert str(Tok(k_number)) == 'Tok("k_number")'
    assert str(k_number + Rule("factor")) == 'ItemList(Tok("k_number"), Rule("factor"))'
    assert str(~Tok(k_number) + Sig(Rule("factor"), none_sig) | Rule("term")) == (
            'CaseList(ItemList(Not(Tok("k_number")), Rule("factor")(t_float, None)), '
            'ItemList(Rule("term")))')
    assert str(Tok(k_number) + Rule("factor") | Root(Rule("term")) | Pratt() | k_number) == (
            'CaseList(ItemList(Tok("k_number"), Rule("factor")), '
            'ItemList(Root(Rule("term"))), ItemList(Pratt("None")), '
            'ItemList(Tok("k_number")))')
    assert str(k_number | k_number | Root(Rule("term")) | Pratt()) == (
            'CaseList(ItemList(Tok("k_number")), ItemList(Tok("k_number")), '
            'ItemList(Root(Rule("term"))), ItemList(Pratt("None")))')
    assert str(k_number + k_number | k_number | Root(Rule("term")) | ~k_plus) == (
            'CaseList(ItemList(Tok("k_number"), Tok("k_number")), '
            'ItemList(Tok("k_number")), ItemList(Root(Rule("term"))), '
            'ItemList(Not(Tok("k_plus"))))')

    wff = ( Rule("wff") + ~k_plus + Rule("wff")
          | Pratt() | k_ast
          | Rule("wff") + ~k_ast  + Rule("wff")
          | Optional(Rule("wff") + OneOrMore(k_plus + k_plus))
          )

    assert str(wff) == (
        'CaseList(ItemList(Rule("wff"), Not(Tok("k_plus")), Rule("wff")), '
        'ItemList(Pratt("None")), ItemList(Tok("k_ast")), '
        'ItemList(Rule("wff"), Not(Tok("k_ast")), Rule("wff")), '
        'ItemList(Optional(Rule("wff"), OneOrMore(Tok("k_plus"), Tok("k_plus")))))')

