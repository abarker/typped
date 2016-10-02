# -*- coding: utf-8 -*-
"""

Test the code in the production_rules.py file.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run(self_test=True, pytest_args="-vv")

pytest_helper.autoimport()

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

    _ = UNDERSCORE

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

def test_failed_rule_shortcut():
    """Tests of FAILED attempt at shortcut _<"wff">_ for Rule."""
    skip()
    parser = PrattParser()
    parser.def_default_whitespace()
    def_expression_tokens_and_literals(parser)
    g = Grammar()

    _ = UNDERSCORE

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

    # TODO these are a MESS, need to work on later or not use...
    #"""
    expression2 = ( _<"term1">_ + k_plus + _<"term2">_
                  | _<"term3">_ + k_minus + _<"term4">_
                  | _<"term5">_
                  )

    assert str(expression2) == str(expression)

    # This tests when the returned middle case is a CaseList but no saved_args
    # have been saved.
    testrule_caselist_init = k_number | _<"rule1">_ | k_number | _<"rule2">_
    assert str(testrule_caselist_init) == ('CaseList(ItemList(Tok("k_number")), '
                                              'ItemList(Rule("rule1")), '
                                              'ItemList(Tok("k_number")), '
                                              'ItemList(Rule("rule2")))')

    testrule = k_ast + _<"factor2">_
    assert str(testrule) == 'ItemList(Tok("k_ast"), Rule("factor2"))'

    testrule2 = _<"factor1">_ + k_ast + _<"factor2">_
    assert str(testrule2) == 'ItemList(Rule("factor1"), Tok("k_ast"), Rule("factor2"))'

    # Test function modifiers in expressions using Rule alternative symbols.
    testrule3 = Optional(k_ast + _<"factor2">_)
    assert str(testrule3) == 'ItemList(Optional(Tok("k_ast"), Rule("factor2")))'

    testrule4 = k_ast + Optional(_<"factor2">_)
    assert str(testrule4) == 'ItemList(Tok("k_ast"), Optional(Rule("factor2")))'

    testrule5 = k_ast + Optional(_<"factor2">_) | k_ast
    assert str(testrule5) == ('CaseList(ItemList(Tok("k_ast"), '
                                'Optional(Rule("factor2"))), ItemList(Tok("k_ast")))')

    #return
    # TODO: below test case fails, error is initializer to ItemList passed True.
    # Also, doesn't properly clear saved_comparison_args.  Note middle part,
    # _ + Optional(....) will be separately evaluated.  It will end up as an
    # ItemList.  Will it end with underscore and cause the second > to return True?
    # Needs to NOT end with underscore after eval AND be an Item.  It fails the latter!
    #
    # called > from instance ItemList
    #     the calling_instance is: ItemList(Tok("k_ast"), Item(None))
    #      the saved_comparison_args are: CaseList(ItemList(Rule("factor1")))
    # 
    # Note the Item(None) in there...  bad stripping? conflict in saved_comparison_args?
    #
    # Reproduced error below, assuming fail in paren-eval handling:  Error is
    #
    #    Overloading of operator '+' is not defined between instances of class
    #    bool and instances of class Item.  The two operands are Item(None) and True.
    #
    # The second _< thinks it is inside a group, not the BEGINNING of a subgroup...
    # No way to detect???????   If not then a DEAL KILLER for this notation.
    #
    # What if you always return the actual thing, not True, put an attribute on
    # the returned thing as to what case returned it.

    testrule7 = _<"factor1">_ + (_<"factor2">_)
    #testrule7 = _ < [ "factor1">_ + (_<"factor2">_) ]
    #                "factor1" > [ _ + (_<"factor2">_) ]
    #                             _ + [ (_<"factor2">_) ] # Note difference...
    #                                 _ < [_<"factor2">_ ]
    #                                     _ < [ "factor2">_ ]
    #                                           "factor2" > [ _ ]
    print(testrule7)
    assert testrule7 == "egg"

    return # =======
    # Original test fail below.
    testrule6 = _<"factor1">_ + Optional(k_ast + _<"factor2">_)
    print(testrule6)
    assert str(testrule6) == ('CaseList(ItemList(Tok("k_ast"), '
                                'Optional(Rule("factor2"))), ItemList(Tok("k_ast")))')

    # ==================================================================
    return # TODO temporary stop =======================================

    term2 = ( _<"factor1">_ + Optional(k_ast + _<"factor2">_)  # FAILS, messes up compile check
            | _<"factor3">_ + k_fslash + _<"factor4">_
            | _<"factor5">_
            )

    assert str(term2) == str(term)

    factor2 = ( k_number
              | k_lpar + _<"expression">_ + k_rpar  # FAILS
              )

    assert str(factor2) == str(factor)
    #"""


