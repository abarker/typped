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

    # TODO these are a MESS, need to work on later or not use...
    #"""
    expression2 = ( _<"term1">_ + k_plus + _<"term2">_
                  | _<"term3">_ + k_minus + _<"term4">_
                  | _<"term5">_
                  )

    assert str(expression2) == str(expression)
   

    print(">>>>>>>>>>>> before testrule")
    testrule = k_ast + _<"factor2">_
    print(testrule)
    assert str(testrule) == 'CaseList(ItemList(Tok("k_ast"), Rule("factor2")))'
    print(">>>>>>>>>>>> testrule is", testrule)

    testrule2 = _<"factor1">_ + k_ast + _<"factor2">_

    # TODO: Optional returns a CaseList, causes fail.
    #testrule3 = Optional(k_ast + _<"factor2">_) # Optional should return ItemList
    assert str(testrule3) == "egg"

    term2 = ( _<"factor1">_ + Optional(k_ast + _<"factor2">_)  # FAILS

            | _<"factor3">_ + k_fslash + _<"factor4">_
            | _<"factor5">_
            )

    assert str(term2) == str(term)

    factor2 = ( k_number
              | k_lpar + _<"expression">_ + k_rpar  # FAILS
              )

    assert str(factor2) == str(factor)
    #"""


def test_basic_expression_grammar():
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

def test_overload_expression_grammar():
    skip()
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
    print("1", str(  Tok(k_number)  ))
    print("2", str(  Tok(k_number)  ))
    print("3", str(  k_number + Rule("factor")  ))
    print("4", str(  Tok(k_number) + Sig(Rule("factor"), none_sig) | Rule("term")  ))
    print("5", str(  Tok(k_number) + ~Rule("factor") | Root(Rule("term")) | ~Pratt() | k_number  ))
    print("6", str(  k_number | k_number | Root(Rule("term")) | Pratt()  ))
    print("7", str(  k_number + k_number | k_number | Root(Rule("term")) | ~k_plus  ))

    wff = ( Rule("wff") + ~k_plus + Rule("wff")
          | Rule("wff") + ~k_ast  + Rule("wff")
          | Optional(Rule("wff") + Optional(k_plus + k_plus))
          )
    print(str(wff))

    _ = pp.UNDERSCORE

    print()
    print("======================")
    #print("printing CaseList(_<right1) value:", CaseList(_<"right1")) # ERROR test
    #print("\nprinting _<right1>_ value:", _<"right1">_, "\n")

    # BELOW case -1 works without the rightmost "right1" rule (case -2), but not with it.
    # It is dropping the left part.  The next-to-last > still needs to save it,
    # but currently doesn't (how to turn off if it does??)
    #
    # With no | the things seem to work OK (what has been tested).  They delete
    # info on the closing > and no info is saved for > if 
    print("\nprinting combo-2:", _<"combo-2">_+_<"left1">_ + _<"left2">_ + Rule("rule2.5") + Rule("rule2.6") + _<"left3">_ + _<"left4">_ + _<"left5">_, "\n")
    print("\nprinting combo-1:", _<"left1">_ + _<"left2">_ + Rule("rule2.5") \
            + Rule("rule2.6") + _<"left3">_ + _<"left4">_ 
            | k_number + _<"right1">_, "\n")

    #print("\nprinting combo0:", CaseList(Rule("left")) | _<"right1">_)
    #print("\nprinting combo0.5:", CaseList(Rule("left")) | _<"right1">_ + Rule("right2"))
    #print("\nprinting combo1:", CaseList(Rule("left"))
    #       | _<"right1">_ + _<"right2">_, "\n")
    #print("\nprinting combo1.5:", CaseList(Rule("left"))
    #        | _<"right1">_ + _<"right2">_ + _<"right3">_, "\n")
    #print("\nprinting combo1.6:", CaseList(Rule("left"))
    #        | _<"right1">_ + _<"right2">_ + _<"right3">_
    #        | k_number + _<"far_right">_, "\n")
    #print("\nprinting combo2:", CaseList(Rule("left")) | _<"right">_)

    fail()

