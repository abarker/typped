# -*- coding: utf-8 -*-
"""

Test the code in the production_rules.py file.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run(self_test=True, pytest_args="-v")

pytest_helper.autoimport()

from typped import *

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
    k_number = parser.def_token("k_number", r"[0-9]+")

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

def test_basic_expression_grammar():
    skip()

    parser = PrattParser()
    parser.def_default_whitespace()

    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #

    g = Grammar()

    g.add_production_case("factor", [k_number])
    g.add_production_case("factor", [k_lpar, Rule("expression"), k_rpar])

    g.add_production_case("term", [Rule("factor"), k_ast, Rule("factor")])
    g.add_production_case("term", [Rule("factor"), k_fslash, Rule("factor")])
    g.add_production_case("term", [Rule("factor")])


    g.add_production_case("expression", [Rule("term"), k_plus, Rule("term")])
    g.add_production_case("expression", [Rule("term"), k_minus, Rule("term")])
    g.add_production_case("expression", [Rule("term")])

    #
    # Parse some expressions.
    #

    g.compile(parser)

    #parser.pstate_stack = ["expression"]
    print()
    print()
    simple_example = parser.parse("4*4", pstate="expression").tree_repr(3)
    print("simple_example:", simple_example)
    simple_string_rep_example = parser.parse("4*4", pstate="expression").string_tree_repr()
    print("simple_string_rep_example", simple_string_rep_example)
    assert str(simple_string_rep_example) == ("<k_null-string,'expression'>("
                         "<k_null-string,'term'>(<k_null-string,'factor'>("
                         "<k_number,'4'>),<k_ast,'*'>,<k_null-string,'factor'>("
                         "<k_number,'4'>)))")
    #fail()

def test_basic_expression_grammar():
    parser = PrattParser()
    parser.def_default_whitespace()

    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
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

    print(locals())
    g.compile("expression", parser, locals())

    #parser.pstate_stack = ["expression"]
    print()
    print()
    simple_example = parser.parse("4*4", pstate="expression").tree_repr(3)
    print("simple_example:", simple_example)
    simple_string_rep_example = parser.parse("4*4", pstate="expression").string_tree_repr()
    print("simple_string_rep_example", simple_string_rep_example)
    
    simple_string_rep_example = parser.parse("4*4", pstate="expression").string_tree_repr()
    assert str(simple_string_rep_example) == ("<k_null-string,'expression'>("
                         "<k_null-string,'term'>(<k_null-string,'factor'>("
                         "<k_number,'4'>),<k_ast,'*'>,<k_null-string,'factor'>("
                         "<k_number,'4'>)))")

    # TODO below parse fails.  It should parse as follows:
    # expression
    #     term
    #        factor            # First two fail, third succeeds.
    #           k_number, 5
    #     k_plus
    #     term
    #        factor            # First two fail, third succeeds.
    #           k_lpar, (         # First fail, second succeeds.
    #           expression
    #              term        # First two fail, third succeeds.
    #                 factor
    #                    k_number, 444
    #                 k_ast, *
    #                 factor
    #                    k_number, 32
    #           k_rpar, )
    #  Success.
    another_example = "5 * (444 + 32)"
    another_example_result = parser.parse(another_example, pstate="expression").tree_repr()
    print(another_example, "\n\n", another_example_result)
    fail()

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
    print("1", str(  Tok(k_number)[4]  ))
    print("2", str(  Tok(k_number)[4]  ))
    print("3", str(  k_number[10] + Rule("factor")  ))
    print("4", str(  Tok(k_number) + Sig(Rule("factor"), none_sig) | Rule("term")  ))
    print("5", str(  Tok(k_number) + ~Rule("factor") | Root(Rule("term")) | ~Pratt() | k_number  ))
    print("6", str(  k_number | k_number | Root(Rule("term")) | Pratt()  ))
    print("7", str(  k_number + k_number | k_number | Root(Rule("term")) | ~k_plus  ))

    wff = ( Rule("wff") + ~k_plus[10] + Rule("wff")
          | Rule("wff") + ~k_ast[20]  + Rule("wff")
          | Optional(Rule("wff") + Optional(k_plus + k_plus))
          )
    print(str(wff))

    _ = Item()

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

