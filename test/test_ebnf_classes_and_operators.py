# -*- coding: utf-8 -*-
"""

Test the code in the `ebnf_classes_and_overloads.py` file as well as the
functions in `register_grammar_with_parser.py` and the `PrattParser` code for
handling null-string tokens, etc.

Classic EBNF expression grammar below is from:
    https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#classic

The pure BNF expression grammar is similar to one from Wikipedia.
   https://en.wikipedia.org/wiki/Syntax_diagram

Consider also using postal address example from
   https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    pytest_helper.script_run(self_test=True, pytest_args="-vv")

pytest_helper.autoimport()
#pytest_helper.sys_path("../src")

from typped import * # Assumes typped is installed locally.

def def_expression_tokens_and_literals(parser):
    """Define some general tokens."""
    #
    # Define the tokens.
    #
    parser.def_default_whitespace()
    tok = parser.def_token

    # Operators.
    k_plus = tok("k_plus", r"\+")
    k_minus = tok("k_minus", r"\-")
    k_fslash = tok("k_fslash", r"/")
    k_ast = tok("k_ast", r"\*")
    k_caret = tok("k_caret", r"^")

    # Grouping.
    k_lpar = tok("k_lpar", r"\(")
    k_rpar = tok("k_rpar", r"\)")

    # Numbers.
    k_number = tok("k_number", r"[1-9][0-9]*")

    #
    # Define the literal tokens (terminals).
    #
    literal = parser.def_literal

    literal("k_plus")
    literal("k_minus")
    literal("k_fslash")
    literal("k_ast")
    literal("k_caret")
    literal("k_lpar")
    literal("k_rpar")
    literal("k_number")

    pytest_helper.locals_to_globals()

def test_EBNF_like_expressions():
    """Test the Python grammar for representing EBNF grammars."""
    parser = PrattParser()
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

def test_wrappers_for_multiple_items():
    """Test wrappers wrapping combined arguments."""
    test = Tok("k_dot") + Opt(Rule("test") + Tok("k_dot")) + Tok("semi")
    assert str(test) == 'ItemList(Tok("k_dot"), Opt(Rule("test"), Tok("k_dot")), Tok("semi"))'

    multiple = (0,4) * (Rule("test") + Tok("dot")) # Parens on args.
    print(str(multiple))
    assert str(multiple) == 'ItemList(Repeat(0, 4, Rule("test"), Tok("dot")))'

def test_parsing_from_simplified_expression_grammar():
    """Test the actual parser creation and execution from a grammar."""
    parser = PrattParser()
    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #

    g = Grammar()

    # Note only one top-level add or subtract per expression is allowed by this
    # very simple grammar!
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
    # Register the grammar with the parser and parse some expressions.
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
                         simple_example2, pstate="expression").tree_repr()

    assert simple_example2_parse == unindent(7, """
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
                   <k_rpar,')'>

       """)

    simple_example3 = "(5 + 99) * 388"
    simple_example3_parse = parser.parse(
                         simple_example3, pstate="expression").tree_repr()

    assert simple_example3_parse == unindent(7, """
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
                   <k_number,'388'>

       """)

    simple_example4 = "5 + 99 * 388"
    simple_example4_parse = parser.parse(
                         simple_example4, pstate="expression").tree_repr()

    assert simple_example4_parse == unindent(7, """
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
                   <k_number,'388'>

       """)

    simple_example5 = "5 * 7 - 388"
    simple_example5_parse = parser.parse(
                         simple_example5, pstate="expression").tree_repr()

    assert simple_example5_parse == unindent(7, """
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
                   <k_number,'388'>

       """)

    # Fails!
    #simple_example6 = "5 - 7 - 388"
    #simple_example6_parse = parser.parse(
    #                     simple_example6, pstate="expression").tree_repr()


def test_parsing_from_classic_expression_grammar():
    """Test the actual parser classic recursive descent expression grammar."""
    skip()
    parser = PrattParser()
    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #

    g = Grammar()

    expression = Rule("term") + ZeroOrMore(
                                 Opt(Tok("k_plus"), Tok("k_minus")) + Rule("term"))
    print("Expression is", expression)

    term = Rule("factor") + ZeroOrMore(
                                 Opt(Tok("k_ast"), Tok("k_fslash")) + Rule("factor"))
    print("Term is", term)

    factor = Rule("base") + Opt(Tok("k_caret") + Rule("factor"))
    print("Factor is", factor)

    base = ( k_number
           | k_lpar + Rule("expression") + k_rpar
           )
    print("Base is", base)

    fail()
    #
    # Register the grammar with the parser and parse some expressions.
    #

    g.compile("expression", parser, locals())

    #parser.pstate_stack = ["expression"]
    #parser.top_level_production

    simple_example1_parse = parser.parse(
                                "4*4", pstate="expression").string_tree_repr()

    assert simple_example1_parse == unindent(7, """
       "todo"

       """)

def test_parsing_from_BNF_expression_grammar():
    """Test the actual parser classic recursive descent expression grammar."""
    parser = PrattParser()
    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #

    k_letter = parser.def_token("k_letter", r"[a-z]")
    parser.def_literal("k_letter")

    expression = Rule("term") + k_plus + Rule("expression") | Rule("term")
    term       = Rule("factor") + k_ast + Rule("term") | Rule("factor")
    factor     = Rule("number") | Rule("variable") | k_lpar + Rule("expression") + k_rpar
    variable   = k_letter
    number     = k_number

    print("Expression is", expression)
    print("Term is", term)
    print("Factor is", factor)

    #
    # Register the grammar with the parser and parse some expressions.
    #

    Grammar("expression", parser, locals()) # Compile grammar and register with parser.

    simple_example_parse = parser.parse(
                                "4+2*2", pstate="expression").tree_repr()

    print(simple_example_parse)
    fail()
    assert simple_example_parse == unindent(7, """
       todo

       """)

def test_shortcut_operator_overloads_in_expression_grammar():
    parser = PrattParser()
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
          | Opt(Rule("wff") + OneOrMore(k_plus + k_plus))
          )

    assert str(wff) == (
        'CaseList(ItemList(Rule("wff"), Not(Tok("k_plus")), Rule("wff")), '
        'ItemList(Pratt("None")), ItemList(Tok("k_ast")), '
        'ItemList(Rule("wff"), Not(Tok("k_ast")), Rule("wff")), '
        'ItemList(Opt(Rule("wff"), Repeat(1, None, Tok("k_plus"), Tok("k_plus")))))')

    assert str(nExactly(3, Tok(k_number))) == str(3 * Tok(k_number))
    assert str(nOrMore(3, Tok(k_number))) == str((3,) * Tok(k_number))
    assert str(Between(3, 4, Tok(k_number))) == str((3,4) * Tok(k_number))

