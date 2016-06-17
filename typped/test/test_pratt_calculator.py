# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path(add_parent=True)

from pratt_parser import *


# TOKEN DEFINITIONS #################################################################

def define_whitespace_tokens(lex_or_pp):
    lex_or_pp.define_token("space", r"[ \t]+", ignore=True) # note + NOT *
    lex_or_pp.define_token("newline", r"[\n\f\r\v]+", ignore=True) # note + NOT *
    #lex_or_pp.define_token("whitespace", r"\s+", ignore=True) # note + NOT *

def define_basic_tokens(lex_or_pp):
    define_whitespace_tokens(lex_or_pp)
    lex_or_pp.define_begin_and_end_tokens("begin", "end")
    lex_or_pp.define_token("natural_number", r"\d+")
    lex_or_pp.define_token("decimal_number", r"\d*[.]+\d+")
    lex_or_pp.define_token("imag_natural", r"\d+[i]")
    lex_or_pp.define_token("imag_decimal", r"\d+[i]")
    lex_or_pp.define_token("double_ast", r"(?:\*\*|\^)") # Note ^ is defined as synonym.
    lex_or_pp.define_token("plus", r"\+")
    lex_or_pp.define_token("minus", r"\-")
    lex_or_pp.define_token("fslash", r"/")
    lex_or_pp.define_token("ast", r"\*")
    lex_or_pp.define_token("lpar", r"\(")
    lex_or_pp.define_token("rpar", r"\)")
    lex_or_pp.define_token("lbrac", r"\[")
    lex_or_pp.define_token("rbrac", r"\]")
    lex_or_pp.define_token("comma", r",")
    lex_or_pp.define_token("bang", r"!")
    #lex_or_pp.define_token("question", r"\?")
    #lex_or_pp.define_token("colon", r"\:")
    #lex_or_pp.define_token("semicolon", r";")

def define_identifier_token(lex_or_pp):
    # The last part of below only needs \w, but good example of pattern.
    #lex_or_pp.define_token("identifier", r"[a-zA-Z_](?:[\w|\d]*)", on_ties=-1)
    lex_or_pp.define_token("identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

def define_default_tokens(lex_or_pp):
    """Defines some default tokens for testing either a Lexer or a PrattParser."""
    define_basic_tokens(lex_or_pp)
    define_identifier_token(lex_or_pp)

    #pp.define_infix_operator("divsign", u"÷") 
    #pp.define_infix_operator("caret", "^") 

def define_comment_to_EOL_token(lex_or_pp, begin_string):
    # Note that comment_to_endline is non-greedy due to *? symbol.
    lex_or_pp.define_token("comment_to_EOL", r"{0}.*?[\n]"
                           .format(begin_string), ignore=True)

# SYNTAX DEFINITIONS ################################################################

def define_syntax(pp):
    # Note that we are using overloading on return types to implement "casting"
    # up to higher types, starting from the literals.
    pp.define_literal("natural_number_literal", "natural_number", val_type="t_integer")
    pp.define_literal("decimal_number_literal", "natural_number", val_type="t_decimal")
    pp.define_literal("complex_number_literal", "natural_number", val_type="t_complex")

    pp.define_literal("decimal_number_literal", "decimal_number", val_type="t_decimal")
    pp.define_literal("decimal_number_literal", "decimal_number", val_type="t_complex")

    pp.define_literal("imag_natural_literal", "imag_natural", val_type="t_complex")
    pp.define_literal("imag_decimal_literal", "imag_decimal", val_type="t_complex")

    pp.define_literal("variable_literal", "identifier")

    pp.define_stdfun_lookahead("stdfun", "identifier", "lpar", "rpar", "comma")

    pp.define_infix_operator("addition", "plus", 10, Assoc.left)
    pp.define_infix_operator("subtraction", "minus", 10, Assoc.left)

    pp.define_infix_operator("multiplication", "ast", 20, Assoc.left)
    pp.define_infix_operator("division", "fslash", 20, Assoc.left)

    pp.define_prefix_operator("positive", "plus", 100)
    pp.define_prefix_operator("negative", "minus", 100)

    pp.define_postfix_operator("factorial", "bang", 100, allow_ignored_before=False)

    pp.define_infix_operator("exponentiation", "double_ast", 30, Assoc.right)

    pp.define_bracket_pair("paren_brackets", "lpar", "rpar", 0)
    pp.define_bracket_pair("bracket_brackets", "lbrac", "rbrac", 0)

    #pp.define_multi_infix_operator("comma list", ["comma"], 5, Assoc.left,
    #                               in_tree=False, repeat=True)

    #pp.define_multi_infix_operator("statements", ["semicolon"], 3, Assoc.left,
    #                               repeat=True)

# TESTS #############################################################################

def run_and_print(prog):
    ast_form = True
    print("---------------\n")
    print(prog)
    root = pp.parse(prog)
    if not ast_form:
        print(root.tree_repr())
    else:
        ast = root.convert_to_AST(ast_table.get_AST_node)
        print(ast.tree_repr())

def run_local_tests(pp):
    #run_and_print("3+5 // comment \n-30")
    #run_and_print("4 * (f(x), 3)")
    #define_comment_to_EOL_token(pp, "//")
    #run_and_print("4,3,7+f(3 * 2// comment \n * 3 * 4)")
    run_and_print("3!")
    run_and_print("f(3!)")
    run_and_print("1+f(x)")
    run_and_print("f( x,y,z)")
    run_and_print("+1+ (1+2*3)*3 / 7")

ast_table = AST_NodeTable()
pp = PrattParser(overload_on_ret_types=True)
define_default_tokens(pp)
define_syntax(pp)
run_local_tests(pp)

@fixture(params=[1,2])
def basic_setup(request):
    lookahead_num = request.param
    global pp
    pp = PrattParser(lookahead_num)
    define_default_tokens(pp)
    define_syntax(pp)
    locals_to_globals() # Check if this is needed... probably.  Added later.
    #fail()

def test_pratt_parser(basic_setup):
    # be sure to test with both 1 and 2 token lookahead... easy to copy whole section
    # when in separate file....
    assert pp.parse("1").old_repr() == "[literal 1]"
    tree = pp.parse("+1")
    print(tree, tree.old_repr())
    assert pp.parse("+1").old_repr() == "[+ [literal 1]]"
    assert pp.parse("-1").old_repr() == "[- [literal 1]]"
    assert pp.parse("1+2").old_repr() == "[+ [literal 1] [literal 2]]"
    assert pp.parse("1+2+3").old_repr() == "[+ [+ [literal 1] [literal 2]] [literal 3]]"
    assert pp.parse("1+2-3").old_repr() == "[- [+ [literal 1] [literal 2]] [literal 3]]"
    assert pp.parse("1-2+3").old_repr() == "[+ [- [literal 1] [literal 2]] [literal 3]]"
    assert pp.parse("1+2*3").old_repr() == "[+ [literal 1] [* [literal 2] [literal 3]]]"
    assert pp.parse("1*2+3").old_repr() == "[+ [* [literal 1] [literal 2]] [literal 3]]"
    assert pp.parse("1+2*3").old_repr() == "[+ [literal 1] [* [literal 2] [literal 3]]]"
    assert pp.parse("1*2**3**4").old_repr() == \
            "[* [literal 1] [** [literal 2] [** [literal 3] [literal 4]]]]"
    assert pp.parse("(1+2) * 3 / 7").old_repr() == \
            "[/ [* [lpar [+ [literal 1] [literal 2]] rpar] [literal 3]] [literal 7]]"
    assert pp.parse("((1+2) * (3)) / 7").old_repr() == \
            "[/ [lpar [* [lpar [+ [literal 1] [literal 2]] rpar] [lpar [literal 3] rpar]] rpar] [literal 7]]"
    assert pp.parse("400**2").old_repr() == "[** [literal 400] [literal 2]]"
    assert pp.parse("44*3!-4").old_repr() == "[- [* [literal 44] [! [literal 3]]] [literal 4]]"
    assert pp.parse("44*3?4:7").old_repr() == \
            "[* [literal 44] [? [literal 3] [literal 4] [literal 7]]]"
    #fail("Just to see output.")

def test_error_conditions(basic_setup):
    with raises(LexerException): pp.parse("3%2") # unknown symbol
    with raises(ParserException): pp.parse("f (x,y)")
    with raises(ParserException): pp.parse("1+ (x,y")
    with raises(ParserException): pp.parse("1+ x-y) / 3")
    with raises(ParserException): pp.parse("3 !")
    #fail("Just to see output.")

def test_jop(basic_setup): 
    # TODO basic setup will not work, may individual defs of identifiers and vars
    # or else better dispatching.
    skip()
    print("pp symbol table is", pp.symbol_table.token_subclass_dict.keys())
    pp.define_jop_token("jop_token", "space")
    pp.define_jop("multiplication", 20, Assoc.left)
    expr = "5  9"
    print("\nworking on", expr)
    print(pp.parse(expr))
    expr = "2 pi - 4 z"
    print("\nworking on", expr)
    print(pp.parse(expr))
    expr = "2 pi (x+y) - 4^z s"
    print("\nworking on", expr)
    print(pp.parse(expr))
    #fail()

