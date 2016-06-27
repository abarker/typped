# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path(add_parent=True)

import pratt_parser

# Some naming conventions.
#
# Lexer:
#    tokens:    k_number
# Syntax:
#    literals:  l_number
#    types:     t_int
#    operators: op_add
#    otherwise: no currently defined prefix
# Semantics:
#    eval fun:  e_add

# TOKEN DEFINITIONS #################################################################

def define_whitespace_tokens(lexer_or_parser):
    #lexer_or_parser.def_token("whitespace", r"\s+", ignore=True) # note + NOT *

    whitespace_tokens = [
            ("k_space", r"[ \t]+"), # note + symbol, NOT * symbol
            ("k_newline", r"[\n\f\r\v]+") # note + symbol, NOT * symbol
            ]
    lexer_or_parser.def_ignored_tokens(whitespace_tokens)

def define_basic_tokens(lexer_or_parser):
    define_whitespace_tokens(lexer_or_parser)
    token_list = [
            ("k_number", r"\d+"),
            ("k_imag_number", r"\d+[i]"),
            ("k_double_ast", r"(?:\*\*|\^)"), # Note ^ is defined as synonym.
            ("k_plus", r"\+"),
            ("k_minus", r"\-"),
            ("k_fslash", r"/"),
            ("k_ast", r"\*"),
            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_comma", r","),
            ("k_bang", r"!"),
            ("k_question", r"\?"),
            ("k_colon", r"\:"),
            ("k_semicolon", r";")
            ]
    lexer_or_parser.def_tokens(token_list)

    # NOTE that the exponentiation function could alternately be defined twice,
    # once for a ** token as the operator and once for a ^ token.  (They can
    # both be given the same AST label.)  What is done here instead is to
    # define multiple symbols for a single token (via the regex), making ^ an
    # alias for **.

def define_identifier_token(lexer_or_parser):
    # The last part of below only needs \w, but good example of pattern.
    #lexer_or_parser.def_token("k_identifier", r"[a-zA-Z_](?:[\w|\d]*)", on_ties=-1)
    lexer_or_parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

def define_default_tokens(lexer_or_parser):
    """Defines some default tokens for testing either a Lexer or a PrattParser."""
    define_basic_tokens(lexer_or_parser)
    define_identifier_token(lexer_or_parser)

    #lexer_or_parser.def_infix_op("divsign", u"÷") 
    #lexer_or_parser.def_infix_op("caret", "^") 

def define_comment_to_EOL_token(lexer_or_parser, begin_string):
    # Note that comment_to_endline is non-greedy due to *? symbol.
    lexer_or_parser.def_token("k_comment_to_EOL", r"{0}.*?[\n]"
                           .format(begin_string), ignore=True)

# SYNTAX DEFINITIONS ################################################################

def define_syntax(parser):
    Assoc = pratt_parser.Assoc # Enum for association.

    parser.def_literal("l_number", "k_number")
    parser.def_literal("l_imag_number", "k_imag_number")
    parser.def_literal("l_variable", "k_identifier")

    parser.def_stdfun_lookahead("stdfun", "k_identifier", "k_lpar", "k_rpar", "k_comma") 
    #parser.def_stdfun_lpar_tail("stdfun", "k_identifier", "k_lpar", "k_rpar", "k_comma", 20) # 20 is prec of (

    parser.def_infix_op("op_add", "k_plus", 10, Assoc.left)
    parser.def_infix_op("op_subract", "k_minus", 10, Assoc.left)

    parser.def_infix_op("op_mult", "k_ast", 20, Assoc.left)
    parser.def_infix_op("op_divide", "k_fslash", 20, Assoc.left)

    parser.def_prefix_op("op_positive", "k_plus", 100)
    parser.def_prefix_op("op_negative", "k_minus", 100)

    parser.def_postfix_op("op_factorial", "k_bang", 100, allow_ignored_before=False)

    parser.def_infix_op("op_exp", "k_double_ast", 30, Assoc.right)

    parser.def_multi_infix_op("test", ["k_question", "k_colon"], 90, Assoc.right)

    # Note we have two exp operators, and we might want exp() standard fun too...
    # They should probably all have the same AST node which saves how they were
    # originally formatted.

    parser.def_bracket_pair("paren_brackets", "k_lpar", "k_rpar", 0)

    #parser.define_comma_list("comma list", "k_comma", 5, Assoc.right)
    parser.def_multi_infix_op("comma list", ["k_comma"], 5, Assoc.left,
                                   in_tree=False, repeat=True)

    parser.def_multi_infix_op("statements", ["k_semicolon"], 3, Assoc.left,
                                   repeat=True)

# OLD PRINT TESTS ####################################################################

def run_and_print(ast_table, parser, prog):
    ast_form = True
    print("---------------\n")
    print(prog)
    tree_root = parser.parse(prog)
    if not ast_form:
        print(tree_root.tree_repr())
    else:
        ast = tree_root.convert_to_AST(ast_table.get_AST_node)
        print(ast.tree_repr())

def run_local_tests():
    ast_table = pratt_parser.AST_NodeTable()
    parser = pratt_parser.PrattParser()
    define_default_tokens(parser)
    define_syntax(parser)

    #run_and_print(ast_table, parser, "3+5 // comment \n-30")
    pratt_parser.run_and_print(ast_table, parser, "22, 44 + 44, 55, 44, 55")
    pratt_parser.run_and_print(ast_table, parser, "4 * (f(x), 3)")
    #define_comment_to_EOL_token(parser, "//")
    #pratt_parser.run_and_print(ast_table, parser, "4,3,7+f(3 * 2// comment \n * 3 * 4)")
    pratt_parser.run_and_print(ast_table, parser, "3 + 4 ; egg55a; f(y)")
    pratt_parser.run_and_print(ast_table, parser, "+4+5-ccc?4i:5")
    pratt_parser.run_and_print(ast_table, parser, "3!")
    pratt_parser.run_and_print(ast_table, parser, "f(3!)")
    pratt_parser.run_and_print(ast_table, parser, "1+f(x)")
    pratt_parser.run_and_print(ast_table, parser, "f( x,y,z)")
    pratt_parser.run_and_print(ast_table, parser, "+1+ (1+2*3)*3 / 7")

#run_local_tests()

## TESTS #############################################################################

@fixture(params=[1,2])
def basic_setup(request):
    lookahead_num = request.param
    global parser
    parser = pratt_parser.PrattParser(lookahead_num)
    define_default_tokens(parser)
    define_syntax(parser)

def test_pratt_parser(basic_setup):
    # be sure to test with both 1 and 2 token lookahead... easy to copy whole section
    # when in separate file....
    assert parser.parse("1").old_repr() == "[literal 1]"
    tree = parser.parse("+1")
    print(tree, tree.old_repr())
    assert parser.parse("+1").old_repr() == "[+ [literal 1]]"
    assert parser.parse("-1").old_repr() == "[- [literal 1]]"
    assert parser.parse("1+2").old_repr() == "[+ [literal 1] [literal 2]]"
    assert parser.parse("1+2+3").old_repr() == "[+ [+ [literal 1] [literal 2]] [literal 3]]"
    assert parser.parse("1+2-3").old_repr() == "[- [+ [literal 1] [literal 2]] [literal 3]]"
    assert parser.parse("1-2+3").old_repr() == "[+ [- [literal 1] [literal 2]] [literal 3]]"
    assert parser.parse("1+2*3").old_repr() == "[+ [literal 1] [* [literal 2] [literal 3]]]"
    assert parser.parse("1*2+3").old_repr() == "[+ [* [literal 1] [literal 2]] [literal 3]]"
    assert parser.parse("1+2*3").old_repr() == "[+ [literal 1] [* [literal 2] [literal 3]]]"
    assert parser.parse("1*2**3**4").old_repr() == \
            "[* [literal 1] [** [literal 2] [** [literal 3] [literal 4]]]]"
    assert parser.parse("(1+2) * 3 / 7").old_repr() == \
            "[/ [* [k_lpar [+ [literal 1] [literal 2]] k_rpar] [literal 3]] [literal 7]]"
    assert parser.parse("((1+2) * (3)) / 7").old_repr() == \
            "[/ [k_lpar [* [k_lpar [+ [literal 1] [literal 2]] k_rpar] [k_lpar [literal 3] k_rpar]] k_rpar] [literal 7]]"
    assert parser.parse("400**2").old_repr() == "[** [literal 400] [literal 2]]"
    assert parser.parse("44*3!-4").old_repr() == "[- [* [literal 44] [! [literal 3]]] [literal 4]]"
    assert parser.parse("44*3?4:7").old_repr() == \
            "[* [literal 44] [? [literal 3] [literal 4] [literal 7]]]"
    #fail("Just to see output.")

def test_error_conditions(basic_setup):
    LexerException = pratt_parser.LexerException
    ParserException = pratt_parser.ParserException
    with raises(LexerException): parser.parse("3%2") # unknown symbol
    with raises(ParserException): parser.parse("f (x,y)")
    with raises(ParserException): parser.parse("1+ (x,y")
    with raises(ParserException): parser.parse("1+ x-y) / 3")
    with raises(ParserException): parser.parse("3 !")
    #fail("Just to see output.")

def test_jop(basic_setup): 
    # TODO basic setup will not work, maybe individual defs of identifiers and vars
    # or else better dispatching.  (Later: what does this mean??? Tests below work.
    # Maybe identifiers need testing???)
    #skip()
    Assoc = pratt_parser.Assoc # Enum for association.

    print("parser symbol table is", parser.symbol_table.token_subclass_dict.keys())
    parser.def_jop_token("k_jop", "k_space")
    parser.def_jop("op_mult", 20, Assoc.left)
    expr = "5  9"
    #print("\nworking on", expr)
    #print(parser.parse(expr))
    assert str(parser.parse(expr)) == "<k_jop,None>(<k_number,5>,<k_number,9>)"
    expr = "2 pi - 4 z"
    #print("\nworking on", expr)
    #print(parser.parse(expr))
    assert str(parser.parse(expr)) == ("<k_minus,->"
                                  "(<k_jop,None>(<k_number,2>,<k_identifier,pi>),"
                                   "<k_jop,None>(<k_number,4>,<k_identifier,z>))")
    expr = "2 pi (x+y) - 4^z s"
    #print("\nworking on", expr)
    assert str(parser.parse(expr)) == ("<k_minus,->"
                          "(<k_jop,None>("
                              "<k_jop,None>("
                                  "<k_number,2>,<k_identifier,pi>),"
                              "<k_lpar,(>(<k_plus,+>(<k_identifier,x>,<k_identifier,y>))),"
                          "<k_jop,None>(<k_double_ast,^>("
                              "<k_number,4>,<k_identifier,z>),<k_identifier,s>))")
    #fail()

def test_types_mixed_numerical_bool_expressions(): 
    """Test type-checking on number-valued and bool-valued expressions, using
    only overloading on argument types."""
    # setup
    Assoc = pratt_parser.Assoc # Enum for association.
    TypeError = pratt_parser.TypeError
    TypeSpec = pratt_parser.TypeSpec

    parser = pratt_parser.PrattParser()
    define_default_tokens(parser)
    parser.def_token("k_exp", r"exp") # general identifier has lower on_ties

    # define the types to be used
    parser.def_type("t_number")
    parser.def_type("t_bool")

    # define initial syntax
    parser.def_literal("l_number", "k_number", val_type="t_number")
    parser.def_literal("l_variable", "k_identifier", val_type="t_number")
    parser.def_stdfun_lookahead("op_exp", "k_exp",
                 "k_lpar", "k_rpar", "k_comma", val_type="t_number", arg_types=["t_number"])
    parser.def_infix_op("op_add", "k_plus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    parser.def_infix_op("op_subract", "k_minus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    parser.def_infix_op("op_mult", "k_ast", 20, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    parser.def_infix_op("op_exp", "k_double_ast", 30, Assoc.right,
                         val_type="t_number", arg_types=["t_number","t_number"])

    # run simple tests with just one type
    assert parser.parse("5 + 100").string_repr() == "<k_plus,+>(<k_number,5>,<k_number,100>)"
    # note k_double_ast ** and with an alias ^ on next
    assert parser.parse("5^100").string_repr() == "<k_double_ast,^>(<k_number,5>,<k_number,100>)"
    assert parser.parse("exp(100)-5^100^2").string_repr() == \
            "<k_minus,->(<k_exp,exp>(<k_number,100>)," \
            "<k_double_ast,^>(<k_number,5>,<k_double_ast,^>(<k_number,100>,<k_number,2>)))"

    # define "True" and "False" as boolean value literals.
    parser.def_token("k_true", r"True")
    parser.def_token("k_false", r"False")
    parser.def_literal("l_true", "k_true", val_type="t_bool")
    parser.def_literal("l_false", "k_false", val_type="t_bool")
    # define some new functions, first giving them unique tokens (required for
    # types to work correctly????????????? what are requirements?  where is
    # type info stored exactly, to see what can conflict?)
    # define a function taking t_bool and t_number args returning t_number
    parser.def_token("f_bn2n", r"f_bn2n")
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])
    parser.def_token("f_nb2b", r"f_nb2b")
    parser.def_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_bool", arg_types=["t_number", "t_bool"])

    # test some mixed type expressions
    assert parser.parse("f_bn2n(True,100)+100").string_repr() == \
            "<k_plus,+>(<f_bn2n,f_bn2n>(<k_true,True>,<k_number,100>),<k_number,100>)"
    with raises(TypeError) as e:
        parser.parse("f_bn2n(True,True)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("f_bn2n(100,100)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("f_bn2n(True,100)+False")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("f_bn2n(True,100)+f_nb2b(100,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # more functions

    # parser.def_literal("l_true", "k_true", val_type="t_bool") # redefinition
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    parser.def_token("g_bn2n", r"g_bn2n")
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    tree = parser.parse("f_bn2n(True,100) + g_bn2n(False,20)")
    assert tree.token_label == "k_plus"
    assert tree.children[0].token_label == "f_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))

    # functions overloaded on arguments with same token_label, g_bn2n takes
    # both number,bool and bool,number args (they could have diff tokens, too)
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_number","t_bool"])
    tree = parser.parse("g_bn2n(True,100) + g_bn2n(33,False)")
    assert tree.children[0].token_label == "g_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_number', 't_bool'))
    with raises(TypeError) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(True,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("g_bn2n(0,100) + g_bn2n(1,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(1,False) + True")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # test multiple matches without fun overloading: give f_nb2b a number return version
    parser.def_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_number", "t_bool"])
    with raises(TypeError) as e:
        parser.parse("f_nb2b(1,False)")
    assert str(e.value).startswith("Actual argument types match multiple signatures.")

def test_types_overloaded_return(): 
    """Repeat the general tests from test_types_mixed_numerical_bool_expressions
    to make sure none of them break, then test cases specific to overloading on
    return types."""
    # setup
    Assoc = pratt_parser.Assoc # Enum for association.
    TypeError = pratt_parser.TypeError
    TypeSpec = pratt_parser.TypeSpec

    parser = pratt_parser.PrattParser(overload_on_ret_types=True)
    define_default_tokens(parser)
    parser.def_token("k_exp", r"exp") # general identifier has lower on_ties

    # define the types to be used
    parser.def_type("t_number")
    parser.def_type("t_bool")

    # define initial syntax
    parser.def_literal("l_number", "k_number", val_type="t_number")
    parser.def_literal("l_variable", "k_identifier", val_type="t_number")
    parser.def_stdfun_lookahead("op_exp", "k_exp",
                 "k_lpar", "k_rpar", "k_comma", val_type="t_number", arg_types=["t_number"])
    parser.def_infix_op("op_add", "k_plus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    parser.def_infix_op("op_subract", "k_minus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    parser.def_infix_op("op_mult", "k_ast", 20, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    parser.def_infix_op("op_exp", "k_double_ast", 30, Assoc.right,
                         val_type="t_number", arg_types=["t_number","t_number"])

    # run simple tests with just one type
    assert parser.parse(" 5 + 100").string_repr() == "<k_plus,+>(<k_number,5>,<k_number,100>)"
    # note k_double_ast ** and with an alias ^ on next
    assert parser.parse("5^100").string_repr() == "<k_double_ast,^>(<k_number,5>,<k_number,100>)"
    assert parser.parse("exp(100)-5^100^2").string_repr() == \
            "<k_minus,->(<k_exp,exp>(<k_number,100>)," \
            "<k_double_ast,^>(<k_number,5>,<k_double_ast,^>(<k_number,100>,<k_number,2>)))"

    # define "True" and "False" as boolean value literals.
    parser.def_token("k_true", r"True")
    parser.def_token("k_false", r"False")
    parser.def_literal("l_true", "k_true", val_type="t_bool")
    parser.def_literal("l_false", "k_false", val_type="t_bool")
    # define some new functions, first giving them unique tokens
    # define a function taking t_bool and t_number args returning t_number
    parser.def_token("f_bn2n", r"f_bn2n")
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])
    parser.def_token("f_nb2b", r"f_nb2b")
    parser.def_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_bool", arg_types=["t_number", "t_bool"])

    # test some mixed type expressions
    assert parser.parse("f_bn2n(True,100)+100").string_repr() == \
            "<k_plus,+>(<f_bn2n,f_bn2n>(<k_true,True>,<k_number,100>),<k_number,100>)"
    with raises(TypeError) as e:
        parser.parse("f_bn2n(True,True)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("f_bn2n(100,100)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("f_bn2n(True,100)+False")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("f_bn2n(True,100)+f_nb2b(100,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # more functions

    # parser.def_literal("l_true", "k_true", val_type="t_bool") # redefinition
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    parser.def_token("g_bn2n", r"g_bn2n")
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    tree = parser.parse("f_bn2n(True,100) + g_bn2n(False,20)")
    assert tree.token_label == "k_plus"
    assert tree.children[0].token_label == "f_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))

    # functions overloaded on arguments with same token_label, g_bn2n takes
    # both number,bool and bool,number args (they could have diff tokens, too)
    parser.def_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_number","t_bool"])
    tree = parser.parse("g_bn2n(True,100) + g_bn2n(33,False)")
    assert tree.children[0].token_label == "g_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_number', 't_bool'))
    with raises(TypeError) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(True,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("g_bn2n(0,100) + g_bn2n(1,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(1,False) + True")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # overload f_nb2b with a version that returns a number
    print("========== defining first overload of f_nb2b")
    parser.def_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_number", "t_bool"])
    with raises(TypeError) as e:
        parser.parse("f_nb2b(1,False)") # fails since two possibilities
    assert str(e.value).startswith("Ambiguous type resolution (second pass)")
    # with return type overloading this should again work if we add to a number,
    # to get unique resolution at the higher level
    assert parser.parse("f_nb2b(1,False) + 5").string_repr() == "<k_plus,+>" \
                                 "(<f_nb2b,f_nb2b>(<k_number,1>,<k_false,False>),<k_number,5>)"

    # overload f_nb2b again, to also take two number arguments
    parser.def_stdfun_lookahead("test_fun_number_number_to_bool", "f_nb2b",
                               "k_lpar", "k_rpar", "k_comma",
                               val_type="t_number", arg_types=["t_number", "t_number"])
    # now args are ambiguous in this order
    with raises(TypeError) as e:
        parser.parse("f_nb2b(4, f_nb2b(4,False))")
    assert str(e.value).startswith("Ambiguous type resolution (second pass)")
    # but this order should be OK
    assert parser.parse("f_nb2b(f_nb2b(4,False), 4)").string_repr() == "<f_nb2b,f_nb2b>" \
                              "(<f_nb2b,f_nb2b>(<k_number,4>,<k_false,False>),<k_number,4>)"
    # trigger the same message as before at lower level in tree, get 'parent expects'
    # err msg rather than just 'ambiguous' err msg which haparserens at the root
    with raises(TypeError) as e:
        parser.parse("f_nb2b(f_nb2b(2, f_nb2b(1,False)), 3)")
    assert str(e.value).startswith("Token node has multiple signatures with")

#def setup_module(module):
#    """setup module"""
#
#def teardown_module(module):
#    """teardown module"""

