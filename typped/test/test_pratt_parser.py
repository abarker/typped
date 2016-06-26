# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path(add_parent=True)

from pratt_parser import *

# TOKEN DEFINITIONS #################################################################

def define_whitespace_tokens(lex_or_pp):
    #lex_or_pp.define_token("space", r"[ \t]+", ignore=True) # note + NOT *
    #lex_or_pp.define_token("newline", r"[\n\f\r\v]+", ignore=True) # note + NOT *
    #lex_or_pp.define_token("whitespace", r"\s+", ignore=True) # note + NOT *

    whitespace_tokens = [
            ("space", r"[ \t]+"), # note + symbol, NOT * symbol
            ("newline", r"[\n\f\r\v]+") # note + symbol, NOT * symbol
            ]
    lex_or_pp.define_ignored_tokens(whitespace_tokens)

def define_basic_tokens(lex_or_pp):
    define_whitespace_tokens(lex_or_pp)
    #lex_or_pp.define_begin_and_end_tokens("begin", "end")

    # lex_or_pp.define_token("number", r"\d+")
    # lex_or_pp.define_token("imag_number", r"\d+[i]")
    # lex_or_pp.define_token("double_ast", r"(?:\*\*|\^)") # Note ^ is defined as synonym.
    # lex_or_pp.define_token("plus", r"\+")
    # lex_or_pp.define_token("minus", r"\-")
    # lex_or_pp.define_token("fslash", r"/")
    # lex_or_pp.define_token("ast", r"\*")
    # lex_or_pp.define_token("lpar", r"\(")
    # lex_or_pp.define_token("rpar", r"\)")
    # lex_or_pp.define_token("comma", r",")
    # lex_or_pp.define_token("bang", r"!")
    # lex_or_pp.define_token("question", r"\?")
    # lex_or_pp.define_token("colon", r"\:")
    # lex_or_pp.define_token("semicolon", r";")

    token_list = [
            ("number", r"\d+"),
            ("imag_number", r"\d+[i]"),
            ("double_ast", r"(?:\*\*|\^)"), # Note ^ is defined as synonym.
            ("plus", r"\+"),
            ("minus", r"\-"),
            ("fslash", r"/"),
            ("ast", r"\*"),
            ("lpar", r"\("),
            ("rpar", r"\)"),
            ("comma", r","),
            ("bang", r"!"),
            ("question", r"\?"),
            ("colon", r"\:"),
            ("semicolon", r";")
            ]
    lex_or_pp.define_tokens(token_list)

    # NOTE that we could define the exponentiation function twice, once for a
    # ** token and once for a ^ token.  (They can both be given the same AST
    # label.)  What we do here, instead, is define multiple symbols for a single
    # token, making ^ an alias for **.

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
    pp.define_literal("number_literal", "number")
    pp.define_literal("imag_number_literal", "imag_number")
    pp.define_literal("variable_literal", "identifier")

    pp.define_stdfun_lookahead("stdfun", "identifier", "lpar", "rpar", "comma") 
    #pp.define_stdfun_lpar_tail("stdfun", "identifier", "lpar", "rpar", "comma", 20) # 20 is prec of (

    pp.define_infix_operator("addition", "plus", 10, Assoc.left)
    pp.define_infix_operator("subtraction", "minus", 10, Assoc.left)

    pp.define_infix_operator("multiplication", "ast", 20, Assoc.left)
    pp.define_infix_operator("division", "fslash", 20, Assoc.left)

    pp.define_prefix_operator("positive", "plus", 100)
    pp.define_prefix_operator("negative", "minus", 100)

    pp.define_postfix_operator("factorial", "bang", 100, allow_ignored_before=False)

    pp.define_infix_operator("exponentiation", "double_ast", 30, Assoc.right)

    pp.define_multi_infix_operator("test", ["question", "colon"], 90, Assoc.right)

    # Note we have two exp operators, and we might want exp() standard fun too...
    # They should probably all have the same AST node which saves how they were
    # originally formatted.

    pp.define_bracket_pair("paren_brackets", "lpar", "rpar", 0)

    #pp.define_comma_list("comma list", "comma", 5, Assoc.right)
    pp.define_multi_infix_operator("comma list", ["comma"], 5, Assoc.left,
                                   in_tree=False, repeat=True)

    pp.define_multi_infix_operator("statements", ["semicolon"], 3, Assoc.left,
                                   repeat=True)

# OLD PRINT TESTS ####################################################################

def run_and_print(ast_table, pp, prog):
    ast_form = True
    print("---------------\n")
    print(prog)
    tree_root = pp.parse(prog)
    if not ast_form:
        print(tree_root.tree_repr())
    else:
        ast = tree_root.convert_to_AST(ast_table.get_AST_node)
        print(ast.tree_repr())

def run_local_tests():
    ast_table = AST_NodeTable()
    pp = PrattParser()
    define_default_tokens(pp)
    define_syntax(pp)

    #run_and_print(ast_table, pp, "3+5 // comment \n-30")
    run_and_print(ast_table, pp, "22, 44 + 44, 55, 44, 55")
    run_and_print(ast_table, pp, "4 * (f(x), 3)")
    #define_comment_to_EOL_token(pp, "//")
    #run_and_print(ast_table, pp, "4,3,7+f(3 * 2// comment \n * 3 * 4)")
    run_and_print(ast_table, pp, "3 + 4 ; egg55a; f(y)")
    run_and_print(ast_table, pp, "+4+5-ccc?4i:5")
    run_and_print(ast_table, pp, "3!")
    run_and_print(ast_table, pp, "f(3!)")
    run_and_print(ast_table, pp, "1+f(x)")
    run_and_print(ast_table, pp, "f( x,y,z)")
    run_and_print(ast_table, pp, "+1+ (1+2*3)*3 / 7")

#ast_table = AST_NodeTable()
#pp = PrattParser()
#define_default_tokens(pp)
#define_syntax(pp)
#run_local_tests()

## TESTS #############################################################################

@fixture(params=[1,2])
def basic_setup(request):
    lookahead_num = request.param
    global pp
    pp = PrattParser(lookahead_num)
    define_default_tokens(pp)
    define_syntax(pp)

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
    # TODO basic setup will not work, maybe individual defs of identifiers and vars
    # or else better dispatching.  (Later: what does this mean??? Tests below work.
    # Maybe identifiers need testing???)
    #skip()
    print("pp symbol table is", pp.symbol_table.token_subclass_dict.keys())
    pp.define_jop_token("jop_token", "space")
    pp.define_jop("multiplication", 20, Assoc.left)
    expr = "5  9"
    #print("\nworking on", expr)
    #print(pp.parse(expr))
    assert str(pp.parse(expr)) == "<jop_token,None>(<number,5>,<number,9>)"
    expr = "2 pi - 4 z"
    #print("\nworking on", expr)
    #print(pp.parse(expr))
    assert str(pp.parse(expr)) == ("<minus,->"
                                  "(<jop_token,None>(<number,2>,<identifier,pi>),"
                                   "<jop_token,None>(<number,4>,<identifier,z>))")
    expr = "2 pi (x+y) - 4^z s"
    #print("\nworking on", expr)
    assert str(pp.parse(expr)) == ("<minus,->"
                                  "(<jop_token,None>("
                                      "<jop_token,None>("
                                          "<number,2>,<identifier,pi>),"
                                      "<lpar,(>(<plus,+>(<identifier,x>,<identifier,y>))),"
                                  "<jop_token,None>(<double_ast,^>("
                                      "<number,4>,<identifier,z>),<identifier,s>))")
    #fail()

def test_types_mixed_numerical_bool_expressions(): 
    """Test type-checking on number-valued and bool-valued expressions, using
    only overloading on argument types."""
    # setup
    pp = PrattParser()
    define_default_tokens(pp)
    pp.define_token("exp", r"exp") # general identifier has lower on_ties

    # define the types to be used
    pp.define_type("t_number")
    pp.define_type("t_bool")

    # define initial syntax
    pp.define_literal("number_literal", "number", val_type="t_number")
    pp.define_literal("variable_literal", "identifier", val_type="t_number")
    pp.define_stdfun_lookahead("exponentiation", "exp",
                 "lpar", "rpar", "comma", val_type="t_number", arg_types=["t_number"])
    pp.define_infix_operator("addition", "plus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    pp.define_infix_operator("subtraction", "minus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    pp.define_infix_operator("multiplication", "ast", 20, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    pp.define_infix_operator("exponentiation", "double_ast", 30, Assoc.right,
                         val_type="t_number", arg_types=["t_number","t_number"])

    # run simple tests with just one type
    assert pp.parse("5 + 100").string_repr() == "<plus,+>(<number,5>,<number,100>)"
    # note double_ast ** and with an alias ^ on next
    assert pp.parse("5^100").string_repr() == "<double_ast,^>(<number,5>,<number,100>)"
    assert pp.parse("exp(100)-5^100^2").string_repr() == \
            "<minus,->(<exp,exp>(<number,100>)," \
            "<double_ast,^>(<number,5>,<double_ast,^>(<number,100>,<number,2>)))"

    # define "True" and "False" as boolean value literals.
    pp.define_token("true", r"True")
    pp.define_token("false", r"False")
    pp.define_literal("true literal", "true", val_type="t_bool")
    pp.define_literal("false literal", "false", val_type="t_bool")
    # define some new functions, first giving them unique tokens (required for
    # types to work correctly????????????? what are requirements?  where is
    # type info stored exactly, to see what can conflict?)
    # define a function taking t_bool and t_number args returning t_number
    pp.define_token("f_bn2n", r"f_bn2n")
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])
    pp.define_token("f_nb2b", r"f_nb2b")
    pp.define_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "lpar", "rpar", "comma",
                               val_type="t_bool", arg_types=["t_number", "t_bool"])

    # test some mixed type expressions
    assert pp.parse("f_bn2n(True,100)+100").string_repr() == \
            "<plus,+>(<f_bn2n,f_bn2n>(<true,True>,<number,100>),<number,100>)"
    with raises(TypeError) as e:
        pp.parse("f_bn2n(True,True)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("f_bn2n(100,100)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("f_bn2n(True,100)+False")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("f_bn2n(True,100)+f_nb2b(100,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # more functions

    # pp.define_literal("true literal", "true", val_type="t_bool") # redefinition
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    pp.define_token("g_bn2n", r"g_bn2n")
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    tree = pp.parse("f_bn2n(True,100) + g_bn2n(False,20)")
    assert tree.token_label == "plus"
    assert tree.children[0].token_label == "f_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))

    # functions overloaded on arguments with same token_label, g_bn2n takes
    # both number,bool and bool,number args (they could have diff tokens, too)
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_number","t_bool"])
    tree = pp.parse("g_bn2n(True,100) + g_bn2n(33,False)")
    assert tree.children[0].token_label == "g_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_number', 't_bool'))
    with raises(TypeError) as e:
        pp.parse("g_bn2n(True,100) + g_bn2n(True,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("g_bn2n(0,100) + g_bn2n(1,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("g_bn2n(True,100) + g_bn2n(1,False) + True")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # test multiple matches without fun overloading: give f_nb2b a number return version
    pp.define_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_number", "t_bool"])
    with raises(TypeError) as e:
        pp.parse("f_nb2b(1,False)")
    assert str(e.value).startswith("Actual argument types match multiple signatures.")

def test_types_overloaded_return(): 
    """Repeat the general tests from test_types_mixed_numerical_bool_expressions
    to make sure none of them break, then test cases specific to overloading on
    return types."""
    # setup
    pp = PrattParser(overload_on_ret_types=True)
    define_default_tokens(pp)
    pp.define_token("exp", r"exp") # general identifier has lower on_ties

    # define the types to be used
    pp.define_type("t_number")
    pp.define_type("t_bool")

    # define initial syntax
    pp.define_literal("number_literal", "number", val_type="t_number")
    pp.define_literal("variable_literal", "identifier", val_type="t_number")
    pp.define_stdfun_lookahead("exponentiation", "exp",
                 "lpar", "rpar", "comma", val_type="t_number", arg_types=["t_number"])
    pp.define_infix_operator("addition", "plus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    pp.define_infix_operator("subtraction", "minus", 10, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    pp.define_infix_operator("multiplication", "ast", 20, Assoc.left,
                         val_type="t_number", arg_types=["t_number","t_number"])
    pp.define_infix_operator("exponentiation", "double_ast", 30, Assoc.right,
                         val_type="t_number", arg_types=["t_number","t_number"])

    # run simple tests with just one type
    assert pp.parse(" 5 + 100").string_repr() == "<plus,+>(<number,5>,<number,100>)"
    # note double_ast ** and with an alias ^ on next
    assert pp.parse("5^100").string_repr() == "<double_ast,^>(<number,5>,<number,100>)"
    assert pp.parse("exp(100)-5^100^2").string_repr() == \
            "<minus,->(<exp,exp>(<number,100>)," \
            "<double_ast,^>(<number,5>,<double_ast,^>(<number,100>,<number,2>)))"

    # define "True" and "False" as boolean value literals.
    pp.define_token("true", r"True")
    pp.define_token("false", r"False")
    pp.define_literal("true literal", "true", val_type="t_bool")
    pp.define_literal("false literal", "false", val_type="t_bool")
    # define some new functions, first giving them unique tokens
    # define a function taking t_bool and t_number args returning t_number
    pp.define_token("f_bn2n", r"f_bn2n")
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])
    pp.define_token("f_nb2b", r"f_nb2b")
    pp.define_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "lpar", "rpar", "comma",
                               val_type="t_bool", arg_types=["t_number", "t_bool"])

    # test some mixed type expressions
    assert pp.parse("f_bn2n(True,100)+100").string_repr() == \
            "<plus,+>(<f_bn2n,f_bn2n>(<true,True>,<number,100>),<number,100>)"
    with raises(TypeError) as e:
        pp.parse("f_bn2n(True,True)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("f_bn2n(100,100)+100")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("f_bn2n(True,100)+False")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("f_bn2n(True,100)+f_nb2b(100,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # more functions

    # pp.define_literal("true literal", "true", val_type="t_bool") # redefinition
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "f_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    pp.define_token("g_bn2n", r"g_bn2n")
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_bool","t_number"])

    tree = pp.parse("f_bn2n(True,100) + g_bn2n(False,20)")
    assert tree.token_label == "plus"
    assert tree.children[0].token_label == "f_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))

    # functions overloaded on arguments with same token_label, g_bn2n takes
    # both number,bool and bool,number args (they could have diff tokens, too)
    pp.define_stdfun_lookahead("test_fun_bool_number_to_number", "g_bn2n",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_number","t_bool"])
    tree = pp.parse("g_bn2n(True,100) + g_bn2n(33,False)")
    assert tree.children[0].token_label == "g_bn2n"
    assert tree.children[0].type_sig == TypeSpec('t_number', ('t_bool', 't_number'))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].type_sig == TypeSpec('t_number', ('t_number', 't_bool'))
    with raises(TypeError) as e:
        pp.parse("g_bn2n(True,100) + g_bn2n(True,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("g_bn2n(0,100) + g_bn2n(1,False)")
    assert str(e.value).startswith("Actual argument types do not match any signature.")
    with raises(TypeError) as e:
        pp.parse("g_bn2n(True,100) + g_bn2n(1,False) + True")
    assert str(e.value).startswith("Actual argument types do not match any signature.")

    # overload f_nb2b with a version that returns a number
    print("========== defining first overload of f_nb2b")
    pp.define_stdfun_lookahead("test_fun_number_bool_to_bool", "f_nb2b",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_number", "t_bool"])
    with raises(TypeError) as e:
        pp.parse("f_nb2b(1,False)") # fails since two possibilities
    assert str(e.value).startswith("Ambiguous type resolution (second pass)")
    # with return type overloading this should again work if we add to a number,
    # to get unique resolution at the higher level
    assert pp.parse("f_nb2b(1,False) + 5").string_repr() == "<plus,+>" \
                                 "(<f_nb2b,f_nb2b>(<number,1>,<false,False>),<number,5>)"

    # overload f_nb2b again, to also take two number arguments
    pp.define_stdfun_lookahead("test_fun_number_number_to_bool", "f_nb2b",
                               "lpar", "rpar", "comma",
                               val_type="t_number", arg_types=["t_number", "t_number"])
    # now args are ambiguous in this order
    with raises(TypeError) as e:
        pp.parse("f_nb2b(4, f_nb2b(4,False))")
    assert str(e.value).startswith("Ambiguous type resolution (second pass)")
    # but this order should be OK
    assert pp.parse("f_nb2b(f_nb2b(4,False), 4)").string_repr() == "<f_nb2b,f_nb2b>" \
                              "(<f_nb2b,f_nb2b>(<number,4>,<false,False>),<number,4>)"
    # trigger the same message as before at lower level in tree, get 'parent expects'
    # err msg rather than just 'ambiguous' err msg which happens at the root
    with raises(TypeError) as e:
        pp.parse("f_nb2b(f_nb2b(2, f_nb2b(1,False)), 3)")
    assert str(e.value).startswith("Token node has multiple signatures with")

#def setup_module(module):
#    """setup module"""
#
#def teardown_module(module):
#    """teardown module"""

