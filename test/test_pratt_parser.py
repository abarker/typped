# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import typped # Test as a full package.

# Some naming conventions.
#
# Lexer:
#    tokens:    k_number
# Syntax:
#    types:     t_int
# AST data:
#    d_number

# TOKEN DEFINITIONS #################################################################

def define_whitespace_tokens(parser):
    #parser.def_token("whitespace", r"\s+", ignore=True) # note + NOT *

    whitespace_tokens = [
            ("k_space", r"[ \t]+"), # note + symbol, NOT * symbol
            ("k_newline", r"[\n\f\r\v]+") # note + symbol, NOT * symbol
            ]
    parser.def_multi_ignored_tokens(whitespace_tokens)

def define_basic_tokens(parser):
    define_whitespace_tokens(parser)
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
    parser.def_multi_tokens(token_list)

    # NOTE that the exponentiation function could alternately be defined twice,
    # once for a ** token as the operator and once for a ^ token.  (They can
    # both be given the same AST label.)  What is done here instead is to
    # define multiple symbols for a single token (via the regex), making ^ an
    # alias for **.

def define_identifier_token(parser):
    # The last part of below only needs \w, but commented-out line is a good
    # example of using a pattern.
    #parser.def_token("k_identifier", r"[a-zA-Z_](?:[\w|\d]*)", on_ties=-1)
    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

def define_default_tokens(parser):
    """Defines some default tokens for testing either a Lexer or a PrattParser."""
    define_basic_tokens(parser)
    define_identifier_token(parser)

    #parser.def_infix_op("divsign", u"÷")
    #parser.def_infix_op("caret", "^")

def define_comment_to_EOL_token(parser, begin_string):
    # Note that comment_to_endline is non-greedy due to *? symbol.
    parser.def_token("k_comment_to_EOL", r"{0}.*?[\n]"
                           .format(begin_string), ignore=True)

# SYNTAX DEFINITIONS ################################################################

def define_syntax(parser):
    t_number = parser.def_type("t_number")

    # Literals
    literals = [
            ("k_number", t_number), # Types not really used... note also order in list!
            #("k_imag_number", "t_imag_number"),
            ("k_identifier",),
            ]
    #parser.def_literal("k_number", ast_data="d_number")
    #parser.def_literal("k_imag_number", ast_data="d_imag_number")
    #parser.def_literal("k_identifier", ast_data="d_variable")
    parser.def_multi_literals(literals)

    # Operators
    parser.def_stdfun("k_identifier", "k_lpar", "k_rpar", "k_comma",
                                ast_data="d_std_function")
    #parser.def_stdfun_lpar_tail("k_identifier", "k_lpar", "k_rpar", "k_comma", 20,
    #                            ast_data="d_std_function") # 20 is prec of (

    parser.def_infix_op("k_plus", 10, "left", ast_data="d_add")
    parser.def_infix_op("k_minus", 10, "left", ast_data="d_subtract")

    parser.def_infix_op("k_ast", 20, "left", ast_data="d_mult")
    parser.def_infix_op("k_fslash", 20, "left", ast_data="d_divide")

    parser.def_prefix_op("k_plus", 100, ast_data="d_positive")
    parser.def_prefix_op("k_minus", 100, ast_data="d_negative")

    parser.def_postfix_op("k_bang", 100, allow_ignored_before=False, ast_data="factorial")

    parser.def_infix_op("k_double_ast", 30, "right", ast_data="d_exp")

    parser.def_infix_multi_op(["k_question", "k_colon"], 90, "right",
                              ast_data="d_ternary_conditional")

    # Note we have two exp operators, and we might want exp() standard fun too...
    # They should probably all have the same AST node which saves how they were
    # originally formatted.

    parser.def_bracket_pair("k_lpar", "k_rpar", ast_data="paren_brackets")

    # TODO TODO NOTE These tests are using comma as an infix operator, and relying on
    # the behavior of `in_tree` to set the children in the "usual" way.  Not a problem
    # BUT should be noted and multi-arg functions need to also be tested when they
    # consume the commas themselves.  Different exceptions are raised.
    parser.def_infix_multi_op(["k_comma"], 5, "left",
                              in_tree=False, repeat=True, ast_data="comma_list")

    parser.def_infix_multi_op(["k_semicolon"], 3, "left",
                                   repeat=True, ast_data="d_statements")

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
    ast_table = typped.AST_NodeDict()
    parser = typped.PrattParser()
    define_default_tokens(parser)
    define_syntax(parser)

    #run_and_print(ast_table, parser, "3+5 // comment \n-30")
    typped.run_and_print(ast_table, parser, "22, 44 + 44, 55, 44, 55")
    typped.run_and_print(ast_table, parser, "4 * (f(x), 3)")
    #define_comment_to_EOL_token(parser, "//")
    #typped.run_and_print(ast_table, parser, "4,3,7+f(3 * 2// comment \n * 3 * 4)")
    typped.run_and_print(ast_table, parser, "3 + 4 ; egg55a; f(y)")
    typped.run_and_print(ast_table, parser, "+4+5-ccc?4i:5")
    typped.run_and_print(ast_table, parser, "3!")
    typped.run_and_print(ast_table, parser, "f(3!)")
    typped.run_and_print(ast_table, parser, "1+f(x)")
    typped.run_and_print(ast_table, parser, "f( x,y,z)")
    typped.run_and_print(ast_table, parser, "+1+ (1+2*3)*3 / 7")

#run_local_tests()

## TESTS #############################################################################

@fixture(params=[1,2,3])
def basic_setup(request):
    """Set up basic parser with one-token lookahead, two-token lookahead, and no typing."""
    global parser
    lookahead_num = request.param
    if lookahead_num == 3:
        parser = typped.PrattParser(skip_type_checking=True)
    else:
        parser = typped.PrattParser(max_peek_tokens=lookahead_num)
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
    LexerException = typped.LexerException
    ParserException = typped.ParserException
    with raises(LexerException): parser.parse("3%2") # unknown symbol
    with raises(ParserException): parser.parse("f (x,y)")
    with raises(ParserException): parser.parse("1+ (x,y")
    with raises(ParserException): parser.parse("1+ x-y) / 3")
    with raises(ParserException): parser.parse("3 !")
    #fail("Just to see output.")

def test_stdfun_functions(basic_setup):
    """Test the built-in standard function method which uses a precondition on the
    function name token label looking for an left paren."""
    TypeErrorInParsedLanguage = typped.TypeErrorInParsedLanguage
    ParserException = typped.ParserException

    parser.def_token("k_exp", r"exp")
    parser.def_stdfun("k_exp", "k_lpar", "k_rpar", "k_comma", num_args=1)

    # Number of arguments
    if not parser.skip_type_checking:
        assert str(parser.parse("exp(44)")) == "<k_exp,'exp'>(<k_number,'44'>)"
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("exp(33, 33)")
        assert str(e.value).startswith("The number of arguments (2) does not match any sig")
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("exp()")
        assert str(e.value).startswith("The number of arguments (0) does not match any sig")

    parser.def_token("k_add", r"add")
    parser.def_stdfun("k_add", "k_lpar", "k_rpar", "k_comma", num_args=2)
    assert str(parser.parse(
               "add( 44 , 55 )")) == "<k_add,'add'>(<k_number,'44'>,<k_number,'55'>)"
    if not parser.skip_type_checking:
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("add(33, 33, 33)")
        assert str(e.value).startswith("The number of arguments (3) does not match any sig")
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("add(44)")
        assert str(e.value).startswith("The number of arguments (1) does not match any sig")

    # General error conditions
    with raises(ParserException) as e:
        parser.parse("add(30 30)") # Left out comma.
    assert str(e.value).startswith("Function match_next (with peeklevel=1) expected token")
    with raises(ParserException) as e:
        parser.parse("add(30,30,)") # Extra comma.
    assert str(e.value).startswith("No head handler functions at all are defined")
    with raises(ParserException) as e:
        parser.parse("add (30,30)") # Whitespace between.
    assert str(e.value).startswith("No head handler function matched")

def test_stdfun_lpar_tail_functions(basic_setup):
    """Tests for the built-in stdfun implementation that uses a tail-handler on the lpar
    token.  So the token which does the real work is the lpar token for both `k_exp` and
    `k_add` functions."""
    TypeErrorInParsedLanguage = typped.TypeErrorInParsedLanguage
    ParserException = typped.ParserException

    #
    # Define exp for a single argument.
    #

    parser.def_token("k_exp", r"exp")
    parser.def_literal("k_exp")

    parser.def_stdfun_lpar_tail("k_exp", "k_lpar", "k_rpar", "k_comma", prec_of_lpar=50,
            num_args=1)
    assert str(parser.parse("exp(44)")) == "<k_exp,'exp'>(<k_number,'44'>)"
    if not parser.skip_type_checking:
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("exp(33, 33)")
        assert str(e.value).startswith("The number of arguments (2) does not match any sig")
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("exp()")
        assert str(e.value).startswith("The number of arguments (0) does not match any sig")

    #
    # Define addition for two arguments.
    #

    parser.def_token("k_add", r"add")
    parser.def_literal("k_add")

    parser.def_stdfun_lpar_tail("k_add", "k_lpar", "k_rpar", "k_comma", prec_of_lpar=50,
            num_args=2)
    assert str(parser.parse(
               "add( 44 , 55 )")) == "<k_add,'add'>(<k_number,'44'>,<k_number,'55'>)"

    if not parser.skip_type_checking:
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("add(33, 33, 33)")
        assert str(e.value).startswith("The number of arguments (3) does not match any sig")
        with raises(TypeErrorInParsedLanguage) as e:
            parser.parse("add(33)")
        assert str(e.value).startswith("The number of arguments (1) does not match any sig")

    with raises(ParserException) as e:
        parser.parse("add(30 30)") # Left out comma.
    assert str(e.value).startswith("Function match_next (with peeklevel=1) expected token")
    with raises(ParserException) as e:
        parser.parse("add(30,30,)") # Extra comma.
    assert str(e.value).startswith("Function match_next (with peeklevel=1) found unexpected")
    with raises(ParserException) as e:
        parser.parse("add (30,30)") # Whitespace between.
    assert str(e.value).startswith("No tail handler function matched the token with value")

def test_jop(basic_setup):
    # TODO basic setup will not work, maybe individual defs of identifiers and vars
    # or else better dispatching.  (Later: what does this mean??? Tests below work.
    # Maybe identifiers need testing???)
    #skip()

    print("parser token table is", parser.token_table.token_subclass_dict.keys())
    parser.def_jop_token("k_jop", "k_space")
    parser.def_jop(20, "left", ast_data="d_mult")
    expr = "5  9"
    #print("\nworking on", expr)
    #print(parser.parse(expr))
    assert str(parser.parse(expr)) == "<k_jop,None>(<k_number,'5'>,<k_number,'9'>)"
    expr = "2 pi - 4 z"
    #print("\nworking on", expr)
    #print(parser.parse(expr))
    assert str(parser.parse(expr)) == ("<k_minus,'-'>"
                                  "(<k_jop,None>(<k_number,'2'>,<k_identifier,'pi'>),"
                                   "<k_jop,None>(<k_number,'4'>,<k_identifier,'z'>))")
    expr = "2 pi (x+y) - 4^z s"
    #print("\nworking on", expr)
    assert str(parser.parse(expr)) == ("<k_minus,'-'>"
                          "(<k_jop,None>("
                              "<k_jop,None>("
                                  "<k_number,'2'>,<k_identifier,'pi'>),"
                              "<k_lpar,'('>(<k_plus,'+'>(<k_identifier,'x'>,<k_identifier,'y'>))),"
                          "<k_jop,None>(<k_double_ast,'^'>("
                              "<k_number,'4'>,<k_identifier,'z'>),<k_identifier,'s'>))")
    #fail()
    parser.def_token("k_sin", r"sin")
    parser.def_stdfun("k_sin", "k_lpar", "k_rpar", "k_comma")
    assert str(parser.parse("4 sin( 0 )")) == (
                          "<k_jop,None>(<k_number,'4'>,<k_sin,'sin'>(<k_number,'0'>))")


def test_types_mixed_numerical_bool_expressions():
    """Test type-checking on number-valued and bool-valued expressions, using
    only overloading on argument types."""
    # setup

    TypeErrorInParsedLanguage = typped.TypeErrorInParsedLanguage
    TypeSig = typped.TypeSig

    parser = typped.PrattParser()
    define_default_tokens(parser)
    parser.def_token("k_exp", r"exp") # general identifier has lower on_ties

    # define the types to be used
    t_number = parser.def_type("t_number")
    t_bool = parser.def_type("t_bool")

    # define initial syntax
    parser.def_literal("k_number", val_type=t_number, ast_data="d_number")
    parser.def_literal("k_identifier", val_type=t_number, ast_data="d_variable")
    parser.def_stdfun("k_exp", "k_lpar", "k_rpar", "k_comma",
                      val_type=t_number, arg_types=[t_number], ast_data="d_exp")
    parser.def_infix_op("k_plus", 10, "left", val_type=t_number,
                      arg_types=[t_number,t_number], ast_data="d_add")
    parser.def_infix_op("k_minus", 10, "left", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_subtract")
    parser.def_infix_op("k_ast", 20, "left", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_mult")
    parser.def_infix_op("k_double_ast", 30, "right", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_exp")

    # run simple tests with just one type
    assert parser.parse("5 + 100").string_tree_repr() == \
                                  "<k_plus,'+'>(<k_number,'5'>,<k_number,'100'>)"
    # note k_double_ast ** and with an alias ^ on next
    assert parser.parse("5^100").string_tree_repr() == \
                                  "<k_double_ast,'^'>(<k_number,'5'>,<k_number,'100'>)"
    assert parser.parse("exp(100)-5^100^2").string_tree_repr() == \
            "<k_minus,'-'>(<k_exp,'exp'>(<k_number,'100'>)," \
            "<k_double_ast,'^'>(<k_number,'5'>,<k_double_ast,'^'>(<k_number,'100'>,<k_number,'2'>)))"

    # define "True" and "False" as boolean value literals.
    parser.def_token("k_true", r"True")
    parser.def_token("k_false", r"False")
    parser.def_literal("k_true", val_type=t_bool, ast_data="d_true")
    parser.def_literal("k_false", val_type=t_bool, ast_data="d_false")
    # define some new functions, first giving them unique tokens (required for
    # types to work correctly????????????? what are requirements?  where is
    # type info stored exactly, to see what can conflict?)
    # define a function taking t_bool and t_number args returning t_number
    parser.def_token("f_bn2n", r"f_bn2n")
    parser.def_stdfun("f_bn2n", "k_lpar", "k_rpar", "k_comma",
                                val_type=t_number, arg_types=[t_bool,t_number],
                                ast_data="d_test_fun_bool_number_to_number")
    parser.def_token("f_nb2b", r"f_nb2b")
    parser.def_stdfun("f_nb2b", "k_lpar", "k_rpar", "k_comma",
                                val_type=t_bool, arg_types=[t_number, t_bool],
                                ast_data="d_test_fun_number_bool_to_bool")

    # test some mixed type expressions
    assert parser.parse("f_bn2n(True,100)+100").string_tree_repr() == \
            "<k_plus,'+'>(<f_bn2n,'f_bn2n'>(<k_true,'True'>,<k_number,'100'>),<k_number,'100'>)"
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(True,True)+100")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(100,100)+100")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(True,100)+False")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(True,100)+f_nb2b(100,False)")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")

    # more functions

    # parser.def_literal("d_true", "k_true", val_type=t_bool) # redefinition
    parser.def_stdfun("f_bn2n", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_bool,t_number],
                               ast_data="d_test_fun_bool_number_to_number")

    parser.def_token("g_bn2n", r"g_bn2n")
    parser.def_stdfun("g_bn2n", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_bool,t_number],
                               ast_data="d_test_fun_bool_number_to_number")

    tree = parser.parse("f_bn2n(True,100) + g_bn2n(False,20)")
    assert tree.token_label == "k_plus"
    assert tree.children[0].token_label == "f_bn2n"
    assert tree.children[0].expanded_formal_sig == TypeSig(t_number, (t_bool, t_number))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].expanded_formal_sig == TypeSig(t_number, (t_bool, t_number))

    # functions overloaded on arguments with same token_label, g_bn2n takes
    # both number,bool and bool,number args (they could have diff tokens, too)
    parser.def_stdfun("g_bn2n", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_number,t_bool],
                               ast_data="d_test_fun_bool_number_to_number")
    tree = parser.parse("g_bn2n(True,100) + g_bn2n(33,False)")
    assert tree.children[0].token_label == "g_bn2n"
    assert tree.children[0].expanded_formal_sig == TypeSig(t_number, (t_bool, t_number))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].expanded_formal_sig == TypeSig(t_number, (t_number, t_bool))
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(True,False)")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not"
                                   " match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("g_bn2n(0,100) + g_bn2n(1,False)")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not"
                                   " match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(1,False) + True")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not"
                                   " match any type signature")

    # test multiple matches without fun overloading: give f_nb2b a number return version
    parser.def_stdfun("f_nb2b", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_number, t_bool],
                               ast_data="d_test_fun_number_bool_to_bool")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_nb2b(1,False)")
    assert str(e.value).startswith("Ambiguous type resolution: The actual argument"
                                   " types match multiple signatures.")

def test_types_overloaded_return():
    """Repeat the general tests from test_types_mixed_numerical_bool_expressions
    to make sure none of them break, then test cases specific to overloading on
    return types."""
    # setup
    TypeErrorInParsedLanguage = typped.TypeErrorInParsedLanguage
    TypeSig = typped.TypeSig

    parser = typped.PrattParser(overload_on_ret_types=True)
    define_default_tokens(parser)
    parser.def_token("k_exp", r"exp") # general identifier has lower on_ties

    # define the types to be used
    t_number = parser.def_type("t_number")
    t_bool = parser.def_type("t_bool")

    # define initial syntax
    parser.def_literal("k_number", val_type=t_number, ast_data="d_number")
    parser.def_literal("k_identifier", val_type=t_number, ast_data="d_variable")
    parser.def_stdfun("k_exp", "k_lpar", "k_rpar", "k_comma",
                      val_type=t_number, arg_types=[t_number], ast_data="d_exp")
    parser.def_infix_op("k_plus", 10, "left", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_add")
    parser.def_infix_op("k_minus", 10, "left", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_subtract")
    parser.def_infix_op("k_ast", 20, "left", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_mult")
    parser.def_infix_op("k_double_ast", 30, "right", val_type=t_number,
                        arg_types=[t_number,t_number], ast_data="d_exp")

    # run simple tests with just one type
    assert parser.parse(" 5 + 100").string_tree_repr() == \
                                         "<k_plus,'+'>(<k_number,'5'>,<k_number,'100'>)"
    # note k_double_ast ** and with an alias ^ on next
    assert parser.parse("5^100").string_tree_repr() == \
                                   "<k_double_ast,'^'>(<k_number,'5'>,<k_number,'100'>)"
    assert parser.parse("exp(100)-5^100^2").string_tree_repr() == \
         "<k_minus,'-'>(<k_exp,'exp'>(<k_number,'100'>)," \
         "<k_double_ast,'^'>(<k_number,'5'>,<k_double_ast,'^'>(<k_number,'100'>,<k_number,'2'>)))"

    # define "True" and "False" as boolean value literals.
    parser.def_token("k_true", r"True")
    parser.def_token("k_false", r"False")
    parser.def_literal("k_true", val_type=t_bool, ast_data="d_true")
    parser.def_literal("k_false", val_type=t_bool, ast_data="d_false")
    # define some new functions, first giving them unique tokens
    # define a function taking t_bool and t_number args returning t_number
    parser.def_token("f_bn2n", r"f_bn2n")
    parser.def_stdfun("f_bn2n", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_bool,t_number],
                               ast_data="d_test_fun_bool_number_to_number")
    parser.def_token("f_nb2b", r"f_nb2b")
    parser.def_stdfun("f_nb2b", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_bool, arg_types=[t_number, t_bool],
                               ast_data="d_test_fun_number_bool_to_bool")

    # test some mixed type expressions
    assert parser.parse("f_bn2n(True,100)+100").string_tree_repr() == \
            "<k_plus,'+'>(<f_bn2n,'f_bn2n'>(<k_true,'True'>,<k_number,'100'>),<k_number,'100'>)"
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(True,True)+100")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(100,100)+100")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(True,100)+False")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_bn2n(True,100)+f_nb2b(100,False)")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")

    # more functions

    # parser.def_literal("d_true", "k_true", val_type=t_bool) # redefinition
    parser.def_stdfun("f_bn2n", "k_lpar", "k_rpar", "k_comma",
                                val_type=t_number, arg_types=[t_bool,t_number],
                                ast_data="d_test_fun_bool_number_to_number")

    parser.def_token("g_bn2n", r"g_bn2n")
    parser.def_stdfun("g_bn2n", "k_lpar", "k_rpar", "k_comma",
                                val_type=t_number, arg_types=[t_bool,t_number],
                                ast_data="d_test_fun_bool_number_to_number")

    tree = parser.parse("f_bn2n(True,100) + g_bn2n(False,20)")
    assert tree.token_label == "k_plus"
    assert tree.children[0].token_label == "f_bn2n"
    assert tree.children[0].expanded_formal_sig == TypeSig(t_number, (t_bool, t_number))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].expanded_formal_sig == TypeSig(t_number, (t_bool, t_number))

    # functions overloaded on arguments with same token_label, g_bn2n takes
    # both number,bool and bool,number args (they could have diff tokens, too)
    parser.def_stdfun("g_bn2n", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_number,t_bool],
                               ast_data="d_test_fun_bool_number_to_number")
    tree = parser.parse("g_bn2n(True,100) + g_bn2n(33,False)")
    assert tree.children[0].token_label == "g_bn2n"
    assert tree.children[0].expanded_formal_sig == TypeSig(t_number, (t_bool, t_number))
    assert tree.children[1].token_label == "g_bn2n"
    assert tree.children[1].expanded_formal_sig == TypeSig(t_number, (t_number, t_bool))
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(True,False)")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("g_bn2n(0,100) + g_bn2n(1,False)")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("g_bn2n(True,100) + g_bn2n(1,False) + True")
    assert str(e.value).startswith("Type mismatch: The actual argument types do not match any type signature")

    # overload f_nb2b with a version that returns a number
    print("========== defining first overload of f_nb2b")
    parser.def_stdfun("f_nb2b", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_number, t_bool],
                               ast_data="d_test_fun_number_bool_to_bool")
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_nb2b(1,False)") # fails since two possibilities
    assert str(e.value).startswith("Ambiguous type resolution (second pass)")
    # with return type overloading this should again work if we add to a number,
    # to get unique resolution at the higher level
    assert parser.parse("f_nb2b(1,False) + 5").string_tree_repr() == "<k_plus,'+'>" \
                "(<f_nb2b,'f_nb2b'>(<k_number,'1'>,<k_false,'False'>),<k_number,'5'>)"

    # overload f_nb2b again, to also take two number arguments
    parser.def_stdfun("f_nb2b", "k_lpar", "k_rpar", "k_comma",
                               val_type=t_number, arg_types=[t_number, t_number],
                               ast_data="d_test_fun_number_number_to_bool")
    # now args are ambiguous in this order
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_nb2b(4, f_nb2b(4,False))")
    assert str(e.value).startswith("Ambiguous type resolution (second pass)")
    # but this order should be OK
    assert parser.parse("f_nb2b(f_nb2b(4,False), 4)").string_tree_repr() == "<f_nb2b,'f_nb2b'>" \
                   "(<f_nb2b,'f_nb2b'>(<k_number,'4'>,<k_false,'False'>),<k_number,'4'>)"
    # trigger the same message as before at lower level in tree, get 'parent expects'
    # err msg rather than just 'ambiguous' err msg which haparserens at the root
    with raises(TypeErrorInParsedLanguage) as e:
        parser.parse("f_nb2b(f_nb2b(2, f_nb2b(1,False)), 3)")
    assert str(e.value).startswith("Token node has multiple signatures with")

