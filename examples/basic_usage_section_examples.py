# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    #pytest_helper.script_run("../test/test_example_number_string_lang_from_intro.py",
    #                         pytest_args="-v")

#pytest_helper.sys_path("../src") # Only needed when package isn't pip installed.
import typped as pp
import operator
import cmd
import readline

def setup_simple_builtin_example():
    import typped as pp
    parser = pp.PrattParser()

    parser.def_default_whitespace()
    tok = parser.def_token
    tok("k_number", r"\d+")
    tok("k_lpar", r"\(")
    tok("k_rpar", r"\)")
    tok("k_ast", r"\*")
    tok("k_plus", r"\+")
    tok("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

    parser.def_literal("k_number")
    parser.def_literal("k_identifier")
    parser.def_infix_op("k_plus", 10, "left")
    parser.def_infix_op("k_ast", 20, "left")
    parser.def_bracket_pair("k_lpar", "k_rpar")

    return parser

def run_simple_builtin_example():
    parser = setup_simple_builtin_example()
    result_tree = parser.parse("x + (4 + 3)*5")
    print(result_tree.tree_repr())

def setup_string_language_parser():
    """An example from the Sphinx docs overview section.  A simple language
    that uses `+` to add integers and concatenate strings.  Multiplication of a
    number by a string repeats the string.  Multiplication of a string by a
    string is not defined.  It also has simple variables which can represent
    either numbers or strings."""
    parser = pp.PrattParser()

    # Define the tokens and types.

    parser.def_default_whitespace()
    tok = parser.def_token
    tok("k_int", r"-?\d+")
    tok("k_lpar", r"\(")
    tok("k_rpar", r"\)")
    tok("k_ast", r"\*")
    tok("k_plus", r"\+")
    tok("k_equals", r"=")
    tok("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    tok("k_string", r"(\"(.|[\r\n])*?\")")

    t_int = parser.def_type("t_int") # Number type.
    t_str = parser.def_type("t_str") # String type.

    # Define the symbol dict for holding variable values and the type dict
    # for holding the declared types of variables.

    symbol_dict = {}
    symbol_type_dict = {}
    parser.symbol_dict = symbol_dict # Make symbol dict an attribute of the parser instance.
    parser.symbol_type_dict = symbol_type_dict # The type dict, too.
    parser.typped_type_dict = {"int": t_int, "str": t_str}

    # Define a new construct for type definitions in the language.

    def head_handler(tok, lex):
        identifier_tok = tok.recursive_parse(0)
        tok.append_children(identifier_tok)
        symbol_type_dict[identifier_tok.value] = tok.parser_instance.typped_type_dict[tok.value]
        return tok

    parser.def_construct(pp.HEAD, head_handler, "k_identifier",
                         construct_label="c_int_typedef",
                         precond_fun=lambda lex, lookbehind: lex.token.value == "int",
                         precond_priority=10,
                         val_type=t_int)
    parser.def_construct(pp.HEAD, head_handler, "k_identifier",
                         construct_label="c_str_typedef",
                         precond_fun=lambda lex, lookbehind: lex.token.value == "str",
                         precond_priority=10,
                         val_type=t_str)

    # Now define the language.

    parser.def_literal("k_int", val_type=t_int, eval_fun=lambda t: int(t.value))

    # Overload identifiers to return either string or number.
    # Alternative: use a precond on assignment that checks if symbol defined in dict...
    # BUT you still need this, because you need to set the type...
    # REMEMBER, though, that literal identifiers are parsed in the parse step, BUT
    # the evaluation step happens AFTER that.  For full generality you should do the
    # evaluation AS you do the parsing, calling eval_subtree on each thing.  That could
    # maybe be an option.  But don't really need full generality, just assign in different
    # statement is OK here.
    def literal_typesig_override_fun(tok, lex):
        symbol_type_dict = tok.parser_instance.symbol_type_dict
        if tok.value in symbol_type_dict:
            return pp.TypeSig(symbol_type_dict[tok.value], [])
        else:
            return pp.TypeSig(t_int, []) # Default is number 0.

    parser.def_dynamically_typed_literal("k_identifier", default_type=t_int)

    parser.def_literal("k_string", val_type=t_str, eval_fun=lambda t: t.value)

    parser.def_bracket_pair("k_lpar", "k_rpar", eval_fun=lambda t: t[0].eval_subtree())


    parser.def_infix_op("k_plus", 10, "left",
                     val_type=t_int, arg_types=[t_int, t_int],
                     eval_fun=lambda t: t[0].eval_subtree() + t[1].eval_subtree())
    parser.def_infix_op("k_plus", 10, "left",
                     val_type=t_str, arg_types=[t_str, t_str],
                     eval_fun=lambda t: t[0].eval_subtree()[:-1] + t[1].eval_subtree()[1:])

    parser.def_infix_op("k_ast", 20, "left",
                     val_type=t_int, arg_types=[t_int, t_int],
                     eval_fun=lambda t: t[0].eval_subtree() * t[1].eval_subtree())
    parser.def_infix_op("k_ast", 20, "left",
                     val_type=t_str, arg_types=[t_str, t_int],
                     eval_fun=lambda t: (
                         '"' + (t[0].eval_subtree()[1:-1] * t[1].eval_subtree()) + '"'))
    parser.def_infix_op("k_ast", 20, "left",
                     val_type=t_str, arg_types=[t_int, t_str],
                     eval_fun=lambda t: (
                         '"' + (t[1].eval_subtree()[1:-1] * t[0].eval_subtree()) + '"'))

    # No longer FAILS on <-- test for these though
    # (4)
    # z = "egg" * 4


    # NOTE we have a static vs. dynamic type problem here!  Dynamic types are done here,
    # but static types are caught at parse-time!  Probably should use static types
    # for this example.
    #
    # Make static, then assignment must be overloaded...
    parser.def_assignment_op_dynamic("k_equals", 5, "left", "k_identifier",
                      val_type=t_int, arg_types=[None, t_int],
                      eval_fun=parser.eval_dynamically_typed_assignment())
    parser.def_assignment_op_dynamic("k_equals", 5, "left", "k_identifier",
                      val_type=t_str, arg_types=[None, t_str],
                      eval_fun=parser.eval_dynamically_typed_assignment())
    return parser

def run_string_language_parser():

    parser = setup_string_language_parser()

    parser.parse("ee")
    parser.parse("ee = 4")

    class NumberStringLangREPL(cmd.Cmd, object):
        prompt = "> "
        intro = "Enter ^D to exit the string language."

        def __init__(self):
            super(NumberStringLangREPL, self).__init__()
            self.parser = setup_string_language_parser()

        def emptyline(self):
            pass

        def default(self, line):
            try:
                result = parser.parse(line)
                print(result.tree_repr())
                print(result.eval_subtree())
            except (ValueError, ZeroDivisionError) as e:
                print(e)
            except pp.TypeErrorInParsedLanguage as e:
                print(e)
            #except Exception as e:
            #    print(e)

        def do_EOF(self, line):
            print("\nBye.")
            return True

    NumberStringLangREPL().cmdloop()

if __name__ == "__main__":
    print("Example 1\n---------\n")
    run_simple_builtin_example()
    print("Example 2\n---------\n")
    run_string_language_parser()


