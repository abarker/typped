# -*- coding: utf-8 -*-
"""

This file contains the runnable code for the Basic Usage section of the Sphinx docs.

"""
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

def define_parser_tokens_and_literals_simple_example():
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

    return parser

def setup_simple_builtin_example():
    parser = define_parser_tokens_and_literals_simple_example()

    parser.def_literal("k_number")
    parser.def_literal("k_identifier")

    parser.def_infix_op("k_plus", 10, "left")
    parser.def_infix_op("k_ast", 20, "left")

    parser.def_bracket_pair("k_lpar", "k_rpar")

    return parser

def setup_simple_non_builtin_example():
    parser = define_parser_tokens_and_literals_simple_example()

    def literal_head_handler(tok, lex):
        return tok
    parser.def_construct(pp.HEAD, literal_head_handler, "k_number")
    parser.def_construct(pp.HEAD, literal_head_handler, "k_identifier")

    def infix_op_tail_handler_10(tok, lex, left):
        tok.append_children(left, tok.recursive_parse(10)) # Use 9 for right assoc.
        return tok
    parser.def_construct(pp.TAIL, infix_op_tail_handler_10, "k_plus", prec=10)

    def infix_op_tail_handler_20(tok, lex, left):
        tok.append_children(left, tok.recursive_parse(20)) # Use 19 for right assoc.
        return tok
    parser.def_construct(pp.TAIL, infix_op_tail_handler_20, "k_ast", prec=20)

    def paren_head_handler(tok, lex):
        expr = tok.recursive_parse(0)
        lex.match_next("k_rpar", raise_on_fail=True)
        return expr # Do not include the parens themselves, just the arg.
    parser.def_construct(pp.HEAD, paren_head_handler, "k_lpar")

    return parser

def run_simple_builtin_example():
    parser = setup_simple_builtin_example()
    result_tree = parser.parse("x + (4 + 3)*5")
    print(result_tree.tree_repr())

def run_simple_non_builtin_example():
    parser = setup_simple_non_builtin_example()
    result_tree = parser.parse("x + (4 + 3)*5")
    print(result_tree.tree_repr())

def define_basic_infix_parser_from_scratch():
    parser = pp.PrattParser()

def setup_string_language_parser_dynamic_typing():
    """A simple dynamically-typed language that uses `+` to add integers and
    concatenate strings.  Multiplication of a number by a string repeats the
    string.  Multiplication of a string by a string is not defined.  It also
    has simple variables which can represent either numbers or strings."""
    parser = pp.PrattParser()

    # Define the tokens.

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

    # Define the types.

    t_int = parser.def_type("t_int") # Integer type.
    t_str = parser.def_type("t_str") # String type.

    # Define the syntax of the language, supplying evaluation functions.

    parser.def_literal("k_int", val_type=t_int, eval_fun=lambda t: int(t.value))
    parser.def_literal("k_string", val_type=t_str, eval_fun=lambda t: t.value)

    parser.def_literal_typed_from_dict("k_identifier", create_eval_fun=True,
                                       default_type=t_int, default_eval_value=0)

    parser.def_bracket_pair("k_lpar", "k_rpar", eval_fun=lambda t: t[0].eval_subtree())

    infix = parser.def_infix_op
    infix("k_plus", 10, "left",
          val_type=t_int, arg_types=[t_int, t_int],
          eval_fun=lambda t: t[0].eval_subtree() + t[1].eval_subtree())
    infix("k_plus", 10, "left",
          val_type=t_str, arg_types=[t_str, t_str],
          eval_fun=lambda t: t[0].eval_subtree()[:-1] + t[1].eval_subtree()[1:])

    infix("k_ast", 20, "left",
          val_type=t_int, arg_types=[t_int, t_int],
          eval_fun=lambda t: t[0].eval_subtree() * t[1].eval_subtree())
    infix("k_ast", 20, "left",
          val_type=t_str, arg_types=[t_str, t_int],
          eval_fun=lambda t: (
                   '"' + (t[0].eval_subtree()[1:-1] * t[1].eval_subtree()) + '"'))
    infix("k_ast", 20, "left",
          val_type=t_str, arg_types=[t_int, t_str],
          eval_fun=lambda t: (
                   '"' + (t[1].eval_subtree()[1:-1] * t[0].eval_subtree()) + '"'))

    # Define assignment as an infix equals operator.
    parser.def_assignment_op_dynamic("k_equals", 5, "left", "k_identifier",
                                     val_type=None, allowed_types=[t_int, t_str],
                                     create_eval_fun=True)
    return parser

def run_string_language_dynamic_typing_parser():

    class NumberStringLangREPL(cmd.Cmd, object):
        prompt = "> "
        intro = "Enter ^D to exit the dynamically-typed number and string language."

        def __init__(self):
            super(NumberStringLangREPL, self).__init__()
            self.parser = setup_string_language_parser_dynamic_typing()

        def emptyline(self):
            pass

        def default(self, line):
            try:
                result = self.parser.parse(line)
                print(result.tree_repr())
                print(result.eval_subtree())
            except (ValueError, ZeroDivisionError) as e:
                print(e)
            except pp.TypeErrorInParsedLanguage as e:
                print(e)
            except Exception as e:
                print(e)

        def do_EOF(self, line):
            print("\nBye.")
            return True

    NumberStringLangREPL().cmdloop()

def setup_string_language_parser_static_typing():
    """A simple statically-typed language that uses `+` to add integers and
    concatenate strings.  Multiplication of a number by a string repeats the
    string.  Multiplication of a string by a string is not defined.  It also
    has simple variables which can represent either numbers or strings."""
    parser = pp.PrattParser()

    # Define the tokens.

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

    # Define the types.

    t_int = parser.def_type("t_int") # Integer type.
    t_str = parser.def_type("t_str") # String type.

    # Define a new construct for type definitions in the language.  The
    # `typped_type_dict` is used to map type names in the implemented language
    # (the strings "int" and "str") to the corresponding Typped types
    # (the `TypeObject` instances t_int and t_str).

    parser.typped_type_dict = {"int": t_int,
                               "str": t_str}

    def head_handler(tok, lex):
        """Handler function that parses type declarations."""
        if not lex.match_next("k_identifier", consume=False):
            raise pp.ParserException("Type declaration not followed by an identifier.")

        parser.symbol_type_dict[lex.peek().value] = parser.typped_type_dict[tok.value]
        type_decl_expr = tok.recursive_parse(0)
        if type_decl_expr.children and type_decl_expr.token_label != "k_equals":
            # TODO: Could be done by argument type checking, too.
            raise pp.ParseException("Only identifiers or assignment expressions are"
                                    " allowed in type declarations.")
        tok.append_children(type_decl_expr)
        # TODO: wouldn't hurt to set the typesig override here, too.
        return tok

    def precond_fun(lex, lookbehind):
        """Construct will only be triggered for identifiers in `parser.typped_type_dict`."""
        return (lex.token.token_label == "k_identifier" and
                lex.token.value in parser.typped_type_dict)

    def operator_eval_fun(t):
        return t[0].value + " " + t.value + " " + t[1].value

    def eval_fun(tok):
        """Evaluate a type declaration when interpreting the language."""
        if tok[0].token_label == "k_equals":
            return operator_eval_fun(tok[0])
        else:
            return "None"

    parser.def_construct(pp.HEAD, head_handler, "k_identifier",
                         construct_label="c_type_declaration", precond_fun=precond_fun,
                         precond_priority=10, val_type=t_int, eval_fun=eval_fun)

    # Now define the syntax of the language.

    literal = parser.def_literal
    literal("k_int", val_type=t_int, eval_fun=lambda t: t.value)
    literal("k_string", val_type=t_str, eval_fun=lambda t: t.value)

    parser.def_literal_typed_from_dict("k_identifier",
                                       eval_fun=lambda t: t.value,
                                       default_type=t_int, default_eval_value=0)

    parser.def_bracket_pair("k_lpar", "k_rpar", # TODO square too, to be different...
                            eval_fun=lambda t: "(" + t[1].eval_subtree() + ")")

    infix = parser.def_infix_op
    infix("k_plus", 10, "left", val_type=t_int, arg_types=[t_int, t_int],
            eval_fun=operator_eval_fun)
    infix("k_plus", 10, "left", val_type=t_str, arg_types=[t_str, t_str],
            eval_fun=operator_eval_fun)

    infix("k_ast", 20, "left", val_type=t_int, arg_types=[t_int, t_int],
            eval_fun=operator_eval_fun)
    infix("k_ast", 20, "left", val_type=t_str, arg_types=[t_str, t_int],
            eval_fun=operator_eval_fun)
    infix("k_ast", 20, "left", val_type=t_str, arg_types=[t_int, t_str],
            eval_fun=operator_eval_fun)

    # Define assignment as an infix equals operator.
    parser.def_assignment_op_static("k_equals", 5, "left", "k_identifier",
                                     eval_fun=operator_eval_fun)
    return parser

def run_string_language_static_typing_parser():

    class NumberStringLangREPL(cmd.Cmd, object):
        prompt = "> "
        intro = "Enter ^D to exit the statically-typed number and string language."

        def __init__(self):
            super(NumberStringLangREPL, self).__init__()
            self.parser = setup_string_language_parser_static_typing()
            self.globals_dict = {"__builtins__": __builtins__}
            self.locals_dict = {}

        def emptyline(self):
            pass

        def default(self, line):
            try:
                result = self.parser.parse(line)
                print("Parsed expression tree:\n", result.tree_repr(), sep="")
                print("Translation to Python:")
                python_command = result.eval_subtree()
                print(python_command)
                print("\nPython evaluation:")
                result = eval(compile(python_command, '<string>', 'single'),
                                      self.globals_dict, self.locals_dict)
                print()
            except (ValueError, ZeroDivisionError) as e:
                print(e)
            except pp.TypeErrorInParsedLanguage as e:
                print(e)
            except Exception as e:
                print(e)

        def do_EOF(self, line):
            print("\nBye.")
            return True

    NumberStringLangREPL().cmdloop()


if __name__ == "__main__":
    underline_len = 70
    print("\nExample 1, simple language using builtins.")
    print("=" * underline_len + "\n")
    run_simple_builtin_example()

    print("\nExample 2, simple language not using builtins.")
    print("=" * underline_len + "\n")
    run_simple_non_builtin_example()

    print("\nExample 2, dynamically typed word and string language.")
    print("=" * underline_len + "\n")
    run_string_language_dynamic_typing_parser()

    print("\nExample 3, statically typed word and string language.")
    print("=" * underline_len + "\n")
    run_string_language_static_typing_parser()


