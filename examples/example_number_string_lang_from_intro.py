# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    #pytest_helper.script_run("../test/test_example_number_string_lang_from_intro.py",
    #                         pytest_args="-v")

#pytest_helper.sys_path("../src") # Only needed when not pip installed.
import typped as pp
import operator
import cmd
import readline

def setup_parser():
    """An example from the Sphinx docs overview section.  A simple language
    that uses `+` to add integers and concatenate strings.  Multiplication of a
    number by a string repeats the string.  Multiplication of a string by a
    string is not defined.  It also has simple variables which can represent
    either numbers or strings."""
    parser = pp.PrattParser()
    parser.def_default_whitespace()

    tok = parser.def_token
    tok("k_number", r"\d+")
    tok("k_lpar", r"\(")
    tok("k_rpar", r"\)")
    tok("k_ast", r"\*")
    tok("k_plus", r"\+")
    tok("k_equals", r"=")
    tok("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    tok("k_string", r"(\"(.|[\r\n])*?\")")

    t_number = parser.def_type("t_number") # Number type.
    t_string = parser.def_type("t_string") # String type.

    parser.def_literal("k_number", val_type=t_number, eval_fun=lambda t: int(t.value))

    # Overload identifiers to return either string or number.
    # Alternative: use a precond on assignment that checks if symbol defined in dict...
    # BUT you still need this, because you need to set the type...
    # REMEMBER, though, that literal identifiers are parsed in the parse step, BUT
    # the evaluation step happens AFTER that.  For full generality you should do the
    # evaluation AS you do the parsing, calling eval_subtree on each thing.  That could
    # maybe be an option.  But don't really need full generality, just assign in different
    # statement is OK here.
    symbol_dict = {}
    symbol_type_dict = {}
    parser.symbol_dict = symbol_dict # Make symbol dict an attribute of the parser instance.
    parser.symbol_type_dict = symbol_type_dict # Types, too.

    def literal_typesig_override_fun(tok, lex):
        symbol_type_dict = tok.parser_instance.symbol_type_dict
        if tok.value in symbol_type_dict:
            return pp.TypeSig(symbol_type_dict[tok.value], [])
        else:
            return pp.TypeSig(t_number, []) # Default is number 0.

    parser.def_literal("k_identifier", val_type=None,
                       typesig_override_fun=literal_typesig_override_fun,
                       eval_fun=lambda t: symbol_dict.get(t.value, 0))

    parser.def_literal("k_string", val_type=t_string, eval_fun=lambda t: t.value)

    parser.def_bracket_pair("k_lpar", "k_rpar", eval_fun=lambda t: t[0].eval_subtree())


    parser.def_infix_op("k_plus", 10, "left",
                     val_type=t_number, arg_types=[t_number, t_number],
                     eval_fun=lambda t: t[0].eval_subtree() + t[1].eval_subtree())
    parser.def_infix_op("k_plus", 10, "left",
                     val_type=t_string, arg_types=[t_string, t_string],
                     eval_fun=lambda t: t[0].eval_subtree()[:-1] + t[1].eval_subtree()[1:])

    parser.def_infix_op("k_ast", 20, "left",
                     val_type=t_number, arg_types=[t_number, t_number],
                     eval_fun=lambda t: t[0].eval_subtree() * t[1].eval_subtree())
    parser.def_infix_op("k_ast", 20, "left",
                     val_type=t_string, arg_types=[t_string, t_number],
                     eval_fun=lambda t: (
                         '"' + (t[0].eval_subtree()[1:-1] * t[1].eval_subtree()) + '"'))

    def eval_assign(t):
        """Evaluate the identifier token `t` and save the value in `symbol_dict`."""
        rhs = t[1].eval_subtree()
        symbol_dict[t[0].value] = rhs
        symbol_type_dict[t[0].value] = t[1].expanded_formal_sig.val_type
        return rhs

    # No longer FAILS on <-- test for these though
    # (4)
    # z = "egg" * 4
    def def_assignment_op(parser, assignment_operator_token_label, prec, assoc,
                   left_argument_token_label, precond_priority=0,
                   val_type=None, arg_types=None, eval_fun=None, ast_data=None):
        def precondition_lhs_is_identifier(lex, lookbehind):
            return lex.peek(-1).token_label == "k_identifier"
        parser.def_infix_op(assignment_operator_token_label, prec, assoc,
                            precond_fun=precondition_lhs_is_identifier,
                            precond_label="identifier before assignment checker",
                            precond_priority=precond_priority,
                            val_type=val_type, arg_types=arg_types,
                            eval_fun=eval_fun)
    def_assignment_op(parser, "k_equals", 5, "left", "k_identifier",
                      val_type=t_number, arg_types=[None, t_number], eval_fun=eval_assign)
    def_assignment_op(parser, "k_equals", 5, "left", "k_identifier",
                      val_type=t_string, arg_types=[None, t_string], eval_fun=eval_assign)
    return parser

parser = setup_parser()

parser.parse("ee")
parser.parse("ee = 4")

class NumberStringLangREPL(cmd.Cmd, object):
    prompt = "> "
    intro = "Enter ^D to exit the calculator."

    def __init__(self):
        super(NumberStringLangREPL, self).__init__()
        self.parser = setup_parser()

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
        except Exception as e:
            print(e)

    def do_EOF(self, line):
        print("\nBye.")
        return True

NumberStringLangREPL().cmdloop()


#result_tree = parser.parse("x + (4 + 3)*5")
#print(result_tree.tree_repr())
#print(result_tree.string_repr_with_types())
#assert result_tree.string_repr_with_types() == "<k_plus,+,TypeObject(t_number)>(<k_identifier,x,TypeObject(t_number)>,<k_ast,*,TypeObject(t_number)>(<k_lpar,(,TypeObject(t_number)>(<k_plus,+,TypeObject(t_number)>(<k_number,4,TypeObject(t_number)>,<k_number,3,TypeObject(t_number)>)),<k_number,5,TypeObject(t_number)>))"
#result_tree = parser.parse('"foo" + "bar"')
#print(result_tree.tree_repr())
#print(result_tree.string_repr_with_types())
#assert result_tree.string_repr_with_types() == """<k_plus,+,TypeObject(t_string)>(<k_string,"foo",TypeObject(t_string)>,<k_string,"bar",TypeObject(t_string)>)"""
##fail()
#
