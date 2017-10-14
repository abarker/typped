# -*- coding: utf-8 -*-
"""

Define and parse a basic predicate logic of statements about real numbers.

The grammar
-----------

The basic grammar in EBNF is:
::
    <wff> = <atomic_formula>
          | 'not' <wff>
          | '(' <wff> ')'
          | <wff> <infix_logical_operator> <wff>
          | <quantifier_expr> <wff>
          | <wff_subst_function>

    <atomic_formula> = <predicate_name> '(' <term_list> ')'

    <term> = '(' <term> ')'
           | <function_eval>
           | <variable_name>
           | <constant_name>
           | <term_subst_function>

    <function_eval> = <standard_function_eval>
                    # Note parens on operators here; precedence rules allow removal.
                    | '(' <term> <infix_term_operator> <term> ')'
                    | '(' <prefix_term_operator> <term> ')'
                    | '(' <term> <postfix_term_operator> ')'

    <standard_function_eval> = <function_name> '(' <term_list> ')'

    <quantifier_expr> = '(' <quantifier> <variable_or_wff_list> ')

    <quantifier> = 'forall'
                 | 'exists'
                 | 'exists_1'
                 | 'forall^' <constant_name>
                 | 'exists^' <constant_name>

    <variable_or_wff_list> = <variable_name> ',' <variable_or_wff_list>
                           | <variable_name>
                           | <wff> ',' <variable_or_wff_list>
                           | <wff>

    <term_list> = <term> ',' <term_list>
                | <term>

    <infix_logical_operator> = '->' | '<->' | 'and' | 'or'

    <infix_term_operator> = '+' | '-' | '*' | '/' | '^' | '=' | '<' | '>'
    <prefix_term_operator> = '+' | '-'
    <postfix_term_operator> = '!'

    <variable_name> = <identifier>
    <constant_name> = <identifier>
    <predicate_name> = <identifier>
    <function_name> = <identifier>

    <identifier> = Regex([a-z_]+)

    <wff_subst_function> = TODO
    <term_subst_function> = TODO

Note that ignoring quantifiers and considering the boolean operators as
functions returning boolean values this grammar is nothing but a functional
grammar: Everything is a nested sequence of function calls.  Some of the
function calls just happen to be denoted by infix or prefix operators with
precedences and parenthesization.  So this grammar is simple for a Pratt parser
to parse.

* In the Typped parser atomic formulas and function evaluations can be
  defined as standard functions with `def_stdfun`.  The <term_list> production
  is implicitly handled.

* Grouping parentheses can be defined with `def_bracket_pair`.

* The logical not and prefix positive and negative operators can be defined with
  `def_prefix_op`, and all the infix operators (including the logic operators)
  with `def_infix_op`.  The `term_list` production is then not explicitly needed.

  The operator precedences are as follows, with the chosen numerical values to the
  right.
  ::

        highest
               ( )                        implicit in grammar
               forall, exists (prefix)    130  # TODO should be lowest
               +, - (prefix)              120
               !    (postfix)             110
               ^                          100
               *, /                       90
               +, - (infix)               80
               >, <                       70
               =                          60
               not                        50
               and                        40
               or                         30
               ->                         20
               <->                        10
        lowest

There are two basic types: boolean and whatever type is assigned to objects in
the domain of interpretation (i.e., the type of variables, constants, and
function return values).  They could just have the type `t_termtype`, but for
this example real numbers will be assumed as the domain objects.

Parsing quantifiers
-------------------

Parentheses around quantifier expressions and groups of quantifier expressions
are not strictly necessary logically, but they are quite helpful as far as
visually separating and grouping parts of formulas.  Unfortunately they
complicate the handling of quantifiers.

All of these are valid quantifier expressions:
::
    * AxEy <wff>
    * (Ax)(Ey) <wff>
    * (Ax,y)Ez,w <wff>
    * Ax(Ey) <wff>
    * (Ax)Ey <wff>
    * (AxEy <wff>)
    * ((Ax)Ey <wff>)
    * (((Ax)Ey <wff>))
    * (Ax x<y and Ex x<z)
    * ((AxEy)(AwEb) <wff>)

The variables in the above formulas could also be wffs (with at least one free
variable).  For example, Ez(Ax < y)(x = z)

There are different ways to handle the parsing of such quantifier expressions
in a Pratt parser.  In this example the parsing is done by defining a prefix
operator for the quantifier tokens and a special case in the handler for the
`k_lpar` token for when only quantifier expressions are inside parens.
Predicate expressions are essentially treated like meta-level operators which
operate on a wff to their right.

The final tree structure will have children for each quantifier expression and
the wff as the last child.  Any paren structure around the quantifiers is
maintained.  Every token that is processed as a quantifier expression is marked
by setting the attribute `is_quantifier_expr` true (but it is not set
otherwise, so `hasattr` should be used).

The quantifier prefix operator always does one `recursive_parse` to read its
first variable (or full wff if allowed).  If the peek shows a comma it consumes
it and gets another until no comma is seen.  Then it does one `recursive_parse`
to get the wff to the right (which may itself be quantified).

The usual lpar prefix token is modified in the case where after its first
`recursive_parse` call it has ONLY a child list of (??? list of or ONE, can
define either way ???) quantifer expressions.  In that case it must have been
only around a quantifier expression.  So it consumes the rpar and then does
another `recursive_parse` to read the wff to its right (which may also have
quantifiers).  The result is "converted into" a wff.

Using a wff inside a quantifier is restricted to the case where the quantifier
expression is in parentheses.  This simplifies parsing since cases like
(Ax>10 x>11) are disallowed in favor of ((Ax>10) x>11), which is much easier
to read, anyway.  In this case when not inside parens you can assume a variable
or variable list, and when inside parens you can peek to look for either a comma or
a right paren.  Without parens the parsing problems are especially bad if jops
are used in the wff, for example, (Ax x+y>100), maybe followed by a wff or not,
is ambiguous without deeper analysis or backtracking.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

#pytest_helper.script_run(self_test=True, pytest_args="-v")
#pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import math
import operator
import typped as pp

# Some naming conventions.
#
# Lexer:
#    tokens:    k_float
# Syntax:
#    types:     t_int
# AST labels:
#    a_number

def define_logic_parser():
    """Define a subclass of `PrattParser` as the `LogicParser`, so that it is
    easy add any non-builtin functionality need later."""

    class LogicParser(pp.PrattParser):
        def __init__(self, *args, **kwargs):
            super(LogicParser, self).__init__(*args, **kwargs)

        def old_def_quantifier(self, quant_token_label, variable_token_label,
                           comma_token_label, lpar_token_label, rpar_token_label,
                           var_type, wff_type):
            """Define a prefix operator for a quantifier."""
            def head_handler(tok, lex):
                if lex.peek(-1).token_label != lpar_token_label:
                    raise pp.ParserException("All quantifier expressions must be"
                            " inside parentheses.") # Improve message, where stopped.
                tok.is_quantifier_expr = True # The paren handler looks for this.
                override_arg_list = []
                while True:
                    child = tok.recursive_parse(0)
                    tok.append_children(child)
                    if child.token_label == variable_token_label:
                        override_arg_list.append(var_type)
                    else:
                        if child.val_type != wff_type:
                            raise pp.ParserException("Error in parsing quantifier"
                                    " expression.  A wff was expected but the"
                                    " resulting expression is not boolean-valued."
                                    " The parsed token is: {0}".format(child))
                        override_arg_list.append(wff_type)
                    peek_one = lex.peek()
                    if peek_one.token_label == comma_token_label:
                        tok.match_next(comma_token_label, raise_on_fail=True)
                    elif peek_one.token_label == rpar_token_label:
                        break
                    else:
                        raise pp.ParserException("Incorrectly-formed quantifier"
                                " expression.  Expected a comma or a right paren"
                                " which was not found.") # Improve message.
                override_sig = pp.TypeSig(wff_type, override_arg_list)
                tok.process_and_check_kwargs = {"typesig_override": override_sig,
                                                "check_override_sig": True}
                return tok
            return self.def_construct(pp.HEAD, head_handler, quant_token_label)

        def def_quantifier_expr(self, quant_token_label, variable_token_label,
                           comma_token_label, lpar_token_label, rpar_token_label,
                           var_type, wff_type):
            """Define a prefix operator for a quantifier."""
            def quantifier_following_lpar(tok, lex):
                if lex.peek().token_label == quant_token_label: return True
                return False
            # Note precondition label unique to quantifier; distinct handler funs.
            construct_label = "lpar followed by " + quant_token_label

            def head_handler(tok, lex):
                """Note the handler is for the lpar token, but the quantifier token
                is made into the parse tree node."""
                quantifier_tok = tok.recursive_parse(0)
                tok.is_quantifier_expr = True # The paren handler looks for this.
                override_arg_list = []
                while True:
                    child = tok.recursive_parse(0)
                    quantifier_tok.append_children(child)
                    if child.token_label == variable_token_label:
                        override_arg_list.append(var_type)
                    else:
                        if child.val_type != wff_type:
                            raise pp.ParserException("Error in parsing quantifier"
                                    " expression.  A wff was expected but the"
                                    " resulting expression is not boolean-valued."
                                    " The parsed token is: {0}".format(child))
                        override_arg_list.append(wff_type)
                    peek_one = lex.peek()
                    if peek_one.token_label == comma_token_label:
                        tok.match_next(comma_token_label, raise_on_fail=True)
                    elif peek_one.token_label == rpar_token_label:
                        break
                    else:
                        raise pp.ParserException("Incorrectly-formed quantifier"
                                " expression.  Expected a comma or a right paren"
                                " which was not found.") # Improve message.
                tok.match_next(rpar_token_label, raise_on_fail=True)

                # Now read the wff following the quantifier expression.
                quantifier_tok.append_children(tok.recursive_parse(0))
                override_arg_list.append(wff_type)
                override_sig = pp.TypeSig(wff_type, override_arg_list)

                # Note quantifier_tok is the returned root.
                quantifier_tok.process_and_check_kwargs = {
                                           "typesig_override": override_sig,
                                           "check_override_sig": True}
                return quantifier_tok
            # Note that the handler is registered with the lpar token.
            return self.def_construct(pp.HEAD, head_handler, lpar_token_label,
                    construct_label=construct_label, precond_fun=quantifier_following_lpar)

        def old_def_paren_pair(self, lpar_token_label, rpar_token_label, wff_type):
            """Special redefinition of parentheses to handle the case of parens
            around a quantifier expression.  This is needed because a quantifier
            in parens is essentially a prefix operator which, in that case only,
            needs to also read the wff to its right as an argument."""
            def head_handler(tok, lex):
                """Head handler for the lpar token."""
                first_child = tok.recursive_parse(0)
                tok.append_children(first_child)
                if hasattr(first_child, "is_quantifier_expr"):
                    # Parens around a quantifier expression; get wff as second child.
                    delattr(first_child, "is_quantifier_expr")
                    tok.match_next(rpar_token_label, raise_on_fail=True)
                    tok = tok.children[0] # Return the quantifier token, not the paren.
                    tok.append_children(tok.recursive_parse(0))
                    override_sig = pp.TypeSig(tok.val_type,
                                              tok.type_sig.arg_types + (wff_type,))
                else: # Ordinary parens.
                    child_type = tok.children[0].val_type
                    override_sig = pp.TypeSig(child_type, [child_type])
                    tok.match_next(rpar_token_label, raise_on_fail=True)
                tok.process_and_check_kwargs = {"typesig_override": override_sig,
                                                "check_override_sig": True}
                return tok

            return self.def_construct(pp.HEAD, head_handler, lpar_token_label)

    return LogicParser()

def define_logic_language(parser):

    #
    # Some general tokens.
    #

    whitespace_tokens = [
            ("k_space", r"[ \t]+"),       # Note + symbol, one or more, NOT * symbol.
            ("k_newline", r"[\n\f\r\v]+") # Note + symbol, one or more, NOT * symbol.
            ]
    parser.def_multi_ignored_tokens(whitespace_tokens)

    token_list = [
            # Logic operators.
            ("k_not", r"not"),
            ("k_implies", r"->"),
            ("k_iff", r"<->"),
            ("k_and", r"and"),
            ("k_or", r"or"),

            # Term operators.
            ("k_plus", r"\+"),
            ("k_minus", r"-"),
            ("k_asterisk", r"\*"),
            ("k_slash", r"/"),
            ("k_caret", r"\^"),
            ("k_equals", r"="),
            ("k_greater", r">"),
            ("k_less", r"<"),
            ("k_bang", r"!"),

            # Quantifiers. High lexer priority because they are keywords.
            ("k_forall", r"A", 100), # Matches function without priority.
            ("k_exists", r"E", 100),

            # Grouping and comma.
            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_comma", r","),

            # Bool constants.
            #("k_true", r"True"),
            #("k_false", r"False"),

            # Names (variables, constants, predicates, functions).
            ("k_var_name", r"[vV][a-zA-Z_0-9]*"),
            ("k_const_name", r"[cC][a-zA-Z_0-9]*"),
            ("k_pred_name", r"[pP][a-zA-Z_0-9]*"),
            ("k_fun_name", r"[fF][a-zA-Z_0-9]*"),
            ]
    parser.def_multi_tokens(token_list)

    #
    # Basic syntax.
    #

    t_Bool = parser.def_type("t_Bool")
    t_Real = parser.def_type("t_Real")

    # Note that names which will be defined as functions should not be made literals.
    literals = [
                # Names.
                ("k_var_name", t_Real),
                ("k_const_name", t_Real),
               ]
    parser.def_multi_literals(literals)

    #
    # Parens, highest precedence since they have a head function.
    #

    #parser.def_paren_pair("k_lpar", "k_rpar", t_Bool) # Use the newly-defined method.

    #
    # Define predicates functions as standard functions.
    #

    parser.def_stdfun("k_pred_name", "k_lpar", "k_rpar", "k_comma",
                                                 val_type=t_Bool, arg_types=t_Real)
    parser.def_stdfun("k_fun_name", "k_lpar", "k_rpar", "k_comma",
                                                 val_type=t_Real, arg_types=t_Real)

    #
    # Define the quantifiers.  These are handlers for the leading lpar with precond.
    #

    # Here we slightly abuse preconditions to generate better error messages when
    # someone forgets to put parens around a quantifier expression.  If the
    # precondition ever matches at all it raises an exception.
    def preceeded_by_lpar(tok, lex):
        if lex.peek(-1).token_label != "k_lpar":
            raise pp.ParserException("Quantifier expressions must be inside parens.")
        return False
    construct_label = "preceeded by lpar"

    parser.def_literal("k_forall",
            construct_label=construct_label, precond_fun=preceeded_by_lpar)
    parser.def_literal("k_forall") # Note we need the ordinary definition, too.

    parser.def_quantifier_expr("k_forall", "k_var_name", "k_comma", "k_lpar", "k_rpar",
                                                                    t_Real, t_Bool)
    parser.def_literal("k_exists",
            construct_label=construct_label, precond_fun=preceeded_by_lpar)
    parser.def_literal("k_exists")
    parser.def_quantifier_expr("k_exists", "k_var_name", "k_comma", "k_lpar", "k_rpar",
                                                                    t_Real, t_Bool)

    #
    # Operators, from highest to lowest precedence.
    #

    parser.def_prefix_op("k_minus", 120, val_type=t_Real, arg_types=[t_Real])
    parser.def_prefix_op("k_plus", 120, val_type=t_Real, arg_types=[t_Real])

    parser.def_postfix_op("k_bang", 110, val_type=t_Real, arg_types=[t_Real])

    parser.def_infix_op("k_caret", 100, "right", val_type=t_Real, arg_types=[t_Real,t_Real])

    parser.def_infix_op("k_asterisk", 90, "left", val_type=t_Real, arg_types=[t_Real,t_Real])
    parser.def_infix_op("k_slash", 90, "left", val_type=t_Real, arg_types=[t_Real,t_Real])

    parser.def_infix_op("k_plus", 80, "left", val_type=t_Real, arg_types=[t_Real,t_Real])
    parser.def_infix_op("k_minus", 80, "left", val_type=t_Real, arg_types=[t_Real,t_Real])

    parser.def_infix_op("k_greater", 70, "left", val_type=t_Bool, arg_types=[t_Real,t_Real])
    parser.def_infix_op("k_less", 70, "left", val_type=t_Bool, arg_types=[t_Real,t_Real])

    parser.def_infix_op("k_equals", 60, "left", val_type=t_Bool, arg_types=[t_Real,t_Real])

    parser.def_prefix_op("k_not", 50, val_type=t_Bool, arg_types=[t_Bool])

    parser.def_infix_op("k_and", 40, "left", val_type=t_Bool, arg_types=[t_Bool,t_Bool])

    parser.def_infix_op("k_or", 30, "left", val_type=t_Bool, arg_types=[t_Bool,t_Bool])

    parser.def_infix_op("k_implies", 20, "left", val_type=t_Bool, arg_types=[t_Bool,t_Bool])

    parser.def_infix_op("k_iff", 10, "left", val_type=t_Bool, arg_types=[t_Bool,t_Bool])


def dummy_take_what_is_needed():
    # Note that the token for the exponentiation operator has multiple symbols
    # defined for it in its regex (both '**' and '^').  The same thing could
    # alternately be done by keeping the tokens separate and later defining a
    # handler function for each one, both of which do exponentiation.

    #
    # Standard functions.
    #

    # Note we use the None argument types to check the number of arguments.
    parser.def_token("k_sin", r"sin")
    parser.def_stdfun("k_sin", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.sin(t[0].eval_subtree()))
    parser.def_token("k_cos", r"cos")
    parser.def_stdfun("k_cos", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.cos(t[0].eval_subtree()))
    parser.def_token("k_sqrt", r"sqrt")
    parser.def_stdfun("k_sqrt", "k_lpar", "k_rpar", "k_comma", arg_types=[None],
                      eval_fun=lambda t: math.sqrt(t[0].eval_subtree()))

    # Note that log is overloaded because different numbers of arguments are
    # specified, and they have different eval funs.
    parser.def_token("k_log", r"log")
    parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma", num_args=1,
                      eval_fun=lambda t: math.log(t[0].eval_subtree()))
    parser.def_stdfun("k_log", "k_lpar", "k_rpar", "k_comma", num_args=2,
               eval_fun=lambda t: math.log(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Jop as synonym for multiplication.
    #

    jop_required_token = "k_space" # Set to None to not require any whitespace.
    parser.def_jop_token("k_jop", jop_required_token)
    parser.def_jop(20, "left",
            eval_fun=lambda t: operator.mul(t[0].eval_subtree(), t[1].eval_subtree()))

    #
    # Assign simple variable.
    #

    parser.calculator_symbol_dict = {} # Store symbol dict as a new parser attribute.
    symbol_dict = parser.calculator_symbol_dict

    symbol_dict["pi"] = math.pi # Predefine pi.
    symbol_dict["e"] = math.e # Predefine e.

    # Note that on_ties is -1, so function names will take precedence over identifiers.
    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    parser.def_literal("k_identifier",
            eval_fun=lambda t: symbol_dict.get(t.value, 0.0))

    def eval_assign(t):
        rhs = t[1].eval_subtree()
        symbol_dict[t[0].value] = rhs
        return rhs

    parser.def_infix_op("k_equals", 5, "right", ast_data="a_assign",
                        eval_fun=eval_assign)




def read_eval_print_loop(parser):
    try:
        read_input = raw_input
    except NameError:
        read_input = input

    print("Enter ^C to exit.")
    while True:
        try:
            line = read_input("> ")
        except (KeyboardInterrupt, EOFError):
            print("\nBye.")
            break
        if not line: continue

        try:
           parse_tree = parser.parse(line)
           #eval_value = parse_tree.eval_subtree()
        except (ValueError, ZeroDivisionError,
                pp.ParserException, pp.LexerException) as e:
            print(e)
            continue
        else:
            print(parse_tree.tree_repr())
            #print(eval_value)


def define_and_run_logic_repl():
    import readline
    parser = define_logic_parser()
    define_logic_language(parser)
    read_eval_print_loop(parser)

define_and_run_logic_repl()

