# -*- coding: utf-8 -*-
"""

These are predefined, built-in parsing routines designed to parse various
general types of constructs.  The functions can be called directly, accessed
from the `typped` package namespace, or as methods of `PrattParser` instances.
(The functions are copied to the `PrattParser` class after they are defined
because the namespace is convenient to use.)

As a reminder, here a comparison of the terminology used in the Typped package
with the traditional Pratt parser terminology:

+----------------------------------+--------------------------+
| This code                        | Pratt's terminology      |
+==================================+==========================+
| token precedence                 | left binding power, lbp  |
+----------------------------------+--------------------------+
| subexpression precedence         | right binding power, rbp |
+----------------------------------+--------------------------+
| head handler function            | null denotation, nud     |
+----------------------------------+--------------------------+
| tail handler function            | left denotation, led     |
+----------------------------------+--------------------------+

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_production_rules.py",
                              "../../test/test_example_calculator.py",
                              "../../test/test_parser_called_from_parser.py",
                              "../../test/test_pratt_parser.py"
                              ], pytest_args="-v")

from .shared_settings_and_exceptions import (HEAD, TAIL, ParserException,
        NoHandlerFunctionDefined, CalledBeginTokenHandler, CalledEndTokenHandler)
from .lexer import Lexer, TokenNode, TokenTable, multi_funcall
from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage


#
# Methods defining syntax elements.
#

# TODO these define and undefine methods each need a corresponding undefine
# method (or one that does all); should be easy with undef_handler method.
# Is it necessary to call undef_handler, or does undef the token do that
# too?  Look at undef_handler and see........ use below if needed.
#
# ALSO, consider defining them in a separate file and then importing them
# and pasting them onto the class, just to keep the code separate.

#
# Token literals.
#

def def_literal(parser, token_label, val_type=None,
                construct_label=None, precond_fun=None, precond_priority=1,
                typesig_override_fun=None,
                eval_fun=None, ast_data=None):
    """Defines the token with label `token_label` to be a literal in the
    syntax of the language being parsed.  This method adds a head handler
    function to the token.  Literal tokens are the leaves of the expression
    trees; they are things like numbers and variable names in a numerical
    expression.  They always occur as the first (and only) token in a
    subexpression being evaluated by `recursive_parse`, so they need a head
    handler but not a tail handler.  (Though note that the token itparser
    might also have a tail handler.)

    A function `typesig_override_fun` can be passed in, taking a token and
    a lexer as its two arguments and returning a `TypeSig` object.  If it
    is set then it will be called from the head handler and the type
    signature of the node will be assigned the returned signature.  Can be
    useful for dynamic typing such as when identifiers in an interpreted
    language are generic variables."""
    def head_handler_literal(tok, lex):
        if typesig_override_fun:
            tok.process_and_check_kwargs = {"typesig_override":
                                            typesig_override_fun(tok, lex)}
                                            #"check_override_sig": True,
        return tok
    return parser.def_construct(HEAD, head_handler_literal, token_label,
                              val_type=val_type, arg_types=(),
                              construct_label=construct_label,
                              precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              eval_fun=eval_fun, ast_data=ast_data)


def def_multi_literals(parser, tuple_list):
    """An interface to the `def_literal` method which takes a list of
    tuples.  The `def_literal` method will be called for each tuple, unpacked
    in the order in the tuple.  Unspecified optional arguments get their default
    values."""
    # TODO: This unfortunately makes it difficult to change the order in the
    # def_literal function arguments... also causes problems with keyword
    # argument setting (need to be non-keywords).  Deprecate this, maybe,
    # at least make it preferred to use `lit = parser.def_literal` and then just
    # write that a bunch of times...
    return multi_funcall(parser.def_literal, tuple_list)

#
# Brackets and parens.
#

def def_bracket_pair(parser, lbrac_token_label, rbrac_token_label,
                     construct_label=None, precond_fun=None, precond_priority=0,
                     eval_fun=None, ast_data=None):
    """Define a matching bracket grouping operation.  The returned type is
    set to the type of its single child (i.e., the type of the contents of
    the brackets).  Defines a head handler for the left bracket token, so
    effectively gets the highest evaluation precedence.  As far as types,
    it is treated as a function that takes one argument of wildcard type
    and returns whatever type the argument has."""
    # TODO: Maybe allow optional comma_token_label for comma-separated.
    # Define a head for the left bracket of the pair.
    def head_handler(tok, lex):
        tok.append_children(tok.recursive_parse(0))
        lex.match_next(rbrac_token_label, raise_on_fail=True)
        if not parser.skip_type_checking:
            child_type = tok.children[0].expanded_formal_sig.val_type
            tok.process_and_check_kwargs = { #"check_override_sig":True,
                    "typesig_override": TypeSig(child_type, [child_type])}
        return tok

    return parser.def_construct(HEAD, head_handler, lbrac_token_label,
                             construct_label=construct_label, precond_fun=precond_fun,
                             precond_priority=precond_priority,
                             eval_fun=eval_fun, ast_data=ast_data)

#
# Assignment.
#

#def def_assignment_op(assignment_operator_label, prec, assoc, not_in_tree=False,
#                      val_type=None, arg_types=None, eval_fun=None, ast_data=None):


#
# Standard functions.
#

def def_stdfun(parser, fname_token_label, lpar_token_label,
                  rpar_token_label, comma_token_label,
                  precond_priority=1,
                  val_type=None, arg_types=None, eval_fun=None, ast_data=None,
                  num_args=None):
    """This definition of stdfun uses lookahead to the opening paren or
    bracket token.

    Note that all tokens must be defined as literal tokens except
    `fname_token_label` (which ends up as the root of the function
    evaluation subtree).  If the latter is also a literal token then
    `precond_priority` may need to be increased to give this use priority.

    The `num_args` parameter is optional for specifying the number of
    arguments when typing is not being used.  If it is set to a nonnegative
    number then it will automatically set `arg_types` to the corresponding
    list of `None` values; if `arg_types` is set then it is ignored."""
    if not parser.skip_type_checking and num_args is not None and arg_types is None:
        arg_types = [None]*num_args

    def preconditions(lex, lookbehind):
        """Must be followed by a token with label 'lpar_token_label', with no
        whitespace in-between."""
        peek_tok = lex.peek()
        if peek_tok.ignored_before: return False
        if peek_tok.token_label != lpar_token_label: return False
        return True

    def head_handler(tok, lex):
        # Below match is for a precondition, so it will match and consume.
        lex.match_next(lpar_token_label, raise_on_fail=True)
        # Read comma-separated subexpressions until the peek is rpar_token_label.
        while not lex.match_next(rpar_token_label, consume=False):
            tok.append_children(tok.recursive_parse(0))
            if not lex.match_next(comma_token_label):
                break
            else:
                # This checks for errors like f(x,)
                lex.match_next(rpar_token_label, raise_on_true=True)
        lex.match_next(rpar_token_label, raise_on_fail=True) # Closing rpar.
        tok.process_not_in_tree() # Need when comma is an operator that gets removed.
        if (parser.skip_type_checking and num_args is not None
                                    and len(tok.children) != num_args):
            print("tok is", tok, "tok children are", tok.children)
            raise ParserException("Wrong number of arguments for function {0}:"
                        " expected {1} and got {2}.""".format(tok.token_label,
                        num_args, len(tok.children)))
        return tok

    construct_label = "lpar after, no whitespace between"
    return parser.def_construct(HEAD, head_handler, fname_token_label, prec=0,
                construct_label=construct_label,
                precond_fun=preconditions, precond_priority=precond_priority,
                val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                ast_data=ast_data)


def def_stdfun_lpar_tail(parser, fname_token_label, lpar_token_label,
                         rpar_token_label, comma_token_label, prec_of_lpar,
                         val_type=None, arg_types=None, eval_fun=None, ast_data=None,
                         num_args=None):
    """This is an alternate version of stdfun that defines lpar as an infix
    operator (i.e., with a tail handler).  This function works in the usual cases
    but the current version without preconditions may have problems distinguishing
    "b (" from "b(" when a multiplication jop is set.  The lookahead version
    `def_stdfun` is usually preferred.  This method assumes type checking is on
    if `num_arg` is set."""
    if num_args is not None and arg_types is None:
        arg_types = [None]*num_args

    def precond_fun(lex, lookbehind):
        """Check that the peek backward token label for the function name
        is `fname_token_label`.  This is necessary to get the type sig info
        to work when different functions take different numbers (and
        possibly different types) of arguments.   Otherwise, defining two
        different functions for different tokens like `k_add` and `k_exp`
        is treated as an overload since both are really handled by the
        `lpar_token_label` token.  The label would otherwise never be checked.

        One could do a similar thing checking the value of the previous token
        if the fnames are all, say, identifiers or some common token kind.
        Maybe even have a flag to indicate this when worked out better."""
        prev_tok = lex.peek(-1)
        if prev_tok.token_label != fname_token_label: return False
        if lex.token.ignored_before: return False # No space allowed after fun name.
        return True

    def tail_handler(tok, lex, left):
        # Nothing between fun name and lpar_token.
        lex.no_ignored_before(raise_on_fail=True)
        while not lex.match_next(rpar_token_label, consume=False):
            left.append_children(tok.recursive_parse(prec_of_lpar))
            if not lex.match_next(comma_token_label):
                break
            else:
                lex.match_next(rpar_token_label, raise_on_true=True)
        lex.match_next(rpar_token_label, raise_on_fail=True)
        return left

    # Note we need to generate a unique construct_label for each fname_token_label.
    construct_label = "match desired function name of " + fname_token_label
    return parser.def_construct(TAIL, tail_handler, lpar_token_label,
                              prec=prec_of_lpar,
                              construct_label=construct_label,
                              precond_fun=precond_fun,
                              val_type=val_type, arg_types=arg_types,
                              eval_fun=eval_fun, ast_data=ast_data)

#
# Infix operators.
#

def def_infix_multi_op(parser, operator_token_labels, prec, assoc,
                       repeat=False, not_in_tree=False,
                       construct_label=None, precond_fun=None, precond_priority=0,
                       val_type=None, arg_types=None, eval_fun=None, ast_data=None):
    # TODO only this utility method currently supports "not_in_tree" kwarg.
    # General and easy mechanism, though.  Test more and add to other
    # methods.  Does in-tree keep the first one? how is it defined for this
    # thing?  Comma operator is example of not_in_tree=True, but how does it
    # handle the root??  TODO: How about in-tree that works at root iff the
    # node only has one child?
    """Takes a list of operator token labels and defines a multi-infix
    operator.

    If `repeat=True` it will accept any number of repetitions of
    the list of operators (but type-checking for that is not implemented
    yet).  For a single operator, repeating just has the effect of putting
    the arguments in a flat argument/child list instead of as nested binary
    operations based on left or right association.  Any argument-checking
    is done after any node removal, which may affect the types that should
    be passed-in in the list arg_types of parent constructs.

    If `not_in_tree` is false.......
    """
    if assoc not in ["left", "right"]:
        raise ParserException('Argument assoc must be "left" or "right".')
    recurse_bp = prec
    if assoc == "right": recurse_bp = prec - 1

    def tail_handler(tok, lex, left):
        tok.append_children(left, tok.recursive_parse(recurse_bp))
        while True:
            for op in operator_token_labels[1:]:
                lex.match_next(op, raise_on_fail=True)
                #assert tok.prec() == recurse_bp or tok.prec()-1 == recurse_bp # DEBUG
                tok.append_children(tok.recursive_parse(recurse_bp))
            if not repeat: break
            # Peek ahead and see if we need to loop another time.
            if lex.peek().token_label != operator_token_labels[0]: break
            lex.match_next(operator_token_labels[0], raise_on_fail=True)
            tok.append_children(tok.recursive_parse(recurse_bp))
        if not_in_tree: tok.not_in_tree = True
        tok.process_and_check_kwargs = {"repeat_args": repeat}
        return tok
    return parser.def_construct(TAIL, tail_handler, operator_token_labels[0], prec=prec,
                              construct_label=construct_label, precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              val_type=val_type, arg_types=arg_types,
                              eval_fun=eval_fun, ast_data=ast_data)


def def_infix_op(parser, operator_token_label, prec, assoc, not_in_tree=False,
                 construct_label=None, precond_fun=None, precond_priority=0,
                 val_type=None, arg_types=None, eval_fun=None, ast_data=None):
    """This just calls the more general method `def_multi_infix_op`."""
    return parser.def_infix_multi_op([operator_token_label], prec, assoc,
                                   not_in_tree=not_in_tree, construct_label=construct_label,
                                   precond_fun=precond_fun,
                                   precond_priority=precond_priority,
                                   val_type=val_type, arg_types=arg_types,
                                   eval_fun=eval_fun, ast_data=ast_data)

#
# Prefix operators.
#

def def_prefix_op(parser, operator_token_label, prec,
                  construct_label=None, precond_fun=None, precond_priority=0,
                  val_type=None, arg_types=None,
                  eval_fun=None, ast_data=None):
    """Define a prefix operator.  Note that head handlers do not have
    precedences, only tail handlers.  (With respect to the looping in
    `recursive_parse` it wouldn't make a difference.)  But, within the head
    handler, the call to `recursive_parse` can be made with a nonzero
    precedence.  This allows setting a precedence to determine the argument
    expressions that the prefix operators grabs up (or doesn't)."""
    def head_handler(tok, lex):
        tok.append_children(tok.recursive_parse(prec))
        return tok

    return parser.def_construct(HEAD, head_handler, operator_token_label,
                              construct_label=construct_label, precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                              ast_data=ast_data)

#
# Postfix operators.
#


def def_postfix_op(parser, operator_token_label, prec, allow_ignored_before=True,
                   construct_label=None, precond_fun=None, precond_priority=0,
                   val_type=None, arg_types=None, eval_fun=None,
                   ast_data=None):
    """Define a postfix operator.  If `allow_ignored_before` is false then
    no ignored token (usually whitespace) can appear immediately before the
    operator."""
    def tail_handler(tok, lex, left):
        if not allow_ignored_before:
            lex.no_ignored_before(raise_on_fail=True)
        tok.append_children(left)
        return tok

    return parser.def_construct(TAIL, tail_handler, operator_token_label, prec=prec,
                             construct_label=construct_label, precond_fun=precond_fun,
                             precond_priority=precond_priority,
                             val_type=val_type, arg_types=arg_types,
                             eval_fun=eval_fun, ast_data=ast_data)


#
# Juxtaposition operators.
#

def def_jop(parser, prec, assoc,
            construct_label=None, precond_fun=None, precond_priority=None,
            val_type=None, arg_types=None, eval_fun=None,
            ast_data=None):
    """The function `precond_fun` is called to determine whether or not to
    infer a juxtaposition operator between the previously-parsed
    subexpression result and the next token.  This function will be passed
    the lexer as well as the lookbehind list as arguments.  Note that the
    `jop_precond` function has access to the type information for the
    potential left operand but not for the potential right operand.  If
    this function returns `True` then a jop is inferred and the parse
    proceeds assuming there is a jop token in the token stream.

    Note that if the juxtaposition operator always resolves to a single
    type signature based on its argument types then, even if overloading on
    return types is in effect, the jop can be effectively inferred based on
    type signature information."""
    if assoc not in ["left", "right"]:
        raise ParserException('Argument assoc must be "left" or "right".')
    recurse_bp = prec
    if assoc == "right": recurse_bp = prec - 1

    def tail_handler(tok, lex, left):
        right_operand = tok.recursive_parse(recurse_bp)
        tok.append_children(left, right_operand)
        return tok

    return parser.def_construct(TAIL, tail_handler, parser.jop_token_label, prec=prec,
                              construct_label=construct_label, precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              val_type=val_type, arg_types=arg_types,
                              eval_fun=eval_fun, ast_data=ast_data)

#
# The list of defined functions to be made into methods of the PrattParser class.
#

parse_methods = [def_literal,
                 def_multi_literals,
                 def_infix_multi_op,
                 def_infix_op,
                 def_prefix_op,
                 def_postfix_op,
                 def_bracket_pair,
                 def_stdfun,
                 def_stdfun_lpar_tail,
                 def_jop,
                 ]

