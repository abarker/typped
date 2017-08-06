# -*- coding: utf-8 -*-
"""

These are predefined, built-in parsing routines designed to parse various
general types of constructs.  These functions are methods of the `PrattParser`
class because that namespace is convenient to use.  They can also be used
directly, as standalone functions.  It can be useful to look at the source code
for ideas of how to implement general constructs which are not covered by a
builtin routine.

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
    pytest_helper.script_run(["../../test/test_ebnf_classes_and_operators.py",
                              "../../test/test_example_calculator.py",
                              "../../test/test_parser_called_from_parser.py",
                              "../../test/test_pratt_parser.py"
                              ], pytest_args="-v")

from .shared_settings_and_exceptions import (HEAD, TAIL, ParserException,
        NoHandlerFunctionDefined, CalledBeginTokenHandler, CalledEndTokenHandler)
from .lexer import Lexer, TokenNode, TokenTable, multi_funcall
from .pratt_types import (TypeTable, TypeSig, TypeErrorInParsedLanguage,
                         actual_matches_formal_default)
from .helpers import all_precond_funs

#
# Methods defining syntax elements.
#

# TODO these define and undefine methods each need a corresponding undefine
# method (or one that does all).   They all return the construct (or should)
# so consider it the undef_construct method is sufficient...

#
# Token literals.
#

def def_literal(parser, token_label, val_type=None,
                precond_fun=None, precond_priority=0,
                val_type_override_fun=None,
                eval_fun=None, ast_data=None):
    """Defines the token with label `token_label` to be a literal in the
    syntax of the language being parsed.  This method adds a head handler
    function to the token.  Literal tokens are the leaves of the expression
    trees; they are things like numbers and variable names in a numerical
    expression.  They always occur as the first (and only) token in a
    subexpression being evaluated by `recursive_parse`, so they need a head
    handler but not a tail handler.  (Though note that the token itparser
    might also have a tail handler.)

    A function `val_type_override_fun` can be passed in, taking a token and a
    lexer as its two arguments and returning a `TypeObject` instance.  If it is
    set then it will called in the handler at parse-time to get the type to set
    as the `val_type` of the node.  This can be useful for dynamic typing such
    as when identifiers in an interpreted language are generic variables which
    can holding different types.  This option currently does not work for
    overloading on return types."""
    def head_handler_literal(tok, lex):
        if val_type_override_fun:
            tok.process_and_check_kwargs = {"val_type_override":
                                            val_type_override_fun(tok, lex)}
        return tok

    construct_label = "def_literal with {} tokens as triggers".format(token_label)
    return parser.def_construct(HEAD, head_handler_literal, token_label,
                              val_type=val_type, arg_types=(),
                              construct_label=construct_label,
                              precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              eval_fun=eval_fun, ast_data=ast_data)


def def_multi_literals(parser, tuple_list):
    """An interface to the `def_literal` method which takes a list of
    tuples.  The `def_literal` method will be called for each tuple, unpacked
    in the order in the tuple.  Unspecified optional arguments are assigned
    their default values.

    Usually it is better to define `literal = parser.def_literal` and use that
    as a shorter alias.  This method does not allow for keyword arguments and
    depends on argument ordering."""
    return multi_funcall(parser.def_literal, tuple_list)

#
# Brackets and parens.
#

def def_bracket_pair(parser, lbrac_token_label, rbrac_token_label, in_tree=True,
                     precond_fun=None, precond_priority=0,
                     eval_fun=None, ast_data=None):
    """Define a matching bracket grouping operation.  The returned type is
    set to the type of its single child (i.e., the type of the contents of
    the brackets).  Defines a head handler for the left bracket token, so
    effectively gets the highest evaluation precedence.  As far as types,
    it is treated as a function that takes one argument of wildcard type
    and returns whatever type the argument has."""
    # TODO: Maybe allow optional comma_token_label for comma-separated items
    # inside brackets.

    def head_handler(tok, lex):
        """A head handler for the left bracket of the pair."""
        tok.append_children(tok.recursive_parse(0))
        lex.match_next(rbrac_token_label, raise_on_fail=True)
        if not parser.skip_type_checking:
            child_type = tok.children[0].expanded_formal_sig.val_type
            tok.process_and_check_kwargs = {"val_type_override": child_type}
        if in_tree:
            return tok
        else:
            return tok[0]

    construct_label = "def_bracket_pair with {} tokens as triggers".format(lbrac_token_label)
    return parser.def_construct(HEAD, head_handler, lbrac_token_label,
                             construct_label=construct_label,
                             precond_fun=precond_fun, precond_priority=precond_priority,
                             eval_fun=eval_fun, ast_data=ast_data)

#
# Standard functions.
#

def def_stdfun(parser, fname_token_label, lpar_token_label,
               rpar_token_label, comma_token_label, precond_priority=1,
               val_type=None, arg_types=None, eval_fun=None, ast_data=None,
               num_args=None, value_key=None):
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
                lex.match_next(rpar_token_label, raise_on_success=True)
        lex.match_next(rpar_token_label, raise_on_fail=True) # Closing rpar.
        tok.process_not_in_tree() # Needed when comma is an operator that gets removed.
        if (parser.skip_type_checking and num_args is not None
                                    and len(tok.children) != num_args):
            print("tok is", tok, "tok children are", tok.children)
            raise ParserException("Wrong number of arguments for function {0}:"
                        " expected {1} and got {2}.""".format(tok.token_label,
                        num_args, len(tok.children)))
        return tok

    construct_label = "def_stdfun with {} tokens as triggers".format(fname_token_label)
    return parser.def_construct(HEAD, head_handler, fname_token_label, prec=0,
                construct_label=construct_label,
                precond_fun=preconditions, precond_priority=precond_priority,
                val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                ast_data=ast_data, value_key=value_key)


def def_stdfun_lpar_tail(parser, fname_token_label, lpar_token_label,
                         rpar_token_label, comma_token_label, prec_of_lpar,
                         val_type=None, arg_types=None, eval_fun=None, ast_data=None,
                         num_args=None, value_key=None):
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
                lex.match_next(rpar_token_label, raise_on_success=True)
        lex.match_next(rpar_token_label, raise_on_fail=True)
        return left

    # Note we need to generate a unique construct_label for each fname_token_label.
    construct_label = "def_stdfun_lpar_tail with {} tokens as triggers".format(
                                                             fname_token_label)
    return parser.def_construct(TAIL, tail_handler, lpar_token_label,
                              prec=prec_of_lpar,
                              construct_label=construct_label,
                              precond_fun=precond_fun,
                              val_type=val_type, arg_types=arg_types,
                              eval_fun=eval_fun, ast_data=ast_data, value_key=value_key)

#
# Infix operators.
#

def def_infix_multi_op(parser, operator_token_labels, prec, assoc,
                       repeat=False, not_in_tree=False,
                       precond_fun=None, precond_priority=0,
                       val_type=None, arg_types=None, eval_fun=None, ast_data=None):
    # TODO only this utility method currently supports "not_in_tree" kwarg.
    # General and easy mechanism, though.  Test more and add to other
    # methods.  Does in-tree keep the first one? how is it defined for this
    # thing?  Comma operator is example of not_in_tree=True, but how does it
    # handle the root??  TODO: How about in-tree that works at root iff the
    # node only has one child?
    """Takes a list of operator token labels and defines a multi-infix
    operator.

    If `repeat=True` then any number of repetitions of the list of operators
    will be accepted.  For example, a comma operator could be used to parse a
    full comma-separated list.  When `arg_types` is also set use the `Varargs`
    object in the list to check the repetitions.  For a single operator,
    repeating just has the effect of putting the arguments in a flat
    argument/child list instead of as nested binary operations based on left or
    right association.  Any argument-checking is done after any node removal,
    which may affect the types that should be passed-in in the list arg_types
    of parent constructs.

    If `not_in_tree` is false then the root node will not appear in the final parse
    tree (unless it is the root).
    """
    if assoc not in ["left", "right"]:
        raise ParserException('Argument assoc must be "left" or "right".')
    recurse_bp = prec
    if assoc == "right":
        recurse_bp = prec - 1

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
        return tok
    construct_label = "def_infix_multi_op with {} tokens as operators".format(
                                                        operator_token_labels)
    return parser.def_construct(TAIL, tail_handler, operator_token_labels[0], prec=prec,
                              construct_label=construct_label, precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              val_type=val_type, arg_types=arg_types,
                              eval_fun=eval_fun, ast_data=ast_data)


def def_infix_op(parser, operator_token_label, prec, assoc, not_in_tree=False,
                 precond_fun=None, precond_priority=0,
                 val_type=None, arg_types=None, eval_fun=None, ast_data=None):
    """This just calls the more general method `def_multi_infix_op`."""
    return parser.def_infix_multi_op([operator_token_label], prec, assoc,
                                   not_in_tree=not_in_tree,
                                   precond_fun=precond_fun,
                                   precond_priority=precond_priority,
                                   val_type=val_type, arg_types=arg_types,
                                   eval_fun=eval_fun, ast_data=ast_data)

#
# Prefix operators.
#

def def_prefix_op(parser, operator_token_label, prec,
                  precond_fun=None, precond_priority=0,
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

    construct_label = "def_prefix_op with {} tokens as triggers".format(
                                                   operator_token_label)
    return parser.def_construct(HEAD, head_handler, operator_token_label,
                              construct_label=construct_label, precond_fun=precond_fun,
                              precond_priority=precond_priority,
                              val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                              ast_data=ast_data)

#
# Postfix operators.
#


def def_postfix_op(parser, operator_token_label, prec, allow_ignored_before=True,
                   precond_fun=None, precond_priority=0,
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

    construct_label = "def_postfix_op with {} tokens as triggers".format(
                                                    operator_token_label)
    return parser.def_construct(TAIL, tail_handler, operator_token_label, prec=prec,
                             construct_label=construct_label, precond_fun=precond_fun,
                             precond_priority=precond_priority,
                             val_type=val_type, arg_types=arg_types,
                             eval_fun=eval_fun, ast_data=ast_data)


#
# Juxtaposition operators.
#

def def_jop(parser, prec, assoc, precond_fun=None, precond_priority=None,
            val_type=None, arg_types=None, eval_fun=None, ast_data=None):
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

    construct_label = "jop defined from def_jop"
    return parser.def_construct(TAIL, tail_handler, parser.jop_token_label, prec=prec,
                                construct_label=construct_label, precond_fun=precond_fun,
                                precond_priority=precond_priority,
                                val_type=val_type, arg_types=arg_types,
                                eval_fun=eval_fun, ast_data=ast_data)

#
# Statically typed variable assignments and evaluations.
#

def _setup_symbol_dicts(parser, symbol_value_dict, symbol_type_dict):
    """Return the dicts to use, substituting defaults for `None` and creating the
    defaults if they do not exist.  The defaults are `parser.symbol_value_dict` and
    `parser.symbol_type_dict`."""
    if symbol_value_dict is None:
        if not hasattr(parser, "symbol_value_dict"):
            parser.symbol_value_dict = {}
        symbol_value_dict = parser.symbol_value_dict
    if symbol_type_dict is None:
        if not hasattr(parser, "symbol_type_dict"):
            parser.symbol_type_dict = {}
        symbol_type_dict = parser.symbol_type_dict
    return symbol_value_dict, symbol_type_dict

def _eval_statically_typed_assignment(parser, symbol_value_dict):
    """Return an evaluation function to implement a dynamically-typed
    assignment.  The token argument to the returned function must be the token
    for the assignment operator, at the root of the subtree, with the left child
    holding the identifier for the variable, and the right child holding the value
    to assign.  The returned evaluation function returns the assigned value as its
    value (so `x=4` returns `4` which can then be part of another expression)."""
    def eval_fun(subtree_tok):
        rhs = subtree_tok[1].eval_subtree()
        symbol_value_dict[subtree_tok[0].value] = rhs
        return rhs

    return eval_fun

def set_static_type(parser, symbol_value_dict=None, symbol_type_dict=None):
    """Called when a static type definition is parsed in the object language.  It
    associates a Typped type with the type in the language.  This allows static type
    checking to work.  The default `symbol_type_dict` is `parser.symbol_type_dict`."""
    symbol_value_dict, symbol_type_dict = _setup_symbol_dicts(parser, symbol_value_dict,
                                                                      symbol_type_dict)

def def_assignment_op_static(parser, assignment_op_token_label, prec, assoc,
                             identifier_token_label,
                             symbol_value_dict=None, symbol_type_dict=None,
                             allowed_types=None,
                             precond_fun=None, precond_priority=0,
                             val_type=None, eval_fun=None,
                             create_eval_fun=False, ast_data=None):
    """Define an infix assignment operator which is statically typed, with
    types checked at parse time.  Each identifier (with token label
    `identifier_token_label` must already have a type associated with it in the
    `symbol_type_dict`.  This dict and the type values in it should be set via
    whatever kind of a type definition construct the language uses.

    A precondition checks that the l.h.s. of the assignment operator is a token
    with label `identifier_token_label`.  If not an exception is raised.

    An evaluation function can optionally be created automatically, but by default is
    not.  See the `def_assignment_op_dynamic` routine for more details since the
    mechanism is the same.  If `eval_fun` is set then that evaluation function
    will always be used.

    This method may not correctly set the return type when overloading on
    return types because currently `val_type_override` is used to set it."""
    symbol_value_dict, symbol_type_dict = _setup_symbol_dicts(parser, symbol_value_dict,
                                                                      symbol_type_dict)
    def precondition_lhs_is_identifier(lex, lookbehind):
        return lex.peek(-1).token_label == identifier_token_label

    # Combine above precond fun with user's precond fun if one was supplied.
    precond_fun = all_precond_funs(precondition_lhs_is_identifier, precond_fun)

    # Create an eval fun if requested.
    if create_eval_fun:
        eval_fun = _eval_statically_typed_assignment(parser, symbol_value_dict)

    # Create the infix operator assignment construct and register it.

    if assoc not in ["left", "right"]:
        raise ParserException('Argument assoc must be "left" or "right".')
    recurse_bp = prec
    if assoc == "right":
        recurse_bp = prec - 1

    def tail_handler(tok, lex, left):
        """Parse the assignment operation, checking that the types match."""
        tok.append_children(left, tok.recursive_parse(recurse_bp))
        identifier = tok[0].value
        if not parser.skip_type_checking:
            # Set return type to r.h.s. type (TODO later maybe have option...)
            rhs_type = tok[1].expanded_formal_sig.val_type
            if not val_type:
                val_t = rhs_type
            else:
                val_t = val_type
            # Set the type returned by the assignment operation.
            tok.process_and_check_kwargs = {"val_type_override": val_t}
            # Check that the static types match.
            formal_type = symbol_type_dict.get(identifier, None)
            if formal_type is None:
                raise TypeErrorInParsedLanguage(
                        "Variable identifier '{0}' has not been declared with a type."
                        .format(identifier))
            if not rhs_type.matches_formal_type(formal_type):
                # TODO: should do a reverse lookup on Typped types back to implemented lang
                # types for better error message... but need that dict available.
                raise TypeErrorInParsedLanguage("The value assigned to variable '{0}'"
                            " does not match its defined type {1}.  Its actual type"
                            " is {2}.".format(identifier, formal_type, rhs_type))
        return tok

    construct_label = "def_assignment_op_static with {} tokens as triggers".format(
                                                         assignment_op_token_label)
    return parser.def_construct(TAIL, tail_handler, assignment_op_token_label,
                                prec=prec, construct_label=construct_label,
                                precond_fun=precond_fun,
                                precond_priority=precond_priority,
                                val_type=val_type,
                                eval_fun=eval_fun, ast_data=ast_data)


#
# Dynamically typed variable assignments and evaluations.
#

def _eval_dynamically_typed_assignment(parser, symbol_value_dict, symbol_type_dict):
    """Return an evaluation function to implement a dynamically-typed
    assignment.  The token argument to the returned function must be the token
    for the assignment operator, at the root of the subtree, with the left child
    holding the identifier for the variable, and the right child holding the value
    to assign.  The returned evaluation function returns the assigned value as its
    value (so `x=4` returns `4` which can then be part of another expression)."""
    def eval_fun(subtree_tok):
        if (hasattr(parser, "allowed_dynamic_assignment_types") and not
                any(subtree_tok[1].expanded_formal_sig.val_type.matches_formal_type(formal_type)
                    for formal_type in parser.allowed_dynamic_assignment_types)):
                raise TypeErrorInParsedLanguage("Actual type {0} in assignment does not"
                    " match any types in the list {1}.".format(
                                  subtree_tok[1].expanded_formal_sig.val_type,
                                  parser.allowed_dynamic_assignment_types))
        rhs = subtree_tok[1].eval_subtree()
        symbol_value_dict[subtree_tok[0].value] = rhs
        symbol_type_dict[subtree_tok[0].value] = subtree_tok[1].expanded_formal_sig.val_type
        return rhs

    return eval_fun

def def_assignment_op_dynamic(parser, assignment_op_token_label, prec, assoc,
                              identifier_token_label,
                              symbol_value_dict=None, symbol_type_dict=None,
                              allowed_types=None,
                              precond_fun=None, precond_priority=0,
                              val_type=None, eval_fun=None,
                              create_eval_fun=False, ast_data=None):
    """Define an infix assignment operator which is dynamically typed, with
    types checked at evaluation time (i.e., when the tree is interpreted).

    A precondition checks that the l.h.s. of the assignment operator is a token
    with label `identifier_token_label`.  If not an exception is raised.

    No type-checking is done on the r.h.s. by default.  To limit the types that
    can be assigned you can pass in a list or iterable of `TypeObject`
    instances as the argument `allowed_types`.  These formal types are stored
    as the list attribute `allowed_dynamic_assignment_types` of the parser
    instance.  An exception will be raised by the generated evaluation function
    if an assigned value does not have an actual type consistent with a formal
    type on that list.  If new types are created later they can be directly
    appended to that list without having to overload the assignment operator.

    If `create_eval_fun` is true (and `eval_fun` is not set) then an evaluation
    function will be created automatically.  The `symbol_value_dict` is used
    to store the values, which defaults to the parser attribute of the same
    name.

    This method may not correctly set the return type when overloading on
    return types because currently `val_type_override` is used to set it."""

    symbol_value_dict, symbol_type_dict = _setup_symbol_dicts(parser, symbol_value_dict,
                                                                      symbol_type_dict)
    if allowed_types is not None:
        parser.allowed_dynamic_assignment_types = allowed_types

    def precondition_lhs_is_identifier(lex, lookbehind):
        return lex.peek(-1).token_label == identifier_token_label

    # Combine above precond fun with user's precond fun if one was supplied.
    precond_fun = all_precond_funs(precondition_lhs_is_identifier, precond_fun)

    # Create an eval fun if requested.
    if create_eval_fun:
        eval_fun = _eval_dynamically_typed_assignment(parser, symbol_value_dict,
                                                               symbol_type_dict)

    # Create the infix operator assignment construct and register it.

    if assoc not in ["left", "right"]:
        raise ParserException('Argument assoc must be "left" or "right".')
    recurse_bp = prec
    if assoc == "right":
        recurse_bp = prec - 1

    def tail_handler(tok, lex, left):
        # Set return type to r.h.s. type (TODO later maybe have return type option...)
        tok.append_children(left, tok.recursive_parse(recurse_bp))
        if not parser.skip_type_checking:
            rhs_type = tok[1].expanded_formal_sig.val_type
            tok.process_and_check_kwargs = {"val_type_override": rhs_type}
        return tok

    construct_label = "def_assignment_op_dynamic with {} tokens as triggers".format(
                                                          assignment_op_token_label)
    return parser.def_construct(TAIL, tail_handler, assignment_op_token_label,
                                prec=prec, construct_label=construct_label,
                                precond_fun=precond_fun,
                                precond_priority=precond_priority,
                                val_type=val_type,
                                eval_fun=eval_fun, ast_data=ast_data)

def def_literal_typed_from_dict(parser, token_label, symbol_value_dict=None,
                                symbol_type_dict=None,
                                default_type=None,
                                default_eval_value=None,
                                eval_fun=None, create_eval_fun=False,
                                precond_fun=None, precond_priority=1):
    """Define a dynamically typed literal, usually a variable-name identifier.
    The type is looked up in the dict `symbol_type_dict`, keyed by the string
    value of the token literal.

    If `create_eval_fun` is true (and `eval_fun` is not set) then this method
    will provides an evaluation function automatically.  This function returns
    the value looked up from `symbol_value_dict`, keyed by the literal token's
    string value.  The default value returned by the evaluation if the symbol
    is not in the dict is set via `default_eval_value`.  (Currently there must
    be some default rather than raising an exception, with the default default
    value set to `None`.) Setting `create_eval_fun` false will skip the setting
    of an evaluation function.

    The `def_assignment_op_dynamic` routine should be used to handle the
    corresponding variable assignment operation.  That is, the assignment that
    dynamically sets the type of the literal to the type of the assigned value
    (storing it in `symbol_type_dict` by default).

    This method may not correctly set the return type when overloading on
    return types because currently `val_type_override` is used to set it."""
    symbol_value_dict, symbol_type_dict = _setup_symbol_dicts(parser, symbol_value_dict,
                                                                      symbol_type_dict)

    def literal_val_type_override_fun(tok, lex):
        """Function hook passed to `def_literal` to assign a type signature."""
        if tok.value in symbol_type_dict:
            return symbol_type_dict[tok.value]
        else:
            return default_type

    if create_eval_fun:
        def eval_fun(tok):
            return symbol_value_dict.get(tok.value, default_eval_value)

    parser.def_literal(token_label, val_type=None,
                       val_type_override_fun=literal_val_type_override_fun,
                       eval_fun=eval_fun)

#
# Utility functions used by this module.
#

#
# The list of defined functions to be made into methods of the PrattParser class.
#

parse_methods = [
                 # Literals.
                 def_literal,
                 def_multi_literals,

                 # Operators.
                 def_infix_multi_op,
                 def_infix_op,
                 def_prefix_op,
                 def_postfix_op,

                 # Brackets.
                 def_bracket_pair,

                 # Functions.
                 def_stdfun,
                 def_stdfun_lpar_tail,
                 def_jop,

                 # Assignment-related methods.
                 def_assignment_op_static,
                 def_assignment_op_dynamic,
                 def_literal_typed_from_dict,
                 ]

