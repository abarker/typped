# -*- coding: utf-8 -*-
"""

Define a wff parser that uses a sub-parser (called from its handler functions)
to parse term subexpressions.

An EBNF grammar for a wff in a simple logic (with predicates left off) is:
::
    <wff> = <atomic_formula>
          | 'not' <wff>
          | '(' <wff> ')'
          | <wff> <infix_logical_operator> <wff>

    <atomic_formula> = <predicate_name> '(' <term_list> ')'

    <term> = '(' <term> ')'
           | <function_eval>
           | <variable_name>
           | <constant_name>

This test case defines a parser for terms and then calls it from a handler for
wffs.  It uses standard Pratt-style parsing for parsing in both parsers.

Note that to use a parser as an inner parser it needs to be able to parse
subexpressions which do not necessarily extend to the end-of-text end-token.
Set `partial_expressions=True` in the inner parsers so they will return partial
results.  This can also be done dynamically, by modifying the attribute of
the parser instance.

The token definition spaces for different parsers are completely distinct.  To
use two parsers together, though, the outer parser needs to have defined at
least enough tokens of the inner parser to do any necessary peeks into
expressions of that sublanguage.  The precedences, etc., of those tokens also
need to be correct or at least interact correctly (generally the head of a
subexpression starts with a precedence of 0).  So the outer parser should at
least have a token for each beginning-token of an expression in the
sublanguage.  They generally do not need to be assigned handlers (such as
by declaring them literals).

If the token spaces can be identical without causing problems then you can just
define a function to make the token definitions and then call it for each
parser.

Using jops may or may not work in combination with inner and outer parsers.
They will probably work in the inner parser, at least.  The possible problem
is that they check for head handlers before deciding to infer a jop.

TODO: Types are not yet worked as to how they should interact in the
type-checking.  Currently types not used in this example.  It should be
possible to share types between parsers --- tokens cannot be shared only
because pratt parsers also package them with handler methods.  (And that
restriction could be lifted by associating handlers with tokens and parser
instances and possibly keeping several groups of handlers with tokens, maybe
keyed in a dict).

"""
from __future__ import print_function, division, absolute_import
import typped as pp

def run_basic_example():
    print("\n======= parsing terms ========================\n")
    term_parser = define_term_parser()
    test_term = "f(x,x33)"
    print("parsing term:", test_term, "\n")
    print(term_parser.parse(test_term).tree_repr(indent=3))

    print("\n======= parsing wffs =========================\n")
    wff_parser = define_wff_parser(term_parser)
    test_wff = "not A(f(x,x33))"
    print("parsing wff:", test_wff, "\n")
    print(wff_parser.parse(test_wff).tree_repr(indent=3))

    test_wff = "not A(f(x,x33)) and A44() and A9(x1, c2, f(c1))"
    print("parsing wff:", test_wff, "\n")
    print(wff_parser.parse(test_wff).tree_repr(indent=3))

#
# Function to define the term parser.
#

def define_term_parser():
    """Define a Pratt parser to parse terms.  Note that each parser is independent,
    with its own tokens, literals, etc."""
    term_parser = pp.PrattParser()
    term_parser.def_default_whitespace()

    token_list = [
            ("k_varname", r"x[\d]*"),
            ("k_constname", r"c[\d]*"),
            ("k_funname", r"f[\d]*"),
            ("k_plus", r"\+"),
            ("k_minus", r"\-"),
            ("k_fslash", r"/"),
            ("k_ast", r"\*"),
            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_comma", r","),
            ]
    term_parser.def_multi_tokens(token_list)

    literals = [
            ("k_varname",),
            ("k_constname",),
            ("k_rpar",),
            ("k_comma",),
            ]
    term_parser.def_multi_literals(literals)

    term_parser.def_bracket_pair("k_lpar", "k_rpar")
    term_parser.def_stdfun("k_funname", "k_lpar", "k_rpar", "k_comma")

    return term_parser

#
# Define a handler for atomic formulas and use that to define the wff parser.
#

def def_atomic_formula(parser, term_parser, formula_name_token_label,
                       lpar_token_label, rpar_token_label, comma_token_label):
    """This definition uses lookahead to parse an atomic formula.  The
    parameter `parser` is the parser which will have atomic formulas defined
    for it.  The `term_parser` is a parser which parses and returns a term.
    are parsed by the separate `PrattParser` instance `parser`.

    Note that this utility function is not written as the method of a subclass,
    like in some of the other examples.  (It could be made into a method by
    just making it an attribute of the subclass, renaming `parser` to `self`,
    and making sure the superclass initializer is called.)

    This code could also just be in-line, since it is only called once, but it
    is nice to separate it out."""

    def preconditions(lex, lookbehind):
        """Must be followed by a token with label 'lpar_token_label', with no
        whitespace in-between."""
        peek_tok = lex.peek()
        if peek_tok.ignored_before: return False
        if not lex.match_next(lpar_token_label, consume=False): return False
        return True
    construct_label = "lpar after, no whitespace between" # Should be a unique label.

    def head_handler(tok, lex):
        # Below match is for a precondition, so it will match and consume.
        lex.match_next(lpar_token_label, raise_on_fail=True)
        # Read comma-separated subexpressions until the peek is rpar_token_label.
        while not lex.match_next(rpar_token_label, consume=False):
            #lex.go_back(1) # Because we already consumed the formula name.
            # ======> THE TERM PARSER IS CALLED HERE!!!!!!!!!!!!!!!!!!
            term_subtree = term_parser.parse_from_lexer(lex)
            tok.append_children(term_subtree)
            if not lex.match_next(comma_token_label):
                break
            else:
                lex.match_next(rpar_token_label, raise_on_true=True)
        lex.match_next(rpar_token_label, raise_on_fail=True)
        return tok

    #arg_types = [None]*num_args
    parser.def_construct(formula_name_token_label, prec=0,
                         head=head_handler, construct_label=construct_label,
                         precond_fun=preconditions, precond_priority=1)

def define_wff_parser(term_parser):
    """Define a parser for wffs that uses `term_parser` to parse terms."""

    wff_parser = pp.PrattParser()
    wff_parser.def_default_whitespace()

    token_list = [
            ("k_predname", r"A[\d]*"),
            ("k_funname", r"f[\d]*"), # Needed for peek into first of subexpressions.
            ("k_varname", r"x[\d]*"), # Needed for peek into first of subexpressions.
            ("k_constname", r"c[\d]*"), # Needed for peek into first of subexpressions.
            ("k_not", r"not"),
            ("k_and", r"and"),
            ("k_lpar", r"\("),
            ("k_rpar", r"\)"),
            ("k_comma", r","),
            ]
    wff_parser.def_multi_tokens(token_list)


    # Note that k_funname, k_constname, and k_varname are defined above as
    # tokens, but they are not made literals.  Also, no handlers will be
    # defined for them.  The term parser will do that.
    literals = [
            ("k_rpar",),
            ("k_comma",),
            ]
    wff_parser.def_multi_literals(literals)

    wff_parser.def_bracket_pair("k_lpar", "k_rpar")

    #t_wff = wff_parser.def_type("t_wff") # TODO: consider how types interact here...

    # The not symbol, must be followed by a space.
    wff_parser.def_prefix_op("k_not", 100, val_type=None, arg_types=None,
            construct_label="space after not",
            precond_fun=lambda lex, lb: True if lex.peek().ignored_before else False)

    # Note no space around "and" for now... def_infix_op doesn't take precond
    # parameters as of now.
    wff_parser.def_infix_op("k_and", 100, "left", val_type=None, arg_types=None)

    # If the below function were made a method of a subclass of PrattParser the
    # call would instead be: wff_parser.def_atomic_formula(term_parser,....)
    def_atomic_formula(wff_parser, term_parser,
                       "k_predname", "k_lpar", "k_rpar", "k_comma")

    return wff_parser

if __name__ == "__main__":
   run_basic_example()

