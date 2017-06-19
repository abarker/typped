# -*- coding: utf-8 -*-
"""

This module contains convenience functions for defining commonly-used groups of
tokens.  The functions are all copied to the `PrattParser` class as methods
because the namespace is convenient.  They can be used standalone or as methods
of a parser instance.

"""

# TODO: add method to define a float, a number, an identifier.  Just take from
# existing code.

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_production_rules.py",
                              "../../test/test_example_calculator.py",
                              "../../test/test_parser_called_from_parser.py",
                              "../../test/test_pratt_parser.py"
                              ], pytest_args="-v")
from .lexer import multi_funcall
from .shared_settings_and_exceptions import ParserException

default_token_label_dict = {
        "~": "k_tilde",
        "`": "k_backtick",
        "!": "k_bang",
        "@": "k_at_sign",
        "#": "k_pound",
        "$": "k_dollar",
        "%": "k_percent",
        "^": "k_caret",
        "&": "k_ampersand",
        "*": "k_asterisk",
        "(": "k_lpar",
        ")": "k_rpar",
        "_": "k_underscore",
        "-": "k_minus",
        "+": "k_plus",
        "=": "k_equals",
        "{": "k_lcurlybrac",
        "}": "k_rcurlybrac",
        "[": "k_lbrac",
        "]": "k_rbrac",
        "|": "k_vert",
        "\\": "k_backslash",
        ":": "k_colon",
        ";": "k_semicolon",
        "\"": "k_quote",
        "'": "k_singlequote",
        "<": "k_lessthan",
        ">": "k_greaterthan",
        ",": "k_comma",
        ".": "k_period",
        "?": "k_question",
        "/": "k_slash",
        }

def def_default_whitespace(parser, space_label="k_space", space_regex=r"[ \t]+",
                    newline_label="k_newline", newline_regex=r"[\n\f\r\v]+",
                    options=None):
    """Define the standard whitespace tokens for space and newline, setting
    them as ignored tokens."""
    # Note + symbol for one or more, NOT the * symbol for zero or more.
    tok = parser.def_ignored_token
    tok(space_label, space_regex, options=options)
    tok(newline_label, newline_regex, options=options)

def def_default_single_char_tokens(parser, chars=None, exclude=None, make_literals=False):
    """The characters in the string `chars` are defined as tokens with default labels.
    Spaces are ignored in the string.  If `chars` is not set then all the labels will be
    defined except those in the string `exclude`.  If `make_literals` is true then
    the tokens will also be defined as token literals (via `def_literal`)."""
    if chars is None:
        chars = default_token_label_dict.keys()
    for c in chars:
        if c == " " or c in exclude:
            continue
        token_label = default_token_label_dict(c)
        parser.def_token(token_label, re.escape(c))
        if make_literals:
            parser.def_literal(token_label)

def def_multi_tokens(parser, tuple_list):
    """A convenience function, to define multiple tokens at once.  Each element
    of the passed-in list should be a tuple containing the arguments to the
    ordinary `def_token` method.  Calls the equivalent `Lexer` function."""
    # TODO: take keyword args and pass them all to the def_token routine.
    return multi_funcall(parser.def_token, tuple_list, ParserException)

def def_multi_ignored_tokens(parser, tuple_list):
    """A convenience function, to define multiple ignored tokens at once.
    Each element of the passed-in list should be a tuple containing the arguments
    to the ordinary `def_token` method with `ignore=True`.  Calls the equivalent
    `Lexer` function."""
    return multi_funcall(parser.def_ignored_token, tuple_list, ParserException)


# This list of functions is copied to the PrattParser class as methods.
token_defining_methods = [
                         def_default_whitespace,
                         def_default_single_char_tokens,
                         def_multi_tokens,
                         def_multi_ignored_tokens,
                         ]

