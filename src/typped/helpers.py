# -*- coding: utf-8 -*-
"""

Some helper routines.

For builtin parsing methods see the `builtin_parse_methods` module.

For predefined token sets see the `predefined_token_sets` module.

"""

from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run("../../test/test_pratt_parser.py", pytest_args="-v")

from typped.shared_settings_and_exceptions import ParserException

#
# Functions often used inside head and tail handlers.
#

#
# Predefined full preconditions functions.
#

#
# Boolean-valued functions useful inside preconditions functions.
#

#
# Combining preconditions functions.
#

def combine_precond_funs(*funs, **kwargs):
    """Takes any number of function/callable objects as arguments.  Returns a
    combined preconditions function that computes the conjunction ("and",
    "all") function of all the preconditions functions passed in.  Also returns
    the concatenation of the labels.  Any `None` object arguments are ignored.

    If the keyword argument `all_true` is set `False` then then disjunction
    ("or", "any") is used instead of conjunctiobn and the combined function is
    true when any of the individual functions are true, not necessarily all of
    them."""
    for key in kwargs: # Catch kwarg errors; kwargs done like this for Python2 compat.
        if key != "all_true":
            raise ParserException("Invalid kwarg to combine_precond_funs: {0}."
                                  .format(key))
    all_true = kwargs.get("all_true", True)

    funs = [f for f in funs if f]
    if not funs:
        return None
    elif len(funs) == 1:
        return funs[0]

    if all_true:
        def combined_fun(lex, lookbehind):
            return all(fun(lex, lookbehind) for fun in funs)
    else: # any_true
        def combined_fun(lex, lookbehind):
            return any(fun(lex, lookbehind) for fun in funs)

    return combined_fun

