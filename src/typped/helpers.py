# -*- coding: utf-8 -*-
"""

Some helper routines.

"""

from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run("../../test/test_pratt_parser.py", pytest_args="-v")

from typped.shared_settings_and_exceptions import ParserException

#
# Predefined tokens.
#

#
# Functions helpful in writing head and tail handlers.
#


#
# Predefined preconditions functions.
#

#
# Boolean-valued functions useful inside preconditions functions.
#


#
# Combining preconditions functions.
#

def all_precond_funs(*funs):
    """Return a combined preconditions function that computes the conjunction function
    of the preconditions functions passed in."""
    def combine(lex, lookbehind):
        return all(fun(lex, lookbehind) for fun in funs)
    return combine

def any_precond_fun(*funs):
    """Return a combined preconditions function that computes the disjunction function
    of the preconditions functions passed in."""
    def combine(lex, lookbehind):
        return any(fun(lex, lookbehind) for fun in funs)
    return combine

