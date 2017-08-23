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

def combine_precond_funs(*fun_label_tuples, all_true=True):
    """Takes any number of tuples `(precond_fun, precond_label)` as arguments.  Returns
    a combined preconditions function that computes the conjunction function
    of the preconditions functions passed in.  Also returns the concatenation of the
    labels.  Any `None` object arguments are ignored.

    If `all_true` is set false then the combined function is true when any of
    the individual functions are true, not all of them."""
    labels = [t[1] for t in fun_label_tuples if t[1] is not None]
    combo_label = "".join(labels)
    combo_label = combo_label if combo_label != "" else None

    funs = [t[0] for t in fun_label_tuples if t[0] is not None]
    if not funs:
        return None, combo_label
    elif len(fun_label_tuples) == 1:
        return fun_label_tuples[0]

    if all_true:
        def combined_fun(lex, lookbehind):
            return all(fun(lex, lookbehind) for fun in funs)
    else: # any_true
        def combined_fun(lex, lookbehind):
            return any(fun(lex, lookbehind) for fun in funs)

    return combined_fun, combo_label

