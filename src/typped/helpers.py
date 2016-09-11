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
# Functions helpful in writing head and tail handlers.
#




#
# Predefined preconditions functions.
#


