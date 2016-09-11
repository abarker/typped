# -*- coding: utf-8 -*-
"""

Terminology:

* Production rules are called `productions`.

* Production rules with the same l.h.s. or separated by the "or" symbol on a
  line are called different `cases` of the production.

* The separate symbol groups within a case are called the elements of that case.

"""

from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run("../../test/test_pratt_parser.py", pytest_args="-v")

import sys
from typped.shared_settings_and_exceptions import ParserException

#
# The class for production rule elements.
#

class CaseElem(object):
    """Base class for elements that make up the cases of the production rules,
    productions and terminals.  Cases are lists of case elems; productions are
    lists of cases but also can be case elems (recursing into the grammar)."""
    def __init__(self):
        pass
    def __or__(self, case_elem):
        pass
        # TODO: combine lists, set cases, and return a Production
        # How they are combined will depend on the type...
        # Should work for Productions and general CaseElems.
    def __add__(self, case_elem):
        pass
        # TODO combine list, set cases, and return a CaseElem

class Production(CaseElem):
    """Productions are made up of a list of cases.  Each case is a tuple of
    terminals and productions."""
    def __init__(self, production_name):
        self.name = production_name
        self.cases = [] # A list of tuples with all the case info.
    def add_case(self, case):
        self.cases.append(case)

class Terminal(CaseElem):
    """Terminals are tuples of tokens.  They can be initialized with labels."""
    def __init__(self, token_or_label):
        if isinstance(token_or_label, str):
            pass # TODO look up actual token, but need token table, maybe don't allow
            #case_elems = [token]
        else:
            case_elems = [token_or_label]

class Optional(CaseElem):
    pass

class OneOrMore(CaseElem):
    pass

class ZeroOrMore(CaseElem):
    pass

# TODO: consider operators | and maybe + overloaded to combine case elems and
# return a new one.

