# -*- coding: utf-8 -*-
"""

Test the file `example_expression_grammar.py` in the examples dir.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")
pytest_helper.sys_path("../examples")

import typped as pp
import example_expression_grammar

def test_example_from_Sphinx_recursive_descent_page():
    # This example already has a regular assert in it, which pytest will use.
    example_expression_grammar.example_from_Sphinx_recursive_descent_page()

