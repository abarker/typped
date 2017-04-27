# -*- coding: utf-8 -*-
"""

A simple calculator example using the Typped parser.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")
pytest_helper.sys_path("../examples")

import typped as pp
import example_pratt_calculator

#
# Setup and utility functions.
#

def close_to(x, y):
    return abs(x - y) < 1E-7

def setup_basic_calculator():
    parser = pp.PrattParser()
    example_pratt_calculator.define_basic_calculator(parser)
    return parser

def parse_and_eval(parser, expr):
    parse_tree = parser.parse(expr)
    return parse_tree.eval_subtree()

#
# Actual tests below.
#

def test_basic_arithmetic():
    parser = setup_basic_calculator()
    assert parse_and_eval(parser, "3 + 3") == 6
    assert parse_and_eval(parser, "4 cos(0) - 4") == 0

def test_variable_setting():
    parser = setup_basic_calculator()
    assert parse_and_eval(parser, "x") == 0 # Uninitialzed vars always return zero.
    assert close_to(parse_and_eval(parser, "e"), 2.71828182845) # Preset variable.
    parse_and_eval(parser, "x = e^2")
    assert close_to(parse_and_eval(parser, "x"), 7.3890560989) # Preset variable.
    assert close_to(parse_and_eval(parser, "log(sqrt(x))"), 1)
    assert close_to(parse_and_eval(parser, "x sin(pi/2)"), 7.3890560989)

def test_overloaded_function_name_eval():
    parser = setup_basic_calculator()
    assert parse_and_eval(parser, "log(e)") == 1
    assert close_to(parse_and_eval(parser, "log(e, 10)"), 0.43429448190)
    assert parse_and_eval(parser, "log(e, e)") == 1

