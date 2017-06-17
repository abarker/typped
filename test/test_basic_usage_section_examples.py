# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path("../examples")

from basic_usage_section_examples import (setup_string_language_parser,
                                          setup_simple_builtin_example,
                                          )

def test_simple_builtin_example():
    parser = setup_simple_builtin_example()
    result_tree = parser.parse("x + (4 + 3)*5")
    assert str(result_tree) == ("<k_plus,'+'>(<k_identifier,'x'>,<k_ast,'*'>("
           "<k_lpar,'('>(<k_plus,'+'>(<k_number,'4'>,<k_number,'3'>)),<k_number,'5'>))")


def test_string_language_example_from_overview_docs():
    """An example from the Sphinx docs overview section.  A simple language
    that uses `+` to add numbers and concatenate strings.  Multiplication of a
    number by a string repeats the string.  Multiplication of a string by a
    string is not defined.  It also has simple variables which can represent
    either numbers or strings."""
    parser = setup_string_language_parser()

    result_tree = parser.parse("x + (4 + 3)*5")
    print(result_tree.tree_repr())
    print(result_tree.string_repr_with_types())
    assert result_tree.string_repr_with_types() == "<k_plus,+,TypeObject(t_number)>(<k_identifier,x,TypeObject(t_number)>,<k_ast,*,TypeObject(t_number)>(<k_lpar,(,TypeObject(t_number)>(<k_plus,+,TypeObject(t_number)>(<k_number,4,TypeObject(t_number)>,<k_number,3,TypeObject(t_number)>)),<k_number,5,TypeObject(t_number)>))"
    result_tree = parser.parse('"foo" + "bar"')
    print(result_tree.tree_repr())
    print(result_tree.string_repr_with_types())
    assert result_tree.string_repr_with_types() == """<k_plus,+,TypeObject(t_string)>(<k_string,"foo",TypeObject(t_string)>,<k_string,"bar",TypeObject(t_string)>)"""
    #fail()

