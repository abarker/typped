# -*- coding: utf-8 -*-
"""

Tests the example code in the file `examples/basic_usage_section_examples.py`.  This
code contains the examples in the Basic Usage section of the Sphinx documentation.

"""
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path("../examples")

from basic_usage_section_examples import (setup_string_language_parser_dynamic_typing,
                                          setup_string_language_parser_static_typing,
                                          setup_simple_builtin_example,
                                          )

#TODO: part of pytest-helper now... use that version when package updated.
def unindent(unindent_level, string):
    """Strip indentation from a docstring.  This function is useful in tests
    where you have assertions that something equals a multi-line string.  It
    allows the strings to be represented as multi-line docstrings but indented
    in a way that matches the surrounding code.  Calling this function on a
    string will 1) remove any leading or trailing newlines, and 2) strip
    `indent_level` characters from the beginning of each line.  Raises an
    exception on an attempt to strip non-whitespace."""
    string = string.strip("\n")
    lines = string.splitlines()
    for l in lines:
        string_to_strip = l[0:unindent_level]
        if not string_to_strip.lstrip() == "":
            raise pytest_helper.PytestHelperException("Attempt to unindent non-whitespace at"
                    " the beginning of this line:\n'{0}'".format(l))
    stripped = "\n".join(s[unindent_level:] for s in lines)
    return stripped

def test_simple_builtin_example():
    parser = setup_simple_builtin_example()
    result_tree = parser.parse("x + (4 + 3)*5")
    assert str(result_tree) == ("<k_plus,'+'>(<k_identifier,'x'>,<k_ast,'*'>("
           "<k_lpar,'('>(<k_plus,'+'>(<k_number,'4'>,<k_number,'3'>)),<k_number,'5'>))")

def test_string_language_parser_dynamic():
    """An example from the Sphinx docs overview section."""
    parser = setup_string_language_parser_dynamic_typing()

    # Test basic parsing to syntax tree.
    result_tree = parser.parse("x + (4 + 3)*5")
    print(result_tree.tree_repr_with_types(indent=12))
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,+,TypeObject('t_int')>
                <k_identifier,x,TypeObject('t_int')>
                <k_ast,*,TypeObject('t_int')>
                    <k_lpar,(,TypeObject('t_int')>
                        <k_plus,+,TypeObject('t_int')>
                            <k_int,4,TypeObject('t_int')>
                            <k_int,3,TypeObject('t_int')>
                    <k_int,5,TypeObject('t_int')>
            """)
    result_tree = parser.parse('"foo" + "bar"')
    #print(resul1t_tree.tree_repr())
    print(result_tree.tree_repr_with_types())
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,+,TypeObject('t_str')>
                <k_string,"foo",TypeObject('t_str')>
                <k_string,"bar",TypeObject('t_str')>
            """)
    # Test evaluations.
    result = parser.parse("x")
    assert result.eval_subtree() == 0
    result = parser.parse("x = 4")
    assert result.eval_subtree() == 4

def test_string_language_parser_static():
    """An example from the Sphinx docs overview section."""
    parser = setup_string_language_parser_static_typing()

    # Test basic parsing to syntax tree.
    result_tree = parser.parse("x + (4 + 3)*5")
    print(result_tree.tree_repr_with_types(indent=12))
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,+,TypeObject('t_int')>
                <k_identifier,x,TypeObject('t_int')>
                <k_ast,*,TypeObject('t_int')>
                    <k_lpar,(,TypeObject('t_int')>
                        <k_plus,+,TypeObject('t_int')>
                            <k_int,4,TypeObject('t_int')>
                            <k_int,3,TypeObject('t_int')>
                    <k_int,5,TypeObject('t_int')>
            """)
    result_tree = parser.parse('"foo" + "bar"')
    #print(resul1t_tree.tree_repr())
    print(result_tree.tree_repr_with_types())
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,+,TypeObject('t_str')>
                <k_string,"foo",TypeObject('t_str')>
                <k_string,"bar",TypeObject('t_str')>
            """)
    # Test evaluations.
    result = parser.parse("x")
    assert result.eval_subtree() == 0
    result = parser.parse("x = 4")
    assert result.eval_subtree() == 4

