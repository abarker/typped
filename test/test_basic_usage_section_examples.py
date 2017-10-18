# -*- coding: utf-8 -*-
"""

Tests the example code in the file `examples/basic_usage_section_examples.py`.  This
code contains the examples in the Basic Usage section of the Sphinx documentation.

"""
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-vv")
pytest_helper.auto_import()
pytest_helper.sys_path("../examples")

from basic_usage_section_examples import (setup_string_language_parser_no_typing,
                                          setup_string_language_parser_dynamic_typing,
                                          setup_string_language_parser_static_typing,
                                          setup_simple_builtin_example,
                                          pp
                                          )

def test_simple_builtin_example():
    parser = setup_simple_builtin_example()
    result_tree = parser.parse("x + (4 + 3)*5")
    assert str(result_tree) == ("<k_plus,'+'>(<k_identifier,'x'>,<k_ast,'*'>("
           "<k_lpar,'('>(<k_plus,'+'>(<k_number,'4'>,<k_number,'3'>)),<k_number,'5'>))")

def test_string_language_parser_untyped():
    """An example from the Sphinx docs overview section."""
    parser = setup_string_language_parser_no_typing()

    # Test basic parsing to syntax tree.
    result_tree = parser.parse("x + (4 + 3)*5")
    #print(result_tree.tree_repr(indent=12))
    assert result_tree.tree_repr() == unindent(12, """
            <k_plus,'+'>
                <k_identifier,'x'>
                <k_ast,'*'>
                    <k_lpar,'('>
                        <k_plus,'+'>
                            <k_int,'4'>
                            <k_int,'3'>
                    <k_int,'5'>

            """)
    result_tree = parser.parse('"foo" + "bar"')
    #print(result_tree.tree_repr())
    assert result_tree.tree_repr() == unindent(12, """
            <k_plus,'+'>
                <k_string,'"foo"'>
                <k_string,'"bar"'>

            """)
    # Test evaluations.
    result = parser.parse("x")
    assert result.eval_subtree() == 0
    result = parser.parse("x = 4")
    assert result.eval_subtree() == 4
    result = parser.parse('x * "egg"')
    assert result.eval_subtree() == '"eggeggeggegg"'

def test_string_language_parser_dynamic():
    """An example from the Sphinx docs overview section."""
    parser = setup_string_language_parser_dynamic_typing()

    # Test basic parsing to syntax tree.
    result_tree = parser.parse("x + (4 + 3)*5")
    #print(result_tree.tree_repr_with_types(indent=12))
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,'+',TypeObject('t_int')>
                <k_identifier,'x',TypeObject('t_int')>
                <k_ast,'*',TypeObject('t_int')>
                    <k_lpar,'(',TypeObject('t_int')>
                        <k_plus,'+',TypeObject('t_int')>
                            <k_int,'4',TypeObject('t_int')>
                            <k_int,'3',TypeObject('t_int')>
                    <k_int,'5',TypeObject('t_int')>

            """)
    result_tree = parser.parse('"foo" + "bar"')
    #print(resul1t_tree.tree_repr())
    #print(result_tree.tree_repr_with_types())
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,'+',TypeObject('t_str')>
                <k_string,'"foo"',TypeObject('t_str')>
                <k_string,'"bar"',TypeObject('t_str')>

            """)
    # Test evaluations.
    result = parser.parse("x")
    assert result.eval_subtree() == 0
    result = parser.parse("x = 4")
    assert result.eval_subtree() == 4
    result = parser.parse('x * "egg"')
    assert result.eval_subtree() == '"eggeggeggegg"'

def test_string_language_parser_static():
    """An example from the Sphinx docs overview section."""
    parser = setup_string_language_parser_static_typing()

    builtins_dict = {"__builtins__": __builtins__}
    locals_dict = {}

    def python_evaluate_command(python_command):
        result = eval(compile(python_command, '<string>', 'exec'),
                      builtins_dict, locals_dict)
        return result

    # Test basic parsing to syntax tree.
    with raises(pp.ErrorInParsedLanguage):
        result_tree = parser.parse("x + (4 + 3)*5")
    result = parser.parse("int x")
    assert result.eval_subtree() == 'None'
    result_tree = parser.parse("x + (4 + 3)*5")

    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,'+',TypeObject('t_int')>
                <k_identifier,'x',TypeObject('t_int')>
                <k_ast,'*',TypeObject('t_int')>
                    <k_lpar,'(',TypeObject('t_int')>
                        <k_plus,'+',TypeObject('t_int')>
                            <k_int,'4',TypeObject('t_int')>
                            <k_int,'3',TypeObject('t_int')>
                    <k_int,'5',TypeObject('t_int')>

            """)
    result_tree = parser.parse('"foo" + "bar"')
    assert result_tree.tree_repr_with_types() == unindent(12, """
            <k_plus,'+',TypeObject('t_str')>
                <k_string,'"foo"',TypeObject('t_str')>
                <k_string,'"bar"',TypeObject('t_str')>

            """)

    # Test evaluations.
    result = parser.parse("x")
    assert result.eval_subtree() == "x"
    result = parser.parse("x =    4")
    assert result.eval_subtree() == "x = 4"
    result = parser.parse('x    * "egg"')
    assert result.eval_subtree() == 'x * "egg"'
    with raises(pp.TypeErrorInParsedLanguage):
        result = parser.parse('x = "water"')
    result = parser.parse('str x')
    assert str(result) == "<k_identifier,'str'>(<k_identifier,'x'>)"
    result = parser.parse('str x = "water"')
    print(result)
    assert str(result) == "<k_identifier,'str'>(<k_equals,'='>(<k_identifier,'x'>,<k_string,'\"water\"'>))"
    python_evaluated_result = python_evaluate_command(result.eval_subtree())
    assert result.eval_subtree() == 'x = "water"; x'
    result = parser.parse('x + "tree"')
    assert str(result) == "<k_plus,'+'>(<k_identifier,'x'>,<k_string,'\"tree\"'>)"
    assert str(result.eval_subtree()) == 'x + "tree"'
    python_evaluated_result = python_evaluate_command(result.eval_subtree())
    assert python_evaluated_result == None
    python_evaluated_result = python_evaluate_command('x')
    #assert python_evaluated_result == '"watertree"' # Need to capture output as string...

