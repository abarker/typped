# -*- coding: utf-8 -*-
"""

Tests of the matcher.py code.

"""
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

from typped.matcher import * # Test as an individual module.

def test_basic():
    m = Matcher()
    m.default_insert_options = "trie" # TODO for testing trie matcher
    m.insert_pattern("k_underscore", r"_")
    m.insert_pattern("k_science", r"zoology")
    m.insert_pattern("k_place", r"zoo")
    #m.insert_pattern("k_lowercase_string", r"[a-z]+", on_ties=-1)
    # TODO below patt for rtd matcher
    m.insert_pattern("k_lowercase_string", "\\*1\\(\\[a\\-z\\]\\)", on_ties=-1)

    test_string = "zooxyzwzoologywwwzodd"
    first_match = m.get_next_token_label_and_value(test_string, (0, len(test_string)))
    assert first_match == ("k_lowercase_string", test_string)

    test_string = "zoo_xyzw_zoology_wwwzodd"
    begin = 0
    end = len(test_string)
    first_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert first_match == ("k_place", "zoo")

    begin += len(first_match[1])
    second_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert second_match == ("k_underscore", "_")

    begin += len(second_match[1])
    third_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert third_match == ("k_lowercase_string", "xyzw")

    begin += len(third_match[1])
    fourth_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert fourth_match == ("k_underscore", "_")

    begin += len(fourth_match[1])
    fifth_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert fifth_match == ("k_science", "zoology")

    begin += len(fifth_match[1])
    sixth_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert sixth_match == ("k_underscore", "_")

    # TODO: rtd matcher gets to here and fails....
    begin += len(sixth_match[1])
    seventh_match = m.get_next_token_label_and_value(test_string, (begin, end))
    assert seventh_match == ("k_lowercase_string", "wwwzodd")

    # Match past bounds of string.
    with raises(MatcherException):
        begin += len(seventh_match[1])
        eighth_match = m.get_next_token_label_and_value(test_string, (begin, end))

    # Delete a pattern.
    m.remove_pattern("k_lowercase_string")
    test_string = "zooxyzwzoologywwwzodd"
    first_match = m.get_next_token_label_and_value(test_string, (0, len(test_string)))
    assert first_match == ("k_place", "zoo")

