# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

from typped.regex_trie_dict import *
from typped.regex_trie_dict_scanner import *
from py.test import raises, fail

#
# Test TrieDictScanner.
#

def test_usage_example():
    """Test example usage from intro documentation."""

    td = RegexTrieDict()
    scanner = RegexTrieDictScanner(td)
    #td.insert("te")
    td.insert("text")
    td.insert("\\(test\\|tesb\\)")
    td.insert("\\*\\( \\)") # Any number of spaces.
    #td.insert(" ")
    td.insert("\\[swq\\]tring")

    td.insert("h\\*\\(ere\\)") # At the end of text this requires assert_end_of_text to match

    # Text string may instead be a realtime string of chars.
    text_string = "test  string here"

    # Match before asserting end of text.
    prefix_match_list = scanner.append_text(text_string)
    #scanner.assert_end_of_text()
    matches = scanner.get_prefix_matches()
    print("The matches are:", matches)
    assert matches == ['test', '  ', 'string', ' ']

    # Now assert end of text and try again.
    scanner.assert_end_of_text()
    matches = scanner.get_prefix_matches()
    assert matches == ['here']

    # Now reset text and assert end of text before matching anything.
    with raises(TrieDictScannerError):
        prefix_match_list = scanner.append_text(text_string) # Can't append after asserting end.
    scanner.reset() # So reset.
    prefix_match_list = scanner.append_text(text_string)
    scanner.assert_end_of_text()
    matches = scanner.get_prefix_matches()
    print("The matches are:", matches)
    assert matches == ['test', '  ', 'string', ' ', 'here']

def test_TrieDictScannerBasic():
    td = RegexTrieDict()
    scanner = RegexTrieDictScanner(td)

    td.insert("egg")
    td.insert("r")
    td.insert("really")
    td.insert("eggbert")

    scanner.append_text("e")
    prefix_matches = scanner.get_prefix_matches()
    assert prefix_matches is None # None means no match yet found.
    scanner.append_text("g")
    prefix_matches = scanner.get_prefix_matches()
    assert prefix_matches is None
    scanner.append_text("g")
    prefix_matches = scanner.get_prefix_matches()
    assert prefix_matches is None
    scanner.append_text("r")
    prefix_matches = scanner.get_prefix_matches()
    assert prefix_matches == ["egg"]
    # Note that the scanner resets after returning ["egg"], so new prefix is ["r"]
    scanner.append_text("xyz")
    prefix_matches = scanner.get_prefix_matches()
    assert prefix_matches == ["r"]

    assert scanner.curr_prefix_text == ["x", "y", "z"]
    assert scanner.get_prefix_matches() == [] # [] means no matches possible with current trie.
    assert scanner.cannot_match

def test_parse_quotes():
    td = RegexTrieDict()
    scanner = RegexTrieDictScanner(td)
    td.insert(r"'\*\(\[ a-z\]\)'")
    td.print_tree()

    text = "'hello there'"
    scanner.append_text(text)
    prefix_matches = scanner.get_prefix_matches()
    print(prefix_matches)

    del td[r"'\*\(\[ a-z\]\)'"]
    td.insert(r"'\*\(\[^'\]\)'")
    td.insert(r"W\.W")
    td.print_tree()
    scanner.reset()

    text = "'hello there'WxW"
    scanner.append_text(text)
    prefix_matches = scanner.get_prefix_matches()
    print(prefix_matches)

    fail()

