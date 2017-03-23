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

    # Text string may instead be a realtime string of chars.
    text_string = "test string here"

    for char in text_string:
        prefix_match_list = scanner.add_text_elem(char)
        if prefix_match_list:
            for match in prefix_match_list:
                print("Matched a prefix:", match)

    # For a fixed-length string we need to assert that all the characters
    # have been entered to get any final matches.
    final_matches = scanner.assert_end_of_text()
    for match in final_matches:
        print("Matched a prefix:", match)

    print("The prefix of this suffix-sequence of characters never matched:")
    print(scanner.curr_prefix_text)
    fail()

def test_TrieDictScannerBasic():
    td = RegexTrieDict()
    scanner = RegexTrieDictScanner(td)

    td.insert("egg")
    td.insert("r")
    assert "egg" in td
    td.insert("eggbert")
    assert "eggbert" in td
    assert "egg" in td

    # TODO this mostly works but needs better API for using in practice.
    # Needs close inspection, though, to make sure cannot_match is correctly done...
    # It should recognize the string eggbert BEFORE the x is inserted... what
    # are the criteria?  No active loops and all else at a trie leaf?
    #
    # Worst case you could temporarily insert a magic elem that matches anything
    # and see if anything is left.  More expensive, though.

    print("\nquerying 'eggbert' character by character")
    for c in "eggbert" + "x": # NOTE x also added for now
        scanner.add_text_elem(c)
        scanner_text = scanner.curr_prefix_text
        print("Current scanner prefix:", scanner_text)
        last_matches = scanner.matching_nodes
        print("Last matches on inserting", c, "are", last_matches)
        all_states = scanner.prefix_matcher.node_data_list
        print("All states on inserting", c, "are", all_states)
        print()

    fail() # TODO above here gives fail due to trie modification.

    print("\nquerying 'eggber' character by character")
    for c in "eggber":
        scanner.add_text_elem(c)
        scanner.print_token_deque()
    print("\nquerying 'x' character, should cause fail and make 'egg' match")
    scanner.add_text_elem("x")
    scanner.print_token_deque()

    scanner.reset_text()
    scanner.clear_token_data_deque()
    testString = "moneypursemoneysxegg"
    print(testString)
    for char in testString:
        scanner.add_text_elem(char)
    scanner.assert_end_of_text()
    print(scanner.get_token_data_deque())

def test_trieDictTokenizeMore():
    skip()
    td = RegexTrieDict()
    scanner = RegexTrieDictScanner(td)
    bugString = "oneonefive\none"
    td["\n"] = ("whitespace", None)
    td["one"] = ("var", 1); td["two"] = ("var", 2); td["three"] = ("var", 3)
    td["four"] = ("var", 4); td["five"] = ("var", 5)
    td["oneone"] = ("var2", 1); td["twotwo"] = ("var2", 2)
    td["threethree"] = ("var2", 3)
    td["fourfour"] = ("var2", 4); td["fivefive"] = ("var2", 5)
    td.print_tree()
    for st in bugString:
        scanner.add_text_elem(st)
    scanner.assert_end_of_text()
    print()
    print(scanner.get_token_data_deque())
    print()
    print(td["one"])

    # test delete all the way to root
    td["abigstring"] = True
    td.print_tree()
    del td["abigstring"]
    td.print_tree()

    """
   # check unsigned int recognition
   tok.clear_token_data_deque()
   tok.setMatchUnsignedIntAfterLeadingDigit(True, ("unsignedInt", None))
   testStr = "oneoneone3344two\n444\nthree123\n"
   print("test string:", testStr)
   for char in testStr:
      tok.add_text_elem(char)
   tok.assert_end_of_text()
   tok.print_token_deque()

   """
