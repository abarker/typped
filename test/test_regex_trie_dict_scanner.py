# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path("../src")

from typped.regex_trie_dict import *
from typped.regex_trie_dict_scanner import *
from py.test import raises, fail

#
# Test TrieDictScanner.
#

def test_TrieDictScannerBasic():
    td = RegexTrieDict()
    tok = RegexTrieDictScanner(td)

    td.insert("egg")
    td.insert("r")
    assert "egg" in td
    td.insert("eggbert")
    assert "eggbert" in td
    assert "egg" in td
    print("\nquerying 'eggbert' character by character")
    for i in "eggbert":
        tok.insertSeqElem(i)
        tok.printTokenDeque()
    print("\nquerying 'eggber' character by character")
    for i in "eggber":
        tok.insertSeqElem(i)
        tok.printTokenDeque()
    print("\nquerying 'x' character, should cause fail and make 'egg' match")
    tok.insertSeqElem("x")
    tok.printTokenDeque()

    tok.resetSeq()
    tok.clearTokenDataDeque()
    testString = "moneypursemoneysxegg"
    print(testString)
    for char in testString:
        tok.insertSeqElem(char)
    tok.assertEndOfSeq()
    print(tok.getTokenDataDeque())


def test_trieDictTokenizeMore():
    td = RegexTrieDict()
    tok = RegexTrieDictScanner(td)
    bugString = "oneonefive\none"
    td["\n"] = ("whitespace", None)
    td["one"] = ("var", 1); td["two"] = ("var", 2); td["three"] = ("var", 3)
    td["four"] = ("var", 4); td["five"] = ("var", 5)
    td["oneone"] = ("var2", 1); td["twotwo"] = ("var2", 2)
    td["threethree"] = ("var2", 3)
    td["fourfour"] = ("var2", 4); td["fivefive"] = ("var2", 5)
    td.print_tree()
    for st in bugString:
        tok.insertSeqElem(st)
    tok.assertEndOfSeq()
    print()
    print(tok.getTokenDataDeque())
    print()
    print(td["one"])

    # test delete all the way to root
    td["abigstring"] = True
    td.print_tree()
    del td["abigstring"]
    td.print_tree()

    """
   # check unsigned int recognition
   tok.clearTokenDataDeque()
   tok.setMatchUnsignedIntAfterLeadingDigit(True, ("unsignedInt", None))
   testStr = "oneoneone3344two\n444\nthree123\n"
   print("test string:", testStr)
   for char in testStr:
      tok.insertSeqElem(char)
   tok.assertEndOfSeq()
   tok.printTokenDeque()

   """
