# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path("../src")

import random
import string
import re
from typped.trie_dict import *
from py.test import raises, fail

#
# Test basic TrieDict operations.
#


def mappingInvariants(m):
    """Called from test_basicTrieDict.  Test the invariants that a map should
    have, from PEP 3119."""
    assert len(m.values()) == len(m.keys()) == len(m.items()) == len(m)
    assert [value for value in m.values()] == [m[key] for key in m.keys()]
    assert [item for item in m.items()] == [(key, m[key]) for key in m.keys()]


def test_basicTrieDict():
    """Very basic inserts, deletes, etc."""
    td = TrieDict()
    assert sorted(td.keys(asLists=True)) == []
    assert sorted(td.keys()) == []
    assert len(td) == 0
    td["eggsalad"] = "x"
    assert td.keys(asLists=True) == [["e", "g", "g", "s", "a", "l", "a", "d"]]
    assert len(td) == 1
    mappingInvariants(td)
    td["egg"] = "eeee"
    del td["egg"]
    assert len(td) == 1
    td["egg"] = "ffff"
    td["eggattack"] = "q"
    td["eggattack"] = "y"
    assert sorted(td.keys()) == sorted(["eggsalad", "egg", "eggattack"])
    assert len(td) == 3
    assert td["eggsalad"] == "x"
    assert td["eggattack"] == "y"
    td.insert("ebggattack")
    td.insert("eggattac")
    td.delitem("eggattac")
    with raises(KeyError):
        td.delitem("eggattac")
    with raises(KeyError):
        td['eggattac']
    td.insert("eggattack1")
    mappingInvariants(td)
    td.delitem("eggattack1")
    assert td["eggattack"] == "y"
    td.insert("ebert")
    td["elf"] = "z"
    td.insert("money")
    td.insert("moneys")
    td.insert("moneypurse")
    td.print_tree()
    del td["moneys"]
    assert "moneys" not in td
    if not "money" in td:
        fail("The string should be in the dict.")
    assert "money" in td
    td["money"] = 55
    assert td["money"] == 55
    mappingInvariants(td)

    # is_prefix_of_key
    assert td.is_prefix_of_key("money")
    assert td.is_prefix_of_key("mone")
    assert td.is_prefix_of_key("m")
    assert not td.is_prefix_of_key("ZxZ")

    # some_key_is_prefix
    assert td.some_key_is_prefix("money")
    assert not td.some_key_is_prefix("mon")
    td.insert("zzzz123456789")
    assert not td.some_key_is_prefix("zzzz12345678")
    assert len(td) == len(td.keys())

    # clear
    td.clear()
    assert len(td) == 0
    td["egg"] = 5
    td["e"] = 4
    assert td["e"] == 4 and td["egg"] == 5

    td.print_tree() # will show up on errors

#
# Random insertions and deletions.
#


def test_randomInsertionsAndDeletions():
    """Generate some random strings and insert and delete them."""
    td = TrieDict()

    def genRandomString():
        strlen = random.randint(1, 20)
        chars = string.ascii_letters + string.digits
        if random.randint(1, 2) == 1: chars = ["A", "B"] # so more similar prefixes
        charList = [random.choice(chars) for i in range(strlen)]
        randStr = "".join(charList)
        return randStr
    for i in range(100):
        strList = []
        for j in range(10):
            # insert a list of random strings
            s = genRandomString()
            if s not in strList: strList.append(s)
            td[s] = 99
            assert s in td
            # one final random insert and delete
            s = genRandomString()
            if not s in strList:
                td.insert(s)
                del td[s]
        print("random string list", strList)
        assert set(td.keys()) == set(strList)
        for s in strList:
            assert s in td
            assert td[s] == 99
            del td[s]
            assert s not in td


#
# Test some error conditions.
#

def test_someErrorConditions():
    """Some error patterns.  Some are caught on insert, others are caught
    on queries.  Could later catch more on insert, to catch sooner."""
    # empty elem is not allowed to be inserted (by def it has no elements to store)
    td = TrieDict()
    #
    # KeyError in the trie.
    #
    td.insert("xxx")
    with raises(KeyError):
        td.insert("")
    with raises(KeyError):
        x = td["yyy"]
    with raises(KeyError):
        del td["www"]
    with raises(KeyError):
        del td["xxxx"]
    with raises(KeyError):
        del td["xx"]
    assert "" not in td

#
# Test a TrieDict with non-string sequences and non-character elements.
#


def test_TrieDictWithNonStringSequences():
    """Remember that anything used as the key of a built-in Python dict must be
    hashable (basically, it must have a unique immutable representation).  We
    can actually weaken that condition: Only the elements need to be hashable,
    not the entire sequence of elements.  Here we test the TrieDict with
    elements defined as strings inside lists.  String-like sequences are then
    lists (or tuples, but not necessarily) of those character-containing tuples.
    For example, ("a") is an element and [("a"), ("a")] is a two-element
    sequence.  For simplicity in the testing example the tuples containing
    characters are treated like the characters themselves as far as
    comparisons."""

    td = TrieDict()

    # keys, note this fails without asLists=True because we have not properly defined
    # the plus operator to "add" two elements into a sequence.
    td.insert([("A"), ("B"), ("C")])
    td.insert([("D"), ("E"), ("F")])
    print(td.keys(asLists=True))
    keys = td.keys(asLists=True)
    assert (keys == [[("A"), ("B"), ("C")], [("D"), ("E"), ("F")]] or
            keys == [[("D"), ("E"), ("F")], [("A"), ("B"), ("C")]])
