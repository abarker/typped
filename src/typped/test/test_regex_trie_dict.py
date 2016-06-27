# -*- coding: utf-8 -*-
from __future__ import print_function, division
import pytest_helper

# TODO: random regex fails for Python3, string problem

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path(add_parent=True)

import random
import string
import re
from regex_trie_dict import *
from py.test import raises, fail

#
# Test basic TrieDict operations.
#

def test_semanticsOfPythonRegex():
    """Just to be clear on the semantics of Python regexes, what should be
    implemented."""
    patt = "^([ab])*$"
    # No binding on wildcard in loops; they can vary on each iteration.
    assert re.match(patt, "")
    assert re.match(patt, "a")
    assert re.match(patt, "b")
    assert re.match(patt, "aa")
    assert re.match(patt, "ab")
    assert re.match(patt, "abbbaaaab")
    # Similarly for pattern 'or' groups, they can vary on each iteration.
    patt = "^((AA|BB))*$"
    assert re.match(patt, "AA")
    assert re.match(patt, "AABB")
    assert re.match(patt, "AABBAA")
    assert not re.match(patt, "ABBAAA")
    # Note that each separate pattern in the trie is assumed to be related by
    # 'or'.  Each path from the root to a match is a separate pattern connected
    # by 'or'.  So the patterns in the trie are like an enclosing 'or' at the
    # outer level:
    patt = "^((aA)*|(bB)*)$"
    assert re.match(patt, "aAaA")
    assert re.match(patt, "bB")
    assert not re.match(patt, "aAbB")
    # Loops re-bind on each entry, even within another loop.  No longer memory.
    patt = "^((A)*(B)*)*$"
    assert re.match(patt, "AAABBBBAAAAABBBBBBABBAAAABBBBBB")
    # So fixed characters in a loop are temporarily bound until the loop exits,
    # but no other pattern elements are ever bound.


def mappingInvariants(m):
    """Called from test_basicRegexTrieDict.  Test the invariants that a map should
    have, from PEP 3119."""
    assert len(m.values()) == len(m.keys()) == len(m.items()) == len(m)
    assert [value for value in m.values()] == [m[key] for key in m.keys()]
    assert [item for item in m.items()] == [(key, m[key]) for key in m.keys()]


def test_basicRegexTrieDict():
    """Very basic inserts, deletes, etc."""
    td = RegexTrieDict()
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
    td.printTree()
    del td["moneys"]
    assert "moneys" not in td
    if not "money" in td:
        fail("The string should be in the dict.")
    assert "money" in td
    td["money"] = 55
    assert td["money"] == 55
    mappingInvariants(td)

    # isPrefixOfKey
    assert td.isPrefixOfKey("money")
    assert td.isPrefixOfKey("mone")
    assert td.isPrefixOfKey("m")
    assert not td.isPrefixOfKey("ZxZ")

    # someKeyIsPrefix
    assert td.someKeyIsPrefix("money")
    assert not td.someKeyIsPrefix("mon")
    td.insert("zzzz123456789")
    assert not td.someKeyIsPrefix("zzzz12345678")
    assert len(td) == len(td.keys())

    # clear
    td.clear()
    assert len(td) == 0
    td["egg"] = 5
    td["e"] = 4
    assert td["e"] == 4 and td["egg"] == 5

    td.printTree() # will show up on errors


#
# Random insertions and deletions.
#

def test_randomInsertionsAndDeletions():
    """Generate some random strings and insert and delete them."""
    td = RegexTrieDict()

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
# Test TrieDict regexp operations.
#

def test_basicTrieDictCharRegexpSequences():
    """Basic regex pattern matching."""
    td = RegexTrieDict()
    td.insert("wa\\[z3\\]aa")
    assert td.has_key_meta("wazaa")
    assert td.has_key_meta("wa3aa")

    td.insert("wa\\[ze\\]aa")
    assert td.has_key_meta("wazaa")
    assert td.has_key_meta("waeaa")

    # wildcard after wildcard, as first and last
    td.insert("\\[a\\]\\[a\\]")
    assert td.has_key_meta("aa")
    td.insert("a\\[ral\\]aa")
    td.insert("a\\[ral\\]")
    assert td.has_key_meta("aa")
    td.insert("a\\[ral\\]aa")

    # wildcards with range
    td.insert("\\[q\\-s\\]")
    assert not td.has_key_meta("p")
    assert td.has_key_meta("q")
    assert td.has_key_meta("r")
    assert td.has_key_meta("s")
    assert not td.has_key_meta("t")
    td.insert("A\\[a\\-b\\]")
    td.insert("A\\[a\\-b\\]c")
    assert td.has_key_meta("Ab")
    assert td.has_key_meta("Aac")
    assert td.has_key_meta("Abc")

    # test ordinary repetition
    td.insert("abc\\*\\(dx\\)efg")
    assert td.has_key_meta("abcefg")
    assert td.has_key_meta("abcdxefg")
    assert td.has_key_meta("abcdxdxefg")

    # test repetition at beginning
    td.insert("\\*\\(dx\\)efg")
    assert td.has_key_meta("efg")
    assert td.has_key_meta("dxefg")
    assert td.has_key_meta("dxdxefg")

    # test repetition at end
    td.insert("abc\\*\\(dx\\)")
    assert td.has_key_meta("abc")
    assert td.has_key_meta("abcdx")
    assert td.has_key_meta("abcdxdxdx")

    # repetition after repetition
    td.insert("abc\\*\\(dx\\)\\*\\(e\\)def")
    assert td.has_key_meta("abcdxdxdxeeeedef")

    # nested repetition
    td.insert("ABC\\*\\(dx\\*\\(BB\\)dx\\)DEF")
    assert td.has_key_meta("ABCdxBBdxDEF")
    assert td.has_key_meta("ABCDEF")
    assert td.has_key_meta("ABCdxBBdxdxBBdxDEF")
    td.insert("ABC\\*\\(8\\*\\(bb\\)\\)DEF") # touching nested at end
    assert td.has_key_meta("ABC8bbDEF")
    td.insert("ABC\\*\\(\\*\\(bb\\)8\\)DEF") # touching nested at begin
    assert td.has_key_meta("ABCbb8DEF")

    # illegal nested repetition pattern
    td.insert("ABC\\*\\(\\*\\(bb\\)\\)DEF") # empty rep around another rep
    with raises(PatternMatchError):
        td.has_key_meta("ABCbbDEF")
    with raises(PatternMatchError):
        td.has_key_meta("ABCDEF")
    with raises(PatternMatchError):
        td.has_key_meta("ABCbbbbDEF")
    del td["ABC\\*\\(\\*\\(bb\\)\\)DEF"] # must delete or screws up later ones!

    # repetition after wildcard, wildcard after repetition

    # escape-char after... before ... in ... various things

    # test zero-element-matching patterns at end
    td.insert("ABC\\*\\(dx\\)\\*\\(e\\)DEF\\*\\(x\\)\\*\\(b\\)")
    assert td.has_key_meta("ABCdxDEF")
    assert td.has_key_meta("ABCdxeeDEFxb")
    assert td.has_key_meta("ABCdxDEFxb")
    assert td.has_key_meta("ABCDEFxb")
    assert td.has_key_meta("ABCDEFb")
    assert td.has_key_meta("ABCDEF")
    td.printTree() # will show up on errors


def test_someRepetitionPrefixes():
    """This section is to test whether fixing to a single child in repetition
    works.  If not there can be some crosstalk between multiple patterns in
    the trie and some false matches."""
    td = RegexTrieDict()

    # Repetitions with common prefix.
    td.insert("\\*\\(eggsalad\\)")
    td.insert("\\*\\(egghead\\)")
    assert td.has_key_meta("eggsaladeggsalad")
    assert not td.has_key_meta("eggsaladegghead")

    # Repetitions with wildcards with common prefix.
    td.insert("\\*\\(begin\\[AB\\]\\)")
    td.insert("\\*\\(begin\\[YZ\\]\\)")
    assert td.has_key_meta("beginA")
    assert td.has_key_meta("beginZ")
    assert td.has_key_meta("beginBbeginB")
    assert td.has_key_meta("beginYbeginY")
    assert not td.has_key_meta("beginAbeginY")

    # Repetition with 'or'.
    td.insert("\\*\\(\\(burger\\|taco\\)\\)")
    td.insert("\\*\\(\\(burger\\|soda\\)\\)")
    assert td.has_key_meta("burgertaco")
    assert td.has_key_meta("burgersoda")
    assert not td.has_key_meta("tacosoda")
    assert not td.has_key_meta("burgertacoburgersoda")

    # Nested repetitions with common prefix.
    td.insert("\\*\\(\\*\\(shoe\\)y\\)")
    td.insert("\\*\\(\\*\\(shirt\\)y\\)")
    assert td.has_key_meta("shoeyshoey")
    assert td.has_key_meta("shirtyshirty")
    assert not td.has_key_meta("shoeyshirty")

    # Nested repetitions with common prefix and wildcard.
    td.insert("\\*\\(\\*\\(s\\[qAB\\]\\)y\\)")
    td.insert("\\*\\(\\*\\(s\\[qCD\\]\\)y\\)")
    assert td.has_key_meta("sAy")
    assert td.has_key_meta("sCy")
    assert td.has_key_meta("sAysAy")
    assert not td.has_key_meta("sAysCy")

    # Nested repetition with common prefix and 'or' in inner one, match wildcards above.
    td["\\*\\(\\*\\(s\\(A\\|B\\)\\)y\\)"] = 0
    td["\\*\\(\\*\\(s\\(C\\|D\\)\\)y\\)"] = 1
    assert td.has_key_meta("sAy")
    assert td.has_key_meta("sBy")
    assert td.has_key_meta("sAysAy")
    assert td.has_key_meta("sAysBy")
    assert not td.has_key_meta("sAysCy")
    assert td.has_key_meta("sCysDysCysCy")
    assert not td.has_key_meta("sCysAysCysCy")
    assert not td.has_key_meta("sCysBysCysCy")


def test_patternsWithEscapes():
    """Make sure that literal escapes are handled correctly."""
    td = RegexTrieDict()
    td.insert("\\[\\\\B\\]")
    assert td.has_key_meta("\\")
    assert td.has_key_meta("B")
    td.insert("\\\\")
    assert td.has_key_meta("\\")
    td.insert("\\(\\\\\\|X\\)")
    assert td.has_key_meta("\\")
    assert td.has_key_meta("X")
    td.insert("\\*\\(\\\\abc\\\\\\)")
    assert td.has_key_meta("\\abc\\") # this one still FAILS


def test_characterMatchesWithPythonEscapeSymbols():
    """Test the regex wildcards using the Python escape symbols."""
    td = RegexTrieDict()
    # Test a digit, \d.
    td.insert("\\[\\d\\]")
    assert td.has_key_meta("0")
    assert td.has_key_meta("9")
    assert not td.has_key_meta("a")


def test_orInsideRepetition():
    """Test 'or' patterns inside repetition patterns and vice versa."""
    td = RegexTrieDict()
    td.insert("\\*\\(\\(Aa\\|B\\)\\)")
    assert td.has_key_meta("Aa")
    assert td.has_key_meta("AaB")
    assert td.has_key_meta("AaAa")
    assert td.has_key_meta("B")
    td.insert("\\*\\(\\(NqS\\|v\\)\\)")
    assert td.has_key_meta("vv")
    td.insert("\\*\\(\\(\\[YE\\]\\|\\*\\(\\[GT9m\\]\\)\\)\\)")
    assert td.has_key_meta("YYY")
    "(Y|*(Z))"
    td.insert("\\(\\(M\\|\\*\\(Z\\)\\)\\)")
    td.insert("\\(M\\|\\*\\(Z\\)\\)")
    assert not td.has_key_meta("MMM")
    assert td.has_key_meta("ZZZ")
    td.clear()
    "*((Y|*(Z)))"
    td.insert("\\*\\(\\(Y\\|\\*\\(Z\\)\\)\\)")
    assert not td.has_key_meta("X")
    assert td.has_key_meta("YYY")
    "*((*(Z)|Y))"
    td.insert("\\*\\(\\(\\*\\(Z\\)\\|Y\\)\\)")
    assert td.has_key_meta("Y")
    "*((*(Z)a|Y))"
    td.insert("\\*\\(\\(\\*\\(Z\\)a\\|Y\\)\\)")
    assert td.has_key_meta("Y")
    "*((a*(Z)|Y))"
    td.insert("\\*\\(\\(a\\*\\(Z\\)\\|Y\\)\\)")
    assert td.has_key_meta("Y")
    assert td.has_key_meta("aZaZ")


def test_matcher():
    td = RegexTrieDict()
    mat = Matcher(td)
    # basic string insert
    td.insert("egg", ("data",))
    mat.next_key_elem("e")
    assert mat.get() == []
    mat.next_key_elem("g")
    assert mat.get() == []
    mat.next_key_elem("g")
    assert mat.get() == [("data",)]

    # pattern string insert
    td.insert("x\\[\\d\\]", "data9")
    mat.reset() # needs to be done after insert; insert in td won't do it for mat
    mat.next_key_elem("x")
    assert mat.get() == []
    mat.next_key_elem("9")
    assert mat.get() == ["data9"]

    # pattern when pattern and ordinary string both match (also multiple patts)
    td = RegexTrieDict()
    mat = Matcher(td)
    td.insert("\\[\\w\\]8", "1 pattern data")
    assert td.has_key_meta("x8")
    assert not td.has_key_meta("xx")
    td.insert("x8", "string x8 data")
    assert td.has_key("x8")
    assert td.has_key_meta("x8")
    assert td.has_key_meta("y8")
    assert td.has_key_meta("x8") == 2
    assert td.has_key_meta("y8") == 1
    mat.next_key_elem("x") # turns on seqmeta mode
    assert mat.get() == []
    mat.next_key_elem("8")
    assert sorted(mat.get()) == sorted(["1 pattern data", "string x8 data"])
    td.insert("x\\[\\d\\]", "2 pattern data") # turns off seqmeta mode
    mat.reset() # needs to be done after insert; insert in td won't do it for mat
    mat.next_key_elem("x")
    mat.next_key_elem("8")
    assert sorted(mat.get()) == sorted(["1 pattern data", "2 pattern data",
                                               "string x8 data"])
    td.insert("x\\*\\(\\[\\d\\]\\)", "3 pattern data") # turns off seqmeta mode
    mat.reset() # needs to be done after insert; insert in td won't do it for mat
    mat.next_key_elem("x")
    mat.next_key_elem("8")
    assert sorted(mat.get()) == sorted(["1 pattern data", "2 pattern data",
                                               "3 pattern data", "string x8 data"])

    with raises(ModifiedTrieError):
        td = RegexTrieDict()
        mat = Matcher(td)
        td.insert("egg", ("data",))
        mat.next_key_elem("e") # this doesn't fail for now, maybe should...
        td.insert("bacon", ("data",))
        mat.next_key_elem("e")

#
# Test case of non-halt, worst-case.
#

def test_nonHalt():
    """Test a worst-case example."""
    td = RegexTrieDict()
    patt = "\\*\\(a\\*\\(\\(\\*\\(G\\)\\|G\\)\\)\\)" # works if either G set to F
    ex = "aGGGGaaGGGGGGaG" # this one essentially halts
    ex = "aGGGGaaGGGaG" # this one is just very slow
    ex = "aGaaGaG" # this one is not so slow
    td.insert(patt)
    assert td.has_key_meta(ex)

#
# Test some random regexps.
#


def test_randomRegex():
    """Use the regex generator program to randomly generate some regexes.
    Note this can generate some worst-cases which are quite slow if new
    seeds are chosen."""
    from generate_random_regex import genRandomRegex
    import re
    td = RegexTrieDict()
    seedExample = 33233; seedPattern = 29119
    #seedExample = 38233 ; seedPattern = 29149

    numPatterns = 10
    numExamplesPerPattern = 10
    for i in range(numPatterns):
        seedPattern += 1 # needed because each instance sets seed for new random.Random()
        for j in range(numExamplesPerPattern):
            seedExample += 1
            pythonPatt, triePatt, example = genRandomRegex(
                    maxLength=40, maxNumParts=3, seedPattern=seedPattern,
                    seedExample=seedExample)
            print("pattern, trie format:", triePatt, "  instance: ", example)
            print("pattern, Python format:", pythonPatt, "  instance:  ", example)
            #assert re.match(pythonPatt, example)
            #if i <= 11: continue
            #if i == 12: fail("test")
            td.insert(triePatt) # inefficient repeated insert
            assert td.has_key_meta(example)
            #del td[triePatt] # debug, try to simplify and isolate error

    #fail("show output")


#
# Test some error conditions.
#

def test_someErrorConditions():
    """Some error patterns.  Some are caught on insert, others are caught
    on queries.  Could later catch more on insert, to catch sooner."""
    # empty elem is not allowed to be inserted (by def it has no elements to store)
    td = RegexTrieDict()
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
    # Mismatched parens.
    #
    td.insert("xy\\(a\\|b\\)") # ends with close paren, shouldn't be mismatch error
    with raises(PatternMatchError):
        td.insert("x\\*\\(yy") # mismatched parens
    with raises(PatternMatchError):
        td.insert("x\\(yy") # mismatched parens
    with raises(PatternMatchError):
        td.insert("x\\*\\(xx\\*\\(yy\\)") # mismatched parens
    with raises(PatternMatchError):
        td.insert("xyy\\)zz") # mismatched parens

    #
    # Bad repetition patterns.
    #
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*egg") # no open-group after repetition
        td.has_key_meta("e") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*\\\\egg") # no open-group after repetition
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*a\\(egg\\)") # bad digit in repetition bounds
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*2\\*a\\(egg\\)") # bad digit in repetition bounds
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*\\*4\\(egg\\)") # no digit after first repetition
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*3\\Z4\\(egg\\)") # bad escape character instead of asterick
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("d\\*\\(\\*\\(xxx\\)\\)ddd") # nested repetition, nothing in outer
        td.has_key_meta("dxx") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\*\\(\\)") # empty repetition, caught on insert

    #
    # Bad 'or' patterns.
    #
    with raises(PatternMatchError):
        td.clear()
        td.insert("x\\|bcc") # or without open paren
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("x\\|bc\\(c\\)") # or without open paren
        td.has_key_meta("x") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("xxx\\(\\)dd") # empty 'or' group, caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\(\\)") # empty 'or' group, caught on insert

    #
    # Bad wildcard patterns.
    #
    with raises(PatternMatchError):
        td.clear()
        td.insert("xx\\[xxx") # no closing wildcard bracket
        td.has_key_meta("xxx") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("xx\\[\\]ddd") # empty wildcard bracket
        td.has_key_meta("xxx") # not caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\[\\]") # empty wildcard bracket, caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\[\\]") # empty wildcard bracket, caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\[\\[xyz\\]") # mismatched wildcard bracket, caught on insert
    with raises(PatternMatchError):
        td.clear()
        td.insert("\\[w\\[xyz\\]a\\]") # nested wildcard bracket, caught on insert

    #
    # Bad meta-char.
    #
    with raises(PatternMatchError):
        td.clear()
        td.insert("d\\Bddd") # bad meta-element in string
        td.has_key_meta("dd") # not caught on insert

    # Make sure td is not just messed up somehow causing above tests to fail.
    td.clear()
    td.insert("qqq")
    assert td.has_key_meta("qqq")

    #
    # Test for invalidated tree search due to deletes during it.
    #
    td.insert("\\*\\(ff\\)xx")
    nodeDataList = td.getNextNodesMeta("f", td.get_root_node_data_list())
    with raises(ModifiedTrieError):
        del td["qqq"] # delete during search invalidates whole thing
        nodeDataList = td.getNextNodesMeta("f", nodeDataList)
    # Test for invalidated tree search due to inserts during it.
    td.clear()
    td.insert("\\*\\(ff\\)xx")
    nodeDataList = td.getNextNodesMeta("f", td.get_root_node_data_list())
    with raises(ModifiedTrieError):
        td["qqq"] = 5 # insert during search invalidates whole thing
        nodeDataList = td.getNextNodesMeta("f", nodeDataList)
    # Simple reassignment of a value should not affect the search.
    td.clear()
    td.insert("\\*\\(ff\\)xx")
    nodeDataList = td.getNextNodesMeta("f", td.get_root_node_data_list())
    td["\\*\\(ff\\)xx"] = 5 # value reassignment is OK
    nodeDataList = td.getNextNodesMeta("f", nodeDataList)
    assert td["\\*\\(ff\\)xx"] == 5


def test_repetitionsWithBounds():
    """Test repetition patterns with bounds set."""
    td = RegexTrieDict()

    # Only lower bound set (at least some number of repetitions).
    td.insert("b\\*5\\(X\\)b")
    assert not td.has_key_meta("bXXXXb")
    assert td.has_key_meta("bXXXXXb")
    assert td.has_key_meta("bXXXXXXb")

    # Both lower and upper bound set.
    td.insert("a\\*4\\*5\\(X\\)a")
    assert not td.has_key_meta("aXXXa")
    assert td.has_key_meta("aXXXXa")
    assert td.has_key_meta("aXXXXXa")
    assert not td.has_key_meta("aXXXXXXa")
    td.insert("\\*4\\*4\\(Z\\)")
    assert not td.has_key_meta("ZZZ")
    assert td.has_key_meta("ZZZZ")
    assert not td.has_key_meta("ZZZZZ")

    # Double-digit bounds.
    td.insert("\\*10\\*11\\(Z\\)")
    assert not td.has_key_meta("ZZZZZZZZZ")
    assert td.has_key_meta("ZZZZZZZZZZ")
    assert td.has_key_meta("ZZZZZZZZZZZ")
    assert not td.has_key_meta("ZZZZZZZZZZZZ")
    td.insert("abc\\*10\\(dd\\)efg")
    assert not td.has_key_meta("abcddddddddddddddddddefg")
    assert td.has_key_meta("abcddddddddddddddddddddefg")
    assert td.has_key_meta("abcddddddddddddddddddddddefg")
    assert not td.has_key_meta("abcdddddddddddddddddddddefg") # odd number

    #td.printTree() # will show up on errors


#
# Test 'or' sections
#

def test_orSections():
    td = RegexTrieDict()

    # simple 'or' with one choice
    td.insert("\\(111\\)")
    assert td.has_key_meta("111")
    td.insert("\\(222\\|333\\|444\\)")
    assert td.has_key_meta("222")
    assert td.has_key_meta("333")
    assert td.has_key_meta("444")

    # pattern with same prefixes and different ends
    td.insert("\\(222\\|666\\|777\\)x")
    td.insert("\\(222\\|66\\|888\\)y")
    assert td.has_key_meta("222x")
    assert td.has_key_meta("222y")
    assert td.has_key_meta("666x")
    assert not td.has_key_meta("666y")
    assert td.has_key_meta("66y")
    assert not td.has_key_meta("66x")
    assert td.has_key_meta("777x")
    assert not td.has_key_meta("777y")
    assert td.has_key_meta("888y")
    assert not td.has_key_meta("888x")
    assert not td.has_key_meta("666y")

    # repetition inside a one-section 'or'
    td.insert("x\\(\\*\\(egg\\)\\)y")
    assert td.has_key_meta("xy")
    assert td.has_key_meta("xeggy")
    assert td.has_key_meta("xeggeggy")

    # repetition inside a multi-section 'or' with a wildcard, too
    td.insert("x\\(\\*\\(egg\\)\\|\\[ST\\]\\)y")
    assert td.has_key_meta("xSy")
    assert td.has_key_meta("xTy")
    assert td.has_key_meta("xeggy")
    assert td.has_key_meta("xeggeggy")


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

    td = RegexTrieDict()

    def listRangeTestFun(lowerLstElem, upperLstElem, lstElem):
        return charRangeTest(lowerLstElem[0], upperLstElem[0], lstElem[0])

    def myPattMatchFun(queryElem, pattList, rangeElem, escapeElem):
        return genericWildcardMatchFun(queryElem, pattList, rangeElem, escapeElem,
                                       rangeTestFun=listRangeTestFun)
    td.define_meta_elems(escape=("\\"), repetition=("*"), lGroup=("("), rGroup=(")"),
                         lWildcard=("["), rWildcard=("]"), rangeElem=("-"),
                         wildcardPattMatchFun=myPattMatchFun, canonicalizeFun=None)

    # keys, note this fails without asLists=True because we have not properly defined
    # the plus operator to "add" two elements into a sequence.
    td.insert([("A"), ("B"), ("C")])
    td.insert([("D"), ("E"), ("F")])
    print(td.keys(asLists=True))
    keys = td.keys(asLists=True)
    assert (keys == [[("A"), ("B"), ("C")], [("D"), ("E"), ("F")]] or
            keys == [[("D"), ("E"), ("F")], [("A"), ("B"), ("C")]])

    # basic wildcards
    td.insert([("w"), ("a"), ("\\"), ("["), ("z"), ("3"), ("\\"), ("]"), ("a"), ("a")])
    assert td.has_key_meta([("w"), ("a"), ("z"), ("a"), ("a")])
    assert td.has_key_meta([("w"), ("a"), ("3"), ("a"), ("a")])

    # repetitions
    # TODO
