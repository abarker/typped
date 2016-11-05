# -*- coding: utf-8 -*-
from __future__ import print_function, division
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import random
import string
import re
from typped.regex_trie_dict import *
from py.test import raises, fail

#
# Test basic TrieDict operations.
#

def get_matched(patt, string):
    """Helper fun to get the matched string from Python re.match function."""
    m = re.match(patt, string)
    if not m: return m
    return m.group()

def rtd_match(rtd_patt, string, rtd):
    """Helper fun to get the matched string from a `RegexTrieDict`."""
    rtd.clear()
    rtd.insert(rtd_patt)
    return rtd.has_key_meta(string)

def assert_match_agreement(py_patt, rtd_patt, string, rtd, matches=True):
    """Do match, asserting that it is `True` for both the Python regexes and the
    `RegexTrieDict` regexes.  Setting `neg` does the "not" assertions."""
    if matches:
        assert re.match(py_patt, string)
        assert rtd_match(rtd_patt, string, rtd)
    else:
        assert not re.match(py_patt, string)
        assert not rtd_match(rtd_patt, string, rtd)

def test_basic_Python_semantics():
    """Just to be clear on the semantics of Python regexes, what should be
    implemented."""
    rtd = RegexTrieDict()

    # No binding on wildcard in loops; they can vary on each iteration.
    patt = r"^([ab])*$"
    rtd_patt = r"\*\(\[ab\]\)"
    assert_match_agreement(patt, rtd_patt, "", rtd)
    assert_match_agreement(patt, rtd_patt, "a", rtd)
    assert_match_agreement(patt, rtd_patt, "b", rtd)
    assert_match_agreement(patt, rtd_patt, "aa", rtd)
    assert_match_agreement(patt, rtd_patt, "ab", rtd)
    assert_match_agreement(patt, rtd_patt, "abbbaaaab", rtd)

    # Similarly for pattern 'or' groups, they can vary on each iteration.
    patt = r"^((AA|BB))*$"
    rtd_patt = r"\*\(\(AA\|BB\)\)"
    assert_match_agreement(patt, rtd_patt, "AA", rtd)
    assert_match_agreement(patt, rtd_patt, "AABB", rtd)
    assert_match_agreement(patt, rtd_patt, "AABBAA", rtd)
    assert_match_agreement(patt, rtd_patt, "ABBAAA", rtd, matches=False)

    # Note that each separate patt, rtd_pattern in the trie is assumed to be related by
    # 'or'.  Each path from the root to a match is a separate patt, rtd_pattern connected
    # by 'or'.  So the patt, rtd_patterns in the trie are like an enclosing 'or' at the
    # outer level:
    patt = r"^((aA)*|(bB)*)$"
    rtd_patt = r"\(\*\(aA\)\|\*\(bB\)\)"
    assert_match_agreement(patt, rtd_patt, "aAaA", rtd)
    assert_match_agreement(patt, rtd_patt, "bB", rtd)
    assert_match_agreement(patt, rtd_patt, "aBbB", rtd, matches=False)

    # Loops re-bind on each entry, even within another loop.  No longer-term memory.
    patt = r"^((A)*(B)*)*$"
    rtd_patt = r"\*\(\(A\)\*\(B\)\)"
    assert_match_agreement(patt, rtd_patt, "AAABBBBAAAAABBBBBBABBAAAABBBBBB", rtd)
    # So fixed characters in a loop are temporarily bound until the loop exits,
    # but no other patt, rtd_pattern elements are ever bound.

def test_greedy_vs_nongreedy_Python_repetition_semantics():
    """Test Python semantics involving greedy vs. non-greedy match.

    Note the important distinction between matching full patterns (with
    `fullmatch` in Python 3, or starting with ^ and ending with $) and just
    matching a prefix with `re.match`.  The full pattern must always match, but
    in one case it can match a prefix and in the other it must match the whole
    query key.  Greedy vs. non-greedy affects both, but because *some*
    pattern must always match if it can the results can look quite different.

    Adding `$` to the end of the pattern to match constrains the query string
    to match the existing pattern to the end.  Adding the pattern `.*?$` to the
    end of any non-full pattern match makes it do the "usual" prefix match and
    then just accept everything (in a non-greedy way) until the end.

    Note that the above method always returns a match of the same length, i.e.,
    the length of the query key.  The sub-patterns that are matched within the
    string can be different, though."""

    # TODO: for testing the rtd, it might be better to test the
    # RegexTrieDictScanner here, which uses the PrefixMatcher.

    rtd = RegexTrieDict()

    #
    # Non-greedy.
    #

    patt = r"A.*?B"
    assert get_matched(patt, "A") is None
    assert get_matched(patt, "AB") == "AB"
    assert get_matched(patt, "AxB") == "AxB"
    assert get_matched(patt, "AxxB") == "AxxB"
    # Non-greedy, always stops at first b.
    assert get_matched(patt, "AxBxxx") == "AxB"
    assert get_matched(patt, "AxBxxxB") == "AxB" # Greedy would take "AxBxxxB"
    # Now try the full-pattern match on the last one above.  It will not
    # take the shorter match because it fails on the final $.  Only successful
    # matches are taken (if possible), whether greedy or not.
    patt = r"^A.*?B$"
    assert get_matched(patt, "AxBxxxB") == "AxBxxxB" # Now it gets the full thing.

    # But note what happens when there is a fixed symbol 'q' after the pattern
    # (which could also be an end-of-string $).  The match to the 'q' will
    # match the last example above when a 'q' is added:
    patt = r"A.*?Bq" # non-greedy
    assert get_matched(patt, "AxBAxBq") == "AxBAxBq"
    assert get_matched(patt, "AxBAxBAxBq") == "AxBAxBAxBq"
    # You might expect it to return None, but it doesn't.
    #
    # In terms of multiple states, the state splits into two after the first
    # loop match, but then the first one then dies when it does not see the
    # following 'q'; the second one continues on.

    #
    # Greedy.
    #

    patt = r"A.*B"
    assert get_matched(patt, "A") is None
    assert get_matched(patt, "AB") == "AB"
    assert get_matched(patt, "AxB") == "AxB"
    assert get_matched(patt, "AxxB") == "AxxB"
    # Note greedy, doesn't stop at first b if it finds a later b, otherwise it does.
    assert get_matched(patt, "AxBxxx") == "AxB"
    assert get_matched(patt, "AxBxxxB") == "AxBxxxB"
    assert get_matched(patt, "AxBAxBxxxB") == "AxBAxBxxxB"

    # Note again the difference between the above and the pattern with a 'q'
    # symbol appended to it.
    #
    # It is NOT so greedy that if a loop matches through twice you can kill
    # its first loop-match state assuming that the double-match or higher will
    # be used.  In this example, that would grab up the string "ABAB" and then
    # not match the final "C", but that is not what happens when the match is
    # to the end-of-string $ symbol:
    patt = r"(AB)+(ABC)*q" # greedy, with q suffix.
    assert get_matched(patt, "ABABCq") == "ABABCq"
    # However, when the match is only a prefix match that DOES happen:
    patt = r"(AB)+(ABC)*" # greedy, no fixed suffix.
    assert get_matched(patt, "ABABC") == "ABAB"

    #
    # Test a pairs of matches.

    patt = r"(.*)(B+)" # Two greedy matches.
    assert get_matched(patt, "BBBBB") == "BBBBB"
    m = re.match(patt, "BBBBB")
    print(m.groups())
    assert m.groups() == ("BBBB", "B")
    patt = r"(.*?)(B+?)" # Two non-greedy matches.
    assert get_matched(patt, "BBBBB") == "B"
    m = re.match(patt, "BBBBB")
    assert m.groups() == ("", "B")
    patt = r"(.*?)(B+?)" # Two non-greedy matches.
    assert get_matched(patt, "BBBBB") == "B"
    m = re.match(patt, "BBBBB")
    assert m.groups() == ("", "B")
    patt = r"(.*?)(B+?)$" # Two non-greedy matches to end of query.
    assert get_matched(patt, "BBBBB") == "BBBBB"
    m = re.match(patt, "BBBBB")
    assert m.groups() == ("", "BBBBB")


def test_nongreedy_Python_or_group_semantics():
    """
    Python regex "or" matches are always non-greedy.

    FIRST match is always taken, left to right on ties.  Even if longer match
    would be possible."""

    # TODO: for testing the rtd, it might be better to test the
    # RegexTrieDictScanner here, which uses the PrefixMatcher.

    rtd = RegexTrieDict()

    patt = r"(A*|B*)BB"
    assert get_matched(patt, "A") == None
    assert get_matched(patt, "BBBB") == "BB" # Empty string was first "or" match!
    assert get_matched(patt, "BBBBB") == "BB" # Empty string was still first match.
    assert get_matched(patt, "ABBBB") == "ABB" # 'A' was first match.
    patt = r"(AB|ABC)"
    assert get_matched(patt, "ABC") == "AB" # Longer match is possible but ignored.

    patt = r"^(A|AB)BB$"
    assert get_matched(patt, "ABBB")

    #
    # Set up some named groups to see what Python does, including on length ties.
    #

    # First two groups will match with equal non-greedy length, last will not match.
    # Note there is no outer group for the full "or" containing all the cases.
    # (If there were, beginning it with '(?:' would make the outer "or" group
    # non-capturing so it would give the same results below.)
    patt = r"(?P<first>AB)|(?P<second>A.)|(?P<third>ABC)"
    m = re.match(patt, "ABC")
    if not m: fail()
    groups = m.groups()
    assert groups == ('AB', None, None)
    assert m.group("first") == "AB"
    assert m.group("second") is None
    assert m.group("third") is None
    assert m.lastgroup == "first"

    # Above matches the first-defined group, not the second.  Now swap the first
    # two.  Again the first-defined matching "or" subgroup takes it, rest is
    # ignored and considered unmatched.
    patt = r"(?P<second>A.)|(?P<first>AB)|(?P<third>ABC)"
    m = re.match(patt, "ABC")
    assert m.group("second") == "AB"
    assert m.group("first") is None
    assert m.lastgroup == "second"
    # Note from this that when combining things into a giant "or" (like in
    # a scanner) it is important to order the matches by order of their
    # on_ties priority in case of ties.

    # In both cases above, the possible longer match was ignored.  When that
    # group is first, though, is is no longer ignored.  It becomes the match.
    patt = r"(?P<third>ABC)|(?P<first>AB)|(?P<second>A.)"
    m = re.match(patt, "ABC")
    groups = m.groups()
    assert groups == ("ABC", None, None)
    assert m.group("first") is None
    assert m.group("second") == None
    assert m.group("third") is "ABC"
    assert m.lastgroup == "third"

    # So Python checks the "or" cases left to right, and if one succeeds it
    # never moves on to the next one.  This happens regardless of the length of
    # the match.  So when separate tokens in a scanner are combined into one
    # big "or" you really get "first not longest" behavior.  The ordering would
    # need to take that into account to work around any problems that might
    # cause.

    # This is in contrast to the `RegexTrieDictScanner` which checks all the
    # cases in parallel, both for patterns in the trie and for "or" groups in
    # the patterns.

def mapping_invariants(m):
    """Called from test_basic_RegexTrieDict.  Test the invariants that a map should
    have, from PEP 3119."""
    assert len(m.values()) == len(m.keys()) == len(m.items()) == len(m)
    assert [value for value in m.values()] == [m[key] for key in m.keys()]
    assert [item for item in m.items()] == [(key, m[key]) for key in m.keys()]

def test_basic_RegexTrieDict_operations():
    """Very basic inserts, deletes, etc."""
    td = RegexTrieDict()
    assert sorted(td.keys(as_lists=True)) == []
    assert sorted(td.keys()) == []
    assert len(td) == 0
    td["eggsalad"] = "x"
    assert td.keys(as_lists=True) == [["e", "g", "g", "s", "a", "l", "a", "d"]]
    assert len(td) == 1
    mapping_invariants(td)
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
    mapping_invariants(td)
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
    assert td.get_meta("money")[0] == 55
    mapping_invariants(td)

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

def test_random_insertions_and_deletions():
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

def test_basic_TrieDict_char_regex_sequences():
    """Basic regex pattern matching."""
    td = RegexTrieDict()
    td.insert("wa\\[z3\\]aa")
    assert td.has_key_meta("wazaa")
    assert td.has_key_meta("wa3aa")

    td.insert("wa\\[ze\\]aa")
    assert td.has_key_meta("wazaa")
    assert td.has_key_meta("waeaa")

    # wildcard after wildcard, as first and last
    td.insert("\\[a\\]\\[a\\]", "data_string")
    assert td.has_key_meta("aa")
    assert td.get_meta("aa")[0] == "data_string"
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
    td.print_tree() # will show up on errors


def test_some_repetition_prefixes():
    """This section is to test whether fixing to a single child in repetition
    works.  If not there can be some crosstalk between multiple patterns in
    the trie and some false matches."""
    td = RegexTrieDict()

    # Repetitions with common prefix.
    td.insert("\\*\\(eggsalad\\)", "data_string")
    td.insert("\\*\\(egghead\\)")
    assert td.has_key_meta("eggsalad")
    assert td.has_key_meta("eggsaladeggsalad")
    assert td.get_meta("eggsalad")[0] == "data_string"
    assert td.get_meta("eggsaladeggsalad")[0] == "data_string"
    assert td.get_meta("egghead")[0] != "data_string"
    assert td.get_meta("eggheadegghead")[0] != "data_string"
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


def test_patterns_with_escapes():
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
    assert td.has_key_meta("\\abc\\")

def test_character_matches_with_Python_escape_symbols():
    """Test the regex wildcards using the Python escape symbols."""
    td = RegexTrieDict()
    # Test a digit, \d.
    td.insert("\\[\\d\\]")
    assert td.has_key_meta("0")
    assert td.has_key_meta("9")
    assert not td.has_key_meta("a")

def test_or_inside_repetition():
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
    """Tests of the `PrefixMatcher` object."""
    td = RegexTrieDict()
    mat = PrefixMatcher(td)
    # basic string insert
    td.insert("egg", ("data",))
    mat.add_key_elem("e")
    assert mat.get_meta() == []
    mat.add_key_elem("g")
    assert mat.get_meta() == []
    mat.add_key_elem("g")
    assert mat.get_meta() == [("data",)]

    # pattern string insert
    td.insert("x\\[\\d\\]", "data9")
    mat.reset() # needs to be done after insert; insert in td won't do it for mat
    mat.add_key_elem("x")
    assert mat.get_meta() == []
    mat.add_key_elem("9")
    assert mat.get_meta() == ["data9"]

    # pattern when pattern and ordinary string both match (also multiple patts)
    td = RegexTrieDict()
    mat = PrefixMatcher(td)
    td.insert("\\[\\w\\]8", "1 pattern data")
    assert td.has_key_meta("x8")
    assert not td.has_key_meta("xx")
    td.insert("x8", "string x8 data")
    assert td.has_key("x8")
    assert td.has_key_meta("x8")
    assert td.has_key_meta("y8")
    assert td.has_key_meta("x8") == 2
    assert td.has_key_meta("y8") == 1
    mat.add_key_elem("x") # turns on seqmeta mode
    assert mat.get_meta() == []
    mat.add_key_elem("8")
    assert sorted(mat.get_meta()) == sorted(["1 pattern data", "string x8 data"])
    td.insert("x\\[\\d\\]", "2 pattern data") # turns off seqmeta mode
    mat.reset() # needs to be done after insert; insert in td won't do it for mat
    mat.add_key_elem("x")
    mat.add_key_elem("8")
    assert sorted(mat.get_meta()) == sorted(["1 pattern data", "2 pattern data",
                                               "string x8 data"])
    td.insert("x\\*\\(\\[\\d\\]\\)", "3 pattern data") # turns off seqmeta mode
    mat.reset() # needs to be done after insert; insert in td won't do it for mat
    mat.add_key_elem("x")
    mat.add_key_elem("8")
    assert sorted(mat.get_meta()) == sorted(["1 pattern data", "2 pattern data",
                                               "3 pattern data", "string x8 data"])

    with raises(ModifiedTrieError):
        td = RegexTrieDict()
        mat = PrefixMatcher(td)
        td.insert("egg", ("data",))
        mat.add_key_elem("e") # this doesn't fail for now, maybe should...
        td.insert("bacon", ("data",))
        mat.add_key_elem("e")

#
# Test case of non-halt, worst-case.
#

def test_non_halt():
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


def test_random_regexes():
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

def test_some_error_conditions():
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
    nodeDataList = td.get_next_nodes_meta("f", td.get_root_node_data_list())
    with raises(ModifiedTrieError):
        del td["qqq"] # delete during search invalidates whole thing
        nodeDataList = td.get_next_nodes_meta("f", nodeDataList)
    # Test for invalidated tree search due to inserts during it.
    td.clear()
    td.insert("\\*\\(ff\\)xx")
    nodeDataList = td.get_next_nodes_meta("f", td.get_root_node_data_list())
    with raises(ModifiedTrieError):
        td["qqq"] = 5 # insert during search invalidates whole thing
        nodeDataList = td.get_next_nodes_meta("f", nodeDataList)
    # Simple reassignment of a value should not affect the search.
    td.clear()
    td.insert("\\*\\(ff\\)xx")
    nodeDataList = td.get_next_nodes_meta("f", td.get_root_node_data_list())
    td["\\*\\(ff\\)xx"] = 5 # value reassignment is OK
    nodeDataList = td.get_next_nodes_meta("f", nodeDataList)
    assert td["\\*\\(ff\\)xx"] == 5

def test_repetitions_with_bounds():
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

    #td.print_tree() # will show up on errors


#
# Test 'or' sections
#

def test_or_sections():
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

def test_TrieDict_with_non_string_sequences():
    """Remember that anything used as the key of a built-in Python dict must be
    hashable (basically, it must have a unique immutable representation).  That
    condition is weakened for a `RegexTrieDict`: Only the elements need to be
    hashable, not the entire sequence of elements.  Here we test the `TrieDict`
    with elements defined as strings inside tuples rather than as the strings
    themselves.  String-like sequences are then lists (or tuples) of those
    character-containing tuples.  For example, ("a",) is an element and
    [("a",), ("a",)] is a two-element sequence.  For simplicity in the testing
    example the tuples containing characters are treated like the characters
    themselves as far as comparisons."""

    rtd = RegexTrieDict()

    def listRangeTestFun(lowerLstElem, upperLstElem, lstElem):
        return char_range_test(lowerLstElem[0], upperLstElem[0], lstElem[0])

    def myPattMatchFun(queryElem, pattList, rangeElem, escapeElem):
        return generic_wildcard_match_fun(queryElem, pattList, rangeElem, escapeElem,
                                       rangeTestFun=listRangeTestFun)
    rtd.define_meta_elems(escape=("\\",), repetition=("*",), lGroup=("(",), rGroup=(")",),
                         lWildcard=("[",), rWildcard=("]",), rangeElem=("-",),
                         wildcard_patt_match_fun=myPattMatchFun, canonicalize_fun=None)

    # keys, note this fails without as_lists=True because we have not properly defined
    # the plus operator to "add" two elements into a sequence.
    rtd.insert([("A",), ("B",), ("C",)])
    rtd.insert([("D",), ("E",), ("F",)])
    #print(rtd.keys(as_lists=True))
    keys = rtd.keys(as_lists=True)
    assert (keys == [[("A",), ("B",), ("C",)], [("D",), ("E",), ("F",)]] or
            keys == [[("D",), ("E",), ("F",)], [("A",), ("B",), ("C",)]])

    # basic wildcards
    rtd.insert([("w",), ("a",), ("\\",), ("[",), ("z",), ("3",),
                ("\\",), ("]",), ("a",), ("a",)])
    assert rtd.has_key_meta([("w",), ("a",), ("z",), ("a",), ("a",)])
    assert rtd.has_key_meta([("w",), ("a",), ("3",), ("a",), ("a",)])
    assert not rtd.has_key_meta([("w",), ("a",), ("B",), ("a",), ("a",)])

    # repetitions
    # TODO

