# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import random, string, re
from typped.regex_trie_dict_lexer import *
from py.test import raises, fail

#
# Test basic TrieDictLexer operations.
#

def test_everything():
    print("test some basic TextStream stuff\n")
    ts = TextStream()
    # ts.set_raw_in()   # reads data from an interactive shell
    ts.set_string_in("eggbert\nhello")  # reads data from a string
    # ts.set_file_in("b_languageData.py")  # reads data from a file

    while not ts.end_of_text_stream():
        char = ts.next()
        sys.stdout.write(char)
        sys.stdout.flush()

    ts.set_string_in("eggbert2\nhello2")
    for char in ts:
        sys.stdout.write(char)
        sys.stdout.flush()

    print("\nTest the basic RegexTrieDictLexer\n")
    td = RegexTrieDict()
    td["x"] = ("variable", "x")
    td["egg"] = ("variable", "egg")
    td["eggsandwich"] = ("variable", "eggsandwich")
    td["\n"] = "whitespace"

    print("\nNew batch of tests...\n")

    ts = TextStream()
    testString = "fiveoneone  \n one  \ntwotwo"
    print("test string is:", "'"+testString+"'")
    ts.set_string_in(testString)  # reads data from a string

    # define the regexTrieDict
    td = RegexTrieDict()
    td["\n"] = ("whitespace", None)
    td[" "] = ("whitespace", None)
    td["\t"] = ("whitespace", None)
    td["one"] = ("var", 1); td["two"] = ("var", 2); td["three"] = ("var", 3)
    td["four"] = ("var", 4); td["five"] = ("var", 5)
    td["oneone"] = ("var2", 1); td["twotwo"] = ("var2", 2)
    td["threethree"] = ("var2", 3)
    td["fourfour"] = ("var2", 4); td["fivefive"] = ("var2", 5)
    td.print_tree()

    tdlex = BufferedRegexTrieDictLexer(ts, td, " \n") # ignore space and newline

    print("should print: five oneone oneone one twotwo five oneone one twotwo")
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    print(tdlex.peek())
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    if tdlex.beginning_of_line(): print("beginning of line")
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    tdlex.pushback()
    tdlex.pushback()
    tdlex.pushback()
    tdlex.pushback()
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    print(tdlex.next())
    if tdlex.end_of_line(): print("EOL")
    print(tdlex.get_all_to_delimiter(" \n\t"))
    tdlex.print_token_buf_strings()

    #assertFailToPrint = False
    #assert assertFailToPrint

