# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path("../src")

from typped.lexer import * # Test as an individual module.

# TOKEN DEFINITIONS #################################################################

def define_whitespace_tokens(lex):
    lex.def_token("space", r"[ \t]+", ignore=True) # note + NOT *
    lex.def_token("newline", r"[\n\f\r\v]+", ignore=True) # note + NOT *
    #lex.def_token("whitespace", r"\s+", ignore=True) # note + NOT *

def define_basic_tokens(lex):
    define_whitespace_tokens(lex)
    lex.def_begin_end_tokens("begin", "end")
    lex.def_token("number", r"\d+")
    lex.def_token("imag_number", r"\d+[i]")
    lex.def_token("double_ast", r"(?:\*\*|\^)") # Note ^ is defined as synonym.
    lex.def_token("plus", r"\+")
    lex.def_token("minus", r"\-")
    lex.def_token("fslash", r"/")
    lex.def_token("ast", r"\*")
    lex.def_token("lpar", r"\(")
    lex.def_token("rpar", r"\)")
    lex.def_token("comma", r",")
    lex.def_token("bang", r"!")
    lex.def_token("question", r"\?")
    lex.def_token("colon", r"\:")
    lex.def_token("semicolon", r";")

def define_identifier_token(lex):
    # The last part of below only needs \w, but good example of pattern.
    #lex.def_token("identifier", r"[a-zA-Z_](?:[\w|\d]*)", on_ties=-1)
    lex.def_token("identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

def define_default_tokens(lex):
    """Defines some default tokens for testing either a Lexer or a PrattParser."""
    define_basic_tokens(lex)
    define_identifier_token(lex)

class TestLexer:
    def test_basic_stuff(self):
        lex = Lexer()
        define_default_tokens(lex)

        lex.set_text("x  + y")
        t = lex.next() # x
        assert t.is_first == True
        assert lex.curr_token_is_first() == True
        assert t.token_label == "identifier"
        assert t.value == "x"

        t = lex.next() # +
        assert t.is_first == False
        assert lex.curr_token_is_first() == False
        assert t.token_label == "plus"
        assert t.original_text() == "  +"
        assert t.original_text() != " +"

        t = lex.next() # y
        assert t.is_first == False
        assert t.token_label == "identifier"
        assert len(t.ignored_before()) == 1

        t = lex.next() # end

    def test_multi_value_next_call(self):
        lex = Lexer()
        define_default_tokens(lex)

        lex.set_text("1 2 3 4 5 6 7 8")
        t = lex.next()
        assert t.value == "1"
        t = lex.next(5)
        assert t[0].value == "2"
        assert t[1].value == "3"
        assert t[4].value == "6"
        assert len(t) == 5
        t = lex.next(5)
        assert t[0].value == "7"
        assert t[1].value == "8"
        assert t[2].is_end_token()
        assert len(t) == 3

    def test_go_back(self):
        # TODO test line_and_char settings, too, for tokens
        lex = Lexer()
        define_default_tokens(lex)

        lex.set_text("1 2 3 4 5 6 7 8")
        assert lex.peek().value == "1"
        t = lex.next()
        assert t.value == "1"
        assert t.line_and_char[0] == 1
        assert t.line_and_char[1] == 1
        assert lex.peek().value == "2"
        assert lex.next().value == "2"
        t = lex.next()
        assert t.value == "3"
        assert t.line_and_char[1] == 5
        lex.go_back(0)
        assert lex.token.value == "3"
        assert lex.token.line_and_char[1] == 5
        lex.go_back(-1)
        assert lex.token.value == "3"
        lex.go_back(1)
        t = lex.next()
        assert t.value == "3"
        assert lex.token.value == "3"
        assert not t.is_first
        charnumber, linenumber = t.line_and_char
        tc_lex = lex.non_ignored_token_count
        tc_tok = lex.token.non_ignored_token_count
        lex.go_back(2)
        assert lex.token.is_first
        t = lex.next(2)
        assert (charnumber, linenumber) == t[-1].line_and_char
        assert lex.non_ignored_token_count == tc_lex
        assert t[-1].non_ignored_token_count == tc_tok
        lex.go_back(2)
        assert lex.next().value == "2"
        lex.go_back(2)
        assert lex.peek().value == "1"
        assert lex.next().value == "1"
        lex.go_back(2)
        t = lex.token
        assert t.is_begin_token()
        t = lex.next()
        assert t.is_first
        assert t.value == "1" 
        t = lex.go_back(4) # attempt to go back before beginning
        assert t.is_begin_token()
        tt = lex.next(4)
        assert tt[0].value == "1"
        assert tt[3].value == "4"
        lex.go_back()
        lex.next()
        assert lex.token.value == "4"

        # test going back to a state from saved lex.num_non_ignored_token_count
        non_ig_before = lex.non_ignored_token_count
        lc = lex.token.line_and_char 
        lex.next(2)
        non_ig_after = lex.non_ignored_token_count
        diff = non_ig_after - non_ig_before
        assert diff == 2
        lex.go_back(diff)
        assert lex.token.value == "4"
        assert lc == lex.token.line_and_char 

        # test near-end conditions
        tt = lex.next(3)
        assert len(tt) == 3
        assert tt[0].value == "5"
        assert tt[-1].value == "7"
        t = lex.next() # Read the last non-end token.
        assert lex.token.value == "8"
        assert lex.peek().is_end_token() # just before end
        assert lex.peek(2).is_end_token() # buffer fills with end tokens

        # Now we are at the end of the text, but haven't read end token.
        print("\nbefore go back")
        t = lex.go_back()
        assert lex.token.token_label == "number"
        assert lex.token.value == "7"
        assert lex.next().value == "8"

        # Now read a single end token and then go back.
        charnumber, linenumber = lex.token.line_and_char
        tc_lex = lex.non_ignored_token_count
        tc_tok = lex.token.non_ignored_token_count
        assert lex.next().is_end_token()
        lex.go_back()
        assert (charnumber, linenumber) == lex.token.line_and_char
        assert lex.non_ignored_token_count == tc_lex
        assert lex.token.non_ignored_token_count == tc_tok
        assert lex.next().is_end_token()
        lex.go_back(2)
        assert lex.next().value == "8"

        # Read two end tokens and see the StopIteration.
        assert lex.next().is_end_token()
        with raises(StopIteration):
            lex.next()

def test_documentation_example():

    lex = Lexer()

    lex.def_begin_end_tokens("begin", "end")
    lex.def_token("space", r"[ \t]+", ignore=True) # note + NOT *
    lex.def_token("newline", r"[\n\f\r\v]+", ignore=True) # note + NOT *
    tokens = [
        ("k_identifier", r"[a-zA-Z_](?:\w*)"),
        ("k_plus", r"\+"),
        ]
    lex.def_multi_tokens(tokens)
    
    lex.set_text("x  + y")

    for t in lex:
        print(t)

    # example above, tests below

    lex.set_text("x  + y")
    lst = [str(t) for t in lex]
    assert lst[0] == "<k_identifier,'x'>"
    assert lst[1] == "<k_plus,'+'>"
    assert lst[2] == "<k_identifier,'y'>"
    assert lst[3] == "<end,None>"
 
    lex.set_text("x  + y")
    t = next(lex)
    assert str(t) == "<k_identifier,'x'>"

