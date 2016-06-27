# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
pytest_helper.sys_path(add_parent=True)

from lexer import *

# TOKEN DEFINITIONS #################################################################

def define_whitespace_tokens(lex_or_pp):
    lex_or_pp.def_token("space", r"[ \t]+", ignore=True) # note + NOT *
    lex_or_pp.def_token("newline", r"[\n\f\r\v]+", ignore=True) # note + NOT *
    #lex_or_pp.def_token("whitespace", r"\s+", ignore=True) # note + NOT *

def define_basic_tokens(lex_or_pp):
    define_whitespace_tokens(lex_or_pp)
    #lex_or_pp.define_begin_and_end_tokens("begin", "end")
    lex_or_pp.def_token("number", r"\d+")
    lex_or_pp.def_token("imag_number", r"\d+[i]")
    lex_or_pp.def_token("double_ast", r"(?:\*\*|\^)") # Note ^ is defined as synonym.
    lex_or_pp.def_token("plus", r"\+")
    lex_or_pp.def_token("minus", r"\-")
    lex_or_pp.def_token("fslash", r"/")
    lex_or_pp.def_token("ast", r"\*")
    lex_or_pp.def_token("lpar", r"\(")
    lex_or_pp.def_token("rpar", r"\)")
    lex_or_pp.def_token("comma", r",")
    lex_or_pp.def_token("bang", r"!")
    lex_or_pp.def_token("question", r"\?")
    lex_or_pp.def_token("colon", r"\:")
    lex_or_pp.def_token("semicolon", r";")

def define_identifier_token(lex_or_pp):
    # The last part of below only needs \w, but good example of pattern.
    #lex_or_pp.def_token("identifier", r"[a-zA-Z_](?:[\w|\d]*)", on_ties=-1)
    lex_or_pp.def_token("identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

def define_default_tokens(lex_or_pp):
    """Defines some default tokens for testing either a Lexer or a PrattParser."""
    define_basic_tokens(lex_or_pp)
    define_identifier_token(lex_or_pp)

def define_comment_to_EOL_token(lex_or_pp, begin_string):
    # Note that comment_to_endline is non-greedy due to *? symbol.
    lex_or_pp.def_token("comment_to_EOL", r"{0}.*?[\n]"
                           .format(begin_string), ignore=True)

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
        assert t[2].token_label == lex.end_token_label
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
        assert t.token_label == lex.begin_token_label # current token is begin token
        t = lex.next()
        assert t.is_first
        assert t.value == "1" 
        t = lex.go_back(4) # attempt to go back before beginning
        assert t.token_label == lex.begin_token_label
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
        assert lex.peek().token_label == lex.end_token_label # just before end
        assert lex.peek(2).token_label == lex.end_token_label # buffer fills with end tokens

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
        assert lex.next().token_label == lex.end_token_label
        lex.go_back()
        assert (charnumber, linenumber) == lex.token.line_and_char
        assert lex.non_ignored_token_count == tc_lex
        assert lex.token.non_ignored_token_count == tc_tok
        assert lex.next().token_label == lex.end_token_label
        lex.go_back(2)
        assert lex.next().value == "8"

        # Read two end tokens and see the StopIteration.
        assert lex.next().token_label == lex.end_token_label
        with raises(StopIteration):
            lex.next()

def setup_module(module):
    """setup module"""

def teardown_module(module):
    """teardown module"""

