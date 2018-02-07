# -*- coding: utf-8 -*-
"""

This code is for a simple example language posted to /r/python:
    https://www.reddit.com/r/Python/comments/7vps0s/how_do_i_create_a_parser_pyparsing_for_this/

TODO: Finish.
TODO: Simple version here, maybe do more, and include evaluation functions.

"""

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    pytest_helper.script_run(self_test=True, pytest_args="-vv")

pytest_helper.autoimport()
#pytest_helper.sys_path("../src")

from typped import * # Assumes typped is installed locally.

def test_simple_grammar():
    """Test the actual parser creation and execution from a grammar."""
    parser = PrattParser()
    parser.def_default_whitespace()

    #
    # Define the grammar.
    #

    # NOTE that we have to use a common identifier token because a string like
    # "get get filter" is not ruled out.
    #
    # Problem has been noted in the docs, dispatching section.  Need a way to
    # access the "value_key" part of def_construct from the grammar.

    tok = parser.def_token
    k_tablename = tok("k_tokentable", r"[A-Za-z]*")
    k_filter = tok("k_filter", r"filter")
    k_value = tok("k_value", r"value")
    k_get = tok("k_get", r"get")
    k_insert = tok("k_insert", r"insert")

    g = Grammar()
    QUERY = Rule("CMD") + k_tokentable + Rule("FILT_OR_VAL")
    FILT_OR_VAL = k_filter | k_value
    CMD = k_get | k_insert

    g.compile("QUERY", parser, locals())

    simple_example1_parse = parser.parse(
                                "get todos filter", pstate="QUERY").tree_repr()

    print(simple_example1_parse)
    assert simple_example1_parse == unindent(8, """
        <k_null-string,'expression'>
            <k_null-string,'term'>
                <k_null-string,'factor'>
                    <k_number,'4'>
                <k_ast,'*'>
                <k_null-string,'term'>
                    <k_null-string,'factor'>
                        <k_number,'4'>

        """)




