# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import
import pytest_helper

pytest_helper.script_run(self_test=True, pytest_args="-v")
pytest_helper.auto_import()
#pytest_helper.sys_path("../src")

import typped as pp

TypeSig = pp.TypeSig
Varargs = pp.Varargs
TypeObject = pp.TypeObject

def test_typesig_defs():
    parser = pp.PrattParser()
    t_int = parser.def_type("t_int")

    assert str(TypeSig()) == "TypeSig(None, None)"
    assert str(TypeSig(None)) == "TypeSig(None, None)"
    assert str(TypeSig(None, None)) == "TypeSig(None, None)"

    sig = TypeSig(t_int, t_int)
    assert str(sig.arg_types) == "TypeObject('t_int')"
    assert sig.arg_types == t_int
    assert str(sig) == "TypeSig('t_int', 't_int')"

    sig = TypeSig(t_int, [t_int, Varargs(t_int, t_int)])
    assert str(sig) == "TypeSig('t_int', ['t_int', Varargs('t_int', 't_int')])"

def test_type_objects():
    assert str(TypeObject(None)) == "TypeObject(None)"
    assert str(TypeObject("t_float")) == "TypeObject('t_float')"

