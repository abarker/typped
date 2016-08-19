# -*- coding: utf-8 -*-
"""

Some settings and exceptions that are shared between several modules.

"""

from __future__ import print_function, division, absolute_import

class TyppedBaseException(Exception):
    """The base exception for all package-defined exceptions.  All
    potentially-recoverable exceptions should be subclasses of this
    class (i.e., not a builtin Python exception)."""
    pass

class ParserException(TyppedBaseException):
    """Base exception for exceptions in the parser modules."""
    pass

class LexerException(TyppedBaseException):
    """Base exception for exceptions in the lexer modules."""
    pass

