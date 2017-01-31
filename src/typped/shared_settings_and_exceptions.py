# -*- coding: utf-8 -*-
"""

Some settings and exceptions that are shared between several modules.

"""

from __future__ import print_function, division, absolute_import

#
# Exceptions.
#

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

class CalledBeginTokenHandler(ParserException):
    """Called a handler for the begin token."""
    pass

class CalledEndTokenHandler(ParserException):
    """Called a handler for the end token."""
    pass

#
# Utility functions.
#

def is_class(obj):
    """Test if object `obj` is a class."""
    return isinstance(obj, type)

def is_subclass_of(subclass_name, class_name):
    """This is just a call to `issubclass` except that it first checks that
    the first argument is a class, returning false if it is not."""
    return is_class(subclass_name) and issubclass(subclass_name, class_name)

