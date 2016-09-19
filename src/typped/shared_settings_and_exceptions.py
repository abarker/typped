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

#
# Utility functions.
#

def is_subclass_of(class_name, subclass_name):
    """This is just a call to `issubclass` except that it first checks that
    the first argument is a class, returning false if it is not."""
    return isinstance(class_name, type) and issubclass(class_name, subclass_name)

def return_first_exception(*args):
    """Go down the argument list and return the first object that is a
    subclass of the `Exception` class.  Arguments do not need to all be
    classes.  Returns `None` if all fail."""
    for item in args:
        if is_subclass_of(item, Exception):
            return item
    return None

