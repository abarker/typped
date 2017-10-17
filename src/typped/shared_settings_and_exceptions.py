# -*- coding: utf-8 -*-
"""

Some settings and exceptions that are shared between several modules.

"""

from __future__ import print_function, division, absolute_import

# These should stay strings because the strings are also used in printed error messages.
HEAD = "head"
TAIL = "tail"

#
# Preconditions.
#

def DEFAULT_ALWAYS_TRUE_PRECOND_FUN(lex, extra_data):
    """The default precondition function; always returns true.  It is
    never actually called, just tested for with "is" and assumed true."""
    return True

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

class NoHandlerFunctionDefined(ParserException):
    """Only raised by dispatcher function, and only when it fails to find a
    handler function (head or tail, whichever it was looking for)."""
    pass

class ErrorInParsedLanguage(ParserException):
    """Raised for syntax errors and other errors in the language that is being
    parsed.  For type errors the exception `TypeErrorInParsedLanguage` is raised."""
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

