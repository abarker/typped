
"""

This module is just a common place to make some definitions which are 
used by several other modules.

Note that the string in an initial namedtuple definition is essentially just
for error reporting (it defines the name of the tuple subclass returned by the
factory function, but that is usually hidden from the user).  It is convenient
to use the same name as the variable on the rhs which is being assigned.

"""

from __future__ import print_function, division, absolute_import
from enum_wrapper import Enum
import collections

#
# Some very basic convenience definitions.
#

digits = ("0","1","2","3","4","5","6","7","8","9")

class Literals(Enum):
    lpar = "("
    rpar = ")"
    lbrac = "["
    rbrac = "]"
    lcurl = "{"
    rcurl = "}"
    comma = ","
    space = " "
    newline = "\n"
    tab = "\t"

#
# The kinds of tokens.
#

class TokenKinds(Enum):
    literal = 0
    typeName = 1
    typeAlias = 2
    quantName = 3
    constName = 4
    varName = 5
    stdFunName = 6
    preFunName = 7
    infFunName = 8
    substFunName = 9
    uncheckedString = 10
    unsignedInt = 11

#
# The language elements.
#

class LangElems(Enum):
    typeElem = 1
    typeAlias = 2
    quant = 3
    const = 4
    var = 5
    stdFun = 6
    prefixFun = 7
    infixFun = 8
    substFun = 9

