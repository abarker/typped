"""

On Python 3 this local enum module just acts like the system enum package.  On
Python 2 systems the module imports the locally-packaged backport of Python 3.4
enumerations.  It is just used to hide the messy details.

"""
from __future__ import print_function, division, absolute_import
import sys
import os
import external_program_calls as ex

if ex.pythonVersion[0] == "2":
    localEnumPath = os.path.join(ex.projectRootDirectory,
                                 "enum34", "enum34-1.0.4", "enum")
    sys.path.insert(0, localEnumPath)

from enum import *

