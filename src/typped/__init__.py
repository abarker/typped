
from __future__ import print_function, division, absolute_import

# TODO: Make these only include necessary imports or use __all__ in modules.

#if __name__ == "__main__": # This needs to be before the Cython import below.
#    import pytest_helper

try:
    import cython
except ImportError:
    from . import cython_shadow as cython

from .pratt_parser import *
from .pratt_types import *
from .lexer import *
from .shared_settings_and_exceptions import *
from .ebnf_classes_and_operators import *
from .helpers import *

# Pickling below doesn't work, at least not like this.
# https://stackoverflow.com/questions/24364408/pickling-of-dynamic-class-definition
# https://stackoverflow.com/questions/4647566/pickle-a-dynamically-parameterized-sub-class
#import os.path
#import dill
#
#def save_parser_instance_state(parser_instance, filename):
#    filename, file_extension = os.path.splitext(filename)
#    if file_extension != ".pkl":
#        filename += ".pkl"
#    with open(filename, 'wb') as f:
#        dill.dump(parser_instance, f)
#
#def parser_instance_from_saved_state(filename):
#    with open(filename, 'rb') as f:
#        return dill.load(f)

