#!/usr/bin/env python
# -*- encoding: utf-8 -*-
"""

To install as normal Python with pip, go to the home project directory and type::
    pip install .
or to install in developer mode use::
    pip install -e .

To install with pip and the `--use-cython` option go to the home directory and type::
    pip install --install-option="--use-cython" .

To install with Cython in developer mode use::
    pip install --install-option="--use-cython" -e .

To force all the Cython .c files to be regenerated you can use the install
option `--force-regen`.

A setuptools-based setup module.

See:
   https://packaging.python.org/en/latest/distributing.html
   https://github.com/pypa/sampleproject

Docs on the setup function kwargs:
   https://packaging.python.org/distributing/#setup-args

"""

from __future__ import absolute_import, print_function

import glob
import os
import sys
from setuptools import setup, find_packages
import codecs # Use a consistent encoding.

USE_CYTHON = False # Use Cython or at least the .c files already generated by Cython.
REGENERATE_C_CODE = True # Either regenerate C code with Cython or use existing code.
FORCE_RECOMPILATION = False # Force all files to be compiled.
FORCE_REGENERATION = False # Force all .c files to be regenerated (by deleting them).

EXTRA_COMPILE_ARGS = [] # Extra compile arguments used for all compilations.
COMPILER_IS_GCC = True # Extra GCC compile args are used if true and compiling.
EXTRA_GCC_COMPILE_ARGS = ["-O1"] # ["-O3"] # Optimize code more than the default.

PACKAGE_NAME = "typped"
PACKAGE_CODE_DIR = os.path.join("src", PACKAGE_NAME)
#BUILD_DIR = None # Where to put .c files; with None they are put with the .py files.
BUILD_DIR_NAME = "cython_c_code" # Name of dir inside main package.
BUILD_DIR = os.path.join(PACKAGE_CODE_DIR, BUILD_DIR_NAME) # Cython .c files in subpackage.
INCLUDE_DIRS = [] # Include directories.

# https://cython.readthedocs.io/en/latest/src/reference/compilation.html#compiler-directives
GLOBAL_CYTHON_DIRECTIVES = { # Set as compiler_directives kwarg to cythonize.
        "infer_types": True,
        #"embedsignature": True, # For Sphinx.
        #"annotation_typing": False, # Whether to take type info from PEP484 annotations.
        "optimize.use_switch": True,
        "optimize.unpack_method_calls": True,
        }

#
# Handle optional Cython.
#

OLD_CWD = os.getcwd()
PROJECT_DIR = os.path.abspath(os.path.dirname(__file__)) # Dir of this setup.py file.
os.chdir(PROJECT_DIR) # Make paths relative setup.py dir.

cmdclass = {} # Not currently used.

# Set up the .c files as package data (needs paths relative to PACKAGE_CODE_DIR).
os.chdir(PACKAGE_CODE_DIR) # Temporarily make paths relative to code dir.
if BUILD_DIR is None:
    CYTHON_C_FILES = glob.glob("*.c")
else:
    CYTHON_C_FILES = glob.glob(os.path.join(BUILD_DIR_NAME, PACKAGE_CODE_DIR, "*.c"))
os.chdir(PROJECT_DIR) # Return CWD to the project root dir.

def delete_c_files():
    """Delete all .c files, to force them to be regenerated."""
    for f in CYTHON_C_FILES:
        os.remove(os.path.join("src", PACKAGE_NAME, f))

def extra_option(option_string):
    """Check for an extra option string `option_string`."""
    try:
        sys.argv.remove(option_string)
        return True
    except ValueError:
        return False

if extra_option("--use-cython"):
    REGENERATE_C_CODE = True
    USE_CYTHON = True

if extra_option("--force-recompile"):
    FORCE_RECOMPILATION = True

if extra_option("--force-regen"):
    delete_c_files()

def no_cython():
    """Code for when not using Cython.  Just deletes any .so files so they
    won't be used.  Returns `None` as the `ext_modules`."""
    try:
        for p in glob.glob(os.path.join(PACKAGE_CODE_DIR, "*.so")):
            os.remove(p)
    except OSError: # File isn't there, permissions don't allow, is a directory, etc.
        pass
    return None # Explicit return.

def use_cython():
    """This code handles all the Cython code generation and building.  It returns the
    extension module to set as `ext_module` in the call to `setup`.

    Reads the module constants `REGENERATE_C_CODE`, `EXTRA_COMPILE_ARGS`,
    `INCLUDE_DIRS`, `BUILD_DIR`, `GLOBAL_CYTHON_DIRECTIVES`, and
    `EXTRA_GCC_COMPILE_ARGS`, `COMPILER_IS_GCC`."""

    PYX = ".pyx" # Standard Cython modules.
    PY = ".py"   # Used for pure Python mode Cython.

    extra_compile_args = EXTRA_COMPILE_ARGS
    if COMPILER_IS_GCC:
        extra_compile_args += EXTRA_GCC_COMPILE_ARGS

    try:
        import Cython
        from Cython.Compiler import Options
        # Options like to cython command-line command.  See source for options.
        # https://github.com/cython/cython/blob/master/Cython/Compiler/Options.py
        #Options.docstrings = False
        #Options.annotate = True # Only works when regenerating .c files.
    except ImportError:
        print("Cython is not importable; compiling existing .c files.", file=sys.stderr)
        cython_is_installed = False
    else:
        cython_is_installed = True

    if REGENERATE_C_CODE and cython_is_installed:
        try:
            from Cython.Build import cythonize
            #from Cython.Distutils import build_ext # Assume OK for setuptools, too.
        except:
            raise ImportError("Cannot import Cython.  It is a dependency for the"
                              " selected option to regenerate Cython C code.")
        #cmdclass["build_ext"] = build_ext # Apparently not needed with setuptools...
    else:
        PYX = PY = ".c" # Compile existing C code.

    """
    # Experimental compile to standalone...
    STANDALONE = True
    if STANDALONE and cython_is_installed:
        global INCLUDE_DIRS
        INCLUDE_DIRS += ["/usr/include/python3.5m"]
        extra_compile_args += ["-I", "/usr/include/python3.3m",
                               "-lpython3.5m", "-lpthread", "-lm",  "-lutil", "-ldl"]
        Options.embed = "embedded_main" # Name of the main() function to generate.
    """

    from setuptools.extension import Extension
    extensions = [
          Extension("*",
                    # Pure python mode: All .py files, skipping cython_shadow.
                    [os.path.join(PACKAGE_CODE_DIR, "[_abd-z]*" + PY)],
                    extra_compile_args=extra_compile_args,
                    include_dirs=INCLUDE_DIRS,
                    ),
          #Extension("typped.matcher",
          #         ["./src/typped/matcher" + PY],
          #         extra_compile_args=extra_compile_args,
          #         #include_dirs=[numpy.get_include()]
          #         ),
          #Extension("typped.lexer",
          #         ["./src/typped/lexer" + PY],
          #         extra_compile_args=extra_compile_args,
          #         ),
          ##Extension("typped.token_buffer",  # Now the one in Lexer is used instead.
          ##         ["./src/typped/token_buffer" + PYX],
          ##         ),
          ]

    for ext in extensions:
        # Mostly useless if all global; need separate extensions to be useful.
        ext.cython_directives = {} # Local Cython directives, per extension.
        #ext.cython_directives["embedsignature"] = True
        #ext.cython_directives["infer_types"] = True

    if REGENERATE_C_CODE and cython_is_installed:
        # Note that calling cythonize with globbed strings, like:
        #    ext_modules = cythonize(["./src/typped/*.py", "./src/typped/matcher.pyx",])
        # does not work for pure Python mode.  Extensions must be explicitly defined.
        ext_modules = cythonize(extensions,
                                compiler_directives=GLOBAL_CYTHON_DIRECTIVES,
                                annotate=False, # Only works when .c files regenerated.
                                include_path=[],
                                timestamps=True,
                                force=FORCE_RECOMPILATION,
                                verbose=False,
                                cplus=False, # Compile as c++ code.
                                build_dir=BUILD_DIR,)
    else:
        ext_modules = extensions # This will compile the existing .c files.
    return ext_modules

if USE_CYTHON:
    ext_modules = use_cython()
else:
    ext_modules = no_cython()

#
# Regular setup.py below.
#

# Get the long description from the README.rst file.
with codecs.open(os.path.join(PROJECT_DIR, "README.rst"), encoding="utf-8") as f:
    long_description = f.read()

setup(
    name="typped",
    version="0.1.0", # Versions should comply with PEP440.
    #description="A typped Pratt parser which employs dispatching of handler functions.",
    description="A framework for generalized Pratt parsing which can optionally check simple types.",
    url="https://github.com/abarker/typped",
    keywords=["parser", "Pratt", "grammar", "language", "syntax", "syntax tree",
              "BNF", "EBNF", "typped"],
    install_requires=["pytest-helper", "pytest>=2.0"],

    license="MIT",
    classifiers=[
        # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
        # Development Status: Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        "Development Status :: 3 - Alpha",
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: Unix',
        'Operating System :: POSIX',
        'Operating System :: Microsoft :: Windows',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy',
        # uncomment if you test on these interpreters:
        # 'Programming Language :: Python :: Implementation :: IronPython',
        # 'Programming Language :: Python :: Implementation :: Jython',
        # 'Programming Language :: Python :: Implementation :: Stackless',
        'Topic :: Utilities',
    ],

    # Settings usually the same.
    author="Allen Barker",
    author_email="Allen.L.Barker@gmail.com",
    zip_safe=False,

    #include_package_data=True, # Files must be listed in MANIFEST.in file, don't set.
    package_data={PACKAGE_NAME: CYTHON_C_FILES},

    ext_modules=ext_modules,
    cmdclass=cmdclass,

    # Automated stuff below.
    long_description=long_description,
    packages=find_packages('src'),
    package_dir={'': 'src'},
    py_modules=[os.path.splitext(os.path.basename(path))[0]
                                    for path in glob.glob('src/*.py')],
)

os.chdir(OLD_CWD) # Return to previous CWD.

