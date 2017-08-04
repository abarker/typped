#!/usr/bin/env python
# -*- encoding: utf-8 -*-
"""
A setuptools-based setup module.

See:
   https://packaging.python.org/en/latest/distributing.html
   https://github.com/pypa/sampleproject

Docs on the setup function kwargs:
   https://packaging.python.org/distributing/#setup-args

"""

from __future__ import absolute_import, print_function

import glob
import os.path
from setuptools import setup, find_packages
import codecs # Use a consistent encoding.

#
# Handle optional Cython.
#

USE_CYTHON = False
cmdclass = {}

if USE_CYTHON:
    from setuptools.extension import Extension
    from Cython.Build import cythonize
    from Cython.Distutils import build_ext
    extensions = [
          Extension("typped.matcher",
                   ["./src/typped/matcher.py"],
                   #include_dirs=[numpy.get_include()]
                   ),
          Extension("typped.token_buffer",
                   ["./src/typped/token_buffer.pyx"],
                   ),
          Extension("typped.lexer",
                   ["./src/typped/lexer.py"],
                   ),
    ]

    # Note commented-out way doesn't seem to work for pure Python mode.
    #ext_modules = cythonize(["./src/typped/*.pyx", "./src/typped/matcher.py",])
    cmdclass.update({'build_ext': build_ext})
    ext_modules = cythonize(extensions)
else:
    ext_modules = None

#
# Regular setup.py.
#

# Get the long description from the README.rst file.
current_dir = os.path.abspath(os.path.dirname(__file__))
with codecs.open(os.path.join(current_dir, "README.rst"), encoding="utf-8") as f:
    long_description = f.read()

setup(
    name="typped",
    version="0.1.0", # Versions should comply with PEP440.
    #description="A typped Pratt parser which employs dispatching of handler functions.",
    description="A parser.", # TODO switch
    #url="https://github.com/pypa/sampleproject",
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
    include_package_data=True,
    zip_safe=False,

    ext_modules=ext_modules,
    cmdclass=cmdclass,

    # Automated stuff below.
    long_description=long_description,
    packages=find_packages('src'),
    package_dir={'': 'src'},
    py_modules=[os.path.splitext(os.path.basename(path))[0]
                                    for path in glob.glob('src/*.py')],
)

