.. Typped documentation master file, created by
   sphinx-quickstart on Mon Aug  8 19:14:55 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

==================================================
Typped: A typed Pratt parser employing dispatching
==================================================

.. default-role:: code

The Typped package provides a general Python framework for defining and using
Pratt parsers.  The parsed language can optionally include basic types and
overloading on those types.  Default parser methods are provided for many
standard constructs (such as infix operators and ordinary function calls).
Users can define their own custom functions or methods to allow for parsing
more complicated or more specific grammatical constructs.  An EBNF-like Python
syntax is also provided, which can be used to define and implement recursive
descent parsing of a grammar within the Typped framework and (integrated with
Pratt parsing).

Although the framework is general, the package was originally designed to parse
logic languages and mathematical languages.  Because it is based on Pratt
parsing it is especially good at parsing expression languages or sublanguages
which have various operators at different precedences.  The parser can be
modified on-the-fly, so the parsed languages can be fully dynamic.  Since the
package is written in pure Python it is probably not the best choice if speed
is a crucial factor.

.. warning::

   This is alpha-level software.  Many features are only partially implemented,
   and the documentation is incomplete.  The user API can still change, though
   probably in minor ways.

Installation
============

The current way to install is to clone the GitHub repo and install with pip:

.. code-block:: bash

   git clone https://github.com/abarker/typped
   cd typped
   pip install .

As usual, use a virtual environment or at least run pip with the ``--user``
option unless you really want to modify the system Python installation (in
which case you may also need to use ``sudo`` on a Linux system).

At a more stable development point Typped will be uploaded to PyPI so users can
install Typped directly:

.. code-block:: bash

   pip install typped

Content
=======

.. toctree::
   :maxdepth: 3
   :numbered:

   pratt_parsing_intro
   dispatching.rst
   recursive_descent.rst
   calculator_example
   types_in_typped
   juxtaposition_operators
   extending_prattparser
   multiple_parsers
   templates
   lookbehind.rst
   typped
   other_possible_generalizations

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

