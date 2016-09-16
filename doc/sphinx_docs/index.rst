.. Typped documentation master file, created by
   sphinx-quickstart on Mon Aug  8 19:14:55 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

==================================================
Typped: A typed Pratt parser employing dispatching
==================================================

.. default-role:: code

This package provides a general purpose Python framework for creating Pratt
parsers to parse possibly typed target languages.  It provides default parser
functions for many standard constructs (such as infix operators and ordinary
functions) and can be extended to allow more complicated or specific constructs
to be parsed.

Installation
============

The easiest way to install Typped is to use pip:

.. code-block:: bash

   pip install typped

Introduction
============

The Typped package provides a framework for defining and using Pratt parsers,
with optional type-checking.  Because it is based on Pratt parsing it is
especially good at expression languages with various types of operators.   The
package was designed for logic languages and mathematical languages, but the
framework is general-purpose.  Languages can be fully dynamic, defined and
modified on-the-fly.  Since the package is pure Python it is not the best
choice if speed is the most important factor.

.. toctree::
   :maxdepth: 3
   :numbered:

   pratt_parsing_intro
   dispatching.rst
   lookbehind.rst
   calculator_example
   types_in_typped
   juxtaposition_operators
   extending_prattparser
   multiple_parsers
   typped
   other_possible_generalizations

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

