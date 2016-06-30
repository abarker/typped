.. Typped documentation master file, created by
   sphinx-quickstart on Fri Jun 17 17:44:36 2016.
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

Contents:

.. toctree::
   :maxdepth: 3
   :numbered:

   pratt_parsing_intro
   calculator_example
   types_in_typped
   juxtaposition_operators
   extending_prattparser
   typped
   other_possible_generalizations


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

