.. Typped documentation master file, created by
   sphinx-quickstart on Mon Aug  8 19:14:55 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

=================================================
Typped: Typed Pratt parsers employing dispatching
=================================================

..
   NOTE: Use |nbsp| for nonbreaking space, and |br| for adding line breaks.
   Both are defined in conf.py, which also sets the default role to code,
   globally for all pages.
   http://www.sphinx-doc.org/en/stable/config.html#confval-rst_prolog

..
   todoLaterBelow  doesn't work to highlight inline, see drst file...
   .. role:: python(code)
       :language: py
       :class: highlight
   .. role:: py(code)
      :language: py
      :class: highlight
   this can :python:`trigger("python", "syntax highlight")`
   .. .. role:: bash(code)
      :language: bash
   which you can then use like so:
   Here is some awesome code :python:`a = b + c`.
   Here is some awesome code :py:`a = b + c`.

The Typped package provides a general Python framework for defining and using
Pratt parsers.  The parsed languages can optionally be typed, with basic types
which are checked at parse-time.  Overloading on types is also allowed.

Default parser methods are provided for many standard constructs such as infix
operators and ordinary function calls.  Users can define their own customized
functions or methods in order to parse more-complicated and/or more-specific
grammatical constructs.  An EBNF-like Python syntax is also provided, which can
be used to define and implement recursive descent parsing of a grammar within
the Typped framework (integrated with Pratt parsing).

Although the framework is general, the package was originally designed to parse
logic languages and mathematical languages which use syntax approximating the
syntax which is used in practice.  Because it is based on Pratt parsing it is
especially good at parsing expression languages which have various operators at
different precedence levels.  Pratt parsing tends to be fairly efficient in
this context, but the Typped package focuses more on ease-of-use than execution
speed.  The parsers and lexers are modifiable on-the-fly, allowing the parsed
languages to be fully dynamic.

.. note::

   This is alpha-level software.  The basic parts work but many features are
   experimental and/or partially implemented.  The documentation is incomplete.
   The user API can still change --- probably in minor ways, but not
   necessarily.

Installation
============

The current way to install is to clone `the GitHub repo
<https://github.com/abarker/typped>`_ and install with pip:

.. code-block:: bash

   git clone https://github.com/abarker/typped
   cd typped
   pip install .

As usual, use a virtual environment or at least run pip with the ``--user``
option.  (If you really want to modify the system Python installation the
command needs to be run with administrative privileges.)  Use the ``-e`` option
to pip if you want to experiment with the code.  See the comments in the
``setup.py`` file for how to compile with Cython.

At a more stable development point Typped will be uploaded to PyPI so users can
install Typped directly:

.. code-block:: bash

   pip install typped

Content
=======

.. toctree::
   :maxdepth: 3
   :numbered:

   basic_usage
   pratt_parsing_intro
   dispatching
   calculator_example
   types_in_typped
   juxtaposition_operators
   recursive_descent
   parsers_calling_other_parsers
   lookbehind
   typped
   other_possible_generalizations

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

