# -*- coding: utf-8 -*-
"""

This module implements a nice frontend for parsing grammars using `EBNF
<https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form>`_-like Python
expressions.  The backend is the recursive-descent parsing capabilities of the
`PrattParser` class, implemented using precondition functions and null-string
token handlers.

.. topic:: Similar Python parsing packages.

    These are a few (but not all) similar Python parsing packages for parsing
    from BNF- or EBNF-like grammars (though without the underlying Pratt
    parsing framework).  Some use string representations of the grammar, while
    others use overloaded Python operators.  Most do not automatically produce
    a parse tree.

    * **pyparsing** -- Uses Python overloading to define the grammar, similar
      to this module.  http://pyparsing.wikispaces.com/home

    * **Parsimonius** -- Passed a string containing the EBNF of the grammar and
      returns a parse tree.  https://github.com/erikrose/parsimonious

    * **Parsley** -- Passed a string containing the EBNF of the grammar.
      https://github.com/python-parsley/parsley/

    * **yeanpypa** -- Uses Python overloading, similar to this module.
      https://github.com/DerNamenlose/yeanpypa

    * **Lark** -- Passed a string. Implements Earley & LALR(1) and returns a
      parse tree.  https://github.com/erezsh/Lark

    For more information on the various Python parsers, see these summaries:

    * https://wiki.python.org/moin/LanguageParsing,

    * https://github.com/webmaven/python-parsing-tools

    * https://tomassetti.me/parsing-in-python/.

Terminology and notation
========================

These terms are used in the description:

* The individual rewrite rules such as `<expression> ::= <term>` in a BNF
  grammar are called **production rules**.  They are also called
  **productions** or just **rules**.  The symbols on the l.h.s. of productions
  (which can also appear in the r.h.s.) are called **nonterminal symbols**.
  The r.h.s of a production is called the **parsing expression**.  The r.h.s.
  of productions can contain terminal symbols, **nonterminal symbols** and
  perhaps other symbols such as the special **epsilon symbol** which matches an
  empty string. |br|

* Production rules with the the same l.h.s. nonterminal symbol will be referred to
  as different **cases** of the nonterminal symbol.  A common notation is
  to define multiple cases in one production rule expression by using the "or"
  symbol `|`.  This form of definition is currently *required* by this
  module.  That is, all the cases for any nonterminal must occur
  in a single expression, using `|` if there are multiple cases.  The ordered list
  of all the rule cases for a nonterminal will be called the **caselist** for
  the nonterminal.  Order matters for resolving ambiguity. |br|

* The separate symbol elements within a case will be collectively called the
  **items** of that case.  They include terminal symbols, nonterminal symbols,
  and possibly the epsilon symbol.  In this module there are no explicit
  terminal symbols.  Instead, terminals are either tokens (defined for the
  lexer and returned by it) or else consecutive sequences of tokens.

This is an example of a definition of a caselist with two cases in the Typped
EBNF-like code:

.. code-block:: python

   arglist = ( Rule("arg") + Tok("k_comma") + Rule("arglist")
             | Rule("arg")
             )

The tokens involved must already be defined (and the token itself can be used
in the rule instead of the label inside a call to ``Tok``).  The caselist for
``arg`` is not shown, but it could be defined before or after the ``arglist``
caselist.  The order in which the caselists of production rules are written
does not matter, so they can be written top-down, beginning with the
start-state nonterminal, or in any other convenient way.

Nonterminals are written as strings passed to ``Rule`` precisely so they can be
used in the r.h.s. of caselist definitions even when they have not yet been
defined.  They are resolved later when the `compile` method of the grammar is
called (passed the start nonterminal and the locals dict).  These r.h.s.
strings for nonterminals **must be identical** to the l.h.s. Python variable
names for the nonterminals (since they are looked up in the locals dict).

The order in which the cases of a nonterminal are defined within a caselist
*does* matter, at least for ambiguous grammars and to avoid or minimize
backtracking.  The order of the cases is the order in which the algorithm will
test the cases.  The first successful parse is returned.  In this sense the
grammar is similar to a **parsing expression grammar (PEG)** rather than a
**context-free grammar (CFG)**, which can be ambiguous.  See, e.g.,
https://en.wikipedia.org/wiki/Parsing_expression_grammar

Implementation
==============

This module is a work-in-progress.  As of now the syntactic Python interface
mostly all works, but not all of the features have been coded into the backend
"compiling" algorithm.  Currently it does basic BNF types production rules, but
not many EBNF extensions.  See the test file cases for examples.  Here is a
summary of what is implemented and what is not yet implemented.

Implemented:

   * Backtracking recursive descent search.
   * `Rule`
   * `Tok`

Not yet implemented:

   * `Prec` and precedences in productions
   * `Sig` type handling
   * `Pratt` calls to the Pratt parser
   * `Opt`
   * Repeated items (`OneOrMore`, `ZeroOrMore`, `Between`, etc.)
   * `Not`
   * `OneOf`
   * `Hide`
   * LL(1) optimization
   * Epsilon production handling.
   * Undo compile in the `Grammar` class.

Wrapper functions
=================

Strings in the rule-defining expressions must be wrapped by some function call,
even though allowing the plain strings would be convenient for rules or for
using token labels instead of the token objects.  That would work in most
cases, but since addition is defined for strings it would not work for two
strings at the beginning of the expression.

Wrapper functions:

============   =========================== ============= ===============
Function       Arguments                   Shortcut      Python3 only
============   =========================== ============= ===============
`Rule`         rule-label (a string)
`Tok`          token                       token
`Root`         item
`Prec`         item, prec                  item[prec]
`Sig`          item, type sig              item(sig)
`Pratt`        (optional) pstate, type sig
`Opt`          item (any number of args)
`nExactly`     int, item                   n * item
`nOrMore`      int, item                   (n,) * item   (n,...) * item
`OneOrMore`    item                        (1,) * item   (1,...) * item
`ZeroOrMore`   item                        (0,) * item   (0,...) * item
`Between`      int, int, item              (m,n) * item
`Hide`         item
`Not`          item
`AnyOf`        itemlist
============   =========================== ============= ===============

Note that `(n,m)` and its variations are equivalent to `[n,m]` if that
syntax looks clearer.

Overloaded operator API
=======================

The basic objects that make up rule definitions are `Item` objects, `ItemList`
objects, and `CaseList` objects.  The latter two are just list-like objects
with most of the list operations overloaded.  An `ItemList` only holds `Item`
instances, and a `CaseList` only holds `ItemList` instances.  These objects do
not nest, though, and so there are some important differences from ordinary
Python lists.

The `ItemList` and `CaseList` classes are basically designed for concatenation
operations, since that is what is used to build up production rule expressions.
They are "flat" in the sense that they only hold one kind of object and do
everything they can to convert an object to that type before saving it on the
list.  An `ItemList` instance holds `Item` instances and a `CaseList` instance
holds `ItemList` instances.  Because of the latter property `Case` is defined
as an alias for `ItemList`.  Both of the classes take an arbitrary number of
arguments in their constructors.  All the arguments are converted to elements
of the single type that they hold.  The new instance then initially contains
all those converted arguments as its elements.  When an `ItemList` is passed
another `ItemList` in its initializer argument list it just takes the elements
within that list and extends its list with them.  The `CaseList` class works
similarly.

So the initializers basically form the concatenation of all the passed-in
arguments, after converting each one to the type of object that the list-like
object holds (each holds only one type of object).  The addition and "or"
operations are essentially shorthand for putting both operands on an
initializer list of the appropriate return type.  Appending to a list works the
same except it gives the in-place result of appending that item after
converting it.

Summary of the operations:

* **Addition**: Two `Item` and/or `ItemList` instances can be combined
  with `+`, which always returns an `ItemList`.  The operation is the same as
  if the operands had both been on the initializer list for `ItemList`.  The
  `+=` operator is also defined.  The addition operator is not defined for
  `CaseList` objects in order to possibly catch some syntax errors in
  expressions (although there are ordinary `add` and `iadd` methods). |br|

* **Case joining**: The "or" operation `|` is defined for `Item`,
  `ItemList`, or `CaseList` instances.  It always returns a `CaseList`.  The
  operands are joined together as if they had been arguments to the initializer
  of a `CaseList`. |br|

* **Tokens in expressions**: The `+` and `|` operators are defined for tokens
  (in the `PrattParser` module) to behave in the same way as for `Item`
  instances.  This allows the use of the tokens directly, without having to
  convert them into `Item` instances by wrapping them in the `Tok` function. |br|

* **Other list-like operations**: The methods `append` and
  `insert` are defined for these list-like classes.  They convert their
  argument to the correct type for the container and then append it to the list
  in-place.  Indexing of these list-like objects is also supported, including
  negative indices and slices.  This allows them to be iterated over.  They
  also have a `len`, so they can be tested for emptiness.

.. note::

   Since the `+` operator has a higher precedence than the `|` operator, all
   the additions within a case will always be carried-out before any "or"
   operations.  So each argument to `|` will be either a single token, a single
   `Item` or a single `ItemList`.

   Note that after a full expression containing these objects and operators is
   evaluated the resulting r.h.s. object (which is set to the l.h.s. variable
   name for a production rule) can be 1) a single token, 2) a single `Item`, 3)
   a single `ItemList`, or 4) a `CaseList`.  The `compile` method of a
   `Grammar` instance will always convert the value into a `CaseList` instance.
   (It would be possible to overload the `<<=` operator and use it instead of
   `=` to automatically do the conversion, but that does not seem worth the
   extra notation and boilerplate.)

Modifiers for items
===================

Items can have several begin/end modifiers to indicate when special processing
starts or ends.  These are stored in a list attribute called
`begin_end_modifiers`.  End modifiers are always the string ")".  Begin
modifiers can be any of these (set by the corresponding function applied to the
Item or ItemList):

* `"Opt("`
* `"OneOrMore("`
* `"ZeroOrMore("`

Operator precedences expressed in grammar
=========================================

This is not yet implemented, but will mimic the way a Pratt parser works.

In the EBNF-like grammar precedences are defined using index-style brackets::

    Tok("k_plus")[30]

The above would give a precedence of 30 to the token.

Optimizing the grammar
======================

This section discusses possible optimizations to the grammar.  Predictive
parsing is close to being fully implemented, but is not fully set up yet.

predictive parsing
------------------

In order to optimize the parsing of a recursive descent grammar, many
grammars allow the use of **predictive parsing**, which requires no
backtracking.  Even when predictive parsing is not possible, often
partial predictive parsing can make the algorithm more efficient
(falling back to backtracking search only when necessary).

To use a predicive parse you need to generate a **first set** for
each non-terminal (i.e., recursive rule call) in the grammar.

When epsilon productions are allowed a **follow set** acts similarly to
a first set.

See, e.g.:

* http://faculty.ycp.edu/~dhovemey/fall2010/cs340/lecture/lecture9.html

* http://www.csd.uwo.ca/~moreno//CS447/Lectures/Syntax.html/node12.html

Maybe also consider packrat parsing:

* https://en.wikipedia.org/wiki/Parsing_expression_grammar#Implementing_parsers_from_parsing_expression_grammars

grammar transformations
-----------------------

Not implemented.  Just an idea for now, but you could do any number of
grammar transformations on the rules of a `Grammar` object.

One possibility is to remove at least trivial left recursion.  Just change the
ordering of any obvious left recursive cases.

In a more complicated transformation you could do **left factoring** on the
grammar to remove the left recursion, but that isn't likely to happen.

Consider curtailment of left recursion, too.  If it is going to repeat will it
repeat in n levels, where that is the number of rules?  What is the limit, etc.
See some of those articles and consider if not too much to do.

Code
====

"""

# TODO: Implement function calls for Items with keyword arguments.  Then
# syntax like this can be used:
#
#    plus = Tok("k_plus")(val_type=t_int, arg_types=[t_int,t_int], eval_fun=addfun)
#
# In this case would be better to make a Python alias in a separate code line
# and then plug into the grammar in other places.

# Some simple example grammars for use in tests:
#    https://www.cs.rochester.edu/~nelson/courses/csc_173/grammars/cfg.html
#    https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form

# Todo maybe: Make the call to the external parser a user-settable thing via an
# init parameter.  Then this module becomes a general
# EBNF-like-Python-expression definition and parsing module.  Most of the code
# does not depend on the final parser.  (Also some TokenNode dependencies, though.)

# Interesting discussion of precedence in recursive descent.  Does this essentially
# use one of those methods (assume precedence is implemented to mimic Pratt way)?
# http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    pytest_helper.script_run("../../test/test_ebnf_classes_and_operators.py",
                             pytest_args="-v")

import sys
import threading
from .shared_settings_and_exceptions import ParserException, is_subclass_of, is_class

from .pratt_types import TypeSig, TypeObject, NONE
from .lexer import TokenNode
from .register_grammar_with_parser import register_rule_handlers_with_parser

DEFAULT_COMPILE_DEPTH_LIMIT = 200 # Limit depth of full grammar tree, just in case.

class Grammar(object):
    """An object representing a context-free grammar.  It is basically a
    dict of caselists indexed by nonterminal labels.  Provides various
    methods for processing the caselists."""

    def __init__(self, *args, **kwargs):
        """Initialize the grammar object.  Positional arguments are optional.
        If present they will be passed to the `compile` method, and are the
        same as the parameters for that method.

        Another keyword argument is `compile_depth_limit` which can be used
        to set the failsafe recursion limit on recursing the grammar tree to
        compile it."""
        self.compile_depth_limit = kwargs.pop("compile_depth_limit",
                                              DEFAULT_COMPILE_DEPTH_LIMIT)
        self.parser = None
        self.nonterm_to_caselist_dict = {}
        self.processed_or_being_processed = set() # Avoid infinite recurse in compile.
        if args:
            self.compile(*args, **kwargs)

    def compile(self, start_nonterm_label, parser, locals_dict, register=True):
        """Create the Pratt parser handlers in `parser` to parse the current
        grammar.

        The `start_nonterm_label` is the starting nonterminal.  Only rules
        which are reachable from the rule cases for this starting nonterminal
        will be processed.

        The `parser` is a `PrattParser` instance.

        The `locals_dict` should be passed `locals=locals()`.  If you also need
        globals then you have to merge the `locals()` and `globals()` dicts
        (with locals overwriting) and pass that dict instead.

        If `register` is true the rules are registered with the `PrattParser`
        instance `parser` to enable it to parse the grammar."""
        # TODO: Make sure we don't accidentally re-register something.

        self.nonterm_to_caselist_dict = {} # Reset all the caselists.
        self.processed_or_being_processed = set()
        self.parser = parser
        self.start_nonterm_label = start_nonterm_label
        self.locals_dict = locals_dict

        self._process_nonterm_caselist(start_nonterm_label, 1)
        self.processed_or_being_processed = set() # Empty out the set, no longer needed.
        #for name, caselist in self.nonterm_to_caselist_dict.items():
        #    print("   {0} = {1}".format(name, caselist))
        #print()

        #self._set_first_and_follow_sets()

        if register:
            for label, caselist in self.nonterm_to_caselist_dict.items():
                register_rule_handlers_with_parser(self.parser, label, self)

    def _process_nonterm_caselist(self, nonterm_label, depth):
        """Recursively process rules, converting string labels into their
        definitions from the locals dict, and looking up the tokens that go
        with token labels."""
        if depth > self.compile_depth_limit:
            raise ParserGrammarRuleException(
                    "Recursion depth exceeded in compiling grammar") # Need better msg.
        self.processed_or_being_processed.add(nonterm_label)
        try:
            locals_var_value = self.locals_dict[nonterm_label]
        except KeyError:
            raise ParserGrammarRuleException("The rule '{0}' was not found"
                    " as a variable in the locals dict that was passed to the compile"
                    " method of the `Grammar` class.  Remember that the string passed"
                    " to the Rule function must correspond exactly to the name of a"
                    " Python varible on the l.h.s. of a definition."
                    .format(nonterm_label))
        try:
            locals_caselist = CaseList(locals_var_value)
        except ParserGrammarRuleException:
            raise ParserGrammarRuleException("Could not convert local variable"
                  " '{0}' into a CaseList.  The variable had value:\n   {1}"
                  .format(nonterm_label, locals_var_value))

        processed_caselist = CaseList()
        for itemlist in locals_caselist:
            new_itemlist = ItemList()
            for item in itemlist:
                if item.kind_of_item == "token":
                    if isinstance(item.value, str):
                        item.value = self.parser.get_token(item.value)
                elif item.kind_of_item == "nonterminal":
                    recursion_nonterm_label = item.value
                    if recursion_nonterm_label in self.processed_or_being_processed:
                        pass # Nonterminal is currently being processed.
                    elif recursion_nonterm_label in self.nonterm_to_caselist_dict:
                        pass # Should be redundant with processing_in_progress....
                    else:
                        self._process_nonterm_caselist(recursion_nonterm_label, depth+1)
                new_itemlist.append(item)
            processed_caselist.append(new_itemlist)

        processed_caselist.grammar_object = self
        processed_caselist.parser = self.parser
        processed_caselist.nonterm_label = nonterm_label
        self.nonterm_to_caselist_dict[nonterm_label] = processed_caselist

        return processed_caselist

    def uncompile(self):
        """Undo the effect of the `compile` command.  Can be used for dynamic
        grammars, but NOT IMPLEMENTED YET."""
        raise NotImplementedError("Not yet implemented")

    #def __iter__(self, rule_label):
    #    """Generator to iteratively return the cases in production rule
    #    `rule_label`"""
    #    for rule in self.production_caselists[rule_label]:
    #        yield rule

    def __getitem__(self, production_label):
        """Access like a dict to get production rules from their labels."""
        return self.nonterm_to_caselist_dict[production_label]

    def __contains__(self, nonterm_label):
        """For use with the 'in' keyword, like testing keys in a dict."""
        return nonterm_label in self.nonterm_to_caselist_dict

    def _optimize_grammar_tree(self):
        """Do a search of the grammar tree and find lookahead tokens to
        make the parsing more efficient. Called from the `compile` method.
        NOT IMPLEMENTED."""
        # In future optimizations this routine could search the tree the
        # grammar to find the tokens for, say LL(1) grammars which can be used
        # to avoid the backtracking by using a lookahead.  They are relayed
        # back up the CFG tree after a recursive search down.  After they are
        # found they can be passed to the Pratt null-string handler function.
        raise NotImplementedError("Not implemented.")

    def _set_first_and_follow_sets(self):
        """Set the first and follow sets for every case of every nonterminal."""
        for nonterm_label, caselist in self.nonterm_to_caselist_dict.items():
            for case in caselist:
                self._recursive_set_first_sets(nonterm_label, case)
        # TODO do follow sets separately, if done at all.

    def _recursive_set_first_sets(self, nonterm_label, case):
        """Recursively compute the first set for each rule and store it with that
        rule object."""
        # First sets are based on the token label of the token returned by the
        # lexer.
        #
        # The labels are necessarily mutually exclusive, and the lexer is not
        # context sensitive.  This is imporant because to prune the search tree
        # based on lookahead we need to guarantee that there is no possibility
        # for the excluded branches to ever match.
        #
        # As of now only the first token literal is considered in computing
        # first sets, but really any sequence of tokens could serve as a
        # longer, more-effective required sequence of peeks.
        #
        # See the basic algorithm on these pages:
        # http://faculty.ycp.edu/~dhovemey/fall2010/cs340/lecture/lecture9.html
        # http://www.csd.uwo.ca/~moreno/CS447/Lectures/Syntax.html/node12.html
        #
        # Note that epsilon is neither a terminal nor a nonterminal.  Terminals
        # cannot expand to epsilon.
        #
        # Code for calculating follow sets is only partially implemented.
        if case.first_set and case.follow_set:
            return case

        case.first_set = set()
        case.expands_to_epsilon = False
        case.first_item_not_epsilon_expanding = None

        if case[0].kind_of_item == "epsilon": # Handle epsilon production.
            pass # TODO, also raise exception if len not one, return.

        for item in case:
            if item.kind_of_item == "nonterminal":
                # Recursively set the attributes of the subrule.
                subrule_nonterm_label = item.value
                if subrule_nonterm_label != nonterm_label:
                    # Do we want cases or caselist here?  Needs second argument...
                    subrule = self._recursive_set_first_sets(subrule_nonterm_label)
                else:
                    continue # A self-recursion.

                # Union the current first set with subrule's first set.
                case.first_set |= subrule.first_set

                # Handle epsilon stuff.
                if subrule.expands_to_epsilon:
                    case.expands_to_epsilon = True
                if (not subrule.expands_to_epsilon
                        and case.first_item_not_epsilon_expanding is None):
                    case.first_item_not_epsilon_expanding = item

            else: # terminal (i.e., a token item)
                case.first_set.add(item) # Note: could sequential tokens, too.
                if case.first_item_not_epsilon_expanding is None:
                    case.first_item_not_epsilon_expanding = item

        # Include epsilon-based first items.
        if case.first_item_not_epsilon_expanding is None:
            case.first_set.add(EPSILON)
        else:
            case.first_set |= case.first_item_not_epsilon_expanding

        print("case is:", case, "\nits first set is:", case.first_set)
        return case

    def print_grammar(self):
        for nonterm_label, caselist in self.nonterm_to_caselist_dict.items():
            print(nonterm_label, "=", caselist)
            print()


class Item(object):
    """Class representing the basic elements that make up the cases of the
    production rules."""

    def __init__(self, value=None):
        """Create an initial `Item` instance.  These plain `Item` instances are
        not used in expressions.  Before that they must be "decorated" by
        functions like `Tok` and `Rule` to set the `kind_of_item` attribute,
        and possibly others.

        If a string is passed in as the value it is assumed to be a nonterminal
        label.

        If a token is passed in it is converted to an `Item`.

        If an `Item` instance is passed in then its attributes will be copied
        to this instance.

        If no `value` is specified or value is `None` then a dummy `Item` is
        created, which must be processed further to be used in expressions."""
        if isinstance(value, Item): # Already was an Item, just copy attrs.
            self.value = value.value
            self.root = value.root
            self.prec = value.prec
            self.pstate = value.pstate
            self.type_sig = value.type_sig
            self.modifiers = value.modifiers
            self.kind_of_item = value.kind_of_item
            return

        self.value = value
        self.root = False
        self.prec = 0
        self.pstate = None
        self.type_sig = None
        self.modifiers = [] # Things like "Opt(" and ")" added by funs.

        if value is None: # A dummy Item; must be set to something else to be used.
            self.kind_of_item = "dummy"
        elif isinstance(value, str):
            # A string nonterminal label, unless the Tok function resets it.
            self.kind_of_item = "nonterminal"
        elif is_subclass_of(value, TokenNode): # A token.
            self.kind_of_item = "token"
        else:
            raise ParserGrammarRuleException("Unrecognized case item: {0}"
                                             .format(value))

    def __getitem__(self, arg):
        # No longer used; lists with [...] will be optional sections.
        """Use bracket-indexing as a shortcut for the `Prec` function."""
        return Prec(self, arg)

    def __call__(self, type_sig):
        """Use function call as a synonym for the `Sig` function."""
        # TODO: consider changing so that the arguments are automatically
        # passed to type_sig... since it will always be one.  But if it is
        # already one, then just use that (check isinstance).
        self.type_sig = type_sig
        return Sig(self, type_sig)

    def __repr__(self):
        string = "Item({0})".format(self.value)
        if self.kind_of_item == "token":
            if isinstance(self.value, str):
                token_label = self.value
            else:
                token_label = self.value.token_label
            string = "Tok(\"{0}\")".format(token_label)
        elif self.kind_of_item == "nonterminal":
            string = "Rule(\"{0}\")".format(self.value)
        elif self.kind_of_item == "pratt_call":
            string = "Pratt(\"{0}\")".format(self.value)
        if self.type_sig:
            val = self.type_sig.val_type.type_label
            if val == NONE: val = "None"
            arg_list = self.type_sig.arg_types
            if isinstance(self.type_sig.arg_types, TypeObject):
                arg_list = self.type_sig.arg_types.type_label
                if arg_list == NONE: arg_list = "None"
            else:
                arg_list = [t.type_label if t.type_label != NONE
                                         else "None" for t in arg_list]
                arg_list = "[" + ", ".join(arg_list) + "]"
            sig_string = val + ", " + str(arg_list)
            string += "({0})".format(sig_string)
        if self.prec:
            string += "[{0}]".format(self.prec)
        if self.root:
            string = "Root({0})".format(string)
        for m in self.modifiers:
            if m == ")": string += m
            else: string = m + string
        return string

    def __add__(self, right_other):
        """Overload `+` from the left operand."""
        raise_if_not([Item, ItemList], [TokenNode], right_other, self, "+")
        return ItemList(self) + ItemList(right_other)

    def __radd__(self, left_other):
        """Overload `+` from the right operand."""
        raise_if_not([Item, ItemList], [TokenNode], left_other, self, "+")
        return ItemList(left_other) + ItemList(self)

    def __or__(self, right_other):
        """Overload `|` from the left operand."""
        raise_if_not([Item, ItemList, CaseList], [TokenNode], right_other, self, "|")
        return CaseList(self) | CaseList(right_other)

    def __ror__(self, left_other):
        """Overload `|` from the right operand."""
        raise_if_not([Item, ItemList, CaseList], [TokenNode], left_other, self, "|")
        return CaseList(left_other) | CaseList(self)

    def __rmul__(self, left_other):
        """The expression `n*item` for an int `n` is "n occurrences of" `item`."""
        return Repeat(left_other, self)

    def __invert__(self):
        """Overload the prefix operator '~'."""
        # TODO, raise_if_not doesn't work yet for unary operators.
        return Not(self)

class ItemList(object):
    """A list of `Item` instances."""
    def __init__(self, *args):
        """Initialize an `ItemList` with one or more items.  The arguments
        can include `Item` instances, `ItemList` instances, and `TokenNode`
        subclasses."""
        self.data_list = []
        for a in args:
            if is_subclass_of(a, TokenNode):
                self.data_list.append(Item(a))
            elif isinstance(a, Item):
                self.data_list.append(a)
            elif isinstance(a, ItemList):
                self.data_list.extend(a.data_list)
            else:
                raise ParserGrammarRuleException("Unknown type in initializer to"
                        " ItemList class.  Object is: {0}.".format(a))
        self.first_set = None
        self.follow_set = None

    def append(self, item):
        """Append an item to the list."""
        self.data_list.append(Item(item))

    def insert(self, index, item):
        """Insert an item."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list.insert(index, Item(item))

    def __getitem__(self, index):
        """Index an element of the `ItemList`."""
        if isinstance(index, slice):
            return self.data_list[index.start:index.stop:index.step]
        if index < 0: # Handle negative indices.
            index += len(self)
        return self.data_list[index]

    def __setitem__(self, index, value):
        """Set an element of the `ItemList`."""
        if isinstance(index, slice):
            self.data_list[
                    index.start:index.stop:index.step] = [Item(v) for v in value]
            return
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list[index] = Item(value)

    def __len__(self):
        return len(self.data_list)

    def __delitem__(self, index):
        if index < 0: # Handle negative indices.
            index += len(self)
        del self.data_list[index]

    def __add__(self, right_other):
        raise_if_not([Item, ItemList], [TokenNode], right_other, self, "+")
        """Overload `+` from the left operand."""
        return ItemList(self, right_other)

    def __radd__(self, left_other):
        raise_if_not([Item, ItemList], [TokenNode], left_other, self, "+")
        """Overload `+` from the right operand."""
        return ItemList(left_other, self)

    def __iadd__(self, other):
        """Overload `+=` operation."""
        raise_if_not([Item, ItemList], [TokenNode], other, self, "+")
        self.data_list += ItemList(other).data_list
        return self

    def __or__(self, right_other):
        raise_if_not([Item, ItemList, CaseList], [TokenNode], right_other, self, "|")
        """Overload `|` from the left operand."""
        return CaseList(self) | CaseList(right_other)

    def __ror__(self, left_other):
        raise_if_not([Item, ItemList, CaseList], [TokenNode], left_other, self, "|")
        """Overload `|` from the right operand."""
        return CaseList(left_other) | CaseList(self)

    def __rmul__(self, left_other):
        """The expression `n*itemlist` for an int `n` is "n occurrences of"
        `itemlist`."""
        return Repeat(left_other, self)

    def __repr__(self):
        return "ItemList({0})".format(", ".join([str(i) for i in self.data_list]))

Case = ItemList # Case is an alias for an ItemList; Items converted on init.

class CaseList(object):
    """A list of `Case` objects.  Note, though, that a single Item or ItemList can
    also be a case (when there are no "or" operations to form the case)."""
    def __init__(self, *args):
        """Take an arbitrary number of `ItemList` arguments and make a `CaseList`
        out of them.  Arguments can include `Item` instances and `TokenNode`
        subclasses."""
        self.data_list = []
        for a in args:
            if is_subclass_of(a, TokenNode):
                self.data_list.append(ItemList(a))
            elif isinstance(a, Item):
                self.data_list.append(ItemList(a))
            elif isinstance(a, ItemList):
                self.data_list.append(a)
            elif isinstance(a, CaseList):
                self.data_list.extend(a)
            else:
                raise ParserGrammarRuleException("Unknown type in initializer to"
                        " CaseList class.  Object is: {0}.".format(a))

    def append(self, item):
        """Append an item to the list."""
        self.data_list.append(ItemList(item))

    def insert(self, index, item):
        """Insert an item."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list.insert(index, ItemList(item))

    def __getitem__(self, index):
        """Index and element of the `ItemList`."""
        if isinstance(index, slice):
            return self.data_list[index.start:index.stop:index.step]
        if index < 0: # Handle negative indices.
            index += len(self)
        return self.data_list[index]

    def __setitem__(self, index, value):
        """Set an element of the `ItemList`."""
        if isinstance(index, slice):
            self.data_list[
                    index.start:index.stop:index.step] = [ItemList(v) for v in value]
            return
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list[index] = ItemList(value)

    def __len__(self):
        return len(self.data_list)

    def __delitem__(self, index):
        if index < 0: # Handle negative indices.
            index += len(self)
        del self.data_list[index]

    def add(self, other):
        """Add a `CaseList` to some other object, which must be convertable to
        a `CaseList`.  This method is purposely not overloaded with the operator
        `+` because that operator is used in the production rule strings for
        `ItemList` objects, but in that context is an error if applied to
        `CaseList` objects."""
        # Note this doesn't add in-place.  Returns a new one.
        raise_if_not([Item, ItemList, CaseList], [TokenNode], other, self, "+")
        return CaseList(self, other)

    def iadd(self, other):
        """Add a `CaseList` to some other object, in place.  Like `add` but in-place."""
        # Note this doesn't add in-place.  Returns a new one.
        raise_if_not([Item, ItemList, CaseList], [TokenNode], other, self, "+")
        self.data_list += CaseList(other).data_list
        return self

    def __add__(self, right_other):
        raise_if_not([], [], right_other, self, "+") # Error to add CaseLists.

    def __radd__(self, left_other):
        raise_if_not([], [], left_other, self, "+") # Error to add CaseLists.

    def __iadd__(self, other):
        raise_if_not([], [], other, self, "+=") # Error to add CaseLists.

    def __or__(self, right_other):
        """Overload `|` from the left operand."""
        raise_if_not([Item, ItemList, CaseList], [TokenNode], right_other, self, "|")
        if not isinstance(right_other, CaseList):
            return self | CaseList(right_other)
        return CaseList(self, right_other)

    def __ror__(self, left_other):
        """Overload `|` from the right operand."""
        raise_if_not([Item, ItemList, CaseList], [TokenNode], left_other, self, "|")
        if not isinstance(left_other, CaseList):
            return CaseList(left_other) | self
        return CaseList(left_other, self)

    def __rmul__(self, left_other):
        """The expression `n*caselist` for an int `n` is "n occurrences of"
        `caselist`."""
        return Repeat(left_other, self)

    def __repr__(self):
        return "CaseList({0})".format(", ".join([str(i) for i in self.data_list]))

#
# Define some special items.
#

EPSILON = Item() # For an epsilon production, item expands to empty string.
EPSILON.kind_of_item == "epsilon"

DOLLAR = Item() # Dollar sign, matches end of text, i.e., end-token.
DOLLAR.kind_of_item = "dollar"

#
# Define wrapper functions.
#

def Rule(nonterm_label):
    """Return an `Item` to represent the nonterminal with the string label
    `nonterm_label`."""
    item = Item(nonterm_label)
    item.kind_of_item = "nonterminal"
    return item

def Tok(token):
    """Turn a token into an item.  Used before overloading defined on tokens.
    Can be passed a token object or a string token label."""
    if not (isinstance(token, str) or is_subclass_of(token, TokenNode)):
        raise ParserGrammarRuleException("Bad argument {0} passed to the"
                " Tok function.".format(token))
    item = Item(token)
    item.kind_of_item = "token"
    return item

#def T(token_regex):
#    """Define a regex which will be automatically made into a token and registered
#    with the grammar but only for use in the rules it appears in."""
#    if not (isinstance(token, str):
#        raise ParserGrammarRuleException("Bad argument {0} passed to the"
#                " T function.".format(token))
#    item = Item(token_regex)
#    item.kind_of_item = "undefined_token"
#    return item

def Root(item_init_arg, prec=None):
    """A function to designate that the token for the item should made into the
    root of the resulting parse subtree, if possible."""
    # This can only be called once per case, and can only take one `Item` argument.
    item = Item(item_init_arg)
    item.root = True
    return item

def Prec(item_init_arg, prec):
    """Set the operator precedence when called from a tail handler.  Can only
    wrap an item."""
    item = Item(item_init_arg)
    item.prec = prec
    return item

def Sig(item_init_arg, type_sig):
    """Set the type signature for an item."""
    item = Item(item_init_arg)
    item.type_sig = type_sig
    return item

def Pratt(pstate=None, type_sig=None):
    # Todo: type_sig no longer needed, but doesn't hurt.
    """Use an ordinary Pratt parser `recursive_parse` to get a subexpression.
    The paramter `pstate` is a state that will be temporarily pushed on the
    `pstate_stack` during the parsing (which can be used as a precondition).
    The optional type signature can also be set to be checked."""
    item = Item()
    item.kind_of_item = "pratt_call"
    item.type_sig = type_sig
    item.pstate = pstate
    return item

def Opt(*args):
    """List of optional arguments, can match any one or none."""
    # TODO: Figure out how to handle this.  Really like a caselist passed in.
    # Could be implemented as a temporary sub-rule which allows epsilon....
    # Could convert its arguments to CaseList, which would allow | in the
    # expressions, too.
    #print("args are", args)
    # test using caselist...
    caselist = CaseList()
    for arg in args:
        caselist.append(arg)
    #print("caselist is", caselist)

    itemlist = ItemList()
    for count, arg in enumerate(args):
        itemlist += ItemList(arg)
        if count != len(args)-1:
            itemlist[-1].modifiers.append(",")
    itemlist[0].modifiers.insert(0, "Opt(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def Repeat(range_spec, arg):
    """Used to process overload of multiplication for repetition."""
    if isinstance(range_spec, tuple) or isinstance(range_spec, list):
        if len(range_spec) == 2 and range_spec[1] is Ellipsis:
            range_spec = [range_spec[0], None]
        elif len(range_spec) == 1:
            #return nOrMore(range_spec[0], arg)
            range_spec = [range_spec[0], None]
        elif len(range_spec) == 2:
            if range_spec[1] is Ellipsis:
                range_spec = (range_spec[0],)
            #return Between(range_spec[0], range_spec[1], arg)
            range_spec = [range_spec[0], range_spec[1]]
        else:
            raise ParserGrammarRuleException("Tuple or list must contain one or two integers.")
    elif isinstance(range_spec, int):
        #return nExactly(range_spec, arg)
        range_spec = [range_spec, range_spec]
    else:
        raise ParserGrammarRuleException("Can only multiply by an int, tuple, or list.")

    itemlist = ItemList(arg)
    itemlist[0].modifiers.insert(0, "Repeat({0}, {1}, ".format(range_spec[0], range_spec[1]))
    itemlist[-1].modifiers.append(")")
    return itemlist


def nExactly(n, arg):
    return Repeat(n, arg)

def nOrMore(n, arg):
    return Repeat((n,), arg)

def Between(m, n, arg):
    return Repeat((m,n), arg)

def OneOrMore(arg):
    return Repeat((1,), arg)

def ZeroOrMore(arg):
    return Repeat((0,), arg)

def Hide(itemlist):
    """Do not show the items in the final tree.  For example, parentheses can
    be ignored in function argument lists."""
    raise NotImplementedError("Not yet implemented.")

def Not(token):
    """The token cannot appear or the case fails."""
    itemlist = ItemList(token)
    if not all(i.kind_of_item == "token" for i in itemlist):
        raise ParserGrammarRuleException("Only tokens can appear inside Not(...).")
    itemlist[0].modifiers.insert(0, "Not(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def OneOf(*args):
    # Give you a choice of possibilities from several, but must be one.
    pass

#
# Utility functions.
#

def raise_if_not(instanceof_list, issubclass_list, operand_or_arg, calling_instance,
                 operator_or_method_string, kind="op"):
    """Error-checking routine called from overloaded operators.  If `operand_or_arg`
    is not an instance a class in `instanceof_list` or a subclass of a class in
    `issubclass_list` then raise a `ParserGrammarRuleException` with a helpful
    error message.

    If `kind` is `"op"` the message is for an operator.  If it is `"method"`
    then the message is for a method.
    """
    if any([isinstance(operand_or_arg, classname) for classname in instanceof_list]):
        return
    if any([is_subclass_of(operand_or_arg, classname) for classname in issubclass_list]):
        return
    if is_class(operand_or_arg):
        operand_string = "class " + operand_or_arg.__name__
    else:
        operand_string = "instances of class " + operand_or_arg.__class__.__name__
    if kind == "op":
        raise ParserGrammarRuleException("Overloading of operator '{0}' is not"
                " defined between {1} and instances of class {2}.  The two"
                " operands are {3} and {4}."
                .format(operator_or_method_string, operand_string,
                        calling_instance.__class__.__name__,
                        calling_instance, operand_or_arg))
    elif kind == "method": # TODO this isn't tested.
        raise ParserGrammarRuleException("Method '{0}' of {1} is not"
                " defined for arguments that are {1}."
                .format(operator_or_method_string,
                        calling_instance.__class__.__name__, operand_string))
    else:
        raise ParserGrammarRuleException("Bad flag to raise_if_not.")

def print_indented_caselist(string_before, caselist):
    """Print routine for debugging."""
    string = string_before + "\nCaselist(\n"
    string2 = ""
    for case in caselist:
        string2 += " "*3 + str(case) + ",\n"
    if string2:
        string += string2[:-2] + "\n)" # Lose last comma, close paren.
    else:
        string += ")"
    print(string)

#
# Exceptions.
#

class ParserGrammarRuleException(ParserException):
    """Exception raised by grammar classes."""
    pass

