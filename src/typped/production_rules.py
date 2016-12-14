# -*- coding: utf-8 -*-
"""

Introduction
------------

This module implements a nice frontend for parsing grammars using EBNF-like
Python expressions.  The backend is the recursive-descent parsing capabilities
of the `PrattParser` class, implemented using precondition functions and
null-string token handlers.

Some (but not all) similar projects which parse a grammar specified in a
Python file are:

* **pyparsing** -- Uses Python overloading to define the grammar, similar to
  this module.
  http://pyparsing.wikispaces.com/home

* **Parsimonius** -- Passed a string containing the EBNF of the grammar.
  https://github.com/erikrose/parsimonious

* **Parsley** -- Passed a string containing the EBNF of the grammar.
  https://github.com/python-parsley/parsley/

* **yeanpypa** -- Uses Python overloading, similar to this module.
  https://github.com/DerNamenlose/yeanpypa

For more, see https://wiki.python.org/moin/LanguageParsing

Terminology
-----------

* **Production rules** are also called **productions**, **parsing rules**,
  or just **rules**.  They are the individual rewrite rules such as
  `<expression> ::= <term>` in a BNF grammar.  The symbols on the l.h.s. of
  productions (which can also appear in the r.h.s.) are called **nonterminal
  symbols**.  The r.h.s of a production is called the **parsing expression**.
  The other possible symbols on the r.h.s. are **terminal symbols** or the
  special **epsilon symbol**.

* Production rules with the the same l.h.s. nonterminal symbol will be
  called different **cases** of the nonterminal symbol.  An alternative
  notation is to define multiple cases in one expression by using "or" symbol
  `|`.  This latter form of definition is currently *required* by this module.
  That is, all the cases of rules defining a nonterminal must be occur in one
  expression, using `|` if there are multiple cases.  The ordered list of all
  the rule cases for a nonterminal will be called the **caselist** for the
  nonterminal.

* The separate symbol elements within a case will be collectively called the
  **items** of that case.  They include terminal symbols, nonterminal symbols,
  and possibly the epsilon symbol.  In this module there are no explicit
  terminal symbols.  Instead, terminals are either tokens (defined for and
  parsed from the lexer) or else consecutive sequences of tokens.  Several
  grammatical constructs are possible to modify the meaning of the items in a
  production rule, which will be discussed later.

The order in which the caselists of production rules are written does not
matter.  So they can be written top-down starting with the starting
nonterminal, or any other convenient way.  Some of the nonterminals in the
r.h.s. of the caselists may not have been defined yet, but they are written as
string labels and are resolved later when the `compile` method of the grammar
is called (passed the start nonterminal and a locals dict).  These r.h.s.
strings for nonterminals **must be identical** to the l.h.s. Python variable
names for the nonterminals (since they are looked up in the locals dict).

The order in which the cases of a nonterminal are defined within a caselist
*does* matter, at least for ambiguous grammars and to avoid or minimize
backtracking.  The order of the cases is the order in which the algorithm will
test the cases.  The first successful parse is returned.  In this sense the
grammar is similar to a **parsing expression grammar (PEG)** rather than a
**context-free grammar (CFG)**, which can be ambiguous.  (PEGs also allow "and"
and "not" predicates which do not consume any input.)

https://en.wikipedia.org/wiki/Parsing_expression_grammar

TODO: note that on page above a PEG essentially allows parsing expressions to
contain other parsing expressions, as in `Sum ← Product (('+' / '-') Product)*`
It does a backtracking until one succeeds or all fail.  Essentially an inlining
of the cases for a sub-rule.  Can ignore, but consider if easily doable.

Implementation
--------------

.. note::

    This module is a work-in-progress.  As of now the syntactic Python
    interface mostly all works, but not all of the features have been coded
    into the backend algorithms.  Currently it does basic BNF types of things,
    but not many EBNF extensions.  See the test file cases for examples.  Here
    is a summary of what is implemented and what is not yet implemented.

    Implemented::

       Backtracking recursive descent search.
       Rule
       Tok

    Not yet implemented::

       Prec and precedences in productions
       Sig type handling
       Pratt calls to the Pratt parser
       Optional
       OneOrMore
       ZeroOrMore
       Not
       AnyOf
       Hide
       Repeat(n, itemlist), exactly n repeats
       overload * as in `3 * k_rpar` for Repeat
       RepeatAtLeast(n, itemlist), n or more repeats
       overload ** as in `3 ** k_rpar` for RepeatAtLeast
       LL(1) optimization
       epsilon production handling
       Undo compile in Grammar class.


TODO: Make this a table????

The kinds of items that are supported are:

* rules         Rule, _<"str">_, ItemList var?, probably _<"str" alone
* tokens        tok_name, Tok(tok_name)
* pratt calls   Pratt(<args>)
* dummy         DummyItem(), just set flag.... then _ = DummyItem()

The possible modifiers are:

* prec       int argument assumed a prec.  only for rules?  tokens too?
* root       Anything, wrap with Root(...) helper fun.
* type_sig   Any instance of TypeSig assumed to set it.

Wrapper functions
-----------------

Strings in the rule-defining expressions must be wrapped by some function call,
even though allowing the plains strings would be convenient for rules or for
using token labels instead of the token objects.  That would work in most
cases, but since addition is defined for strings it would not work for two
strings at the beginning of the expression.

Wrapper functions:

=========   =========================== ==========
Function    Arguments                   Shortcut
=========   =========================== ==========
`Rule`      rule-label (string)
`Tok`       token                       token
`Root`      item                        ~item
`Prec`      item, prec                  item[prec]
`Pratt`     (optional) pstate, type sig
`Sig`       item, type sig              item(sig)
=========   =========================== ==========

Overloaded operator API
-----------------------

The basic objects that make up rule definitions are `Item` objects, `ItemList`
objects, and `CaseList` objects.  The latter two are just list-like objects
with most of the list operations overloaded.  An `ItemList` only holds `Item`
instances, and a `CaseList` only holds `ItemList` instances.  These objects
do not nest, and so they have some important differences from ordinary Python
lists.

The `ItemList` and `CaseList` classes are basically designed for concatenation
operations, since that is what is used to build up production rule expressions.
In their constructors they they both take an arbitrary number of arguments.
All the arguments are converted to elements of the single type that they hold.
The new instance then initially contains all those converted arguments as
elements.  When an `ItemList` is passed another `ItemList` in its initializer
argument list it just takes the elements within that list and puts them on its
list.  The `CaseList` class works similarly.

So the initializers basically form the concatenation of all the passed-in
arguments, after converting to the one type that the list-like object holds.
The addition and "or" operations are essentially shorthand for putting both
operands on an initializer list of the appropriate return type.  Appending
to a list gives the in-place result of adding the list and that item.

Summary of the operations:

* **Addition**: Two `Item` and/or `ItemList` instances can be combined
  with `+`, which always returns an `ItemList`.  The operation is the same as
  if the operands had both been on the initializer list for `ItemList`.  The
  `+=` operator is also defined.  The addition operator is not defined for
  `CaseList` objects in order to possibly catch some syntax errors in
  expressions (although there are ordinary `add` and `iadd` methods).

* **Case joining**: The "or" operation `|` is defined for `Item`,
  `ItemList`, or `CaseList` instances.  It always returns a `CaseList`.  The
  operands are joined together as if they had been arguments to the initializer
  of a `CaseList`.

* **Tokens in expressions**: The `+` and `|` operators are defined for tokens
  (in the `PrattParser` module) to behave in the same way as for `Item`
  instances.  This allows the use of the tokens directly, without having to
  convert them into `Item` instances by wrapping them in the `Tok` function.

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

TODO:

    Update: Implement this: Use `3 * k_digit` for exactly 3, and `3 ** k_digit`
    for three or more.  Then ZeroOrMore is `0**k_digit` and OneOrMore is
    `1**k_digit`.  The `**` operator can be read as `OrMore`.  Both have high
    precedence, shouldn't be a problem!!!  The extra asterisk "adds more" to
    the expression.  Also, `Not` is `0*k_comma`.

    * `*` = "occurrences of"
    * `**` = "or more occurrences of"

    Applies to groups in parens for free; the + operators turn the all
    into an ItemList.

    Implement as `Repeat(n, itemlist)` and `RepeatAtLeast(n, itemlist)`.

    Note, though, that 3*_<"wff">_ will grab the 3*_, which must return
    something that works.

    https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form


Modifiers for items
-------------------

Items can have several begin/end modifiers to indicate when special processing
starts or ends.  These are stored in a list attribute called
`begin_end_modifiers`.  End modifiers are always the string ")".  Begin
modifiers can be any of these (set by the corresponding function applied to the
Item or ItemList):

* `"Optional("`
* `"OneOrMore("`
* `"ZeroOrMore("`

Operator precedences expressed in grammar
-----------------------------------------

In order to use Pratt-parser style operator precedences in a grammar two things
must be done.

1. The relevant items in a case of a production must be set to some nonzero
value.

2. The item before the expression (before the first argument) must be wrapped
in a special function.

To see how this works, consider these productions below, parsing the expressions
`x + y * z` (compared with `x * y + z`).

.. code-block:: python

    term = Regex()

    add = Head(Rule("term")) + Rule("operator")

    operator = Tail( k_add[20] + Rule("term")
                   | k_ast[40] + Rule("term")
                   )

1. The `add` production is somehow on the top of the `pstack` and
`recursive_parse(0)` is called.  This invokes the head-handler for the `add`
production (which is a handler for the null-string token with the precondition
that "add" is on top of the stack).

2. The head-handler for the `add` production then pushes `"term"` onto the
`pstack` and calls `recursive_parse(0)` to get the first argument.

3. The head-handler for the `term` rule is then invoked.  It fetches the
first term, which is `x` and returns it as a subtree leaf.

4. We are now back to the `add` handler, having gotten `x` as `processed_left`.
It now pops the `term` state and pushes the `operator` state onto the `pstack`.
It then calls `recursive_parse(0, processed_left)`, which runs as a
tail-handler.  Notice how the recursive descent handler for the `add`
production is essentially mimicking the `recursive_parse` routine but pushing
and popping states.

----> Should the whole loop of recursive_parse run, or just one iteration
      when passed the explicit argument?????  As it is here, just one iteration?
      Could split up the head and tail parts as two functions... each loop
      iteration could be a function (but would be less efficient).

----> Do we need any special wrappers like `Tail`?

5. The tail-handler for `k_add` is now called, since the `+` token was returned
by the lexer.  It pushes the state `term` and calls `recursive_parse(20,
processed_left`.

6. The head-handler for `term` calls `recursive_parse(0)`, which fetches the
token `y`.

7. HERE IS INTERESTING PART, iron out above and figure out how to make it
work...

So the expression `x + y * z` will be evalated as `x + (y*z)`.

Optimizing the grammar
----------------------

predictive parsing
~~~~~~~~~~~~~~~~~~

In order to optimize the parsing of a recursive descent grammar, many
grammars allow the use of **predictive parsing**, which requires no
backtracking.  Even when predictive parsing is not possible, often
partial predictive parsing can make the algorithm more efficient
(falling back to backtracking search only when necessary).

To use a predicive parse you need to generate a **first set** for
each non-terminal (i.e., recursive rule call) in the grammar.  Call
this `first_set(nonterminal)`.

When epsilon productions are allowed a **follow set** acts similarly to
a first set.

See:

http://faculty.ycp.edu/~dhovemey/fall2010/cs340/lecture/lecture9.html
http://www.csd.uwo.ca/~moreno//CS447/Lectures/Syntax.html/node12.html

Maybe also consider packrat parsing:
https://en.wikipedia.org/wiki/Parsing_expression_grammar#Implementing_parsers_from_parsing_expression_grammars

grammar transformations
~~~~~~~~~~~~~~~~~~~~~~~

Not implemented.  Just an idea for now, but you could do any number of
grammar transformations on the rules of a `Grammar` object.

One possibility is to remove at least trivial left recursion.  Just change the
ordering of any obvious left recursive cases.

In a more complicated transformation you could do **left factoring** on the
grammar to remove the left recursion.

Consider curtailment of left recursion, too.  If it is going to repeat will it
repeat in n levels, where that is the number of rules?  What is the limit, etc.
See some of those articles and maybe do a simple thing.

"""

# TODO: some simple example grammars for use in tests:
#    https://www.cs.rochester.edu/~nelson/courses/csc_173/grammars/cfg.html
#    https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form

# TODO: Make the call to external parser a user-settable thing (just as a
# module var, since developers not users would do it) and this becomes a
# general EBNF-like-Python-expression definition and parsing module.
# Most does not depend on the final parser.  Also some TokenNode dependencies.

# TODO really need a TokenList that works like an Item and can go on an
# itemlist.  They are the terminals, after all.  Also: 2 * (k_lpar + k_lpar)
# Does the modifier list stuff handle that OK?  What about an `Item` that holds
# a list of tokens, with addition of tokens producing an `Item`?  But how
# do `ItemList` and `CaseList` handle this?  What about just a postprocessing
# to gather them up or mark them somehow?  Could mark them as a TokenGroup(...)
# or similar.

from __future__ import print_function, division, absolute_import
import pytest_helper

if __name__ == "__main__":
    pytest_helper.script_run("../../test/test_production_rules.py",
                             pytest_args="-v")

import sys
import threading
from .shared_settings_and_exceptions import ParserException, is_subclass_of, is_class

from .pratt_types import TypeSig, TypeObject, NONE
from .lexer import TokenNode


class Grammar(object):
    """An object representing a context-free grammar.  It is basically a
    dict of caselists indexed by nonterminal labels.  Provides various
    methods for processing the caselists."""

    def __init__(self):
        self.delimiter = Item(None) # Could be static but Item would need moving.
        self.parser = None
        self.production_caselists = {}
        self.processing_in_progress = set() # To avoid infinite recurse in compile.

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

        self.production_caselists = {} # Reset all the caselists.
        self.processing_in_progress = set()
        self.parser = parser
        self.start_nonterm_label = start_nonterm_label
        self.locals_dict = locals_dict
        self._process_nonterm_caselist(start_nonterm_label)
        self.processing_in_progress = set()
        print("\nThe final dict is\n")
        for name, caselist in self.production_caselists.items():
            print("   {0} = {1}".format(name, caselist))
        print()

        if register:
            for label, caselist in self.production_caselists.items():
                self.parser.def_production_rules_for_nonterminal(label, self)

    def _process_nonterm_caselist(self, nonterm_label):
        """Recursively process rules, converting string labels into their
        definitions from the locals dict, and looking up the tokens that go
        with token labels."""
        self.processing_in_progress.add(nonterm_label)
        try:
            locals_caselist = self.locals_dict[nonterm_label]
        except AttributeError:
            raise ParserGrammarRuleException("The rule \"{0}\" was not found"
                    " in the locals dict that was passed to the compile method"
                    " of the `Grammar` class.".format(nonterm_label))
        locals_caselist = CaseList(*locals_caselist)
        print("label of caselist being processed is", nonterm_label)
        print("processing this caselist from locals():\n   ", locals_caselist)

        processed_caselist = CaseList()
        for itemlist in locals_caselist:
            new_itemlist = ItemList()
            for item in itemlist:
                if item.kind_of_item == "token":
                    if isinstance(item.value, str):
                        item.value = self.parser.get_token(item.value)
                elif item.kind_of_item == "nonterminal":
                    recursion_nonterm_label = item.value
                    if recursion_nonterm_label in self.processing_in_progress:
                        pass # Nonterminal is currently being processed.
                    elif recursion_nonterm_label in self.production_caselists:
                        pass
                    else:
                        self._process_nonterm_caselist(recursion_nonterm_label)
                new_itemlist.append(item)
            processed_caselist.append(new_itemlist)

        processed_caselist.grammar_object = self
        processed_caselist.parser = self.parser
        processed_caselist.nonterm_label = nonterm_label
        self.production_caselists[nonterm_label] = processed_caselist

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
        return self.production_caselists[production_label]

    def __contains__(self, nonterm_label):
        """For use with the 'in' keyword, like testing keys in a dict."""
        return nonterm_label in self.production_caselists

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
        for nonterm_label, caselist in self.production_caselists.items():
            for rule in caselist:
                self._recursive_set_first_sets(nonterm_label, rule)
        # TODO do follow sets separately, if done at all.

    def _recursive_set_first_sets(self, nonterm_label, rule):
        """Recursively compute the first set for each rule and store it with that
        rule object."""
        #
        # See algorithm on this page:
        # http://faculty.ycp.edu/~dhovemey/fall2010/cs340/lecture/lecture9.html
        # http://www.csd.uwo.ca/~moreno/CS447/Lectures/Syntax.html/node12.html
        #
        # Note that epsilon is neither a terminal nor a nonterminal.  Terminals
        # cannot expand to epsilon.
        if rule.first_set and rule.follow_set:
            return rule

        rule.first_set = set()
        rule.expands_to_epsilon = False
        rule.first_item_not_epsilon_expanding = None

        if rule[0].kind_of_item == "epsilon": # Handle epsilon production.
            pass # TODO, also raise exception if len not one, return.

        for item in rule:
            if item.kind_of_item == "nonterminal":
                # Recursively set the attributes of the subrule.
                subrule_nonterm = item.value
                if subrule_nonterm != nonterm_label:
                    subrule = self._set_first_and_follow_sets(subrule_nonterm)
                else:
                    continue

                # Expand the first set with subrule's first set.
                rule.first_set |= subrule.first_set

                # Handle epsilon stuff.
                if subrule.expands_to_epsilon:
                    rule.expands_to_epsilon = True
                if (not subrule.expands_to_epsilon
                        and rule.first_item_not_epsilon_expanding is None):
                    rule.first_item_not_epsilon_expanding = item

            else: # terminal (i.e., token)
                rule.first_set.add(item) # TODO should be token group....
                if rule.first_item_not_epsilon_expanding is None:
                    rule.first_item_not_epsilon_expanding = item

        # Include epsilon-based first items.
        if rule.first_item_not_epsilon_expanding is None:
            rule.first_set.add(EPSILON)
        else:
            rule.first_set |= rule.first_item_not_epsilon_expanding

        return rule

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
        self.modifiers = [] # Things like "Optional(" and ")" added by funs.

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
        return OccurrencesOf(left_other, self)

    def __rpow__(self, left_other):
        """The expression `n**token` for an int `n` is "n or more occurrences of"
        `token`."""
        return OrMoreOccurrencesOf(left_other, self)

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

    def append(self, item):
        """Append an item to the list."""
        self.data_list.append(Item(item))

    def insert(self, index, item):
        """Insert an item."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list.insert(index, Item(item))

    def __getitem__(self, index):
        """Index an element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
        if isinstance(index, slice):
            return self.data_list[index.start:index.stop:index.step]
        if index < 0: # Handle negative indices.
            index += len(self)
        return self.data_list[index]

    def __setitem__(self, index, value):
        """Set an element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
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
        return OccurrencesOf(left_other, self)

    def __rpow__(self, left_other):
        """The expression `n**token` for an int `n` is "n or more occurrences of"
        `token`."""
        return OrMoreOccurrencesOf(left_other, self)

    def __repr__(self):
        return "ItemList({0})".format(", ".join([str(i) for i in self.data_list]))

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
        """Index and element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
        if isinstance(index, slice):
            return self.data_list[index.start:index.stop:index.step]
        if index < 0: # Handle negative indices.
            index += len(self)
        return self.data_list[index]

    def __setitem__(self, index, value):
        """Set an element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
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
        return OccurrencesOf(left_other, self)

    def __rpow__(self, left_other):
        """The expression `n**token` for an int `n` is "n or more occurrences of"
        `token`."""
        return OrMoreOccurrencesOf(left_other, self)

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
    # TODO: Consider if using the name Rule is better than the longer but more
    # descriptive name NonTerm.
    # Only one string arg allowed.
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

def Optional(arg):
    itemlist = ItemList(arg)
    itemlist[0].modifiers.insert(0, "Optional(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def OneOrMore(arg):
    # Same as 1 ** arg.
    itemlist = ItemList(arg)
    itemlist[0].modifiers.insert(0, "OneOrMore(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def ZeroOrMore(arg):
    # Same as 0 ** arg.
    itemlist = ItemList(arg)
    itemlist[0].modifiers.insert(0, "ZeroOrMore(")
    itemlist[-1].modifiers.append(")")
    return itemlist

# TODO: consider if arg * 3 and arg ** 3 would be better, since then
# you do not have a problem with 3 * 2 ** arg == 6 ** arg (because
# left arg is always a token, Item or ItemList.
#
# But then it reads "arg repeat 3" or "arg repeat at least 3"
#
# BUT still have arg * 3 ** 2 == arg * 9
#
# So just warn them.  It is Python language, after all, and you can put
# in any expression.
#
# Also consider arg // 3 for "repeat at most", but precedence is
# different from ** so caution is warranted...

def OccurrencesOf(n, arg):
    itemlist = ItemList(arg)
    itemlist[0].modifiers.insert(0, "OccurrencesOf({0},".format(n))
    itemlist[-1].modifiers.append(")")
    return itemlist

def OrMoreOccurrencesOf():
    itemlist = ItemList(arg)
    itemlist[0].modifiers.insert(0, "OrMoreOccurrencesOf({0},".format(n))
    itemlist[-1].modifiers.append(")")
    return itemlist

def Not(token):
    """The token cannot appear or the case fails."""
    itemlist = ItemList(token)
    if not all(i.kind_of_item == "token" for i in itemlist):
        raise ParserGrammarRuleException("Only tokens can appear inside Not(...).")
    itemlist[0].modifiers.insert(0, "Not(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def AnyOf(*args):
    # Maybe, give you a choice of possibilities from several.  Same as "Or" but
    # maybe a little more descriptive.
    pass

def Hide(itemlist):
    """Do not show the items in the final tree.  For example, parentheses can
    be ignored in function argument lists."""
    raise NotImplementedError("Not yet implemented.")

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

