# -*- coding: utf-8 -*-
"""

TODO: A production rule is usually what I have been calling a case.  A
CaseList is a list of production rules all with the same l.h.s.

TODO: some simple example grammars:
    https://www.cs.rochester.edu/~nelson/courses/csc_173/grammars/cfg.html
    https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form

TODO: to add more overloads, use * for `ZeroOrMore`.  Precedence high enough
    to probably not be too much problem, if a good idea at all.  Functions
    probably better, this is pushing it too far.
        _<"wff">_*_   vs, OneOrMore(_<"wff">_)
        Rule("wff")*_
        k_number*_

    https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form
    Better: from Wikipedia EBNF page,
        3 * k_number
    should be three k_number tokens.  Easy to do.  It applies to groups
    with parens 3*(k_number + k_comma) for free (as long as ItemList can
    handle it).  Need to consider, though, how parens will affect _<"x">_
    when those subgroups are processed...

    Also from above page, { } is repeat symbol.  Easy to handle, too, just
    let it form a set and have + extract from the set!!!!
    The | operation is the union.  Should work except when two identical
    ones are on opposite sides of | symbol.  Then they crush to one
    and there is no way to tell.  Left-to-right eval won't save you
    if the first thing is {k_number} | {k_number} ...

    Note that raw strings cannot be allowed in the rule expressions because
    "s" + "s" is define and "s" | "s" is not.

    Brackets for optional.  Remember that brackets are lists.  So just
        [k_comma]  vs.  Optional(k_comma)
    Just define the operations to turn lists into Optional things.
    BUT fails in same ways that commas in tuples fails: you cannot
    add two lists, and | is not defined for lists.  So, at the least,
    they cannot be at the end of a case that is followed by another
    that starts with a [ or a {.  Maybe better to use
       _[k_comma]
    which has the annoying _ but otherwise should work fine...

    Maybe postfix minus for something (actually defined in some standard,
    don't recall exactly).
        _<"wff">_-_
        Rule("wff")-_
        k_number-_

Terminology:

* **Production rules** are also called **productions** or just **rules**.
  They are the individual rewrite rules such as `<expression> ::= <term>`
  in BNF.

* Production rules with the the same l.h.s. symbol 
  are called different **cases** of the production.  An alternative notation is
  to separate different cases by the "or" symbol `|`.  The latter form of
  definition is currently *required* in by this module.

* The separate symbol elements within a case are called the **items** of that
  case.

While each case of a l.h.s. symbol (nonterminal) is technically a production
rule itself, in this module the terms production and rule are generally used to
refer to the collection of all the cases having the same l.h.s. symbol.

The order in which the production rules are written does not matter.  So rules
can be written top-down if that is easier to read, even though some of the
productions rules being called have not yet been defined.  This is achieved by
the use of string labels for production names in the r.h.s. of production
rules.  These strings are resolved later in when the `compile` method of the
grammar is called (passed the start state and a locals dict).  These r.h.s.
strings **must** be identical to the l.h.s. Python variable names for the rules
(since they are looked up in the locals dict).

The order in which cases are defined within a production's "or" sections does
matter, at least for ambiguous grammars and to avoid or minimize backtracking.
The order of the cases is the order in which the algorithm will test the cases.
The first successful parse is returned.  The current algorithm is a basic
backtracking search algorithm.  In the future, lookahead tokens could be
incorporated (via preconditions) to make LL(1) and similar grammars efficient.

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
operands on an initializer list of the appropriate return type.

Addition of `Item` or `ItemList` instances with `+` always returns an
`ItemList`.  The operation is the same as if the operands had both been on the
initializer list for `ItemList`.

The "or" operation `|` on `Item`, `ItemList`, or `CaseList` instances always
returns a `CaseList`.  Again, it is the same as if the operands had been
arguments to the initializer of a `CaseList`.

The `+` and `|` operators are defined for tokens in the `PrattParser` module to
behave in the same way as the `Item` instances.  This allows the use of the
tokens directly, without having to convert them into `Item` instances (via the
`Tok` function).

Since the `+` operator has a higher precedence than the `|` operator, all the
additions within a case will always be carried-out before any "or" operations.
So each argument to `|` will be either a single token, a single `Item` or a
single `ItemList`.

Operations such as `append` and `insert` are defined for these list-like
classes.  They convert their argument to the correct type for the container and
then do the expected thing.  Indexing of these list-like objects is also
supported, including negative indices but currently not slices.

Note that after a full expression containing these objects and operators is
evaluated the resulting r.h.s. object (which is set to the l.h.s. variable name
for a production rule) can be 1) a single token, 2) a single `Item`, 3) a
single `ItemList`, or 4) a `CaseList`.  The `compile` method of a `Grammar`
instance will always convert the value into a `CaseList` instance.  (It would
be possible to overload the `<<=` operator and use it instead of `=` to
automatically do the conversion, but that does not seem worth the extra
notation and boilerplate.)

A convenient synonym for the `Rule` function
--------------------------------------------

The `<` and `>` operators are also defined so they can be used as a convenient
synonym for the `Rule` function.  Unfortunately, comparison operators have a
precedence lower than both `+` and `|`.  This significantly complicates things.
The definitions perhaps take more effort than they are worth, but it is a good
puzzle.

We want something like this::

    wff = ( _<"egg">_ + k_plus + _<"egg">_
          | _<"egg">_ + k_minus + _<"egg">_
          )

The symbol `_` is assumed to be a reference to a special dummy item.  If that
symbol is not usable for some reason then some letter can be used, say
`o<"wff">o`.  Or just use the `Rule` function instead.

We know that the operators will always appear in the form
`DummyItem<str>DummyItem`, where `DummyItem` is a special kind of item, and
`str` is the string name for a production rule.  We can end up with these cases
and the reflection of each case:

    * Item < str > Item
    * Item < str > ItemList
    * Item < str > CaseList
    * ItemList < str > Item
    * ItemList < str > ItemList
    * ItemList < str > CaseList
    * CaseList < str > Item
    * CaseList < str > ItemList
    * CaseList < str > CaseList

Because the comparison operators have the lowest precedence, they will be the
last thing evaluated and will split the final `CaseList` (like if someone
arbitrarily cut up a string of balanced parentheses).  It is always possible to
uniquely reassemble such a collection of sublists, and the comparison operators
essentially need to do that.

In order for this to work the comparison operators need to be defined for all
three of the classes above.  They need to convert the string in the middle into
the `Item` for an rule and then reassemble the three objects in the correct
way.  We know, for example, that the rightmost `DummyItem` in the left object
needs to join the leftmost `DummyItem` in the right object and be replaced by
the `Item` for a rule corresponding to the string.

There is another potential problem with the comparisons: It is well-known
that comparison operators cannot in general be overloaded to work as
chained operators.  The BDFL rejected PEP 335 for a more-general overloading
system, but suggested in the rejection that he would be open to a PEP
to allow comparison chaining.  As of now, however, that has not been done
(and would not work for older versions if it were).

Fortunately, this case of using chained comparisons is *not* the general case.
The general case fails because `x<y>z` is always equivalent to `x<y and y>z`
with `y` only evaluated once.  The `and` operation is equivalent to `if x is
false, then x, else y` with short-circuiting when `x` is false.  If `x` is
false, then, `y` is never processed or looked at.  But if `x` is always false
then non-chained comparisons are broken.  We, however, don't care about the
boolean values at all in this context.  So the l.h.s. comparisons can always be
true.  We only need to join things together and return the combination.

In this case, since the middle object is a string and the ordering of
operations is fixed.  Only the `<` operation needs to be defined, since the `>`
object will also use that.  (The string type does not have `>` defined for the
type so the reflection will be used, see
https://docs.python.org/2/reference/datamodel.html).

The algorithm, then works as follows.  The first `<` operator gets a thread
lock and writes its arguments to a global module variable.  The following `>`
appends its arguments to the same list (leaving out duplicates).  This
continues until the `>` operator only has a single dummy `Item` (the symbol
`_`) as its right argument.  In that case the saved data is read, the save
location is cleared, the thread lock is released, and the data is returned.

The rule above for detecting the last `>` in a production expression works
because the `>` symbol has the lowest precedence of any of the operators.
So the rightmost one (say in `_<"wff">_`) *must* have `_` as its r.h.s.
argument.  In every other case the r.h.s. is not an individual `Item`,
since the smallest case is something like `_<"wff">_+_<"wff">_`.  The
middle `_+_` is evaluated to produce an `ItemList`, which is not an `Item`.

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

Operator precedences
--------------------

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

"""

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

lock = threading.Lock() # Only used for intermediate operations in overloaded < and >.

class Grammar(object):
    """An object representing a context-free grammar.  It consists of
    production rules, which in turn are made up of different cases of that
    rule."""

    def __init__(self):
        self.delimiter = Item(None) # Could be static but Item would need moving.
        self.parser = None
        self.production_rules = {}
        self.processing_in_progress = set() # Save rules to avoid infinite recurse.

    def old_add_production_case(self, rule_label, case_item_list):
        """Add a production rule to the grammar.  A `case_item_list` is
        an ordered list of `Item` instances or things that can
        be converted to one.  The order that they are added is the
        order that they will be evaluated in."""
        print("case item list", case_item_list)
        case_item_list = ItemList(*case_item_list)
        case_item_list.rule_label = rule_label

        if rule_label in self.production_rules:
            self.production_rules[rule_label].append(case_item_list)
        else:
            self.production_rules[rule_label] = [case_item_list]

    def add_production_case(self, rule_label, caselist):
        """Add a production rule to the grammar.  A `case_item_list` is
        an ordered list of `Item` instances or things that can
        be converted to one.  The order that they are added is the
        order that they will be evaluated in."""
        # TODO not used by _process now...
        print("case item list", case_item_list)
        if rule_label in self.production_rules:
            self.production_rules[rule_label].append(case_item_list)
        else:
            raise ParserGrammarRuleException("All cases must be currently"
                    " be added through the `compile` function with all"
                    " cases present.")

    def compile(self, start_rule_label, parser, locals_dict, register=True):
        """Create the Pratt parser handlers in `parser` to parse the current
        grammar.
        
        If `register` is true the rules are registered with the `PrattParser`
        instance `parser` to enable it to parse the grammar."""

        print("call to compile")
        self.production_rules = {} # Reset all the rules.
        self.processing_in_progress = set()
        self.parser = parser
        self.start_rule_label = start_rule_label
        self.locals_dict = locals_dict
        self._process_rule(start_rule_label)
        self.processing_in_progress = set()
        print("\nThe final dict is\n")
        for name, caselist in self.production_rules.items():
            print("   {0} = {1}".format(name, caselist))
        print()

        if register:
            for label, rule in self.production_rules.items():
                self.parser.def_production_rule(label, self)

    def _process_rule(self, rule_label):
        """Recursively process rules, converting string labels into their
        definitions from the locals dict, and looking up the tokens that go
        with token labels."""
        self.processing_in_progress.add(rule_label)
        try:
            locals_rule = self.locals_dict[rule_label]
        except AttributeError:
            raise ParserGrammarRuleException("The rule \"{0}\" was not found"
                    " in the locals dict that was passed to the compile method"
                    " of the `Grammar` class.".format(rule_label))
        locals_rule = CaseList(*locals_rule)
        print("label of rule being processed is", rule_label)
        print("processing this rule from locals():\n   ", locals_rule)

        processed_caselist = CaseList()
        for itemlist in locals_rule:
            new_itemlist = ItemList()
            for item in itemlist:
                if item.kind_of_item == "token":
                    if isinstance(item.value, str):
                        item.value = self.parser.get_token(item.value)
                elif item.kind_of_item == "production":
                    recursion_rule_label = item.value
                    if recursion_rule_label in self.processing_in_progress:
                        pass # Rule is currently being processed.
                    elif recursion_rule_label in self.production_rules:
                        pass
                    else:
                        self._process_rule(recursion_rule_label)
                new_itemlist.append(item)
            processed_caselist.append(new_itemlist)

        processed_caselist.grammar_object = self
        processed_caselist.parser = self.parser
        processed_caselist.rule_label = rule_label
        self.production_rules[rule_label] = processed_caselist

        return processed_caselist

    def uncompile(self):
        """Undo the effect of the `compile` command.  Can be used for dynamic
        grammars, but NOT IMPLEMENTED YET."""
        raise NotImplementedError("Not yet implemented")

    def __iter__(self, rule_label):
        """Generator to iteratively return the cases in production rule
        `rule_label`"""
        for rule in self.production_rules[rule_label]:
            yield rule

    def __getitem__(self, production_label):
        """Access like a dict to get production rules from their labels."""
        return self.production_rules[production_label]

    def __contains__(self, production_label):
        """For use with the 'in' keyword, like testing keys in a dict."""
        return self.production_label in production_rules

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
        self._recurse_on_grammar_tree(self.start_rule_label, num_productions)

    def _recurse_on_grammar_tree(self, rule_label, max_depth, curr_depth=0):
        """Recursively walk the grammar tree.  Limit depth to `depth`."""
        rule = self.production_rules[rule_label]
        for item in rule:
            if item.kind_of_item == "production":
                if curr_depth <= max_depth:
                    subrule_label = item.value
                    recurse_val = self._recurse_on_grammar_tree(
                                     subrule_label, max_depth, curr_depth+1)
        return retval

class Item(object):
    """Class representing the basic elements that make up the cases of the
    production rules."""
    
    def __init__(self, value=None, allow_str=False):
        """The list `item_list` should be a list of production labels, tokens,
        type signatures, and integer operator precedence values.
        
        If `root` is true then the item will be made the root of the subtree
        for the production that it is in (in place of the null-string token for
        the production rule).
        
        If `prec` is set to an integer then that value will be used as the
        precedence of the item when it is called by a tail-handler function.
        
        Passing a value of `None` creates a dummy `Item` instance which is only
        used for overloading purposes (to define the `delimiter` attribute
        of `Grammar`)."""
        # TODO: implement the root and prec stuff in the parser.

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

        if value is None: # A dummy Item.
            self.kind_of_item = "dummy"
        elif isinstance(value, str):
            # A string production rule label, unless the Tok function resets it.
            self.kind_of_item = "production"
        elif is_subclass_of(value, TokenNode): # A token.
            self.kind_of_item = "token"
        else:
            raise ParserGrammarRuleException("Unrecognized case item: {0}"
                                             .format(value))

    #def __getitem__(self, arg):
    #    # No longer used; lists with [...] will be optional sections.
    #    """Use bracket-indexing as a shortcut for the `Prec` function."""
    #    return Prec(self, arg)

    def __call__(self, type_sig):
        """Use function call as a synonym for the `Sig` function."""
        # TODO: consider changing so that the arguments are automatically
        # passed to type_sig... since it will always be one.  But if it is
        # already one, then just use that (check isinstance).
        self.type_sig = type_sig
        return Sig(self, arg)

    def __repr__(self):
        if self.kind_of_item == "token":
            if isinstance(self.value, str):
                token_label = self.value
            else:
                token_label = self.value.token_label
            string = "Tok(\"{0}\")".format(token_label)
        elif self.kind_of_item == "production":
            string = "Rule(\"{0}\")".format(self.value)
        elif self.kind_of_item == "pratt_call":
            string = "Pratt(\"{0}\")".format(self.value)
        else:
            string = "Item({0})".format(self.value)
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
            string = "~" + string
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

    def __invert__(self):
        """Overload the prefix operator '~'."""
        # TODO, raise_if_not doesn't work yet for unary operators.
        return Root(self)

    def __lt__(self, other):
        """Overload `<` from the left operand.  Reflected for '>'."""
        raise_if_not([str, Item, ItemList, CaseList], [TokenNode], other, self, "<")
        return handle_overloaded_lt_comparison(self, other)

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
        self.data_list.append(item) # What if not an item?

    def insert(self, index, item):
        """Insert an item."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list.insert(index, data_item) # What if not an item?

    def __getitem__(self, index):
        """Index an element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
        if index < 0: # Handle negative indices.
            index += len(self)
        return self.data_list[index]

    def __setitem__(self, index, value):
        """Set an element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list[index] = value

    def __len__(self):
        return len(self.data_list)

    def __delitem__(self, index):
        if index < 0: # Handle negative indices.
            index += len(self)
        del self.data_list[index]

    def __add__(self, right_other):
        raise_if_not([Item, ItemList], [TokenNode], right_other, self, "+")
        """Overload `+` from the left operand."""
        if not isinstance(right_other, ItemList):
            return self + ItemList(right_other) # TODO prob unnecessary, doesn't __init__ do it?
        return ItemList(self, right_other)

    def __radd__(self, left_other):
        raise_if_not([Item, ItemList], [TokenNode], left_other, self, "+")
        """Overload `+` from the right operand."""
        if not isinstance(left_other, ItemList):
            return ItemList(left_other) + self
        return ItemList(left_other, self)

    def __iadd__(self, other):
        """Overload `+=` operation."""
        raise_if_not([Item, ItemList], [TokenNode], left_other, self, "+")
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

    def __lt__(self, other):
        raise_if_not([str, Item, ItemList, CaseList], [TokenNode], other, self, "<")
        """Overload `<` from the left operand.  Reflected for '>'."""
        return handle_overloaded_lt_comparison(self, other)

    def __repr__(self):
        return "ItemList({0})".format(", ".join([str(i) for i in self.data_list]))


class CaseList(object):
    """A list of `Case` objects.  Note, though, that a single Item or ItemList can
    also be a case (when there are no "or" operations to form the case)."""
    def __init__(self, *args):
        """Take an arbitrary number of `ItemList` arguments and make a `CaseList`
        out of them.  Arguments can include `Item` instances and `TokenNode`
        subclasses."""
        # TODO: fix and uncomment this bug check after rest works.  Need to turn
        # off checking when doing operations inside < processing... Can't take
        # keyword arg with *args, at least not older Python, so maybe **kwargs.
        #if saved_comparison_args:
        #    raise ParserGrammarRuleException("Error converting to CaseList:"
        #            " intermediate arguments to '<' are still saved.  Check"
        #            " for unbalanced '<' and '>' symbols.")
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
        self.data_list.append(item)

    def insert(self, index, item):
        """Insert an item."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list.insert(index, data_item)

    def __getitem__(self, index):
        """Index and element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
        if index < 0: # Handle negative indices.
            index += len(self)
        return self.data_list[index]

    def __setitem__(self, index, value):
        """Set an element of the `ItemList`.  Negative indices are implemented,
        but slices are not."""
        if index < 0: # Handle negative indices.
            index += len(self)
        self.data_list[index] = value

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
        raise_if_not([Item, ItemList, CaseList], [TokenNode], other, self, "+")
        return CaseList(self, other)

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

    def __lt__(self, other):
        raise_if_not([str, Item, ItemList, CaseList], [TokenNode], other, self, "<")
        """Overload `<` from the left operand.  Reflected for '>'."""
        return handle_overloaded_lt_comparison(self, other)

    def __repr__(self):
        return "CaseList({0})".format(", ".join([str(i) for i in self.data_list]))

def add_caselists(c1, c2):
    """Not overloaded in the class, but this function will add caselists."""
    return CaseList(c1, c2)

def Rule(production_rule_label):
    """Return an `Item` to represent the rule with the string label
    `production_rule_label`."""
    # Only one string arg allowed.
    item = Item(production_rule_label)
    item.kind_of_item = "production"
    return item

def Tok(token):
    """Turn a token into an item.  Used before overloading defined on tokens."""
    # TODO: when compile handles these it needs to turn string values into
    # the actual tokens by looking them up.
    if not (isinstance(token, str) or is_subclass_of(token, TokenNode)):
        raise ParserGrammarRuleException("Bad argument {0} passed to the"
                " Tok function.".format(token))
    item = Item(token, allow_str=True)
    item.kind_of_item = "token" # Not needed.
    return item

def Root(item_init_arg, prec=None):
    """A function to designate that the token for the item should made into the
    root of the resulting parse subtree, if possible.  The prefix operator `~`
    is defined as a synonym.  Can only wrap an item."""
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

def Optional(*args):
    # Usually just one argument
    itemlist = ItemList(*args)
    itemlist[0].modifiers.insert(0, "Optional(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def OneOrMore(*args):
    # Usually just one argument
    itemlist = ItemList(*args)
    itemlist[0].modifiers.insert(0, "OneOrMore(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def ZeroOrMore(*args):
    # Usually just one argument
    itemlist = ItemList(*args)
    itemlist[0].modifiers.insert(0, "ZeroOrMore(")
    itemlist[-1].modifiers.append(")")
    return itemlist

def AnyOf(*args):
    # Maybe, give you a choice of possibilities from several.
    pass

#
# Handle overloading of '<' and '>' for expressions like: _<"string">_
#

inside_matched_pair = False
saved_comparison_args = []
locked = False

def handle_overloaded_lt_comparison(calling_instance, other):
    """Overload `<` from the left operand.  Reflected for the right operand."""
    # Reflection info: https://docs.python.org/3.1/reference/datamodel.html
    global inside_matched_pair, saved_comparison_args, locked

    if not inside_matched_pair: # Must be the < case not the > case.
        print("\ncalled < in", calling_instance.__class__.__name__)
        # === THREAD LOCK =========
        if not locked:
            lock.acquire()
            locked = True
        inside_matched_pair = True
        raise_if_not([str], [], other, calling_instance, "<")

        # Save the arguments for the closing > operator to use.
        saved_comparison_args += [calling_instance, Rule(other)]

        print("    saving", saved_comparison_args)
        return True # Always the l.h.s. of an "and" with the real value.

    else: # not inside_matched_pair
        print("\ncalled > in", calling_instance.__class__.__name__)
        inside_matched_pair = False # Must be done so we know next comparison is <.
        raise_if_not([str], [], other, calling_instance, ">")
        raise_if_not([Item, ItemList, CaseList], [], calling_instance, other, ">")
        print("   new 'calling_instance' value is:", calling_instance)
      
        # Save the arguments for the next < operator to use, but only if the
        # calling ItemList or CaseList on the right ends in a dummy Item (need
        # to save iff that is the case.  If not, release the lock and delete
        # the saved saved_comparison_args info.
        if not (isinstance(calling_instance, Item) or 
                isinstance(calling_instance, ItemList) or
                isinstance(calling_instance, CaseList)):
            raise ParserGrammarRuleException("BAD ASSUMPTION, not Item or ItemList"
                    "or CaseList. It is {0}.".format(calling_instance))

        if ((isinstance(calling_instance, ItemList) and
                            calling_instance[-1].kind_of_item == "dummy")
                or (isinstance(calling_instance, CaseList) and
                            calling_instance[-1][-1].kind_of_item == "dummy")):
            print("\n\nAPPENDING saved_comparison_args\n\n")
            #saved_comparison_args += [Rule(other), calling_instance]
            saved_comparison_args += [calling_instance]
            recovered_args = []
            return True
        else:
            print("\n\nCLEARING saved_comparison_args\n\n")
            #recovered_args = saved_comparison_args + [Rule(other), calling_instance]
            recovered_args = saved_comparison_args + [calling_instance]
            saved_comparison_args = []
            if locked:
                locked = False
                lock.release() # === THREAD UNLOCK ========
            return combine_comparison_overload_pieces(*recovered_args)

def combine_comparison_overload_pieces(*args):
    caselist = CaseList(*args) # Converts all to ItemList.
    return caselist
    i = -1
    while True: 
        i += 1
        print("\n====> caselist is:\n   ", caselist, "\n")
        if i == len(caselist):
            break
        if caselist[i][-1].kind_of_item == "dummy":
            # Remove the dummy items.
            del caselist[i][-1]
            caselist[i] = caselist[i] + caselist[i+1]

            for j in range(i, len(caselist)):
                if caselist[j][0].kind_of_item == "dummy":
                    del caselist[j][0]
                    caselist[i] = caselist[i] + caselist[j]
                    break
                caselist[i] = caselist[i] + caselist[j]

            for j in reversed(range(1,j+1)):
                del caselist[i+j]
            # Resume the loop.
            i -= 1
            continue
    return caselist

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
    then the message is for a method."""
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

#
# Exceptions.
#

class ParserGrammarRuleException(ParserException):
    """Exception raised by grammar classes."""
    pass

