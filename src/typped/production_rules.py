# -*- coding: utf-8 -*-
"""

Terminology:

* **Production rules** are also called **productions** or just **rules**.

* Production rules with the same l.h.s. or separated by the "or" symbol on a
  line are called different **cases** of the production.

* The separate symbol elements within a case are called the **items** of that case.

The order in which the production rules are written does not matter.  So rules
can be written top-down if that is easier to read, even though some of the
productions rules being called have not yet been defined.  This is achieved by
allowing the use of string labels in the r.h.s. of production rules.  These
strings are resolved later in when the `compile` method of the grammar is
called (passed the start state and a locals dict).  These strings **must** be
the same as the l.h.s. variable names for the rules since they are looked up in
the locals dict.

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
with most of the list operations overloaded.  The one major difference is in
the constructor.  They both take an arbitrary number of arguments, and make all
elements of the list after converting them to the appropriate type.  An
`ItemList` only holds `Item` instances, and a `CaseList` only holds `ItemList`
instances.  One difference from ordinary lists is that when an `ItemList` is
passed another `ItemList` in its initializer it just extends the list with the
elements of that list (and similarly for `CaseList`).  So these lists do not
nest.

Overloads for `Item` objects are:

   * `+` to produce an `ItemList`
   * `|` to convert both arguments to `ItemList` and return a `CaseList`

Overloads for `ItemList` objects are:

   * `+` to produce another `ItemList` simply combining the items
   * `|` to convert both arguments to `ItemList` and return a `CaseList`

Overloads for `CaseList` objects are:

   * `|` to convert both arguments to `CaseList` and then combine the items.

The `+` and `|` operators are defined for tokens in the `PrattParser` module to
simply return a tuple of the tokens.  That is sufficient, since all operations
other than addition 

The `+` operator has a higher precedence than the `|` operator.  So all the
additions within a case will always be carried out before any "or" operations.
So each argument to `|` will be either a single `Item` or a single `ItemList`.

Note that the resulting r.h.s. object which is set to the l.h.s. variable name
for a production rule can be a single `Item`, a single `ItemList`, a
`CaseList`, a `TokenNode` or a tuple of `TokenNodes`.  The `compile` method
converts them all into `CaseList` objects.  (It would be possible to overload
the `<<=` operator and use it instead of `=` to automatically do the
conversion, but that does not seem worth the extra notation.)

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

6. The head-handler for `term` calls `recursive_parse(0)`, which fetches the token
`y`.

7. HERE IS INTERESTING PART, iron out above and figure out how to make it work...

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

    def add_production_case(self, rule_label, case_item_list):
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

    def compile(self, parser, locals_dict=None):
        """Create the Pratt parser handlers in `parser` to parse the current
        grammar."""
        self.parser = parser
        for label, production in self.production_rules.items():
            print("\nregister production", production)
            parser.def_production_rule(label, self)

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

    def optimize_grammar_tree(self):
        """Do a search of the grammar tree and find lookahead tokens to
        make the parsing more efficient. Called from the `compile` method.
        NOT IMPLEMENTED."""
        # In future optimizations this routine could search the tree the
        # grammar to find the tokens for, say LL(1) grammars which can be used
        # to avoid the backtracking by using a lookahead.  They are relayed
        # back up the CFG tree after a recursive search down.  After they are
        # found they can be passed to the Pratt null-string handler function
        # (as an arg to def_production_rule).
        raise NotImplementedError("Not implemented.")

# TODO make sure all overloads use raise_if_not if they can

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

    def __getitem__(self, arg):
        """Use bracket-indexing as a shortcut for the `Prec` function."""
        return Prec(self, arg)

    def __call__(self, type_sig):
        """Use function call as a synonym for the `Sig` function."""
        # TODO: consider changing so that the arguments are automatically
        # passed to type_sig... since it will always be one.  But if it is
        # already one, then just use that (check isinstance).
        self.type_sig = type_sig
        return Sig(self, arg)

    def __repr__(self):
        if self.kind_of_item == "token":
            string = "Tok({0})".format(self.value.token_label)
        elif self.kind_of_item == "production":
            string = "<{0}>".format(self.value)
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
        return Root(self)

    def __lt__(self, other):
        """Overload `<` from the left operand.  Reflected for '>'."""
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
            return self + ItemList(right_other)
        return ItemList(self, right_other)

    def __radd__(self, left_other):
        raise_if_not([Item, ItemList], [TokenNode], left_other, self, "+")
        """Overload `+` from the right operand."""
        if not isinstance(left_other, ItemList):
            return ItemList(left_other) + self
        return ItemList(left_other, self)

    def __iadd__(self, other):
        """Overload `+=` operation."""
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
        """Overload `<` from the left operand.  Reflected for '>'."""
        return handle_overloaded_lt_comparison(self, other)

    def __repr__(self):
        return "ItemList({0})".format(", ".join([str(i) for i in self.data_list]))

# TODO: can catch many unbalanced by checking `CaseList.args_to_lt_saved==False` in
# all the overloaded ops like + and |.  Also check in conversions to CaseList to
# catch at the very end (compile) if everything converted to that.... In fact, why not
# just do that?

class CaseList(object):
    """A list of `Case` objects.  Note, though, that a single Item or ItemList can
    also be a case (when there are no "or" operations to form the case)."""
    def __init__(self, *args):
        """Take an arbitrary number of `ItemList` arguments and make a `CaseList`
        out of them.  Arguments can include `Item` instances and `TokenNode`
        subclasses."""
        # TODO: fix and uncomment this bug check after rest works, move vars to up above.
        #if args_to_lt_saved:
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
        self.item_list.append(item)

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
    item_list = ItemList(*args)
    item_list[0].modifiers.insert(0, "Optional(")
    item_list[-1].modifiers.append(")")
    return item_list

def OneOrMore(*args):
    # Usually just one argument
    item_list = ItemList(*args)
    item_list[0].modifiers.insert(0, "OneOrMore(")
    item_list[-1].modifiers.append(")")
    return item_list

def ZeroOrMore(*args):
    # Usually just one argument
    item_list = ItemList(*args)
    item_list[0].modifiers.insert(0, "ZeroOrMore(")
    item_list[-1].modifiers.append(")")
    return item_list

def AnyOf(*args):
    # Maybe, give you a choice of possibilities from several.
    pass

##
## Handle overloading of '<' and '>' for expressions like: _<"string">_
##
#
#args_to_lt_saved = False
#args_to_lt = []
#args_to_gt = []
#
## TODO TODO try implementing in the simpler "save all to one list"
## way described up in the top docstring text.
#
#def handle_overloaded_lt_comparison(calling_instance, other):
#    """Overload `<` from the left operand.  Reflected for the right operand."""
#    # Reflection info: https://docs.python.org/3.1/reference/datamodel.html
#    global args_to_lt, args_to_gt
#    if not args_to_lt: # Must be the < case not the > case.
#        print("called < in", calling_instance.__class__.__name__)
#        raise_if_not([str], [], other, calling_instance, "<")
#
#        # See if the previous > operator saved any arguments which need to be used.
#        # If not, free the lock.
#        if args_to_gt:
#            recovered_args_from_gt = args_to_gt
#            args_to_gt = []
#            #print("recovered these args from gt:", recovered_args_from_gt)
#            # Keep lock; need to process the saved args from gt.
#        else:
#            recovered_args_from_gt = []
#            lock.acquire() # === THREAD LOCK =========
#
#        # Save the arguments for the closing > operator to use.
#        args_to_lt += recovered_args_from_gt + [calling_instance, Rule(other)]
#        # TODO below is less info, maybe better... still out of order...
#        # args_to_lt = recovered_args_from_gt + [calling_instance, Rule(other)]
#
#        print("    saving", args_to_lt)
#        return True # Always the l.h.s. of an "and" with the real value.
#
#    else:
#        print("called > in", calling_instance.__class__.__name__)
#        raise_if_not([str], [], other, calling_instance, ">")
#        raise_if_not([Item, ItemList, CaseList], [], calling_instance, other, ">")
#        #print("   recovering saved args", args_to_lt)
#        print("   new 'calling_instance' value is:", calling_instance)
#        if not args_to_lt:
#            raise ParserGrammarRuleException("No saved arguments for '>'."
#                    " Missing a matching '<' before it?")
#       
#        # Save the arguments for the next < operator to use, but only if the
#        # calling ItemList or CaseList on the right ends in a dummy Item (need
#        # to save iff that is the case.  If not, release the lock and delete
#        # the saved args_to_gt info.
#        if not (isinstance(calling_instance, Item) or 
#                isinstance(calling_instance, ItemList) or
#                isinstance(calling_instance, CaseList)):
#            raise ParserGrammarRuleException("BAD ASSUMPTION, not Item or ItemList"
#                    "or CaseList. It is {0}.".format(calling_instance))
#
#        if isinstance(calling_instance, ItemList):
#            print("\n\nItemList end thing is:", calling_instance[-1])
#            print("Its kind_of_item is", calling_instance[-1].kind_of_item, "\n\n")
#
#        if (#(isinstance(calling_instance, Item) and
#            #                calling_instance.kind_of_item == "dummy") or
#                (isinstance(calling_instance, ItemList) and
#                            calling_instance[-1].kind_of_item == "dummy")
#                or (isinstance(calling_instance, CaseList) and
#                            calling_instance[-1][-1].kind_of_item == "dummy")):
#            print("\n\nAPPENDING args_to_gt\n\n")
#            args_to_gt += args_to_lt + [Rule(other), calling_instance]
#        else:
#            # TODO this is never called????  Only APPENDING shows up...
#            print("\n\nCLEARING args_to_gt\n\n")
#            args_to_gt = [] 
#
#        # Recover the arguments that the preceeding < operator saved.
#        recovered_args_from_lt = args_to_lt
#        args_to_lt = [] # Must be done so we know next comparison is <.
#
#        if not args_to_gt:
#            lock.release() # === THREAD UNLOCK ========
#
#        args_to_combine = recovered_args_from_lt + [Rule(other), calling_instance]
#        retval = combine_comparison_overload_pieces(*args_to_combine)
#        print("   returning:", retval)
#        return retval # The real value.
#
#def combine_comparison_overload_pieces(*args):
#    # First convert left and right to CaseList.
#    #left_case = CaseList(left_piece, ignore_lt_saved=True)
#    #right_case = CaseList(right_piece, ignore_lt_saved=True)
#    #del left_case[-1][-1] # Dummy Item for `_`
#    #del right_case[0][0] # Dummy Item for `_`
#    #middle_itemlist = left_case[-1] + ItemList(rule) + right_case[0]
#    #middle_itemlist = CaseList(rule)
#    #print("   args in combine are ============>", args)
#    #del left_case[-1]
#    #del right_case[0]
#    retval = CaseList(*args, ignore_lt_saved=True)
#    #print("   combined caselist is ====>", retval)
#    return retval

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

