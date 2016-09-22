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

The basic objects that make up rule definitions are `Item` objects,
`ItemList` objects, and `CaseList` objects.

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
last thing evaluated and will split the final `CaseList` like if someone
arbitrarily cut up a string of balanced parentheses.  It is always possible to
uniquely reassemble such a collection of sublists, and the comparison operators
essentially need to do that.

In order for this to work the comparison operators need to be defined for all
three of the classes above.  The comparison operators need to convert the
string in the middle into the `Item` for an rule and then reassemble the three
objects in the correct way.  We know, though, that the rightmost `DummyItem` in
the left object needs to join the leftmost `DummyItem` in the right object and
be replaced by the `Item` for a rule corresponding to the string.

Note that all non-dummy arguments can be promoted to `CaseList` to reduce the
number of special cases.  So the methods of all classes but `CaseList` can
simply promote their arguments and return the comparison of those items.

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
then non-chained comparisons are broken.  We don't care about the boolean
values at all, though, so the first comparison can always be true.  We only
want to join things together and return the combination.

In this case, since the middle object is a string and the ordering of
operations is fixed, we only need to define the `<` operation, since the `>`
object will also use that.  (The string type does not have `>` defined for the
type so the reflection will be used, see
https://docs.python.org/2/reference/datamodel.html).

So the first comparison saves its arguments to a fixed location, then the next
comparison fetches that value and can use that (and its own arguments) to get
all three parts.  It then reassembles them and returns the result.

TODO ======> See file in my scratch directory, last case.  This WORKS.  Not
too complicated to code (more complicated to explain).

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
    # No test file for now, just run the parser's tests.
    pytest_helper.script_run(self_test=True, pytest_args="-v")

import sys
from .shared_settings_and_exceptions import ParserException, is_subclass_of

from .pratt_types import TypeSig, TypeObject, NONE
from .lexer import TokenNode

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

class Item(object):
    """Class representing the basic elements that make up the cases of the
    production rules."""
    
    def __init__(self, value=None):
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
        elif isinstance(value, str): # A string production rule label.
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
                print("only one", arg_list)
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
        return ItemList(self, right_other)

    def __radd__(self, left_other):
        return ItemList(left_other, self)

    def __or__(self, right_other):
        return CaseList(ItemList(self), ItemList(right_other))

    def __ror__(self, left_other):
        return CaseList(ItemList(left_other), ItemList(self))

    def __invert__(self):
        return Root(self)

class ItemList(object):
    """A list of `Item` instances."""
    def __init__(self, *args):
        item_list = []
        for a in args:
            if isinstance(a, Item):
                item_list.append(a)
            elif isinstance(a, ItemList):
                item_list.extend(a.item_list)
            else:
                item_list.append(Item(a)) # Assume can be made an item.
        self.item_list = item_list

    def __getitem__(self, index):
        return self.item_list[index]

    def __len__(self):
        return len(self.item_list)

    def __add__(self, right_other):
        return ItemList(self, right_other)

    def __radd__(self, left_other):
        return ItemList(left_other, self)

    def __or__(self, right_other):
        if isinstance(right_other, Item):
            return CaseList(self, ItemList(right_other))
        elif isinstance(right_other, ItemList):
            return CaseList(self, right_other)

    def __ror__(self, left_other):
        if isinstance(left_other, Item):
            return CaseList(ItemList(left_other), self)
        elif isinstance(left_other, ItemList):
            return CaseList(left_other, self)

    def __repr__(self):
        return "ItemList({0})".format(", ".join([str(i) for i in self.item_list]))

class CaseList(object):
    """A list of `Case` objects.  Note, though, that a single Item or ItemList can
    also be a case (when there are no "or" operations to form the case)."""
    def __init__(self, *args):
        """Take an arbitrary number of `ItemList` arguments and make a `CaseList`
        out of them."""
        self.list_of_item_lists = []
        for a in args:
            if isinstance(a, CaseList):
                self.list_of_items.extend(a)
            elif isinstance(a, ItemList):
                self.list_of_item_lists.append(a)
            else:
                self.list_of_item_lists.append(ItemList(a))

    def __getitem__(self, index):
        return self.list_of_item_lists[index]

    def __len__(self):
        return len(self.list_of_item_lists)

    def __repr__(self):
        return "CaseList({0})".format(", ".join([str(i) for i in self.list_of_item_lists]))

    def __or__(self, right_other):
        # Could probably just make a CaseList out of everything now, since the
        # __init__ handles all the cases.
        if isinstance(right_other, Item):
            return self | ItemList(right_other)
        elif isinstance(right_other, ItemList):
            return self | CaseList(right_other)
        elif isinstance(right_other, CaseList):
            self.list_of_item_lists.extend(right_other.list_of_item_lists)
            return self

    def __ror__(self, left_other):
        if isinstance(left_other, Item):
            return ItemList(left_other) | self
        elif isinstance(left_other, ItemList):
            return CaseList(left_other) | self
        elif isinstance(left_other, CaseList):
            left_other.list_of_item_lists.extend(self.list_of_item_lists)
            self.list_of_item_lists = left_other.list_of_item_lists
            return self

def Rule(production_rule_label):
    """Return an `Item` to represent the rule with the string label
    `production_rule_label`."""
    # Only one string arg allowed.
    item = Item(production_rule_label)
    item.kind_of_item = "production"
    return item

def Tok(token):
    """Turn a token into an item.  Used before overloading defined on tokens."""
    # TODO: later allow token labels, too.  Just return a token item with an
    # extra attribute such as is_token_label.  The compile can look up.
    item = Item(token)
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
    item_list.item_list[0].modifiers.insert(0, "Optional(")
    item_list.item_list[-1].modifiers.append(")")
    return item_list

def OneOrMore(*args):
    # Usually just one argument
    item_list = ItemList(*args)
    item_list.item_list[0].modifiers.insert(0, "OneOrMore(")
    item_list.item_list[-1].modifiers.append(")")
    return item_list

def ZeroOrMore(*args):
    # Usually just one argument
    item_list = ItemList(*args)
    item_list.item_list[0].modifiers.insert(0, "ZeroOrMore(")
    item_list.item_list[-1].modifiers.append(")")
    return item_list

def AnyOf(*args):
    # Maybe, give you a choice of possibilities from several.
    pass

#
# Exceptions.
#

class ParserGrammarRuleException(ParserException):
    """Exception raised by grammar classes."""
    pass

#
# Tests.
#

import pytest_helper
pytest_helper.autoimport()
#from .pratt_parser import PrattParser
import typped as pp

def def_expression_tokens_and_literals(parser):
    #
    # Define the tokens.
    #

    # Operators.
    k_plus = parser.def_token("k_plus", r"\+")
    k_minus = parser.def_token("k_minus", r"\-")
    k_fslash = parser.def_token("k_fslash", r"/")
    k_ast = parser.def_token("k_ast", r"\*")

    # Grouping.
    k_lpar = parser.def_token("k_lpar", r"\(")
    k_rpar = parser.def_token("k_rpar", r"\)")

    # Numbers.
    k_number = parser.def_token("k_number", r"[0-9]+")

    #
    # Define the literals (terminals).
    #

    literals_list = [
            ("k_plus",),
            ("k_minus",),
            ("k_fslash",),
            ("k_ast",),
            ("k_lpar",),
            ("k_rpar",),
            ("k_number",),
            ]
    parser.def_multi_literals(literals_list)
    pytest_helper.locals_to_globals()

def test_basic_expression_grammar():
    #skip()

    parser = pp.PrattParser()
    parser.def_default_whitespace()

    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #

    g = Grammar()

    g.add_production_case("factor", [k_number])
    g.add_production_case("factor", [k_lpar, Rule("expression"), k_rpar])

    g.add_production_case("term", [Rule("factor"), k_ast, Rule("factor")])
    g.add_production_case("term", [Rule("factor"), k_fslash, Rule("factor")])
    g.add_production_case("term", [Rule("factor")])


    g.add_production_case("expression", [Rule("term"), k_plus, Rule("term")])
    g.add_production_case("expression", [Rule("term"), k_minus, Rule("term")])
    g.add_production_case("expression", [Rule("term")])

    #
    # Parse some expressions.
    #

    g.compile(parser)

    #parser.pstate_stack = ["expression"]
    print()
    print()
    simple_example = parser.parse("4*4", pstate="expression").tree_repr(3)
    print(simple_example)
    simple_string_rep_example = parser.parse("4*4", pstate="expression").string_tree_repr()
    print(simple_string_rep_example)
    assert str(simple_string_rep_example) == ("<k_null-string,'expression'>("
                         "<k_null-string,'term'>(<k_null-string,'factor'>("
                         "<k_number,'4'>),<k_ast,'*'>,<k_null-string,'factor'>("
                         "<k_number,'4'>)))")
    #fail()

def test_overload_expression_grammar():
    #skip()
    parser = pp.PrattParser()
    parser.def_default_whitespace()

    def_expression_tokens_and_literals(parser)

    #
    # Define the grammar.
    #
   
    print("token number now looks like this", k_number)
    t_float = parser.def_type("t_float")
    none_sig = pp.TypeSig(t_float, None)

    tok_str = Tok(k_number)
    assert tok_str.kind_of_item == "token"
    print(str(  Tok(k_number)[4]  ))
    print(str(  k_number[10] + Rule("factor")  ))
    print(str(  Tok(k_number) + Sig(Rule("factor"), none_sig) | Rule("term")  ))
    print(str(  Tok(k_number) + ~Rule("factor") | Root(Rule("term")) | ~Pratt() | k_number  ))
    print(str(  k_number | k_number | Root(Rule("term")) | Pratt()  ))
    print(str(  k_number + k_number | k_number | Root(Rule("term")) | ~k_plus  ))

    wff = ( Rule("wff") + ~k_plus[10] + Rule("wff")
          | Rule("wff") + ~k_ast[20]  + Rule("wff")
          | Optional(Rule("wff") + Optional(k_plus + k_plus))
          )
    print(str(wff))

    fail()

