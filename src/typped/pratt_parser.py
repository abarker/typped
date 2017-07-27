# -*- coding: utf-8 -*-
"""

A general Pratt parser module that uses dispatching of handler functions and
can check types.  The API is documented here.  See the general Sphinx
documentation for Typped for how to use the class and examples.

API details
===========

These are some aspects of the API that are not covered by the function signatures
and docstrings below.

Extra attributes added to tokens
--------------------------------

Token instances straight from a `Lexer` instance have certain attributes set,
as documented in the `lexer` module.  In particular, the `token_label`,
`value`, and `children` attributes are commonly used.  The `pratt_parser`
module defines its own subclass of the `TokenNode` class, which additionally
assigns some extra attributes when tokens are defined.  The parsing process
also sets several user-accessible attributes.

Attributes set on `TokenNode` subclasses (representing kinds of tokens):

* `token_label`

Attributes set on token instances (scanned tokens) during parsing:

* `parser_instance` -- the parser instance that parsed the token
* `original_formal_sig` -- a `TypeSig` instance of the resolved original formal signature
* `expanded_formal_sig` -- a `TypeSig` instance of the expanded formal signature
* `actual_sig` -- a `TypeSig` instance of the actual signature
* `construct_label` -- the string label of the winning construct

Note that both `original_formal_sig` and `expanded_formal_sig` are set to the
string `"Unresolved"` before the token is parsed.  The actual signature is
found during parsing and type-checking.  Out of all possible overloads in the
original formal signatures associated with the token (via `modify_token`) the
one which matches the actual arguments is chosen.  The expanded formal
signature is the same as the original formal signature except that wildcards,
etc., are expanded in the attempt to match the actual arguments.

These two attributes are actually properties which look up the value if
necessary (to avoid unnecessary lookups during parsing).  They both only
work after parsing, since they use the `original_formal_sig` to look up
the corresponding data or function.

* `ast_data` -- any AST data that was set with the construct for the resolved type
* `eval_fun` -- any eval_fun that was set with the construct for the resolved type

Optional attributes that can be set to a node inside a handler:

* `not_in_tree` -- set on a root node returned by the handler to hide it
* `process_and_check_kwargs` -- kwargs dict to pass to type-checking routine

Implementation details
======================

This section gives a general overview of the lower-level details of the
`PrattParser` implementation.

The basic class structure
-------------------------

TODO: Update diagram to and discussion to have ConstructTable.

There are five basic classes, with instances which interact.  The main class is
the `PrattParser` class, which users will mostly interact with.  The overall
relationships are shown in this image, with discussion below.

.. image:: relationshipsBetweenMainClasses.svg
    :width: 600px
    :align: center

The next three classes are defined in the lexer module, although one is
redefined here.  They are the `TokenSubclass`, `TokenTable`, and `Lexer`
classes.

A `Lexer` instance is always initialized with a `TokenTable` instance, whether
it is passed-in as an argument or created internally as an empty token table.
A `PrattParser` instance always creates its own token table and then passes
that to the lexer, which it also creates.

Every `PrattParser` instance contains a fixed `TokenTable` instance, which
never changes (except for the tokens in it).  So each token-table created by a
parser can save a pointer back to the parser which "owns" it.  Each
`PrattParser` instance also contains a `Lexer` instance, which contains
a pointer to a parser instance (so the lexer can access the parser).

The `TokenSubclass` class is a subclass of the `TokenNode` class (which is
defined in the lexer module).  The subclassing adds many additional methods and
attributes which are needed in the parsing application.  The `TokenSubclass`
class is actually defined inside a factory function, called
`token_subclass_factory`, which produces a different subclass to represent each
kind of token that is defined (tokens are defined via the `def_token` method of
`PrattParser`).  Instances of those subclasses represent the actual tokens
(i.e., tokens scanned and returned by the lexer containing individual
text-string values).

A `TokenTable` instance is basically a dict for holding all the defined
token-subclasses.  But it also has related methods and attributes associated
with it.  It is where all new tokens are ultimately created and defined, for
example (although other classes like the parser class can add extra attributes
to the created tokens).

A `TokenTable` instance contains all the tokens defined for a language, and
stays with the `PrattParser` instance which created it (from which the tokens
were necessarily defined).  A `Lexer` instance can use different `TokenTable`
instances, possibly switching on-the-fly.  A lexer instance always has a
pointer to its *current* token-table instance, but that can change on-the-fly
(such as when separate parsers are swapped in to parse sub-languages in the
same text stream).  This is used when parser instances call other parser
instances.

Tokens defined by a parser also save a pointer to their defining parser, since
the token-table has a fixed association to the parser.

Tokens also need to know their current lexer instance because they need to call
the `next` and `peek` methods, if nothing else.  This is equivalent to the
token table knowing its current lexer instance.  So, whenever a token table is
associated with a lexer using the lexer's `set_token_table` method it is also
given a pointer to that lexer as an attribute.

The final class of the five is the `TypeTable` class.  This is essentially a
dict to store all the defined types, but it also provides a nice place to
define many methods for acting on types.  It is defined in the `pratt_types`
module and imported.

Using different parsers inside handler functions
------------------------------------------------

It is useful to be able to call different `PrattParser` instances from inside
handler functions in order to parse subexpressions which are defined as
sublanguages, having their own parsers.  The implementation supports this as
follows.

Essentially, a common lexer is passed around and told which token table (and
hence parser) to use at any given time.  It would be possible to pass around a
text stream of unprocessed text, but then the lexers would need to be
initialized each time, and saving information like line numbers and columns in
the text would need to move to the text stream object.

The `parse` routine of a `PrattParser` takes an optional lexer argument, which
will be used instead of the default lexer.  But, it first temporarily sets the
`TokenTable` instance of the lexer to be the same as the token table instance
of the *current* parser (using the lexer's `set_token_table` method).  So you
can call the `parse` method of a *different* parser instance from within a
handler function, passing that other parser's `parse` function the *current*
parser's lexer as an argument.  (Recall that handler functions are associated
with tokens, but defined in the context of a parser/token-table.) So the lexer
will use the token table of the new parser but still read from the same text
stream as the current parser.  Note that a sublanguage must always be parsed
from the beginning, so `parse` must be called.  When this parser fails (with
certain exceptions? needed with multi-expression?) the subexpression is assumed
to be parsed, and the symbol table of the lexer is restored to the symbol table
of the current parser (again using the lexer's `set_token_table` method).

Consider: Is the condition that prec=0, or only a head handler, sufficient to
assume the end???  Does normal multi-expression work?  WHAT CONDITIONS DOES
THIS IMPOSE ON THE SUBLANGUAGES?

Note that the parser or the lexer can determine when a sublanguage expression
ends: either the lexer doesn't recognize a token or else the parser cannot find
a handler, or a handler fails to find what it expects and raises an exception.

What if the lexer keeps a stack of symbol tables, and pops one off whenever it
fails?  Still, you could have lookahead which is correctly lexed in the top
language but then fails to match a handler function.

Code
====

In reading the code, the correspondence between the naming convention used here
and Pratt's original naming conventions is given in this table:

+----------------------------------+--------------------------+
| This code                        | Pratt's terminology      |
+==================================+==========================+
| token precedence                 | left binding power, lbp  |
+----------------------------------+--------------------------+
| subexpression precedence         | right binding power, rbp |
+----------------------------------+--------------------------+
| head handler function            | null denotation, nud     |
+----------------------------------+--------------------------+
| tail handler function            | left denotation, led     |
+----------------------------------+--------------------------+

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_production_rules.py",
                              "../../test/test_example_calculator.py",
                              "../../test/test_parser_called_from_parser.py",
                              "../../test/test_pratt_types.py",
                              "../../test/test_basic_usage_section_examples.py",
                              "../../test/test_pratt_parser.py",
                              ], pytest_args="-v")

import sys
import copy

from .shared_settings_and_exceptions import (HEAD, TAIL, ParserException,
        NoHandlerFunctionDefined, CalledBeginTokenHandler, CalledEndTokenHandler)
from .lexer import Lexer, TokenNode, TokenTable
from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage
from .pratt_constructs import ConstructTable
from .matcher import Matcher
from . import builtin_parse_methods, predefined_token_sets

# TODO: clarify when tokens are assigned the parser_instance attribute, if they
# are at all.  Currently the lexer is passed a function hook that adds the
# parser instance associated with the lexer's current token table to every token
# as an attribute.  Seems OK, including for parsers calling parsers, but consider
# and update docs and comments where not yet changed.

# NOTE that the eval_fun stuff could also be used to also do a conversion to
# AST.

# TODO: Consider allowing a string label of some sort when defining a parser,
# something like "TermParser" or "parser for terms", "WffParser", etc.  Then
# use that string in the exception messages.  When working with parsers called
# from/by parsers these labels in error messages would be helpful for debugging.

# TODO: Consider subclassing the TokenNode class, and then just pass a
# subclassed one to the lexer...  Then the def_token stuff are just convenience
# functions.  Makes it easier to define tokens outside of a particular parser,
# so they could be shared (like have a set of common default tokens, for
# example).  May be conceptually clearer, too.

# Later, consider serialization of defined parsers, such as with JSON or (at
# least) pickle http://www.discoversdk.com/blog/python-serialization-with-pickle
# If a TokenTable is made to fully define a parser then you only need to save that...
# but you need to clutter it with non-token data.

# The default construct label is a tuple so it never matches an actual string.
DEFAULT_CONSTRUCT_LABEL = ("always-true-default-precondition",)

def DEFAULT_ALWAYS_TRUE_PRECOND_FUN(lex, lookbehind):
    """The default precondition function; always returns true."""
    return True

#
# TokenNode
#

"""The `TokenNode` base class is defined in `lexer.py`.  It contains some of
the basic, general methods that apply to tokens and nodes in token trees.
Methods particular to an application need to be defined in a subclass.  The
function `token_subclass_factory` returns a subclass of `TokenNode` which
represents tokens with a given token label.  The `PrattParser` class sets this
function to be used by its `TokenTable` instance in order to
create a token subclass for each kind of token.  Many methods particular to the
`PrattParser` application are added to the subclass."""

# TODO: Since TokenNode now has a metaclass anyway, why not just use it to
# generate the new instances instead of the factory function?  The factory
# function is maybe simpler, but Sphinx does not properly document the
# TokenNode class defined inside it.
#
# Alternative: Define the base class in module scope in the usual way, and
# then have the factory function just subclass it again (from some trivial
# stub) to represent particular kinds of tokens.

class TokenSubclassMeta(type):
    """A trivial metaclass that will actually create the `TokenSubclass`
    objects.  Since tokens are represented by classes, rather than instances,
    this is necessary in order to change their `__repr__` (the defalt one is
    ugly for tokens) and to overload operators to work for token operands
    in the EBNF-like grammar."""
    def __new__(mcs, name, bases, dct):
        new_class = super(TokenSubclassMeta, mcs).__new__(mcs, name, bases, dct)

        # Below is ugly, but avoids mutual import problems.  Used as an easy
        # way to define token addition so that it works in the grammars defined
        # by the production_rules module.
        from .production_rules import (Tok, Not, Prec, nExactly, nOrMore, Repeat)
        # These are saved in a dict below because if they are made attributes
        # then Python 2 complains about "TypeError: unbound method Tok() must
        # be called with TokenClass_k_lpar instance as first argument (got
        # TokenSubclassMeta instance instead)".
        new_class.prod_rule_funs = {
                "Tok": Tok,
                "Not": Not,
                "Prec": Prec,
                "nExactly": nExactly,
                "nOrMore": nOrMore,
                "Repeat": Repeat,
                }
        return new_class

    #
    # Define a nicer-looking __repr__, since the classes represent tokens.
    #

    def __repr__(cls):
        """The representation for tokens.  Tokens are commonly used in the
        code but, being classes, have an ugly default `__repr__` when printed out.
        In this metaclass we can define a better `__repr__` for tokens."""
        string = "TokenClass_{0}".format(cls.token_label)
        return string

    #
    # These overloads work with the production_rules module.
    #

    # TODO: Could have a flag to turn off overloads, maybe... tokens know
    # their parser_instance at parse time, so they can look at instance.  If
    # Grammar is passed a parser and initialized before it could turn on
    # overloads and compile could turn them off.  Kind of restrictive,
    # though.  But could just put calls to a function to check and raise an
    # error... Catches accidental uses, and can suggest maybe they need to
    # init Grammar first...  These all return Item or related anyway,
    # should catch most accidentals just because of that.

    def __add__(cls, other):
        """Addition of two tokens is defined to simply return a tuple of
        both tokens.  This is so raw tokens can be used in the operator
        overloaded form of defining production rules for a grammar."""
        return cls.prod_rule_funs["Tok"](cls) + other

    def __radd__(cls, left_other):
        """The right version of `__add__` above."""
        return left_other + cls.prod_rule_funs["Tok"](cls)

    def __or__(cls, other):
        """The `|` symbol simply converts this object into an `ItemList`
        and then calls `__or__` for those objects."""
        return cls.prod_rule_funs["Tok"](cls) | other

    def __ror__(cls, left_other):
        """The right version of `__or__` above."""
        return left_other | cls.prod_rule_funs["Tok"](cls)

    def __rmul__(cls, left_other):
        """The expression `n*token` for an int `n` is "n occurrences of"
        `token`."""
        return cls.prod_rule_funs["Repeat"](left_other, cls)

    def __rpow__(cls, left_other):
        """The expression `n**token` for an int `n` is "n or more occurrences of"
        `token`."""
        return cls.prod_rule_funs["nOrMore"](left_other, cls)

    def __invert__(cls):
        """Define the `~` operator for production rule grammars."""
        return cls.prod_rule_funs["Not"](cls)

    def __getitem__(cls, arg):
        """Define the bracket indexing operator for production rule grammars
        to set the precedence."""
        return cls.prod_rule_funs["Prec"](cls, arg)

def token_subclass_factory():
    """This function is called from the `create_token_subclass` method of
    `TokenTable` when it needs to create a new subclass to begin
    with.  It should not be called directly.

    Create and return a new token subclass which will be modified and used
    to represent a particular kind of token.  Specifically, each scanned token
    matching the regex defined for tokens with a given token label is
    represented as an instance of the subclass created by calling this function
    (with further attributes, such as the token label, added to it).

    Using a separate subclass for each token label allows for attributes
    specific to a kind of token (including head and tail handler methods) to
    later be added to the class itself without conflicts.  This function
    returns a bare-bones subclass without any head or tail functions, etc."""

    class TokenSubclass(TokenSubclassMeta("TokenSubclass", (object,), {}), TokenNode):
    #class TokenSubclass(TokenNode, metaclass=TokenSubclassMeta):  # Python 3
    #class TokenSubclass(TokenNode):                               # No metaclass.
        """The factory function returns this class to represent tokens.  It is a
        subclass of the `TokenNode` class, defined in the lexer module."""
        static_prec = 0 # The prec value for this kind of token, with default zero.
        token_label = None # Set to the actual value later, by create_token_subclass.
        #parser_instance = None # Set during parsing by recursive_parse. AVOID if not needed.

        def __init__(self, value):
            """Initialize an instance of the subclass for a token of the kind
            labeled with `token_label`.  The `value` is the actual parsed
            string from the text.  This instance represents the token."""
            super(TokenSubclass, self).__init__() # Call base class __init__.
            self.value = value # Set from lex.token_generator; static value=None.

            # The `expanded_formal_sig` attr is set upon parsing.  It is the
            # expanded form of one of the formal sigs which were registered
            # with the token (the expansion handles wildcards, repeat options,
            # etc.)  It is the signature which matched the actual args.  The
            # `self.expanded_formal_sig` has an attribute `original_formal_sig`
            # which is the original, unexpanded formal signature that matched
            # (after expansion) the actual arguments.  After parsing it is set
            # as the token attribute `original_formal_sig`.  This is important
            # because the `eval_fun` and `ast_data` for tokens are saved in
            # dicts keyed by the original signature at the time when they are
            # defined (via `modify_token`).  So we need the resolved original
            # signature to look up the value.  (Note that if overloading on
            # return types is turned off then these dicts are keyed only on the
            # argument portion of the original typesig.)
            self.original_formal_sig = "Unresolved" # The matching orig sig.
            self.expanded_formal_sig = "Unresolved" # Expanded version of above.
            self.actual_sig = "Unresolved" # The actual signature.

        @classmethod
        def prec(cls):
            """Return the precedence for the token.  This is currently a static
            value for each type of token.  Later it may be dynamic value
            associated with the particular tail function which is selected in a
            given context."""
            return cls.static_prec

        def dispatch_handler(self, head_or_tail, lex, left=None, lookbehind=None):
            """Dispatch a callable function what will work as a handler.  The
            function also does type-checking after running the defined handler."""
            # TODO: later consider calling not as token method....
            return self.parser_instance.construct_table.dispatch_handler(
                                        head_or_tail, self, lex, left, lookbehind)

        def process_not_in_tree(self):
            """Removes any immediate children which have `not_in_tree` set."""
            modified_children = []
            for child in self.children:
                if not hasattr(child, "not_in_tree"):
                    modified_children.append(child)
                else:
                    modified_children += child.children
            self.children = modified_children

        def process_and_check_node(self, construct,
                                   val_type_override=None, all_vals_override=None,
                                   typesig_override=None):
            """This routine is automatically called just after a handler
            function returns a subtree.  It is called for the root of the
            returned subtree.  It sets some attributes and checks that the
            actual types match some defined type signature for the function.

            If `val_type_override` is set to a `TypeObject` instance then the
            return type of both the `expanded_formal_sig` and the `actual_sig`
            for the node is set to that type *after* the first pass of
            processing and type-checking for the subtree is finished.  (This
            option does not currently work for two-pass checking when
            overloading on return types is used.)  This option is useful for
            handling things like parentheses and brackets which inherit the
            type of their child (assuming the parens and brackets are kept as
            nodes in the parse tree and not eliminated).  The override value is
            used for type-checking farther up the expression tree.

            If `all_vals_override` is set to a `TypeObject` instance then all
            the possible signatures have their `val_type` changed to that type
            *before* any checking is done.  This should work for both one and
            two-pass checking, but no check is made for producing duplicate
            types due to the `val_type` changes.

            The `typesig_override` argument must be a `TypeSig` instance.  If
            set it will be *assigned* to the node as its only possible
            `expanded_formal_sig` before type checking (instead of looking up
            the registered ones).  Type checking then proceeds as usual."""

            # Get all the sigs registered for the node's construct.
            if typesig_override is not None:
                all_possible_sigs = [typesig_override]
            else:
                all_possible_sigs = construct.original_sigs
            if all_vals_override:
                all_possible_sigs = [TypeSig(all_vals_override, s.arg_types)
                                                      for s in all_possible_sigs]
            self.all_possible_sigs = all_possible_sigs # Temporary attribute.

            # Do the actual checking.
            if not self.parser_instance.overload_on_ret_types: # One-pass.
                self._check_types_one_pass()
            else: # Two-pass.
                self._check_types_first_of_two_passes()
                # If we have a *unique* matching sig, run pass two on the
                # subtree.  In this case, since the signature is fixed by
                # argument types (regardless of where the top-down pass
                # starts from) we can in this case resolve the types in the
                # subtree early.  Note `check_types_in_tree_second_pass` is
                # also called on the root node from the `parse` method.
                if len(self.matching_sigs) == 1: # matching_sigs set by _check_types
                    self.check_types_in_tree_second_pass()

            # Implement val_type override if set.
            if val_type_override:
                # Note that the original_formal_sig attribute was already set.
                # The ast_data and eval_fun are keyed on that.
                self.expanded_formal_sig = TypeSig(val_type_override,
                                                   self.expanded_formal_sig.arg_types)
                self.actual_sig = TypeSig(val_type_override, self.actual_sig.arg_types)

        def _check_types_one_pass(self):
            """Utility function called from `process_and_check_node` to check
            the actual types against their signatures when overloading is only on
            argument types."""
            all_possible_sigs = self.all_possible_sigs

            # One-pass, each child c has a unique c.expanded_formal_sig already set.
            list_of_child_sig_lists = [[c.expanded_formal_sig] for c in self.children]

            # Reduce to only the signatures that the types of the children match.
            self.matching_sigs = TypeSig.get_all_matching_expanded_sigs(
                                      all_possible_sigs, list_of_child_sig_lists,
                                      tnode=self)
            # Below all_possible_sigs is saved ONLY for printing error messages.
            self.all_possible_sigs = all_possible_sigs

            # No overloading on return types so we can finalize the actual types.
            if len(self.matching_sigs) != 1:
                self._raise_type_mismatch_error(self.matching_sigs,
                        "Ambiguous type resolution: The actual argument types match"
                        " multiple signatures.")

            # Found a unique signature; set the node's expanded_formal_sig attribute.
            # Saved sig used for eval_fun resolution, ast_data, semantic action, etc.
            self.expanded_formal_sig = self.matching_sigs[0]
            self.original_formal_sig = self.expanded_formal_sig.original_formal_sig
            self.actual_sig = TypeSig(self.expanded_formal_sig.val_type,
                                      [c.actual_sig.val_type for c in self.children])

            # Delete some temporary attributes no longer needed.
            delattr(self, "matching_sigs")
            delattr(self, "all_possible_sigs")

        def _check_types_first_of_two_passes(self):
            """Utility function called from `process_and_check_node` to do the
            first pass in checking the actual types against their signatures.
            Two-pass checking is needed for overloading on return types.  First
            pass goes up the tree, second pass goes back down."""
            all_possible_sigs = self.all_possible_sigs

            # First pass case, multiple sigs in child's self.matching_sigs list.
            list_of_child_sig_lists = [c.matching_sigs for c in self.children]

            # Reduce to only the signatures that the types of the children match.
            self.matching_sigs = TypeSig.get_all_matching_expanded_sigs(
                                      all_possible_sigs, list_of_child_sig_lists,
                                      tnode=self)

        def check_types_in_tree_second_pass(self, root=False):
            """Recursively run the second pass on the token subtree with the
            `self` node as the root.

            This method currently still needs to be explicitly called for the
            root of the final parse tree, from the `PrattParser` method
            `parse`, as well as from the checking routines here to do partial
            checks on subtrees which are already resolvable."""
            unresolved_children = [
                    c for c in self.children if hasattr(c, "matching_sigs")]
            self._check_types_pass_two() # Call first on self to do top-down.
            for child in unresolved_children: # Recurse on unprocessed children.
                child.check_types_in_tree_second_pass()
            # Delete childrens' matching_sigs lists after they are no longer needed.
            # This also acts as an indicator that the node has been resolved.
            for child in unresolved_children:
                delattr(child, "matching_sigs")
            if root:
                # The final pass in _check_types that deletes these attrs doesn't get
                # run on root, so it is explicitly done here when `root` flag is true.
                delattr(self, "matching_sigs")
                delattr(self, "all_possible_sigs")

        def _check_types_pass_two(self):
            """A second pass is only used when overloading on return types is
            allowed.  It is a top-down pass where each node chooses a unique
            final signature for each of its children.  It depends on the
            node attribute `self.matching_sigs` having been set in the first
            pass."""
            # On FIRST pass: on the way *up* the tree (after getting the
            # literal tokens, the leaves of the tree) get all the signature
            # types for a node which match in arguments for *some* possible
            # return-type choice of the children.  Same as the one-pass
            # version, but now sets of possibilities are allowed and state is
            # saved for the second pass to use: the list of matching sigs is
            # temporarily saved with the node in the self.matched_sigs
            # attribute.
            #
            # Summary: first pass, bottom-up, find all sigs that match possible
            # val_types of the node's children, across all arguments.
            #
            # After the first pass the root should have a unique sig; if not
            # there is ambiguity.  (Each node saved a set of sigs that is
            # satisfiable by some realizable choice of child sigs, and parents
            # can force children to assume any of their possible types).
            #
            # On SECOND pass: On the way *down* the tree, parents choose one
            # sig as final for each child and set it in that child's node as
            # the new self.matching_sigs.  This should always be a unique sig;
            # otherwise there is ambibuity.  Recursively called on children.
            #
            # Summary: second pass, top-down, root is unique and parents assign
            # and set the (unique) signature for each of their children.
            #
            # Note that this algorithm works just as well if the second pass is
            # run on each subtree as soon as the subtree root has a unique
            # signature, and the recursion only goes down to subtrees with
            # roots having a unique signature.  This yields partial results and
            # some error conditions sooner, and is what is implemented here.

            if len(self.matching_sigs) != 1: # The root case needs this.
                self._raise_type_mismatch_error(self.matching_sigs,
                        "Ambiguous type resolution (second pass).  Possible type "
                        "assignments for the children/arguments of the token node match"
                        " {0} possible node signatures: {1}.  Uniqueness is required."
                        .format(len(self.matching_sigs), self.matching_sigs))

            # We have a unique signature; set the node's type attributes
            self.expanded_formal_sig = self.matching_sigs[0] # Save sig for semantic actions.
            self.original_formal_sig = self.expanded_formal_sig.original_formal_sig
            # Start setting the actual signature; children will fill arg types when resolved.
            self.actual_sig = TypeSig(self.expanded_formal_sig.val_type, [])

            # Set the actual_sig arg_type for the parent (appending in sequence with others).
            if self.parent is not None:
                parent_args_so_far = list(self.parent.actual_sig.arg_types)
                parent_args_so_far.append(self.expanded_formal_sig.val_type)
                self.parent.actual_sig = TypeSig(self.parent.actual_sig.val_type,
                                                 parent_args_so_far)

            # Update the matching_sigs attribute for each child (should be singleton).
            for count, child in enumerate(self.children):
                if not hasattr(child, "matching_sigs"):
                    continue # Already resolved.
                matched_sigs = TypeSig.get_child_sigs_matching_return_arg_type(
                                          child, self.expanded_formal_sig.arg_types[count],
                                          child.matching_sigs)
                # From the first pass, we know at least one child sig matches.
                assert len(matched_sigs) != 0 # Debug.
                if len(matched_sigs) > 1:
                    # Recursion could catch this on the next step, but better err msg.
                    child._raise_type_mismatch_error(matched_sigs,
                        "Token node has multiple signatures with return type matching "
                        "type of parent (pass two). Parent expects type '{0}'.  Defined"
                        " signatures are: {1}."
                        .format(self.expanded_formal_sig.val_type, child.matching_sigs))
                child.matching_sigs = matched_sigs
            return

        def _raise_type_mismatch_error(self, matching_sigs, basic_msg):
            """Raise an error, printing a helpful diagnostic message.  Assumes
            that `_check_types` has been called (to set `self.all_possible_sigs`)."""
            # TODO: Will the self.expanded_formal_sig *ever* be resolved when this routine is
            # called?  If not, then it is not very useful and message could be reworded.
            diagnostic = ("  The current token has value '{0}' and label '{1}'.  "
                         " Its expanded formal signature is {2}.  The"
                         " children/arguments have token labels and values of {3} and "
                         "value types {4}.  The list of matching signatures "
                         "is {5}.  The list of possible signatures was {6}"
                         .format(self.value, self.token_label,
                             self.expanded_formal_sig,
                             list(c.summary_repr() for c in self.children),
                             list(c.expanded_formal_sig.val_type
                                 if not isinstance(c.expanded_formal_sig, str)
                                 else c.expanded_formal_sig
                                   for c in self.children), # Note this can be "Unresolved", clean up...
                             matching_sigs, self.all_possible_sigs))
            raise TypeErrorInParsedLanguage(basic_msg + diagnostic)

        #
        # Evaluations and semantic actions.
        #

        @property
        def eval_fun(self):
            """Return the evaluation function saved by `_save_eval_fun_and_ast_data`.
            Must be called after parsing because the `original_formal_sig` attribute
            must be set on the token instance."""
            orig_sig = self.original_formal_sig
            return self.parser_instance.construct_table.get_eval_fun(orig_sig, self)

        @property
        def ast_data(self):
            """Return the ast data saved by `_save_ast_data_and_ast_data`.
            Must be called after parsing because the `construct_label` attribute must
            be set on the token instance."""
            orig_sig = self.original_formal_sig
            return self.parser_instance.construct_table.get_ast_data(orig_sig, self)

        def eval_subtree(self):
            """Run the saved evaluation function on the token, if one was
            registered with it.  Returns `None` if no evaluation function is found."""
            #sig = self.expanded_formal_sig
            orig_sig = self.original_formal_sig

            eval_fun = self.eval_fun

            # TODO: Consider if returning None is better than raising exception...
            if not eval_fun:
                return None

            #if not eval_fun:
            #    raise ParserException("An evaluation function is needed for token with "
            #            "value '{0}' and label '{1}' but no defined and matching "
            #            "evaluation function was found in the dict of eval functions.  "
            #            "The resolved original signature is {2} and the resolve expanded"
            #            " signature is {3}.  The resolved construct_label is {4}.  The "
            #            "token's eval_fun_dict is:\n   {5}."
            #            .format(self.value, self.token_label, orig_sig, sig,
            #                    self.construct_label, self.eval_fun_dict))

            return eval_fun(self)

        #
        # The main recursive_parse function.
        #

        def get_jop_token_instance(self, lex, processed_left, lookbehind, subexp_prec):
            """Returns an instance of the jop token iff one should be inferred in the
            current context; otherwise returns `None`."""
            parser_instance = self.parser_instance

            # Not if jop token is undefined for the parser.
            if not parser_instance.jop_token_subclass:
                return None

            # Not if at end of expression.
            if lex.peek().is_end_token():
                return None

            # Not if the ignored token for jop is set but not present.
            if parser_instance.jop_ignored_token_label and (
                          parser_instance.jop_ignored_token_label
                          not in lex.peek().ignored_before_labels()):
                return None

            # Now infer a jop, but only if 1) its prec would satisfy the while loop
            # in `recursive_parse` as an ordinary token, 2) the next token has
            # a head handler defined in the conditions when the jop will need
            # to run its head handler, and 3) the next token similarly has no
            # tail handler in the context.
            if parser_instance.jop_token_subclass.prec() > subexp_prec:
                # Provisionally infer a jop; create a subclass instance for its token.
                #
                # Todo: Maybe for jop and null-string tokens define methods
                # get_jop_token_instance() which gets the token but calls the
                # lexer to dress it up more like a "real" token, with ignored_before
                # and line numbers, etc. (without putting it in the buffer, of course).
                jop_instance = parser_instance.jop_token_subclass(None)

                # This is a little inefficient (since it uses a `go_back` call)
                # but we need to be sure that when the tail handler of the jop
                # is called and it reads a token that that token has a head
                # handler defined for it *in that precondition context*.
                # Otherwise, no jop will be inferred.  We also make sure that
                # it has no tail handler in the context, since then it would be
                # a lower-precedence (lower precedence because we broke out of
                # the loop above) infix or postfix operator, and no jop is
                # inferred before another operator).
                curr_token = lex.next()
                try:
                    # Dispatch here WITHOUT CALLING, since calling will consume
                    # another token; also, deeper-level recursions could cause false
                    # results to come up the recursion chain.  We are just testing
                    # what handlers are defined for the token.
                    _peek_head_handler = curr_token.dispatch_handler(HEAD, lex)
                    try: # Found head handler, now make sure it has no tail handler.
                        _peek_tail_handler = curr_token.dispatch_handler(
                                           TAIL, lex, processed_left, lookbehind)
                    except NoHandlerFunctionDefined:
                        # This is the only case where an actual token is returned.
                        return jop_instance
                    else:
                        return None
                except NoHandlerFunctionDefined:
                    return None # No precondition matches, assume no jop.
                finally:
                    lex.go_back(1)
            else:
                return None

        def get_null_string_token_and_handler(self, head_or_tail, lex, subexp_prec,
                                         processed_left=None, lookbehind=None):
            """Check for any possible matching null-string token handlers;
            return the token and the matching handler if one is found."""

            # TODO: Need a way to pass the subexp_prec argument to null-string
            # handlers, so they can relay it -- a way that works with recursive
            # calls, too.
            #
            # The idea is that we want to have the recursive descent stuff use
            # the Pratt-style priority mechanisms if possible...
            #
            # Does each production need to declare whether it is a head or tail?
            # Implicit?
            #
            # Jumping into recursive_parse in middle may be better solution...
            # as originally done.  To the handlers it is all the same loop,
            # since they get the same info.  I.e., to handle tails in recursive
            # descent like way...

            parser_instance = self.parser_instance
            curr_token = None
            handler_fun = None
            # See if a null-string token is set and a handler matches preconds.
            if parser_instance.null_string_token_label:
                null_string_token = parser_instance.null_string_token_subclass(None)
                try:
                    handler_fun = null_string_token.dispatch_handler(
                                      head_or_tail, lex, processed_left, lookbehind)
                    curr_token = null_string_token
                    # Save subexp_prec and lookbehind as attributes so null-string
                    # token's handler funs can access them when relaying calls.
                    curr_token.saved_subexp_prec = subexp_prec
                    curr_token.lookbehind = lookbehind
                except NoHandlerFunctionDefined:
                    pass
            return curr_token, handler_fun

        def recursive_parse(self, subexp_prec,
                            # Below parameters ONLY used in null-string handler funs.
                            processed_left=None, lookbehind=None):
            """Parse a subexpression as defined by token precedences. Return
            the result of the evaluation.  Recursively builds up the final
            result in `processed_left`, which is the tree for the part of the
            full expression to the left of the current token.  This is a static
            method so that it can be called from head and tail functions.  Note
            that the function `dispatch_and_call_handler` which is called in
            the code often recursively call `recursive_parse` again.  Each
            recursive call inside the function processes a subexpression,
            sub-subexpression, etc.  (as implicitly defined by the token
            precedences).  The list `lookbehind` saves all the previously
            evaluated subexpressions at this level of recursion (i.e., at the
            top level in the same subexpression) and passes it to the
            `tail_dispatcher` method of the tokens, in case that routine wants
            to make use of it.  For example, the ordinal position of the token
            in the top level of the subexpression can be calculated from the
            length of `lookbehind`.

            This function is made a method of `TokenSubclass` so that handler
            functions can easily call it by using `tok.recursive_parse`, and
            also so that it can access the lexer without it needing to be
            passed as an argument.  It is basically a static function, though.

            If `processed_left` is set (evaluates to true) then the first part
            of `recursive_parse` is skipped and it jumps right to the
            tail-handling loop part using the passed-in values of
            `processed_left` and `lookbehind`.  These parameters should
            generally not be set.  They are only used by certain null-string
            tokens' tail-handlers so they can relay their call as a tail-handler
            to the actual token (which does the real work)."""

            # Note on below code: It is tempting for efficiency to define a jop
            # and null-string token instance, save it, and only use it when
            # necessary (and replace it only when used).  But some things need
            # to be considered.  What if new head handlers are dynamically
            # registered for the token?  Is there any special attribute or
            # other thing that is assigned on creation which might change?
            # Either way, you should probably add extra things like line numbers
            # to mimic ordinary tokens. Note that currently you only really pay
            # the creation cost if you actually use the jop or null-string
            # feature, but a lot of dummy instances are created if you do.

            # Set some convenience variables (the lexer and parser instances).
            lex = self.token_table.lex
            if not hasattr(self, "parser_instance"):
                # This catches some cases of tokens defined via Lexer, not all.
                raise ParserException("All tokens used in the parser must be"
                        " defined in via parser's methods, not the lexer's.")
            #parser_instance = self.parser_instance # If needed, avoid otherwise.

            # Skip head-handling if `processed_left` passed in.  ONLY skipped
            # when called from relaying null-string tail handlers.
            if not processed_left:
                curr_token, head_handler = self.get_null_string_token_and_handler(
                                                           HEAD, lex, subexp_prec)
                if not curr_token:
                    curr_token = lex.next()
                    head_handler = curr_token.dispatch_handler(HEAD, lex)
                curr_token.is_head = True # To look up eval_fun and ast_data later.
                # Call the head-handler looked up above.
                processed_left = head_handler()
                lookbehind = [processed_left]

            while True:

                #
                # The loop below is the main loop in ordinary Pratt parsing.  The
                # Outer loop is ONLY for the special case when a jop is defined.
                #

                while lex.peek().prec() > subexp_prec:
                    curr_token, tail_handler = self.get_null_string_token_and_handler(
                                    TAIL, lex, subexp_prec, processed_left, lookbehind)
                    if not curr_token:
                        curr_token = lex.next()
                        tail_handler = curr_token.dispatch_handler(
                                             TAIL, lex, processed_left, lookbehind)

                    processed_left = tail_handler()
                    lookbehind.append(processed_left)

                #
                # Broke out of main loop, determine whether or not to infer a jop.
                #

                jop_instance = self.get_jop_token_instance(
                                         lex, processed_left, lookbehind, subexp_prec)
                if jop_instance:
                    tail_handler = jop_instance.dispatch_handler(
                                           TAIL, lex, processed_left, lookbehind)
                    processed_left = tail_handler()
                    lookbehind.append(processed_left)
                else:
                    break

            return processed_left

        #
        # Copying instances of tokens.
        #

        def __copy__(self):
            """Return a shallow copy of the token."""
            return copy.copy(self)

        #
        # Some representations that apply to the subclasses.
        #

        def summary_repr_with_types(self):
            """A short summary repr of the node, without its children."""
            return ("<" + str(self.token_label) +
                    "," + str(self.value) +
                    "," + str(self.expanded_formal_sig.val_type) + ">")

        def tree_repr_with_types(self, indent=""):
            """Token representation as the root of a parse subtree, with formatting.
            The optional `indent` parameter can be either an indent string or else
            an integer for the number of spaces to indent.  Note that the ordinary
            `tree_repr` method without types is also available, inherited from
            the base node object."""
            try:
                num_indent = int(indent)
            except ValueError:
                pass
            else:
                indent = " " * num_indent
            string = indent + self.summary_repr_with_types() + "\n"
            for c in self.children:
                string += c.tree_repr_with_types(indent=indent+" "*4)
            return string

        def string_repr_with_types(self):
            """A string repr for the tree that includes type information."""
            string = self.summary_repr_with_types()
            if self.children:
                string += "("
                string += ",".join(c.string_repr_with_types() for c in self.children)
                string += ")"
            return string

        @classmethod
        def class_repr(cls):
            """Print out the `TokenSubclass` classes (representing tokens) in a
            nice way.  The default is hard to read."""
            string = "Token({0})".format(cls.token_label)
            return string

    return TokenSubclass # Return from token_subclass_factory function.

#
# PrattTokenTable
#

class PrattTokenTable(TokenTable):
    """Define and save tokens to be used by the `PrattParser` class and instances."""
    def __init__(self, token_subclass_factory_fun=token_subclass_factory,
                       pattern_matcher_class=Matcher):
        super(PrattTokenTable, self).__init__() # Call base class __init__.

#
# Parser
#

def lexer_add_parser_instance_attribute(lexer, token):
    """Passed to lexer to add a `parser_instance` attribute to each token it
    returns.  This attribute is added to instances at the lexer, from its
    current token table, because of the case where parsers call other parsers.
    (It is not added to general token subclasses in `def_token_master` because
    parsers could potentially share token subclasses.)"""
    token.parser_instance = lexer.token_table.parser_instance
    return token

class PrattParser(object):
    """A parser object.  Each parser object contains its own token table for tokens
    and its own lexer."""
    def __init__(self, max_peek_tokens=None,
                       max_deque_size=None,
                       lexer = None,
                       default_begin_end_tokens=True,
                       type_table = None,
                       skip_type_checking=False,
                       overload_on_arg_types=True,
                       overload_on_ret_types=False,
                       partial_expressions=False):
        """Initialize the parser.

        The `max_peek_tokens` parameter is an optional arbitrary limit on the
        level of peeks allowed in the lexer.  Setting to 1 limits it to a
        single token lookahead.  The default setting `None` gives
        peek-on-demand as far ahead as requested.

        The `max_deque_size` parameter is an optional limit on the size of the
        deque that the lexer keeps for previous tokens, the current token, and
        requested peek tokens.  The default is for it to grow arbitrarily
        (though it is reset on each parse).  Needs to be large enough for the
        max level of peeks required plus the max `go_back` level required.

        If a `Lexer` instance is passed in the parser will use that lexer and
        its token table, otherwise a new lexer is created.  The any other lexer
        options are ignored.

        No default begin and end functions will be set if a lexer is passed in,
        regardless of the value of `default_begin_end_tokens`.  Otherwise,
        default begin and end tokens will be defined unless
        `default_begin_end_tokens` is set false (note that creating them by
        default is the opposite of the default behavior for the lower-level
        Lexer class).

        Setting `skip_type_checking=True` is slightly faster if typing is not
        being used at all.  Setting `overload_on_ret_types` requires an extra
        walk of the token tree, and implies overloading on argument types.

        If `partial_expressions` is set then no check will be made in the
        `parse` method to see if the parsing consumed up to the end-token in
        the lexer.  Multiple expression text can be parsed by repeatedly
        calling `parse` when this option is true and checking whether the
        lexer's current token is the end-token."""

        ## Type-checking options below; these can be changed between calls to `parse`.
        self.skip_type_checking = skip_type_checking # Skip all type checks, faster.
        self.overload_on_arg_types = overload_on_arg_types # Raise error on mult defs?
        self.overload_on_ret_types = overload_on_ret_types # Requires extra processing.
        if overload_on_ret_types:
            self.overload_on_arg_types = True # Overload on ret implies overload on args.

        # If exceptions are not raised on ties below, the last-set one has
        # precedence.  Define this before registering any handlers (done
        # below in `def_begin_end_tokens`).
        self.raise_exception_on_precondition_ties = True

        self.construct_table = ConstructTable() # Dict of registered constructs.

        if lexer: # Lexer passed in.
            self.lex = lexer
            self.token_table = lexer.token_table
        else: # No Lexer passed in.
            self.token_table = TokenTable(
                                token_subclass_factory_fun=token_subclass_factory)
            self.lex = Lexer(self.token_table,
                             max_peek_tokens=max_peek_tokens,
                             max_deque_size=max_deque_size,
                             default_begin_end_tokens=False)
            # Set the begin and end tokens unless the user specified not to.
            if default_begin_end_tokens:
                self.def_begin_end_tokens() # Use function's defaults.
        self.lex.default_helper_exception = ParserException # Default for match_next, etc.
        # Add mod function to lexer here, works even if they passed in a lexer.
        self.lex.final_mod_function = lexer_add_parser_instance_attribute

        self.token_table.parser_instance = self # Give token table ref to parser.

        if type_table:
            self.type_table = type_table
        else:
            self.type_table = TypeTable(self)


        self.num_lookahead_tokens = max_peek_tokens
        self.jop_token_label = None # Label of the jop token, if any.
        self.jop_token_subclass = None # The actual jop token, if defined.
        self.null_string_token_label = None # Label of the null-string token, if any.
        self.null_string_token_subclass = None # The actual null-string token, if any.
        self.partial_expressions = False # Whether to parse multiple expressions.

        self.pstate_stack = [] # Stack of production rules used in grammar parsing.
        self.top_level_production = False # If true, force prod. rule to consume all.

    #
    # Methods defining tokens.
    #

    def def_token_master(self, token_label, regex_string=None, on_ties=0, ignore=False,
                         ignored_token_label=None, token_kind="regular",
                         options=None):
        """The master method for defining tokens; all the convenience methods
        actually call it.  Allows for factoring out some common code and
        keeping the attributes of all the different kinds of tokens up-to-date.
        This routine calls the underlying lexer's `def_token` to get tokens and
        then adds extra attributes needed by the `PrattParser` class.

        The `token_kind` argument must be one of the following strings:
        `"regular"`, `"ignored"`, `"begin"`, `"end"`, `"jop"`, or
        `"null-string"`.

        Tokens can be shared between parsers if all their properties are the
        same.  Null-string and jop tokens are the exception, but they are special
        and are never returned by the lexer, only by a particular parser."""
        token_table = self.token_table

        if token_kind == "regular":
            tok = token_table.def_token(token_label, regex_string,
                       on_ties=on_ties, ignore=ignore, options=options)

        elif token_kind == "ignored":
            tok = token_table.def_token(token_label, regex_string,
                       on_ties=on_ties, ignore=True, options=options)

        elif token_kind == "begin":
            tok = token_table.def_begin_token(token_label)
            self.begin_token_label = token_label
            # Define dummy handlers for the begin-token.
            def begin_head(self, lex):
                """Dummy head handler for begin-tokens."""
                raise CalledBeginTokenHandler("Called head-handler for begin token.")
            def begin_tail(self, lex, left):
                """Dummy tail-handler for begin-tokens."""
                raise CalledBeginTokenHandler("Called tail-handler for begin token.")
            self.def_construct(HEAD, begin_head, token_label)
            self.def_construct(TAIL, begin_tail, token_label)
            self.begin_token_subclass = tok

        elif token_kind == "end":
            tok = token_table.def_end_token(token_label)
            self.end_token_label = token_label
            def end_head(self, lex):
                """Dummy head handler for end-tokens."""
                raise CalledEndTokenHandler("Called head-handler for end token.")
            def end_tail(self, lex, left):
                """Dummy tail-handler for end-tokens."""
                raise CalledEndTokenHandler("Called tail-handler for end token.")
            self.def_construct(HEAD, end_head, token_label)
            self.def_construct(TAIL, end_tail, token_label)
            self.end_token_subclass = tok

        elif token_kind == "jop":
            if self.jop_token_subclass:
                raise ParserException("A jop token is already defined.  It must be "
                                      "undefined before defining a new one.")
            self.jop_token_label = token_label
            self.jop_ignored_token_label = ignored_token_label
            tok = token_table.def_token(token_label, None)
            self.jop_token_subclass = tok
            tok.parser_instance = self # Special token, never returned by lexer.

        elif token_kind == "null-string":
            if self.null_string_token_subclass:
                raise ParserException("A null-string token is already defined.  It"
                         " must be undefined before defining an new one.")
            self.null_string_token_label = token_label
            tok = token_table.def_token(token_label, None)
            self.null_string_token_subclass = tok
            tok.parser_instance = self # Special token, never returned by lexer.

        else:
            raise ParserException("Bad call to def_token_master, with unrecognized"
                    ' string "{0}" for the keyword argument token_kind.'
                    .format(token_kind))

        tok.token_kind = token_kind
        tok.is_head = False # Set true in recursive_parse if instance parses as a head.
        return tok

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False,
                  options=None):
        """Define a token.  Use this instead of the Lexer `def_token` method,
        since it adds extra attributes to the tokens."""
        return self.def_token_master(token_label, regex_string, on_ties, ignore,
                              token_kind="regular", options=options)

    def def_ignored_token(self, token_label, regex_string, on_ties=0,
                          options=None):
        """A convenience function to define a token with `ignored=True`."""
        return self.def_token_master(token_label, regex_string, on_ties, ignore=True,
                              token_kind="ignored", options=options)

    def def_begin_end_tokens(self, begin_token_label="k_begin",
                                   end_token_label="k_end"):
        """Calls the `Lexer` method to define begin- and end-tokens.  The
        subclasses are then given initial head and tail functions for use in
        the Pratt parser.  To use the `PrattParser` this method must be called,
        not the method of `Lexer` with the same name (since it also creates
        head and tail handler functions that raise exceptions for better error
        messages).  The default is to call this method automatically on
        initialization, with the default token labels for the begin and end
        tokens.  If the flag `default_begin_end_tokens` is set false on
        `PrattParser` initalization then the user must call this function
        (setting whatever token labels are desired).  Returns a tuple
        containing the new begin and end `TokenNode` subclasses."""
        begin_tok = self.def_token_master(begin_token_label, token_kind="begin")
        end_tok = self.def_token_master(end_token_label, token_kind="end")
        return begin_tok, end_tok

    def def_jop_token(self, jop_token_label, ignored_token_label):
        """Define a token for the juxtaposition operator.  This token has no
        regex pattern.  An instance is inserted in `recursive_parse` when it is
        inferred to be present.  This method must be explicitly called before a
        juxtaposition operator can be used (i.e., before `def_jop`).  The
        parameter `jop_token_label` is the label for the newly-created token
        representing the juxtaposition operator.  The `ignored_token_label`
        parameter is the label of an ignored token which must be present for a
        jop to be inferred.  Some already-defined token is required; usually it
        will be a token for spaces and tabs.  If set to `None` then no ignored
        space at all is required (i.e., the operands can be right next to each
        other)."""
        return self.def_token_master(jop_token_label,
                                     ignored_token_label=ignored_token_label,
                                     token_kind="jop")

    def def_null_string_token(self, null_string_token_label="k_null-string"):
        """Define the null-string token.  This token has no regex pattern.  An
        instance is inserted in `recursive_parse` when it is inferred to be
        present based.  It can only ever have head handlers, and is not even
        tested for tail handlers.  This method must be called before a
        null-string can be used.  The parameter `null_string_token_label` is
        the label for the newly-created tok representing it."""
        return self.def_token_master(null_string_token_label,
                                     token_kind="null-string")

    def get_token(self, token_label):
        """Return the token with the label `token_label`.  The reverse
        operation, getting a label from a token instance, can be done by
        looking at the `token_label` attribute of the token."""
        return self.token_table[token_label]

    #
    # Undefine tokens.
    #

    def undef_token(self, token_label):
        """A method for undefining any token defined by the `PrattParser` methods.
        Since the `token_kind` was set for all tokens when they were defined
        it knows how to undelete any kind of token."""
        # TODO: Make sure this is up-to-date with the what the token_master class does.
        token_table = self.token_table
        tok = token_table[token_label]
        kind = tok.token_kind
        if kind == "jop":
            token_table.undef_token(self.jop_token_label)
            self.jop_token_subclass = None
            self.jop_token_label = None
            self.jop_ignored_token_label = None
        elif kind == "null-string":
            token_table.undef_token(self.null_string_token_label)
            self.null_string_token_subclass = None
            self.null_string_token_label = None
            self.null_string_ignored_token_label = None
        else:
            token_table.undef_token(token_label)

    #
    # Methods to define and undefine constructs.
    #

    def def_construct(self, head_or_tail, handler_fun, trigger_token_label,
                      construct_label=None, prec=None, precond_fun=None,
                      precond_priority=0, val_type=None, arg_types=None,
                      eval_fun=None, ast_data=None, value_key=None):
        """Define a construct and register it with the token with label
        `trigger_token_label`.  A token with that label must already be in the
        token table, or an exception will be raised.

        Currently returns the token that is modified.

        Both head and tail functions can be passed in one call, but they must
        have the same construct label and precondition priority.

        If `tail` is set then the prec will also be set unless `prec` is
        `None`.  For a head the `prec` value is ignored.  If `tail` is set and
        `prec` is `None` then the prec value defaults to zero.

        The `eval_fun` and the `ast_data` arguments are saved in the dicts
        `eval_fun_dict` and `ast_data_dict` respectively, keyed by the
        `TypeSig` defined by `val_type` and `arg_types`, as well as by
        `arg_types` alone for when overloading on return values is not used.
        This allows for different overloads to have different evaluation
        functions and AST-associated data.

        If `value_key` is set to a string value then that value will be part of
        the key tuple for saving AST data and evaluation functions.  This can
        be used, for example, when overloading a generic identifier with
        different evaluation functions for when the identifier value is `sin`,
        `cos`, etc.  In looking up the AST data and evaluation function the
        parsed token's actual string value (from the program text) is used as
        the key.  If any overload of a particular construct provides a
        `value_key` string then all the other overloads for that construct must
        also (for the time being, at least)."""
        # Note that the parser_instance attribute of tokens is not necessarily
        # set yet when this method is called.

        if isinstance(arg_types, str):
            raise ParserException("The arg_types argument to token_subclass must"
                    " be None or an iterable returning type labels (e.g., a list"
                    " or tuple).")

        if head_or_tail == TAIL and (prec is None):
            prec = 0

        if construct_label is None:
            construct_label = DEFAULT_CONSTRUCT_LABEL

        if precond_fun is None:
            precond_fun = DEFAULT_ALWAYS_TRUE_PRECOND_FUN

        if trigger_token_label in self.token_table:
            token_subclass = self.get_token(trigger_token_label)
        else:
            raise ParserException("In call to def_construct: subclass for"
                    " token labeled '{0}' has not been defined.  Maybe try"
                    " calling `def_token` first.".format(trigger_token_label))
            # Below line formerly just created a subclass, but that can mask errors!
            #TokenSubclass = self.token_table.create_token_subclass(token_label)

        if head_or_tail == TAIL:
            token_subclass.static_prec = prec # Ignore prec for heads; it will stay 0.

        # Create the type sig object.
        type_sig = TypeSig(val_type, arg_types)

        # Register the handler funs.
        construct = self.construct_table.register_construct(head_or_tail, trigger_token_label,
                                              construct_label, handler_fun, precond_fun,
                                              precond_priority, type_sig,
                                              eval_fun, ast_data, value_key,
                                              parser_instance=self)
        return construct

    def undef_construct(self, token_label, head_or_tail, construct_label=None,
                         val_type=None, arg_types=None, all_handlers=False):
        """Undefine a head or tail function with the given `token_label`,
        `construct_label` and type signature.  The `head_or_tail` value should be
        `HEAD` or `TAIL`.  If `all_precond` is set then all heads and tails for all
        preconditions will be undefined.  If `all_overloads` then all
        overloaded type signatures will be undefined.  The token itself is
        never undefined; use the `undef_token` method for that."""
        # TODO: rewrite to undef a construct.  Currently doesn't work!!!!!
        TokenSubclass = self.token_table[token_label]
        TokenSubclass.unregister_construct(head_or_tail, trigger_token_label,
                                           construct_label,
                                           type_sig=TypeSig(val_type, arg_types),
                                           all_handlers=all_handlers)

    #
    # Methods dealing with types.
    #

    def def_type(self, type_label):
        """Define a type associated with the name `type_label`."""
        return self.type_table.create_typeobject(type_label)

    def undef_type(self, type_label):
        """Undefine the type associated with the name `type_label`."""
        self.type_table.undef_typeobject(type_label)

    #
    # The main parse routines.
    #

    def parse_from_lexer(self, lexer_to_use, pstate=None):
        """The same as the `parse` method, but a lexer_to_use is already assumed to be
        initialized.  This is ONLY used when one parser instance calls another
        parser instance (implicitly, via the handler functions of its tokens).
        The outer parser calls this routine of the inner, subexpression parser.
        Such a call to another parser would look something like::

            alternate_parser.parse_from_lexer(lexer_to_use)

        where `lexer_to_use` is the lexer_to_use of the outer parser.  This routine
        temporarily swaps the token table for the passed-in lexer_to_use to be the
        token table for this parser (remember that this parser is the inner
        parser when this routine is called)."""
        # Note you do not need to set the type_table, since the tokens always
        # have fixed references to their own parser instance.

        # Set up to read from lexer_to_use, after setting its token table to
        # be the token table for this parser (this is the inner parser).

        # Temporarily change new lexer's token table.
        lexer_to_use_usual_table = lexer_to_use.token_table
        lexer_to_use.set_token_table(self.token_table)

        # Swap the lexer in.
        usual_lexer = self.lex
        self.lex = lexer_to_use # Swap the lexer.

        try:
            parsed_subexpression = self.parse("IGNORED", pstate=pstate,
                                    partial_expressions=True,
                                    skip_lex_setup=True)
        except ParserException:
            print("Error in `parse_from_lexer` method, in call to `parse` method"
                    " using a different parser's lexer for a subexpression.",
                    file=sys.stderr)
            raise
        finally:
            # Restore the lexer.
            lexer_to_use.set_token_table(lexer_to_use_usual_table) # Restore token table.
            self.lex = usual_lexer
        return parsed_subexpression

    def parse(self, program, pstate=None, partial_expressions=None,
                                                      skip_lex_setup=False):
        """The main routine for parsing a full program or expression.  Users of
        the class should call this method to perform the parsing operations
        (after defining a grammar, of course).

        Unless there was a parsing failure or `partial_expressions` is true
        then the lexer is left with the end-token as the current token.

        If the `pstate` variable is set then the value will be pushed as the
        initial state on the production rule stack `pstate_stack`.  The stack
        is then cleared after a successful call.  (Set the parser attribute
        directly for more control.)

        The parser's `partial_expressions` attribute will be used unless it is
        overridden by the parameter `partial_expressions` here.  When it is
        true no check is made for the end-token after `recursive_parse` returns
        a value.    The lexer will be left at the last token consumed, so a
        check for the end-token will tell when all the text was consumed.
        Users are responsible for making sure their grammars are suitable for
        this kind of parsing if the option is set.

        If the `skip_lex_setup` parameter is true then the text `program` is
        ignored and lexer setup is skipped.  This is generally ONLY used when
        multiple parsers are parsing from a common text stream, and `parse` is
        called from the method `parse_from_lexer`."""
        if pstate:
            self.pstate_stack = [pstate] # For parsing production rule grammars.
        if partial_expressions is None:
            partial_expressions = self.partial_expressions

        if not skip_lex_setup:
            self.lex.set_text(program)
        begin_tok = self.lex.token # Get the first token to access recursive_parse.
        parse_tree = begin_tok.recursive_parse(0)

        # Finalize type-checking for root when overloading on return types.
        if self.overload_on_ret_types:
            parse_tree.check_types_in_tree_second_pass(root=True)

        # See if we reached the end of the token stream.
        if not self.lex.peek().is_end_token() and not partial_expressions:
            raise IncompleteParseException("Parsing never reached the end of"
                " the text.  Parsing stopped with tokens still in the lexer."
                "  No syntax element was recognized. The last-parsed token had"
                " value '{0}' and label '{1}'.  Parsing stopped before a token"
                " in the lexer with value '{2}' and label '{3}'.  The partial"
                " result returned is:\n{4}"
                .format(self.lex.token.value, self.lex.token.token_label,
                        self.lex.peek().value, self.lex.peek().token_label,
                        parse_tree.tree_repr()))

        if pstate:
            self.pstate_stack = []
        return parse_tree


#
# Copy the predefined convenience function to the PrattParser as methods.
#


for method in builtin_parse_methods.parse_methods:
    setattr(PrattParser, method.__name__, method)

for method in predefined_token_sets.token_defining_methods:
    setattr(PrattParser, method.__name__, method)

#
# Exceptions
#

class IncompleteParseException(ParserException):
    """Only raised at the end of the `PrattParser` function `parse` if tokens
    remain in the lexer after the parser finishes its parsing."""
    pass

