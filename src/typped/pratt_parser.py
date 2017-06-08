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

Attributes set on the `TokenNode` subclass:

Attributes set on token instances during parsing:

* `original_formal_sig` -- a `TypeSig` instance of the resolved original formal signature
* `expanded_formal_sig` -- a `TypeSig` instance of the expanded formal signature
* `construct_label` -- the string label of the winning construct

Note that both `original_formal_sig` and `expanded_formal_sig` are set to the
string `"Unresolved"` before the token is parsed.  The actual signature is
found during parsing and type-checking.  Out of all possible overloads in the
original formal signatures associated with the token (via `modify_token`) the
one which matches the actual arguments is chosen.  The expanded formal
signature is the same as the original formal signature except that wildcards,
etc., are expanded in the attempt to match the actual arguments.

Implementation details
======================

This section gives a general overview of the lower-level details of the
`PrattParser` implementation.

The basic class structure
-------------------------

There are five basic classes, with instances which interact.  The main class
is the `PrattParser` class, which users will mostly interact with.  The overall
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

Every `PrattParser` instance contains a **fixed** `TokenTable` instance, which
never changes.  So each token-table created by a parser can save a pointer back
to the parser which "owns" it.  Each `PrattParser` instance also contains a
**fixed** `Lexer` instance, which also never changes.  So each lexer created by
a parser can also save a pointer back to the parser which "owns" it.

The `TokenSubclass` class is a subclass of the `TokenNode` class, which is
defined in the lexer module.  It is redefined in this module, with many
additional methods and attributes which are needed in the parsing application.
The `TokenSubclass` class is actually defined inside a factory function, called
`token_subclass_factory`, which produces a different subclass to represent each
kind of token that is defined (using the `def_token` method of `PrattParser`).
Instances of those subclasses represent the actual tokens, with individual
text-string values, which are returned by the lexer.

A `TokenTable` instance is basically a dict for holding all the defined
token-subclasses.  But it also has many methods and attributes associated with
it.  It is where all new tokens are ultimately created and defined, for example
(although other classes like the parser class can add extra attributes to the
created tokens).

A `TokenTable` instance contains all the tokens defined for a language, and
stays with the `PrattParser` instance which created it (from which the tokens
were necessarily defined).  Since this is Pratt parsing, the collection of
tokens and their associated handler functions fully characterize any given
language.  A `Lexer` instance can use different `TokenTable` instances,
possibly switching on-the-fly.  A lexer instance  always has a pointer to its
*current* token-table instance, but that can change on-the-fly (such as when
separate parsers are swapped in to parse sub-languages in the same text
stream).

Tokens themselves are always created by a method of a `TokenTable`, and all the
instances have a pointer back to the `TokenTable` instance which created them.
As described above, those `TokenTable` instances in turn have a pointer back to
the `PrattParser` instance which created them (assuming that is where they were
created) and the current lexer associated with them.  In that way tokens are
associated with a fixed token table and parser, and a possibly-changing lexer.
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
                              "../../test/test_pratt_parser.py"
                              ], pytest_args="-v")

import sys
import copy
import functools
from collections import OrderedDict, namedtuple

from .shared_settings_and_exceptions import (ParserException,
                              CalledBeginTokenHandler, CalledEndTokenHandler)
from .lexer import (Lexer, TokenNode, TokenTable, multi_funcall)
from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage

# TODO: Consider allowing overloading (or at least the chosen eval_fun/ast_data
# for a token) to vary based on the VALUE of the subtree root token as well as
# on the token_label of the token (as it is now).  Save a dict with the token
# subclass which is used to look up the actual typesig; use None by default but
# let the user manage it and add labels.  So, e.g., a particular identifier
# could be declared a variable of some type, and then the user could add that
# to the dict stored with the identifier token subclass.  Would then check if
# value in the dict, and if not use the None value (or set a dict default
# value, easier).
#
# Note that this would have to be able to switch to different handlers, like
# when an identifier is defined to be a function name versus a variable name.
# Currently overloading is only done on repeated calls when type differs.  When
# precond fun differs you can get a different handlers.  So, one approach would
# be to include the current value as a precondition in a call with a new
# precond fun.  Might not be the most efficient way, though.

# TODO: Maybe define some common helper preconditions.  They are boolean so
# they can easily be composed.  Can store them in a separate module,
# precondition_helpers.py, and then import them in the __init__ for module
# space.  Could store helpers like match_next with them (or similarly) if it is
# determined that they shouldn't be in TokenSubclass namespace.

# NOTE that the evaluate function stuff could also be used to also do a
# conversion to AST.

# TODO: Consider allowing a string label of some sort when defining a parser,
# something like "TermParser" or "parser for terms", "WffParser", etc.  Then
# use that string in the exception messages.  When working with parsers called
# from/by parsers these labels in error messages would be helpful for debugging.

# TODO: Add more built-in methods.  One that does assignments would be useful.

# TODO: Consider the pros and cons of moving to a centralized
# per-parser-instance dict for storing handler functions, eval funs, and extra
# token info.  Info currently is stored with individual tokens.

# Later, consider serialization of defined parsers, such as with JSON or (at
# least) pickle http://www.discoversdk.com/blog/python-serialization-with-pickle
# If a TokenTable is made to fully define a parser then you only need to save that...
# but you need to clutter it with non-token data.

#
# Construct
#

# The default precondition label is a tuple so it never matches an actual string.
DEFAULT_CONSTRUCT_LABEL = ("always-true-default-precondition",)

# The default precondition function always returns true.
def DEFAULT_ALWAYS_TRUE_PRECOND_FUN(lex, lookbehind):
    return True

class SyntaxConstruct(object):
    """A syntax construct in the language.  Usually corresponds to a subtree of
    the final parse tree.  Essentially a data frame for a handler function
    containing extra context information such as the preconditions function,
    etc.  Each construct for a head (tail) and particular token label must have
    a unique label (overloading is assumed if a label is re-used).

    A construct is possibly triggered when a hander needs to be dispatched for
    its kind of token.  The preconditions functions for all such constructs are
    run in priority-sorted order.  A construct is triggered if its
    preconditions function is the first to evaluate to true.  It then provides
    the handler function to be called.

    Constructs are also saved as attributes of the handler function itself, so
    it can easily be accessed from inside the handler function when it runs.
    (Note that the trigger token is not always the token that ends up at the
    root of the returned subtree, which is where typesig checking is done
    from.)

    A list of typesigs must be stored because overloaded functions use the same
    handler function (it is unknown which signature will apply until after the
    arguments/subtree is parsed to resolve the overload).  Some additional
    information (eval functions and ast labels) is stored in dicts keyed by
    typesigs."""
    # Use __slots__ later when implementation works.

    # TODO: Constructs also hold typesig info, and they can run both the
    # handler and then run `process_and_check_node` on the returned subtree.
    # Then it is no longer required to put the call inside handler functions.
    #
    # Need to add the following things to do this:
    # - any options that should be passed to `process_and_check_node` as init args
    # - work out typesig_override problems

    def __init__(self, parser_instance,
                       construct_label,
                       trigger_head_or_tail=None,
                       trigger_token_label=None,
                       handler_fun=None,
                       precond_fun=None,
                       precond_priority=0,
                       original_sig=None,
                       eval_fun_dict = None,
                       ast_data_dict = None):
        self.parser_instance = parser_instance
        self.construct_label = construct_label

        self.trigger_head_or_tail = trigger_head_or_tail
        self.trigger_token_label = trigger_token_label

        self.handler_fun = handler_fun
        self.precond_fun = precond_fun
        self.precond_priority = precond_priority

        if original_sig is None:
            self.original_sigs = []
        else:
            self.original_sigs = original_sig

        # Dicts keyed on original sig (and looked up with one that matches actual sig.)
        if ast_data_dict is None:
            ast_data_dict = {}
        self.ast_data_dict = ast_data_dict
        if eval_fun_dict is None:
            eval_fun_dict = {}
        self.eval_fun_dict = eval_fun_dict
        #self.root_token_label = root_token_label # Token label that ends up at the root of the subtree.

    @staticmethod
    def run(construct, tok, lex, processed_left=None, lookbehind=None):
        """Run the handler associated with the construct.  Check the returned parse
        subtree with `process_and_check_node`.  Return the subtree if it passes type
        checking.  This is a static method because the arguments will be bound using
        `functools.partial` before it is dispatched to be called."""

        # NOTE: A circular reference to the construct is temporarily added as
        # an attribute of the handler function.  The handler function will be
        # immediately called, but we want a simple way to access the construct
        # from inside the handler function (which can see itself).  Since this
        # will only occasionally be used, an extra argument to handlers is
        # avoided.
        #
        # This has to be done just before the call since there's no guarantee
        # that the function object wasn't re-used in some other construct.
        # This and the use of recursive calls means that we also need to keep a
        # stack of constructs with a given function object.  (Maybe just add an
        # argument, esp. if get rid of process_and_check_node call?)

        handler_fun = construct.handler_fun
        if not hasattr(handler_fun, "construct_stack"):
            handler_fun.construct_stack = []
        handler_fun.construct_stack.append(construct)

        if construct.trigger_head_or_tail == HEAD:
            subtree = handler_fun(tok, lex)
        else:
            subtree = handler_fun(tok, lex, processed_left)

        handler_fun.construct_stack.pop()
        if not handler_fun.construct_stack:
            delattr(handler_fun, "construct_stack")
        # TODO: eventually want to call `process_and_check_node` from here, BUT
        # the problem is that the bracket construct dynamically sets the typesig
        # override to the type of the child.  Need a way to specify that.
        # -----> Consider just modifying the construct `original_sig` in the handler,
        # but seems kind of kludgey as it is... maybe define a special attribute,
        # which WILL PROBABLY ALSO NEED TO BE A STACK since constructs used recursively...
        #self.parser_instance.process_and_check_node(subtree, arg,...) # TODO
        # -----> Could make typesig_override a function which returns a typesig...
        return subtree

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
    ugly for tokens) or to overload operators to work for token operands."""
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
    # their parser_instance at runtime, so they can look at instance.  If
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
        construct_dict = {} # Dict of constructs (handler funs and associated data).
        construct_dict[HEAD] = OrderedDict() # Head constructs sorted by priority.
        construct_dict[TAIL] = OrderedDict() # Tail constructs sorted by priority.
        static_prec = 0 # The prec value for this kind of token, with default zero.
        token_label = None # Set to the actual value later, by create_token_subclass.
        parser_instance = None # Set by the `PrattParser` method `def_token`.

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
            #self.actual_sig = {} # Currently just look at tree, can read off the types.

        @classmethod
        def prec(cls):
            """Return the precedence for the token.  This is currently a static
            value for each type of token.  Later it may be dynamic value
            associated with the particular tail function which is selected in a
            given context."""
            return cls.static_prec

        @classmethod
        def register_construct(cls, head_or_tail, handler_fun, construct_label,
                               precond_fun, precond_priority=0,
                               type_sig=TypeSig(None, None)):
            """Register a construct (either head or tail) with the
            subclass for this kind of token, setting the given properties.
            This method is only ever called from the `modify_token`
            method of a `PrattParser` instance.

            The `type_sig` argument must be a valid `TypeSig` instance.  Every
            unique type signature is saved, and non-unique ones have their
            previously-associated data overwritten."""

            # Todo: Consider this possible optimization in looking up handler
            # functions, instead of always linear search.  Some very commonly
            # used preconditions can be hashed on and will often give a unique
            # result.  Instead of using just `head_or_tail` to hash the handler
            # funs and then linear search, what about a tuple of several common
            # conditions:
            #
            #    (head_or_tail, peek_token_label, pstate_stack_top)
            # Then you need to check for those values and the None values,
            # so three hashes:
            #    (head_or_tail, None, pstate_stack_top)
            #    (head_or_tail, peek_token_label, None)
            #    (head_or_tail, None, None)
            #
            # Then in `lookup_construct` go through all three of these sorted
            # sublists in parallel, from the beginning, taking by top priority
            # and running the precond funs.  Almost like a real `case`
            # statement with jumps.  To get the optimized version you would
            # need to specially register that the token uses the precondition,
            # and give a value for it to look for (i.e., to be hashed under).
            # Probably should pass precond assertions list to the
            # `modify_token` routine, maybe something like:
            #    precond_optimize_assert=(None,None,"wff")
            # For those which register the optimization you still need a precond
            # function itself, at least for the unique name, but you can leave off
            # testing the asserted conditions because they won't be considered if
            # the hash doesn't match.
            #
            # Note that the fallback behavior is the same as before (very
            # slight performance penalty, a few hashes, especially with careful coding).
            # On the other hand, how long will the list of handlers ever get?  Is
            # it worth it?  Probably for doing recursive descent it is.
            #
            # Be sure to also update the unregister method if implemented.

            # Get and save any previous type sig data (for overloaded sigs
            # corresponding to a single construct_label).
            sorted_construct_dict = cls.construct_dict[head_or_tail]
            prev_construct = sorted_construct_dict.get(construct_label, None)
            if prev_construct is None:
                prev_sigs = []
                prev_ast_data = {}
                prev_eval_funs = {}
            else:
                prev_sigs = prev_construct.original_sigs
                prev_ast_data = prev_construct.ast_data_dict
                prev_eval_funs = prev_construct.eval_fun_dict

            if prev_construct and not cls.parser_instance.overload_on_arg_types:
                raise TypeErrorInParsedLanguage("Value of cls.overload_on_arg_types"
                       " is False but attempt to redefine and possibly set multiple"
                       " signatures for the {0} function for token with label '{1}'"
                       " with preconditions label '{2}'."
                       .format(head_or_tail, cls.token_label, construct_label))

            # For overloading, append the type_sig to prev_type_sigs_for_precond,
            # saving them all.  A static method of TypeSig currently does it
            # since it depends on the definition of TypeSig equality.
            TypeSig.append_sig_to_list_replacing_if_identical(prev_sigs, type_sig)

            # Set up the new construct.
            new_construct = SyntaxConstruct(cls.parser_instance,
                                            construct_label=construct_label,
                                            trigger_head_or_tail=head_or_tail,
                                            trigger_token_label=cls.token_label,
                                            handler_fun=handler_fun,
                                            precond_fun=precond_fun,
                                            precond_priority=precond_priority,
                                            original_sig=prev_sigs,
                                            ast_data_dict=prev_ast_data,
                                            eval_fun_dict=prev_eval_funs)
            # Also pass on the eval_fun_dict and ast_fun_dict.
            sorted_construct_dict[construct_label] = new_construct

            # Re-sort the OrderedDict, since we added an item.
            resorted_handler_dict = sort_handler_dict(sorted_construct_dict)
            cls.construct_dict[head_or_tail] = resorted_handler_dict

            # Make sure we don't get multiple definitions with the same
            # priority when the new one is inserted.
            #
            # TODO: Note that WE MAY NOT WANT TO DO THIS AT ALL, or just give a
            # warning.  See case of preconds for quantifiers in logic example
            # which use MUTUALLY EXCLUSIVE handlers which have the same priority.
            # Code below works but isn't being run... consider what it should
            # do and delete if no use.
            pre_check_for_same_priority = False # CODE WORKS BUT SHOULD IT EVER RUN?
            if pre_check_for_same_priority:
                for p_label, data_item in resorted_handler_dict.items():
                    if p_label == construct_label:
                        continue
                    if (data_item.precond_priority == precond_priority and
                                cls.parser_instance.raise_exception_on_precondition_ties):
                        raise ParserException("Two preconditions for the token"
                                " subclass named '{0}' for token with label '{1}' have"
                                " the same priority, {2}.  Their precondition labels"
                                " are '{3}' and '{4}'.  If precondition labels are"
                                " the same there may be a redefinition. Set the flag"
                                " False if you actually want to allow precondition"
                                " ties." .format(cls.__name__, cls.token_label,
                                    precond_priority, construct_label, p_label))
            return

        @classmethod
        def unregister_construct(cls, head_or_tail,
                                 construct_label=None, type_sig=None):
            """Unregister the previously-registered construct (head or
            tail).  If `construct_label` is not set then all head or tail
            handlers (as selected by `head_or_tail`) are unregistered.  If
            `type_sig` is not present then all overloads are also unregistered.
            No error is raised if a matching handler function is not found."""
            # TODO Untested method,  NEEDS REWRITING since constructs used now...

            if construct_label is None:
                if head_or_tail in cls.construct_dict:
                    cls.construct_dict[head_or_tail] = OrderedDict()
                    cls.handler_sigs[head_or_tail] = {}
                return

            # Tuple format for sorted_handler_list is:
            #     (precond_fun, precond_priority, handler_fun)
            sorted_handler_dict = cls.construct_dict[head_or_tail]
            if not sorted_handler_dict:
                return

            if not construct_label in sorted_handler_dict:
                return

            if type_sig is None:
                del sorted_handler_dict[construct_label]
                return

            sig_list = sorted_handler_dict[construct_label]

            for i in reversed(range(len(sorted_handler_dict))):
                item = sorted_handler_dict[i]
                handler_fun = item.handler_fun
                new_handler_sigs = [s for s in handler_fun.type_sigs if s != type_sig]
                if not new_handler_sigs:
                    del sorted_handler_dict[i] # No type sigs at all, remove item.
                    continue
                handler_fun.type_sigs = new_handler_sigs
            return

        def lookup_construct(self, head_or_tail, lex=None, lookbehind=None,
                                                           construct_label=None):
            """Look up and return the construct for the given subexpression
            position in `head_or_tail`, based on the current state.

            Either the `lex` parameter or the `construct_label` parameter must be
            set.  If `lex` is set it will be passed to the precondition
            functions as an argument, and similarly for `lookbehind`.

            This method evaluates each preconditions function in the sorted
            dict for this kind of token and the specified kind of construct
            (head or tail), returning the construct associated with the first
            one which evaluates to `True`.  Raises `NoHandlerFunctionDefined`
            if no handler function can be found.

            If the parameter `construct_label` is set then this method returns
            the construct which *would be* returned, assuming that that were
            the label of the "winning" construct.  Not currently used.

            This function also sets the attribute `construct_label` of this token
            instance to the label of the winning precondition function."""

            sorted_construct_dict = self.construct_dict[head_or_tail]
            if not sorted_construct_dict:
                raise NoHandlerFunctionDefined(
                        "No {0} handler functions at all are defined"
                        " for tokens with token label '{1}'.  The token's"
                        " value is '{2}'."
                        .format(head_or_tail, self.token_label, self.value))

            if construct_label: # This condition is not currently used in the code.
                for pre_fun_label, construct in sorted_construct_dict.items():
                    if pre_fun_label == construct_label:
                        return construct.precond_fun

            # Sequentially run sorted precondition functions until one is true.
            for pre_label, construct in sorted_construct_dict.items():
                if construct.precond_fun(lex, lookbehind):
                    # Note construct_label is saved as a user-accesible attribute here.
                    self.construct_label = construct.construct_label
                    return construct

            raise NoHandlerFunctionDefined("No {0} handler function matched the "
                    "token with value '{1}' and label '{2}' in the current "
                    "preconditions."
                    .format(head_or_tail, self.value, self.token_label))

        def dispatch_handler(self, head_or_tail, lex, left=None, lookbehind=None):
            """Look up and return the handler function for the token, with its arguments
            bound."""
            if head_or_tail == HEAD:
                construct = self.lookup_construct(HEAD, lex)
                handler = functools.partial(construct.run, construct, self, lex)
            elif head_or_tail == TAIL:
                construct = self.lookup_construct(TAIL, lex, lookbehind=lookbehind)
                handler = functools.partial(construct.run, construct, self, lex, left)
            else:
                raise ParserException("Bad first argument to dispatch_handler"
                        " function: must be HEAD or TAIL or the equivalent.")
            return handler

        def process_and_check_node(self, fun_object=None,
                                   typesig_override=None, check_override_sig=False,
                                   in_tree=True, repeat_args=False):
            """This routine should always be called from inside the individual
            head and tail handler functions, just before they return a value.
            It sets some attributes and checks that the actual types match some
            defined type signature for the function.

            (This function does not need to be called in a handler if no
            type-checking or type overloading is desired and if no `in_tree`
            options are used.  This function may later also implement other
            options.)

            The `sig_tok` argument is needed when the token originally
            registered with the type signature information (i.e., passed to
            `modify_token`) does not end up as the root of the resulting
            expression subtree.  In that cast `sig_tok` should be passed the
            name of the token which was originally registered to hold the type
            signature.  The signature will then be looked up from the dict in
            that token.  For example, in defining a standard function using a
            tail handler on the opening lpar the lpar token is the one which
            activates the handler.  But the lpar token never actually appears
            in the tree, the function name to the left does.  So on processing
            the tree with root at the function name this routine needs to be
            passed the lpar token as the `sig_tok` argument.

            TODO: eval funs will probably have same problem as above

            The `typesig_override` argument must be a `TypeSig` instance.  It
            will be *assigned* to the node as its `expanded_formal_sig` after
            all normal type-checking, overriding any other settings.  This is
            useful for handling things like parentheses and brackets which
            inherit the type of their child (assuming the parens and brackets
            are kept as nodes in the parse tree and not eliminated).  The
            `eval_fun` and `ast_data` found by the normally-resolved typesig
            are also keyed under the override signature.

            If `check_override_sig` is true then the overridden signature will
            be set to the only possible signature before type-checking and will
            then be type-checked.  It is expanded as usual in the checking
            process.  Note that this causes a conflict if evaluation functions
            or AST data is stored with the token, because that information is
            in a dict keyed by the original type signature.  In this case,
            though, the override signature becomes the original type signature,
            which will generally not have the correct information keyed under
            it.

            If `in_tree` is set false then the node for this token will not
            appear in the final token tree: its children will replace it, in
            order, in its parent node's list of children.  This does not
            (currently) work for the root node, which has no parent.  The
            default is true, i.e., the token appears in the parse tree.

            Setting `in_tree` to `False` can be useful for things like unary
            plus and parentheses which are not wanted in the final tree.

            If `repeat_args` is true then the argument types for defined type
            signatures will be expanded to match the actual number of
            arguments, if possible, by cyclically repeating them an arbitary
            number of times."""
            self.in_tree = in_tree

            # Process the children to implement `in_tree`, if set.
            modified_children = []
            for child in self.children:
                if not hasattr(child, "in_tree"):
                    continue # `process_and_check_node` wasn't called on child.
                if child.in_tree:
                    modified_children.append(child)
                else:
                    modified_children += child.children
            self.children = modified_children

            #
            # Just return and skip type-checking if the skip option is set.
            #

            if self.parser_instance.skip_type_checking:
                return

            #
            # Otherwise, do type checking below.
            #

            # Get all the sigs for the node while we have access to fun_object.
            if typesig_override and check_override_sig:
                all_possible_sigs = [typesig_override]
            else:
                all_possible_sigs = fun_object.construct_stack[-1].original_sigs
            self.all_possible_sigs = all_possible_sigs # Saved ONLY for error messages.

            # Do the actual checking.
            if not self.parser_instance.overload_on_ret_types: # One-pass.
                self._check_types(all_possible_sigs, repeat_args)
            else: # Two-pass.
                self._check_types(all_possible_sigs, repeat_args, first_pass_of_two=True)
                # If we have a *unique* matching sig, run pass two on the
                # subtree.  In this case, since the signature is fixed by
                # argument types (regardless of where the top-down pass
                # starts from) we can in this case resolve the types in the
                # subtree early.  Note `check_types_in_tree_second_pass` is
                # also called on the root node from the `parse` method.
                if len(self.matching_sigs) == 1: # matching_sigs set by _check_types
                    self.check_types_in_tree_second_pass()

            # Do the override in the case where no checking is done on the
            # override sig.  Only the `expanded_formal_sig` is set to the
            # override sig; the `original_formal_sig` is the one found in
            # the actual checking above.
            if typesig_override and not check_override_sig:
                orig_sig = self.expanded_formal_sig.original_formal_sig
                self.original_formal_sig = orig_sig
                prev_eval_fun = self._get_eval_fun(orig_sig)
                prev_ast_data = self._get_ast_data(orig_sig)
                self._save_eval_fun_and_ast_data(self.is_head, self.construct_label,
                                                 typesig_override,
                                                 prev_eval_fun, prev_ast_data)
                # Force the final, actual typesig to be the override sig.
                self.expanded_formal_sig = typesig_override

        def _check_types(self, all_possible_sigs, repeat_args, first_pass_of_two=False):
            """Utility function called from `process_and_check_node` to check
            the actual types against their signatures.  It assumes a single
            pass unless `first_pass_of_two` is set.  The `all_possible_sigs` argument is
            a list (or iterable) of all the possible signatures for the
            node."""
            # Note that self.all_possible_sigs is now saved (currently for error
            # messages) so the all_possible_sigs argument to this function really
            # isn't needed.

            if not first_pass_of_two:
                # Ordinary case, each child c has a unique c.expanded_formal_sig already set.
                list_of_child_sig_lists = [[c.expanded_formal_sig] for c in self.children]
            else:
                # First pass case, multiple sigs in child's self.matching_sigs list.
                list_of_child_sig_lists = [c.matching_sigs for c in self.children]

            # Reduce to only the signatures that the types of the children match.
            self.matching_sigs = TypeSig.get_all_matching_expanded_sigs(
                                      all_possible_sigs, list_of_child_sig_lists,
                                      tnode=self, repeat_args=repeat_args)
            # Below all_possible_sigs is saved ONLY for printing error messages.
            self.all_possible_sigs = all_possible_sigs

            if not first_pass_of_two:
                if len(self.matching_sigs) != 1:
                    self._raise_type_mismatch_error(self.matching_sigs,
                            "Ambiguous type resolution: The actual argument types match"
                            " multiple signatures.")

                # Found a unique signature; set the node's expanded_formal_sig attribute.
                # Saved sig used for eval_fun resolution, ast_data, semantic action, etc.
                self.expanded_formal_sig = self.matching_sigs[0]
                self.original_formal_sig = self.expanded_formal_sig.original_formal_sig

                # Delete some temporary attributes no longer needed since final pass.
                delattr(self, "matching_sigs")
                delattr(self, "all_possible_sigs")
            return

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

            # Update the matching_sigs attribute for each child (should be singleton).
            for count, child in enumerate(self.children):
                if not hasattr(child, "matching_sigs"): continue # Already resolved.
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

        @classmethod
        def _save_eval_fun_and_ast_data(cls, is_head, construct_label,
                                        type_sig, eval_fun, ast_data):
            """This is a utility function that saves data in the `eval_fun_dict`
            and `ast_data_dict` associated with token `token_subclass`, keyed by
            the `TypeSig` instance `typesig` and also by the `arg_types` of that
            typesig.  This is used so overloaded instances can have different
            evaluations and AST data."""
            if is_head: construct = cls.construct_dict[HEAD][construct_label]
            else: construct = cls.construct_dict[TAIL][construct_label]

            # Save in dicts hashed with full signature (full overload with return).
            dict_key = type_sig
            construct.ast_data_dict[dict_key] = ast_data
            construct.eval_fun_dict[dict_key] = eval_fun
            # Also save in dicts hashed only on args (overloading only on args).
            dict_key = type_sig.arg_types
            construct.ast_data_dict[dict_key] = ast_data
            construct.eval_fun_dict[dict_key] = eval_fun
            # Also save in dicts hashed only on precond label (no overloading).
            dict_key = None
            construct.ast_data_dict[dict_key] = ast_data
            construct.eval_fun_dict[dict_key] = eval_fun

        def _get_eval_fun(self, orig_sig):
            """Return the evaluation function saved by `_save_eval_fun_and_ast_data`.
            Must be called after parsing because the `construct_label` attribute must
            be set on the token instance.  Keyed on `is_head`, `construct_label`, and
            original signature."""
            construct_label = self.construct_label # Attribute set during parsing.
            if self.is_head: construct = self.construct_dict[HEAD][construct_label]
            else: construct = self.construct_dict[TAIL][construct_label]

            if self.parser_instance.overload_on_ret_types:
                dict_key = orig_sig
            elif self.parser_instance.overload_on_arg_types:
                dict_key = orig_sig.arg_types
            else:
                dict_key = None
            return construct.eval_fun_dict.get(dict_key, None)

        def _get_ast_data(self, orig_sig):
            """Return the ast data saved by `_save_ast_data_and_ast_data`.
            Must be called after parsing because the `construct_label` attribute must
            be set on the token instance.  Keyed on `is_head`, `construct_label`, and
            original signature."""
            construct_label = self.construct_label # Attribute set during parsing.
            if self.is_head: construct = self.construct_dict[HEAD][construct_label]
            else: construct = self.construct_dict[TAIL][construct_label]

            if self.parser_instance.overload_on_ret_types:
                dict_key = orig_sig
            elif self.parser_instance.overload_on_arg_types:
                dict_key = orig_sig.arg_types
            else:
                dict_key = None
            return construct.ast_data_dict.get(dict_key, None)

        def eval_subtree(self):
            """Run the saved evaluation function on the token, if one was
            registered with it.  Will raise an error if with no such function
            is found."""
            sig = self.expanded_formal_sig
            orig_sig = self.original_formal_sig

            eval_fun = self._get_eval_fun(orig_sig)

            if not eval_fun:
                raise ParserException("An evaluation function is needed for token with "
                        "value '{0}' and label '{1}' but no defined and matching "
                        "evaluation function was found in the dict of eval functions.  "
                        "The resolved original signature is {2} and the resolve expanded"
                        " signature is {3}.  The resolved construct_label is {4}.  The "
                        "token's eval_fun_dict is:\n   {5}."
                        .format(self.value, self.token_label, orig_sig, sig,
                                self.construct_label, self.eval_fun_dict))

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
                    # Dispatch without calling here, since calling will consume
                    # another token; also, deeper-level recursions could cause false
                    # results to come up the recursion chain.
                    peek_head_handler = curr_token.dispatch_handler(HEAD, lex)
                    try: # Found head handler, now make sure it has no tail handler.
                        peek_tail_handler = curr_token.dispatch_handler(
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
            parser_instance = self.parser_instance

            # Skip head-handling if `processed_left` passed in.  ONLY skipped
            # when called from relaying null-string tail handlers.
            if not processed_left:
                curr_token, head_handler = self.get_null_string_token_and_handler(
                                                           HEAD, lex, subexp_prec)
                if not curr_token:
                    curr_token = lex.next()
                    head_handler = curr_token.dispatch_handler(HEAD, lex)
                curr_token.is_head = True
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
            return ("<" + str(self.token_label) +
                    "," + str(self.value) +
                    "," + str(self.expanded_formal_sig.val_type) + ">")

        def tree_repr_with_types(self, indent=""):
            string = self.summary_repr_with_types()
            for c in self.children:
                string += c.tree_repr_with_types(indent=indent+" "*4)
            return string

        def string_repr_with_types(self):
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
# Parser
#

# Some convenient constants, which double as strings to use in error messages.
HEAD = "head"
TAIL = "tail"

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

        If a Lexer is passed in the parser will use that lexer and its token
        table, otherwise a new lexer is created.  The previous lexer options
        are ignored.

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

        # Below is ugly, but avoids mutual import problems.  These classes from
        # the production_rules module are needed for processing productions.
        # TODO: could move that code back into the production_rules module, but
        # then the interface to this module might be a little strange...
        from .production_rules import Item, ItemList, CaseList
        self.Item = Item
        self.ItemList = ItemList
        self.CaseList = CaseList

    #
    # Methods defining tokens.
    #

    def def_token_master(self, token_label, regex_string=None, on_ties=0, ignore=False,
                         ignored_token_label=None, token_kind="regular"):
        """The master method for defining tokens; all the convenience methods
        actually call it.  Allows for factoring out some common code and
        keeping the attributes of all the different kinds of tokens up-to-date.
        This routine calls the underlying lexer's `def_token` to get tokens and
        then adds extra attributes needed by the `PrattParser` class.

        The `token_kind` argument must be one of the following strings:
        `"regular"`, `"ignored"`, `"begin"`, `"end"`, `"jop"`, or
        `"null-string"`.
        """
        token_table = self.token_table

        if token_kind == "regular":
            tok = token_table.def_token(token_label, regex_string,
                                      on_ties=on_ties, ignore=ignore)

        elif token_kind == "ignored":
            tok = token_table.def_token(token_label, regex_string,
                                             on_ties=on_ties, ignore=True)

        elif token_kind == "begin":
            token_table.def_begin_token(token_label)
            self.begin_token_label = token_label
            # Define dummy handlers for the begin-token.
            def begin_head(self, lex):
                raise CalledBeginTokenHandler("Called head-handler for begin token.")
            def begin_tail(self, lex, left):
                raise CalledBeginTokenHandler("Called tail-handler for begin token.")
            tok = self.def_construct(token_label, head=begin_head, tail=begin_tail)
            self.begin_token_subclass = tok

        elif token_kind == "end":
            token_table.def_end_token(token_label)
            self.end_token_label = token_label
            # Define dummy handlers for the begin-token.
            def end_head(self, lex):
                raise CalledEndTokenHandler("Called head-handler for end token.")
            def end_tail(self, lex, left):
                raise CalledEndTokenHandler("Called tail-handler for end token.")
            tok = self.def_construct(token_label, head=end_head, tail=end_tail)
            self.end_token_subclass = tok

        elif token_kind == "jop":
            if self.jop_token_subclass:
                raise ParserException("A jop token is already defined.  It must be "
                                      "undefined before defining a new one.")
            self.jop_token_label = token_label
            self.jop_ignored_token_label = ignored_token_label
            tok = token_table.def_token(token_label, None)
            self.jop_token_subclass = tok

        elif token_kind == "null-string":
            if self.null_string_token_subclass:
                raise ParserException("A null-string token is already defined.  It"
                         " must be undefined before defining an new one.")
            self.null_string_token_label = token_label
            tok = token_table.def_token(token_label, None)
            self.null_string_token_subclass = tok

        else:
            raise ParserException("Bad call to def_token_master, with unrecognized"
                    ' string "{0}" for the keyword argument token_kind.'
                    .format(token_kind))

        tok.token_kind = token_kind
        tok.parser_instance = self
        tok.token_table = self.token_table
        tok.is_head = False # Set true in recursive_parse if instance parses as a head.
        return tok

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False):
        """Define a token.  Use this instead of the Lexer `def_token` method,
        since it adds extra attributes to the tokens."""
        return self.def_token_master(token_label, regex_string, on_ties, ignore,
                                     token_kind="regular")

    def def_ignored_token(self, token_label, regex_string, on_ties=0):
        """A convenience function to define a token with `ignored=True`."""
        return self.def_token_master(token_label, regex_string, on_ties,
                                     ignore=True, token_kind="ignored")

    def def_multi_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once.  Each element
        of the passed-in list should be a tuple containing the arguments to the
        ordinary `def_token` method.  Calls the equivalent `Lexer` function."""
        return multi_funcall(self.def_token, tuple_list, ParserException)

    def def_multi_ignored_tokens(self, tuple_list):
        """A convenience function, to define multiple ignored tokens at once.
        Each element of the passed-in list should be a tuple containing the arguments
        to the ordinary `def_token` method with `ignore=True`.  Calls the equivalent
        `Lexer` function."""
        return multi_funcall(self.def_ignored_token, tuple_list, ParserException)

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
        inferred to be present.  This method must be called before a
        juxtaposition operator can be used.  The parameter `jop_token_label` is
        the label for the newly-created token representing the juxtaposition
        operator.  The `ignored_token_label` parameter is the label of an
        ignored token which must be present for a jop to be inferred.  Some
        already-defined token is required; usually it will be a token for spaces
        and tabs.  If set to `None` then no ignored space at all is required
        (i.e., the tokens can be right next to each other)."""
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

    def def_default_whitespace(self, space_label="k_space", space_regex=r"[ \t]+",
                        newline_label="k_newline", newline_regex=r"[\n\f\r\v]+"):
        """Define the standard whitespace tokens for space and newline, setting
        them as ignored tokens."""
        # Note + symbol for one or more, NOT the * symbol for zero or more.
        self.def_ignored_token(space_label, space_regex)
        self.def_ignored_token(newline_label, newline_regex)

    #
    # Undefine tokens.
    #

    def undef_token(self, token_label):
        """A method for undefining any token defined by the `PrattParser` methods.
        Since the `token_kind` was set for all tokens when they were defined
        it knows how to undelete any kind of token."""
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
    # Methods to access and modify tokens.
    #

    def get_token(self, token_label):
        """Return the token with the label `token_label`.  The reverse
        operation, getting a label from a token instance, can be done by
        looking at the `token_label` attribute of the token."""
        return self.token_table[token_label]

    def def_construct(self, trigger_token_label, prec=None,
                      head=None, tail=None,
                      construct_label=None, precond_fun=None,
                      precond_priority=0, val_type=None, arg_types=None,
                      eval_fun=None, ast_data=None):
        """Look up the subclass of base class `TokenNode` corresponding to the
        label `token_label` (in the token table) and modify its properties.  A
        token with that label must already be in the token table, or an
        exception will be raised.

        Returns the modified class. Saves any given `head` or `tail` functions
        in the handler dict for the token.  Both can be passed in one call, but
        they must have the same preconditions function and priority.

        If `tail` is set then the prec will also be set unless `prec` is
        `None`.  For a head the `prec` value is ignored.  If `tail` is set and
        `prec` is `None` then the prec value defaults to zero.

        The `eval_fun` and the `ast_data` arguments are saved in the dicts
        `eval_fun_dict` and `ast_data_dict` respectively, keyed by the
        `TypeSig` defined by `val_type` and `arg_types`, as well as by
        `arg_types` alone for when overloading on return values is not used.
        This allows for different overloads to have different evaluation
        functions and AST-associated data."""
        # Why not make this a method of TokenNode?  Keep in mind the
        # distinction between the subclass (representing the token in general)
        # and the instances (representing particular lexed tokens).  For now
        # this seems better, as a consequence of using token labels rather than
        # token instances in the API.  The label needs to be looked up in the
        # token table (which this could actually be a method of instead, but
        # the lexer's TokenTable would need to be subclassed to add PrattParser
        # related methods and the call would be less convenient or need a
        # convenience wrapper).  If token instances were used in the API then
        # it would be better as a method of TokenNode, but tokens need to be
        # stored under labels and using labels makes things like multi-def work
        # better.

        # Note that the parser_instance attribute of tokens is not necessarily
        # set yet when this is called from token_master.  It gets set at the end
        # of that routine.

        if isinstance(arg_types, str):
            raise ParserException("The arg_types argument to token_subclass must"
                    " be None or an iterable returning type labels (e.g., a list"
                    " or tuple).")

        if tail and (prec is None):
            prec = 0

        if construct_label is None:
            construct_label = DEFAULT_CONSTRUCT_LABEL
            precond_fun = DEFAULT_ALWAYS_TRUE_PRECOND_FUN

        if trigger_token_label in self.token_table:
            token_subclass = self.get_token(trigger_token_label)
        else:
            raise ParserException("In call to mod_token_subclass: subclass for"
                    " token labeled '{0}' has not been defined.  Maybe try"
                    " calling `def_token` first.".format(trigger_token_label))
            # Below line formerly just created a subclass, but that can mask errors!
            #TokenSubclass = self.token_table.create_token_subclass(token_label)

        if tail:
            token_subclass.static_prec = prec # Ignore prec for heads; it will stay 0.

        # Create the type sig object.
        type_sig = TypeSig(val_type, arg_types)

        # Register the handler funs.
        if head:
            token_subclass.register_construct(HEAD, head,
                               construct_label=construct_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig)
            token_subclass._save_eval_fun_and_ast_data(True, construct_label,
                                                   type_sig, eval_fun, ast_data)
        if tail:
            token_subclass.register_construct(TAIL, tail,
                               construct_label=construct_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig)
            token_subclass._save_eval_fun_and_ast_data(False, construct_label,
                                                   type_sig, eval_fun, ast_data)

        return token_subclass

    def undef_handler(self, token_label, head_or_tail, construct_label=None,
                         val_type=None, arg_types=None, all_handlers=False):
        """Undefine a head or tail function with the given `token_label`,
        `construct_label` and type signature.  The `head_or_tail` value should be
        `HEAD` or `TAIL`.  If `all_precond` is set then all heads and tails for all
        preconditions will be undefined.  If `all_overloads` then all
        overloaded type signatures will be undefined.  The token itself is
        never undefined; use the `undef_token` method for that."""
        # TODO: rewrite to undef a construct
        TokenSubclass = self.token_table[token_label]
        TokenSubclass.unregister_construct(head_or_tail,
                                         construct_label=construct_label,
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
    # Methods defining syntax elements.
    #

    # TODO these define and undefine methods each need a corresponding undefine
    # method (or one that does all); should be easy with undef_handler method.
    # Is it necessary to call undef_handler, or does undef the token do that
    # too?  Look at undef_handler and see........ use below if needed.
    #
    # ALSO, consider defining them in a separate file and then importing them
    # and pasting them onto the class, just to keep the code separate.

    def def_literal(self, token_label, val_type=None,
                    construct_label=None, precond_fun=None, precond_priority=1,
                    typesig_override_fun=None,
                    eval_fun=None, ast_data=None):
        """Defines the token with label `token_label` to be a literal in the
        syntax of the language being parsed.  This method adds a head handler
        function to the token.  Literal tokens are the leaves of the expression
        trees; they are things like numbers and variable names in a numerical
        expression.  They always occur as the first (and only) token in a
        subexpression being evaluated by `recursive_parse`, so they need a head
        handler but not a tail handler.  (Though note that the token itself
        might also have a tail handler.)

        A function `typesig_override_fun` can be passed in, taking a token and
        a lexer as its two arguments and returning a `TypeSig` object.  If it
        is set then it will be called from the head handler and the type
        signature of the node will be assigned the returned signature.  Can be
        useful for dynamic typing such as when identifiers in an interpreted
        language are generic variables."""

        def head_handler_literal(tok, lex):
            if typesig_override_fun:
                tok.process_and_check_node(head_handler_literal,
                                      #check_override_sig=True,
                                      typesig_override=typesig_override_fun(tok, lex))
            else:
                tok.process_and_check_node(head_handler_literal)
            return tok
        return self.def_construct(token_label, head=head_handler_literal,
                                  val_type=val_type, arg_types=(),
                                  construct_label=construct_label,
                                  precond_fun=precond_fun,
                                  precond_priority=precond_priority,
                                  eval_fun=eval_fun, ast_data=ast_data)

    def def_multi_literals(self, tuple_list):
        """An interface to the `def_literal` method which takes a list of
        tuples.  The `def_literal` method will be called for each tuple, unpacked
        in the order in the tuple.  Unspecified optional arguments get their default
        values."""
        # TODO: This unfortunately makes it difficult to change the order in the
        # def_literal function arguments... also causes problems with keyword
        # argument setting (need to be non-keywords).  Deprecate this, maybe,
        # at least make it preferred to use `lit = parser.def_literal` and then just
        # write that a bunch of times...
        return multi_funcall(self.def_literal, tuple_list)

    def def_infix_multi_op(self, operator_token_labels, prec, assoc,
                           repeat=False, in_tree=True,
                           construct_label=None, precond_fun=None, precond_priority=0,
                           val_type=None, arg_types=None, eval_fun=None, ast_data=None):
        # TODO only this utility method currently supports "in_tree" kwarg.
        # General and easy mechanism, though.  Test more and add to other
        # methods.  Does in-tree keep the first one? how is it defined for this
        # thing?  Comma operator is example of in_tree=False, but how does it
        # handle the root??  TODO: How about in-tree that works at root iff the
        # node only has one child?
        """Takes a list of operator token labels and defines a multi-infix
        operator.

        If `repeat=True` it will accept any number of repetitions of
        the list of operators (but type-checking for that is not implemented
        yet).  For a single operator, repeating just has the effect of putting
        the arguments in a flat argument/child list instead of as nested binary
        operations based on left or right association.  Any argument-checking
        is done after any node removal, which may affect the types that should
        be passed-in in the list arg_types of parent constructs.

        If `in_tree` is false.......
        """
        if assoc not in ["left", "right"]:
            raise ParserException('Argument assoc must be "left" or "right".')
        recurse_bp = prec
        if assoc == "right": recurse_bp = prec - 1
        def tail_handler(tok, lex, left):
            tok.append_children(left, tok.recursive_parse(recurse_bp))
            while True:
                for op in operator_token_labels[1:]:
                    lex.match_next(op, raise_on_fail=True)
                    assert tok.prec() == recurse_bp or tok.prec()-1 == recurse_bp # DEBUG
                    tok.append_children(tok.recursive_parse(recurse_bp))
                if not repeat: break
                # Peek ahead and see if we need to loop another time.
                if lex.peek().token_label != operator_token_labels[0]: break
                lex.match_next(operator_token_labels[0], raise_on_fail=True)
                tok.append_children(tok.recursive_parse(recurse_bp))
            tok.process_and_check_node(tail_handler, in_tree=in_tree,
                                                     repeat_args=repeat)
            return tok
        return self.def_construct(operator_token_labels[0], prec=prec, tail=tail_handler,
                                construct_label=construct_label, precond_fun=precond_fun,
                                precond_priority=precond_priority,
                                val_type=val_type, arg_types=arg_types,
                                eval_fun=eval_fun, ast_data=ast_data)

    def def_infix_op(self, operator_token_label, prec, assoc, in_tree=True,
                     construct_label=None, precond_fun=None, precond_priority=0,
                     val_type=None, arg_types=None, eval_fun=None, ast_data=None):
        """This just calls the more general method `def_multi_infix_op`."""
        return self.def_infix_multi_op([operator_token_label], prec, assoc, in_tree=in_tree,
                                       construct_label=construct_label, precond_fun=precond_fun,
                                       precond_priority=precond_priority,
                                       val_type=val_type, arg_types=arg_types,
                                       eval_fun=eval_fun, ast_data=ast_data)

    def def_prefix_op(self, operator_token_label, prec,
                      construct_label=None, precond_fun=None, precond_priority=0,
                      val_type=None, arg_types=None,
                      eval_fun=None, ast_data=None):
        """Define a prefix operator.  Note that head handlers do not have
        precedences, only tail handlers.  (With respect to the looping in
        `recursive_parse` it wouldn't make a difference.)  But, within the head
        handler, the call to `recursive_parse` can be made with a nonzero
        precedence.  This allows setting a precedence to determine the argument
        expressions that the prefix operators grabs up (or doesn't)."""
        def head_handler(tok, lex):
            tok.append_children(tok.recursive_parse(prec))
            tok.process_and_check_node(head_handler)
            return tok
        return self.def_construct(operator_token_label, head=head_handler,
                                 construct_label=construct_label, precond_fun=precond_fun,
                                 precond_priority=precond_priority,
                                 val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                                 ast_data=ast_data)

    def def_postfix_op(self, operator_token_label, prec, allow_ignored_before=True,
                       construct_label=None, precond_fun=None, precond_priority=0,
                       val_type=None, arg_types=None, eval_fun=None,
                       ast_data=None):
        """Define a postfix operator.  If `allow_ignored_before` is false then
        no ignored token (usually whitespace) can appear immediately before the
        operator."""
        def tail_handler(tok, lex, left):
            if not allow_ignored_before:
                lex.no_ignored_before(raise_on_fail=True)
            tok.append_children(left)
            tok.process_and_check_node(tail_handler)
            return tok
        return self.def_construct(operator_token_label, prec=prec, tail=tail_handler,
                                 construct_label=construct_label, precond_fun=precond_fun,
                                 precond_priority=precond_priority,
                                 val_type=val_type, arg_types=arg_types,
                                 eval_fun=eval_fun, ast_data=ast_data)

    def def_bracket_pair(self, lbrac_token_label, rbrac_token_label,
                         construct_label=None, precond_fun=None, precond_priority=0,
                         eval_fun=None, ast_data=None):
        """Define a matching bracket grouping operation.  The returned type is
        set to the type of its single child (i.e., the type of the contents of
        the brackets).  Defines a head handler for the left bracket token, so
        effectively gets the highest evaluation precedence.  As far as types,
        it is treated as a function that takes one argument of wildcard type
        and returns whatever type the argument has."""
        # TODO: Maybe allow optional comma_token_label for comma-separated.
        # Define a head for the left bracket of the pair.
        def head_handler(tok, lex):
            tok.append_children(tok.recursive_parse(0))
            lex.match_next(rbrac_token_label, raise_on_fail=True)
            if self.skip_type_checking:
                tok.process_and_check_node(head_handler)
            else:
                child_type = tok.children[0].expanded_formal_sig.val_type
                tok.process_and_check_node(head_handler,
                                 #check_override_sig=True,
                                 typesig_override=TypeSig(child_type, [child_type]))
            return tok
        return self.def_construct(lbrac_token_label, head=head_handler,
                                 construct_label=construct_label, precond_fun=precond_fun,
                                 precond_priority=precond_priority,
                                 eval_fun=eval_fun, ast_data=ast_data)

    #def def_assignment_op(assignment_operator_label, prec, assoc, in_tree=True,
    #                      val_type=None, arg_types=None, eval_fun=None, ast_data=None):

    def def_stdfun(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label,
                      precond_priority=1,
                      val_type=None, arg_types=None, eval_fun=None, ast_data=None,
                      num_args=None):
        """This definition of stdfun uses lookahead to the opening paren or
        bracket token.

        Note that all tokens must be defined as literal tokens except
        `fname_token_label` (which ends up as the root of the function
        evaluation subtree).  If the latter is also a literal token then
        `precond_priority` may need to be increased to give this use priority.

        The `num_args` parameter is optional for specifying the number of
        arguments when typing is not being used.  If it is set to a nonnegative
        number then it will automatically set `arg_types` to the corresponding
        list of `None` values; if `arg_types` is set then it is ignored."""
        if not self.skip_type_checking and num_args is not None and arg_types is None:
            arg_types = [None]*num_args

        # TODO maybe have option to match value instead of type...
        # Maybe even always use a distinct construct_label unless some `overload=True`
        # flag is set.... CONSIDER, large design change.
        def preconditions(lex, lookbehind):
            """Must be followed by a token with label 'lpar_token_label', with no
            whitespace in-between."""
            peek_tok = lex.peek()
            if peek_tok.ignored_before: return False
            if peek_tok.token_label != lpar_token_label: return False
            return True
        construct_label = "lpar after, no whitespace between" # Should be a unique label.

        def head_handler(tok, lex):
            # Below match is for a precondition, so it will match and consume.
            lex.match_next(lpar_token_label, raise_on_fail=True)
            # Read comma-separated subexpressions until the peek is rpar_token_label.
            while not lex.match_next(rpar_token_label, consume=False):
                tok.append_children(tok.recursive_parse(0))
                if not lex.match_next(comma_token_label):
                    break
                else:
                    # This checks for errors like f(x,)
                    lex.match_next(rpar_token_label, raise_on_true=True)
            lex.match_next(rpar_token_label, raise_on_fail=True) # Closing rpar.
            tok.process_and_check_node(head_handler)
            if (self.skip_type_checking and num_args is not None
                                        and len(tok.children) != num_args):
                print("tok is", tok, "tok children are", tok.children)
                raise ParserException("Wrong number of arguments for function {0}:"
                            " expected {1} and got {2}.""".format(tok.token_label,
                            num_args, len(tok.children)))
            return tok
        return self.def_construct(fname_token_label, prec=0,
                     head=head_handler, construct_label=construct_label,
                     precond_fun=preconditions, precond_priority=precond_priority,
                     val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                     ast_data=ast_data)

    def def_stdfun_lpar_tail(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label, prec_of_lpar,
                      val_type=None, arg_types=None, eval_fun=None, ast_data=None,
                      num_args=None):
        """This is an alternate version of stdfun that defines lpar as an infix
        operator (i.e., with a tail handler).  This function works in the usual cases
        but the current version without preconditions may have problems distinguishing
        "b (" from "b(" when a multiplication jop is set.  The lookahead version
        `def_stdfun` is usually preferred.  This method assumes type checking is on
        if `num_arg` is set."""
        if num_args is not None and arg_types is None:
            arg_types = [None]*num_args

        def precond_fun(lex, lookbehind):
            """Check that the peek backward token label for the function name
            is `fname_token_label`.  This is necessary to get the type sig info
            to work when different functions take different numbers (and
            possibly different types) of arguments.   Otherwise, defining two
            different functions for different tokens like `k_add` and `k_exp`
            is treated as an overload since both are really handled by the
            `lpar_token_label` token.  The label would otherwise never be checked.

            One could do a similar thing checking the value of the previous token
            if the fnames are all, say, identifiers or some common token kind.
            Maybe even have a flag to indicate this when worked out better."""
            prev_tok = lex.peek(-1)
            if prev_tok.token_label != fname_token_label: return False
            if lex.token.ignored_before: return False # No space allowed after fun name.
            return True
        # Note we need to generate a unique construct_label for each fname_token_label.
        construct_label = "match desired function name of " + fname_token_label

        def tail_handler(tok, lex, left):
            # Nothing between fun name and lpar_token.
            lex.no_ignored_before(raise_on_fail=True)
            while not lex.match_next(rpar_token_label, consume=False):
                left.append_children(tok.recursive_parse(prec_of_lpar))
                if not lex.match_next(comma_token_label):
                    break
                else:
                    lex.match_next(rpar_token_label, raise_on_true=True)
            lex.match_next(rpar_token_label, raise_on_fail=True)
            left.process_and_check_node(tail_handler)
            return left
        return self.def_construct(lpar_token_label,
                                 prec=prec_of_lpar, tail=tail_handler,
                                 construct_label=construct_label,
                                 precond_fun=precond_fun,
                                 val_type=val_type, arg_types=arg_types,
                                 eval_fun=eval_fun, ast_data=ast_data)

    def def_jop(self, prec, assoc,
                construct_label=None, precond_fun=None, precond_priority=None,
                val_type=None, arg_types=None, eval_fun=None,
                ast_data=None):
        """The function `precond_fun` is called to determine whether or not to
        infer a juxtaposition operator between the previously-parsed
        subexpression result and the next token.  This function will be passed
        the lexer as well as the lookbehind list as arguments.  Note that the
        `jop_precond` function has access to the type information for the
        potential left operand but not for the potential right operand.  If
        this function returns `True` then a jop is inferred and the parse
        proceeds assuming there is a jop token in the token stream.

        Note that if the juxtaposition operator always resolves to a single
        type signature based on its argument types then, even if overloading on
        return types is in effect, the jop can be effectively inferred based on
        type signature information."""
        if assoc not in ["left", "right"]:
            raise ParserException('Argument assoc must be "left" or "right".')

        recurse_bp = prec
        if assoc == "right": recurse_bp = prec - 1
        def tail_handler(tok, lex, left):
            right_operand = tok.recursive_parse(recurse_bp)
            tok.append_children(left, right_operand)
            tok.process_and_check_node(tail_handler)
            return tok
        return self.def_construct(self.jop_token_label, prec=prec, tail=tail_handler,
                                 construct_label=construct_label, precond_fun=precond_fun,
                                 precond_priority=precond_priority,
                                 val_type=val_type, arg_types=arg_types,
                                 eval_fun=eval_fun, ast_data=ast_data)

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


def sort_handler_dict(d):
    """Return the sorted `OrderedDict` version of the dict `d` passed in,
    sorted by the precondition priority in the items.  Used in
    `PrattParser.register_handler_fun` to keep handlers sorted by priority."""
    # https://docs.python.org/3/library/collections.html#ordereddict-examples-and-recipes
    return OrderedDict(sorted(
           d.items(), key=lambda item: item[1].precond_priority, reverse=True))

#
# Exceptions
#

class NoHandlerFunctionDefined(ParserException):
    """Only raised by dispatcher function, and only when it fails to find a
    handler function (head or tail, whichever it was looking for)."""
    pass

class IncompleteParseException(ParserException):
    """Only raised at the end of the `PrattParser` function `parse` if tokens
    remain in the lexer after the parser finishes its parsing."""
    pass


