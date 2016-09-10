# -*- coding: utf-8 -*-
"""

A general Pratt parser module.

To use the `PrattParser` class you need to do these things:

    1. Create an instance of the PrattParser class.  Example::

          parser = PrattParser()
 
    2. Define each token that will appear in the language, including a string
       label and a regex for to recognize that kind of token.  If necessary,
       the appropriate `on_ties` values can be set to break ties in case of equal
       match lengths (the lexer will always take the longest match over all the
       defined tokens, with ties broken by any `on_ties` values, defaulting to zero).
       Examples::

          parser.def_token("k_number", r"\d+")
          parser.def_token("k_lpar", r"\(")
          parser.def_token("k_ast", r"\*")
          parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)
    
    3. Define the syntactical elements of the language that you are parsing.
       Any necessary token labels must have already been defined in the previous
       step.  The predefined syntax-definition methods of `PrattParser` take as
       arguments token labels, type information, etc.  They also take a string
       as a label for the resulting AST node (but this label can be used in any
       way desired, including in preconditions).  Examples::

          t_number = parser.def_type("t_number")
          parser.def_literal("k_number", val_type=t_number, ast_label="a_number")
          parser.def_infix_op("k_plus", 10, "left",
                           val_type="number", arg_types=[t_number, t_number],
                           ast_label="a_add")

       If the predefined methods are not sufficient you might need to create a
       subclass of `PrattParser` to provide additional methods.  Note that
       literals must be defined as syntax, in addition to being defined as
       tokens.  If typing is to be used then any type information should also be
       set for the syntax elements.

    4. Pass the parser a string of text to parse and save the resulting token
      tree. ::

          result_tree = parser.parse("x + (4 + 3)*5")
          print(result_tree.tree_repr())

    5. You can optionally evaluate the resulting tree (if evaluate functions
       were supplied as kwargs for the appropriate methods), or convert it to an
       AST with a different type of nodes. 

In reading the code, the correspondence between the naming convention used here
and Pratt's original naming conventions is given in this table:

+----------------------------------+--------------------------+
| this code                        | Pratt's terminology      |
+==================================+==========================+
| token precedence                 | left binding power, lbp  |
+----------------------------------+--------------------------+
| subexpression precedence         | right binding power, rbp |
+----------------------------------+--------------------------+
| head handler function            | null denotation, nud     |
+----------------------------------+--------------------------+
| tail handler function            | left denotation, led     |
+----------------------------------+--------------------------+

API details
-----------

These are some aspects of the API that are not covered by the function signatures
and docstrings below.

Extra attributes added to tokens
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tokens straight from a `Lexer` instance have certain attributes set, as documented
in the `lexer` module.  The `pratt_parser` module defines its own subclass of the
base `TokenNode` class, which sets some extra attributes.  The parsing process
sets several user-accessible attributes, which are described here.

* `type_sig` -- a `TypeSig` instance giving the value and argument types
* `val_type` -- the type of the token itself, always equals `type_sig.val_type`

TODO: document more

Implementation details
----------------------

This section gives a general overview of the lower-level details of the
`PrattParser` implementation.

The basic class structure
~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
----

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_pratt_parser.py", pytest_args="-v")

import types
import copy
import functools
from collections import OrderedDict, namedtuple, defaultdict

from .shared_settings_and_exceptions import ParserException
from .lexer import (Lexer, TokenNode, TokenTable, LexerException, BufferIndexError,
                    multi_funcall)
from .pratt_types import TypeTable, TypeSig, TypeErrorInParsedLanguage

# TODO: Consider putting versions of the helper functions like match_next in
# the main module namespace as well as in the TokenSubclass space (can probably
# use the same definitions).  That way it might be easier for people to use
# them along with their own helper methods...  or not...  They are just called
# as tok.match_next(...) instead of match_next(tok, ...).  SO, you could define
# them in the main namespace and just set the functions as attributes in the
# class and it would work OK.

# TODO: Define some common helper preconditions.  They are boolean so they can
# easily be composed.  Can store them in a separate module,
# precondition_helpers.py, and then import them in the __init__ for module
# space.  Could store helpers like match_next with them (or similarly) if it
# is determined that they shouldn't be in TokenSubclass namespace.

# TODO: consider how to define a named tuple to take the arguments of the
# functions like literals and require them to be used in the multi-def things.
# Otherwise, gets confusing about what the arguments are, and cannot use
# keywords... May or may not work, though.

# TODO: if PrattParser requires tokens defined from itself, it should mark them
# and refuse to deal with any others.  It implicitly does, with whatever
# attributes it adds.... but a nice early warning would be good before an
# unexpected fail.  Currently checked at one place in recursive_parse, but
# nowhere else.

# NOTE that you could use the evaluate function stuff to also do the conversion
# to AST.

# TODO: Do preconditions really need labels?  Can the users just be left to
# manage their own preconditions, or maybe a separate class can be defined to
# manage them in a dict, maybe with some predefined ones?  Why can't you just
# use the function names (fully qualified)?
#
# f.__module__ + "." + f.__name__
# 
# Seems like it could be separated out, and reduce complexity.  Is equality
# testing of preconditions ever truly required?  Will using function name fail
# somehow?  If not, why not just use the function objects and leave the user to
# manage their functions however they want.
#
# Main problems:
#
# 1) Presumably you need to keep some handle on them in order to undefine them
# and their related handlers, etc.  Seems fair to require users to provide all
# the information for an undo operation of, say, stdfun.  I.e., the token, the
# actual precond function (or just name) as well as the typesig info.  Could be
# saved inside the original routines and then looked up and deleted, but what
# handle to use to do so?  The token can have multiple overrides stored with
# it; you may want to just delete one.  But, the token class (not instance)
# does store all the typesigs with it.
#
# 2) Lambdas get defined each time and all have the same name.  You might have
# to disallow use of lambdas when expecting function equality...  Lambdas just
# get a `__name__ == <lambda>`.  Rule: Handler functions are equal iff their
# fully-qualified name is the same.  Note that __module__ is always
# fully-qualified, BUT can get into bugs when file imported differently because
# of sys.path (Python problem, though).
#
# --> Implement the undo function for, say, a stdfun.  See what is needed to be
# kept.
#
# Consider requiring users to paste precond labels onto any functions that they
# want to use as precond labels.  Keeps it localized, and keeps the same
# function each time (at least if done right after definition).  (Do it inside
# the function?  No that only works at runtime.) Can easily be checked and give
# error message, too.  Then just register them in the hidden dictionary.  Users
# never need to know it is there.  This also makes it clear that the
# precond_label is really a "part" of the function, and defines what it means
# for function equality.

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

    class TokenSubclass(TokenNode):
        handler_funs = {} # Handler functions, i.e., head and tail handlers.
        handler_funs[HEAD] = OrderedDict() # Head handlers sorted by priority.
        handler_funs[TAIL] = OrderedDict() # Tail handlers sorted by priority.
        preconditions_dict = {} # Registered preconditions for this kind of token.
        static_prec = 0 # The prec value for this kind of token, with default zero.
        token_label = None # Set to the actual value later by create_token_subclass.
        parser_instance = None # Set by a `PrattParser` method `def_token`.

        def __init__(self, value):
            """Initialize an instance of the subclass for a token of the kind
            labeled with `token_label`.  The `value` is the actual parsed
            string from the text.  This instance represents the token."""
            super(TokenSubclass, self).__init__() # Call base class __init__.
            self.value = value # Set from lex.token_generator; static value=None.
            self.type_sig = TypeSig(None, None) # The full type sig, set after parsing.
            self.val_type = self.type_sig.val_type # The type of the token (after parsing).

        @classmethod
        def prec(cls):
            """Return the precedence for the token.  This is currently a static
            value for each type of token.  Later it may be dynamic value
            associated with the particular tail function which is selected in a
            given context."""
            return cls.static_prec

        @classmethod
        def register_precond_fun(cls, precond_label, precond_fun):
            """Save the preconditions function `precond_fun` in a dict keyed by
            `precond_label`.  If there is already a dict entry for the label,
            then the new function overwrites the old one (but they should
            compute the same thing if they have the same label, by definition
            of the uniqueness of the precondition labels).  The priority is
            also replaced.  Using labels allows equality between preconditions
            functions to be easily defined and tested, and allows for
            commonly-used precondition functions to be predefined."""
            cls.preconditions_dict[precond_label] = precond_fun

        @classmethod
        def unregister_precond_fun(cls, precond_label):
            """Un-registers the precondition label and its associated function."""
            try:
                del cls.preconditions_dict[precond_label]
            except KeyError:
                raise ParserException("Attempt to unregister a preconditions "
                        "function which is not registered.  The label is '{0}'"
                        "and the token label is '{1}'."
                        .format(precond_label, cls.token_label))

        @classmethod
        def lookup_precond_fun(cls, precond_label):
            """Look up the preconditions function with label "precond_label."
            First checks the dict local to the token subclass, and if that
            fails it checks the parser-global dict."""
            try:
                precond_fun = cls.preconditions_dict[precond_label]
            except KeyError:
                    raise ParserException("In function lookup_preconditions: the "
                        " preconditions label '{0}' has not been registered."
                        .format(precond_label))
            return precond_fun

        @classmethod
        def register_handler_fun(cls, head_or_tail, handler_fun,
                             precond_label=None, precond_fun=None, precond_priority=0,
                             type_sig=TypeSig(None, None)):
            """Register a handler function (either head or tail) with the
            subclass for this kind of token, setting the given properties.
            Thie method is only ever called from the `modify_token_subclass`
            method of a `PrattParser` instance.
            
            If no precondition label or function is provided a dummy
            precondition that always returns `True` will be used.  If
            `precond_priority` is set it will apply to that dummy function, but
            this will completely override anything with a lower priority.

            If `precond_label` is set but `precond_fun` is not then a
            preconditions function will be assumed to have already been
            registered for that label.  If `precond_fun` is also provided then
            it will be registered as a preconditions function with the given
            label.

            The `type_sig` argument must be a valid `TypeSig` instance.  Any
            extra data attributes which should be associated with signatures
            (such as an evaluation function or AST label) should already have
            been made attributes of the passed-in signature.  Every unique
            type signature is saved, and non-unique ones are overwritten.
           
            Data is saved on lists keyed by the value of `head_or_tail`
            (either `HEAD` or `TAIL`) in a dict, along with any specified
            precondition information.  The lists are sorted by priority."""

            # Get and save any previous type sig data (for overloaded sigs
            # corresponding to a single precond_label).
            prev_handler_data_for_precond = cls.handler_funs[head_or_tail].get(
                                                     precond_label, None)

            if prev_handler_data_for_precond:
                prev_type_sigs_for_precond = (
                              prev_handler_data_for_precond.handler_fun.type_sigs)
            else:
                prev_type_sigs_for_precond = []

            if (prev_handler_data_for_precond
                              and not cls.parser_instance.overload_on_arg_types):
                raise TypeErrorInParsedLanguage("Value of cls.overload_on_arg_types is False "
                       "but attempt to redefine and possibly set multiple signatures "
                       "for the {0} function for token with label '{1}' with "
                       "preconditions label '{2}'."
                       .format(head_or_tail, cls.token_label, precond_label))

            # Type info is stored as an attribute of handler funs.  For
            # overloading, append the type_sig to prev_type_sigs_for_precond,
            # saving them all.
            TypeSig.append_sig_to_list_replacing_if_identical(
                                       prev_type_sigs_for_precond, type_sig)
            handler_fun.type_sigs = prev_type_sigs_for_precond

            if precond_fun:
                cls.register_precond_fun(precond_label, precond_fun)

            if precond_label:
                precond_fun = cls.lookup_precond_fun(precond_label)
            else:
                # If neither precond_fun nor precond_label, use dummy True precond
                def true_fun(lex, lookbehind): return True
                true_fun.priority = precond_priority
                precond_fun = true_fun
                # Default precondition label is a tuple so that no string matches it.
                precond_label = ("always-true-default-precondition",)

            # Get the current dict of head or tail handlers for the node,
            # containing the (precond_fun, precond_priority, handler_fun) named
            # tuples which have already been registered.
            sorted_handler_dict = cls.handler_funs[head_or_tail]

            # Set the new HandlerData named tuple and assign it the head or
            # tail dict keyed by its precond label.  This removes any existing
            # handler with the same precondition label, i.e., in that case it
            # is assumed to be a redefinition of the existing one.
            data_list = HandlerData(precond_fun, precond_priority, handler_fun)
            sorted_handler_dict[precond_label] = data_list

            # Re-sort the OrderedDict, since we added an item.
            cls.handler_funs[head_or_tail] = sort_handler_dict(sorted_handler_dict)
            sorted_handler_dict = cls.handler_funs[head_or_tail]

            # Make sure we don't get multiple definitions with the same priority
            # when the new one is inserted.
            for p_label, data_item in sorted_handler_dict.items():
                if p_label == precond_label: continue
                if (data_item.precond_priority == precond_priority and
                            cls.parser_instance.raise_exception_on_precondition_ties):
                    raise ParserException("Two preconditions for the token"
                            " subclass named '{0}' for token with label '{1}' have"
                            " the same priority, {2}.  Their precondition labels"
                            " are '{3}' and '{4}'.  If precondition labels are"
                            " the same there may be a redefinition. Set the flag"
                            " False if you actually want to allow precondition"
                            " ties." .format(cls.__name__, cls.token_label,
                                precond_priority, precond_label, p_label))

            return

        @classmethod
        def unregister_handler_fun(cls, head_or_tail,
                                   precond_label=None, type_sig=None):
            """Unregister the previously-registered handler function (head or
            tail).  If `precond_label` is not set then all head or tail
            handlers (as selected by `head_or_tail`) are unregistered.  If
            `type_sig` is not present then all overloads are also unregistered.
            No error is raised if a matching handler function is not found."""

            if precond_label is None:
                if head_or_tail in cls.handler_funs:
                    cls.handler_funs[head_or_tail] = OrderedDict()
                return

            # Tuple format for sorted_handler_list is:
            #     (precond_fun, precond_priority, handler_fun)
            sorted_handler_dict = cls.handler_funs[head_or_tail]
            if not sorted_handler_dict:
                return

            if not precond_label in sorted_handler_list:
                return

            if type_sig is None:
                del sorted_handler_dict[precond_label]
                return

            sig_list = sorted_handler_dict[precond_label]

            for i in reversed(range(len(sorted_handler_list))):
                item = sorted_handler_list[i]
                handler_fun = item.handler_fun
                new_handler_sigs = [s for s in handler_fun.type_sigs if s != type_sig]
                if not new_handler_sigs:
                    del sorted_handler_list[i] # No type sigs at all, remove item.
                    continue
                handler_fun.type_sigs = new_handler_sigs
            return

        def lookup_handler_fun(self, head_or_tail, lex=None, lookbehind=None,
                               precond_label=None, recursive_call=False):
            """Look up and return the handler function for the given
            subexpression position in `head_or_tail`, based on the current state.

            Either the `lex` parameter or the `precond_label` parameter must be
            set.  If `lex` is set it will be passed to the precondition
            functions as an argument, and similarly for `lookbehind`.
            
            This method evaluates each preconditions function in the sorted
            dict for this kind of token and the specified kind of handler (head
            or tail), returning the handler function associated with the first
            one which evaluates to `True`.  Raises `NoHandlerFunctionDefined`
            if no handler function can be found.
            
            If the parameter `precond_label` is set then this method returns the
            handler function which *would be* returned, assuming that that were
            the label of the "winning" precondition function.
            
            Note that this function also sets the attribute `precond_label` of
            this token instance to the label of the winning precondition
            function."""
            sorted_handler_dict = self.handler_funs[head_or_tail]
            if not sorted_handler_dict:
                raise NoHandlerFunctionDefined(
                        "No {0} handler functions at all are defined"
                        " for tokens with token label '{1}'.  The token's"
                        " value is '{2}'."
                        .format(head_or_tail, self.token_label, self.value))

            if precond_label:
                for pre_fun_label, data_tuple in sorted_handler_dict.items():
                    if pre_fun_label == precond_label:
                        return data_tuple.precond_fun

            # Sequentially run the sorted precondition functions until one is true.
            for pre_label, (pre_fun, pre_prior, handler) in sorted_handler_dict.items():
                if pre_fun(lex, lookbehind):
                    self.precond_label = pre_label # Set precond label for the instance.
                    return handler

            raise NoHandlerFunctionDefined("No {0} handler function matched the "
                    "token with token label '{1}' and value '{2}' in the current "
                    "preconditions."
                    .format(head_or_tail, self.token_label, self.value))

        def dispatch_handler(self, head_or_tail, lex, left=None, lookbehind=None):
            """Look up and return the handler function for the token."""
            if head_or_tail == HEAD:
                fun = self.lookup_handler_fun(HEAD, lex)
                return functools.partial(fun, self, lex) # Bind all the args, known.
            elif head_or_tail == TAIL:
                fun = self.lookup_handler_fun(TAIL, lex, lookbehind=lookbehind)
                return functools.partial(fun, self, lex, left) # Bind all the args, known.
            else: 
                # TODO: should this be a SyntaxError or something else, more serious?
                raise ParserException("Bad first argument to dispatch_handler"
                        " function: must be HEAD or TAIL or equivalent.")

        def process_and_check_node(self, fun_object,
                                   typesig_override=None, in_tree=True,
                                   repeat_args=False):
            """This routine should always be called from inside the individual
            head and tail handler functions, just before they return a value.
            It sets some attributes and checks that the actual types match some
            defined type signature for the function.
            
            The `fun_object` argument should be a reference to the function
            that called this routine.  This is needed to access signature data
            which is pasted onto the function object as attributes.  Inside a
            handler function this function object is referenced simply by the
            name of the function.
           
            The `typesig_override` argument must be a `TypeSig` instance.  It
            will be *assigned* to the node as its signature after all checking,
            overriding any other settings.  This is useful for handling things
            like parentheses and brackets which inherit the type of their child
            (assuming they are kept as nodes in the parse tree and not
            eliminated).
            
            If `in_tree` is `False` then the node for this token will not
            appear in the final token tree: its children will replace it, in
            order, in its parent node's list of children.  This does not
            (currently) work for the root node, which has no parent.
            
            Setting `in_tree` to `False` can be useful for things like unary
            plus and parentheses which are not wanted in the final tree.
            
            If `repeat_args` is true then the argument types for defined type
            signatures will be expanded to match the actual number of
            arguments, if possible, by repeating them an arbitary number of
            times."""
            
            self.in_tree = in_tree
           
            # Process the children to implement in_tree, if set.
            modified_children = []
            for child in self.children:
                if child.in_tree: modified_children.append(child)
                else: modified_children += child.children
            self.children = modified_children

            # Get all the sigs for the node while we have access to fun_object.
            all_sigs = fun_object.type_sigs

            # Perform the type-checking unless the skip option is set.
            if not self.parser_instance.skip_type_checking:
                if not self.parser_instance.overload_on_ret_types: # One-pass.
                    self._check_types(all_sigs, repeat_args)
                else: # Two-pass.
                    self._check_types(all_sigs, repeat_args, first_pass_of_two=True)
                    # If we have a *unique* matching sig, run pass two on the
                    # subtree.  In this case, since the signature is fixed by
                    # argument types (regardless of where the top-down pass
                    # starts from) we can in this case resolve the types in the
                    # subtree early.
                    if len(self.matching_sigs) == 1: # matching_sigs set by _check_types
                        self.check_types_in_tree_second_pass()

            if typesig_override:
                typesig_override.ast_label = self.type_sig.ast_label
                typesig_override.eval_fun = self.type_sig.eval_fun
                self.type_sig = typesig_override
                self.val_type = typesig_override.val_type

            return

        def _check_types(self, all_sigs, repeat_args, first_pass_of_two=False):
            """Utility function called from `process_and_check_node` to check
            the actual types against their signatures.  It assumes a single
            pass unless `first_pass_of_two` is set.  The `all_sigs` argument is
            a list (or iterable) of all the possible signatures for the
            node."""
            
            if not first_pass_of_two:
                # Ordinary case, each child c has a unique c.type_sig already set.
                list_of_child_sig_lists = [[c.type_sig] for c in self.children]
            else:
                # First pass case, multiple sigs in child's self.matching_sigs list.
                list_of_child_sig_lists = [c.matching_sigs for c in self.children]

            # Reduce to only the signatures that the types of the children match.
            self.matching_sigs = TypeSig.get_all_matching_sigs(
                                      all_sigs, list_of_child_sig_lists,
                                      tnode=self, repeat_args=repeat_args)

            if not first_pass_of_two:
                if len(self.matching_sigs) != 1:
                    self._raise_type_mismatch_error(self.matching_sigs,
                            "Actual argument types match multiple signatures.")

                # Found a unique signature; set the node's val_type to its val_type.
                self.type_sig = self.matching_sigs[0] # Save sig for semantic actions.
                self.val_type = self.type_sig.val_type # Set the node type.
                delattr(self, "matching_sigs")
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
            for child in unresolved_children: delattr(child, "matching_sigs")
            if root: delattr(self, "matching_sigs")

        def _check_types_pass_two(self):
            """A second pass is only used when overloading on return types is
            allowed.  It is a top-down pass where each node chooses a unique
            final signature for each of its children.  It depends on the
            node attribute `self.matching_sigs` having been set in the first
            pass."""
            # On FIRST pass: on the way *up* the tree (after getting the
            # literals, the leaves of the tree) get all the signature types for
            # a node which match in arguments for *some* possible return-type
            # choice of the children.  Same as the one-pass version, but now
            # sets of possibilities are allowed and state is saved for the
            # second pass to use: the list of matching sigs is temporarily
            # saved with the node in the self.matched_sigs attribute.
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
            # run on each subtree as soon as the subtree root has a unique signature,
            # and the recursion only goes down to subtrees with roots having a
            # unique signature.  This yields partial results and some error
            # conditions sooner, and is what is implemented here.

            if len(self.matching_sigs) != 1: # The root case needs this.
                self._raise_type_mismatch_error(self.matching_sigs,
                        "Ambiguous type resolution (second pass).  Possible type "
                        "assignments for the children/arguments of the token node match"
                        " {0} possible node signatures: {1}.  Uniqueness is required."
                        .format(len(self.matching_sigs), self.matching_sigs))

            # We have a unique signature; set the node's type attributes
            self.type_sig = self.matching_sigs[0] # Save signature for semantic actions.
            self.val_type = self.type_sig.val_type # Set the type for the node.

            # Update the matching_sigs attribute for each child (should be singleton).
            for count, child in enumerate(self.children):
                if not hasattr(child, "matching_sigs"): continue # Already resolved.
                matched_sigs = TypeSig.get_child_sigs_matching_return_arg_type(
                                          child, self.type_sig.arg_types[count],
                                          child.matching_sigs)
                # From the first pass, we know at least one child sig matches.
                assert len(matched_sigs) != 0
                #if len(matched_sigs) == 0:
                #    child._raise_type_mismatch_error(matched_sigs,
                #        "Token node has no signatures with return type matching type of "
                #        "parent (pass two). Parent expects type '{0}'.  Defined "
                #        "signatures are: {1}.".format(self.val_type, child.matching_sigs))
                if len(matched_sigs) > 1:
                    # Recursion could catch this on the next step, but better err msg.
                    child._raise_type_mismatch_error(matched_sigs,
                        "Token node has multiple signatures with return type matching "
                        "type of parent (pass two). Parent expects type '{0}'.  Defined "
                        "signatures are: {1}.".format(self.val_type, child.matching_sigs))
                child.matching_sigs = matched_sigs
            return

        def _raise_type_mismatch_error(self, matching_sigs, basic_msg):
            """Raise an error, printing a helpful diagnostic message."""
            diagnostic = ("  Current token node has value '{0}' and label '{1}'.  Its"
                         " signature is {2}.  The"
                         " children/arguments have labels and values of {2} and "
                         "val_types {3}.  The matching signatures "
                         "are {4}.".format(self.value, self.token_label,
                             self.type_sig,
                             tuple(c.summary_repr() for c in self.children),
                             tuple(c.val_type for c in self.children), matching_sigs))
            raise TypeErrorInParsedLanguage(basic_msg + diagnostic)

        #
        # Evaluations and semantic actions.
        #

        def eval_subtree(self):
            """Run the saved evaluation function on the token, if one was
            registered with it.  Will raise an error if with no such function
            is found (the `eval_fun` is initialized to `None`)."""
            if not self.type_sig.eval_fun:
                raise ParserException("Attempted to run an evaluation function for"
                        " token {0} but none was found.""".format(self))
            return self.type_sig.eval_fun(self) # Run the function saved with the instance.

        def semantic_action(self):
            """What should these be, and are they needed????  The handler functions
            can implement any semantic actions they want to...."""
            # TODO decide how to implement and when to call.  Should probably
            # be relative to typesigs/handlers rather than nodes in particular.
            pass

        #
        # Some helper functions for use in handler functions.
        #

        # TODO: consider just moving all of these to the ordinary module space
        # and importing them into the typped package, etc.  Easy, just change
        # the self param to tok.  Invoke as match_next(tok,.... instead of
        # tok.match_next(...  Not that big a difference, but is it really
        # needed, etc???  User defined ones have more parity, at least.  But
        # you can still use in both precond funs (lex.token.match_next) as well
        # as in the handlers tok.match_next....

        def match_next(self, token_label_to_match, peeklevel=1,
                       raise_on_fail=False, raise_on_true=False, consume=True,
                       err_msg_tokens=3):
            """A utility function that tests whether the value of the next
            token label in `lex` equals a given token label, and consumes the
            token from the lexer if there is a match.  Returns a boolean.  The
            parameter `peeklevel` is passed to the peek function for how far to
            look; the default is one.
            
            If `raise_on_fail` set true then a `ParserException` will be raised
            if the match fails.  If `consume` is false then no tokens will be
            consumed.
            
            The parameter `err_msg_tokens` can be set to change how many tokens
            worth of text back the error messages report (as debugging
            information) when an exception is raised.  (The count does not
            include whitespace, but it is printed, too.)"""
            lex = self.token_table.lex
            retval = False
            if token_label_to_match == lex.peek(peeklevel).token_label:
                retval = True
            if consume and retval:
                lex.next() # Eat the token that was matched.

            if retval and raise_on_true:
                    raise ParserException(
                        "Function match_next (with peeklevel={0}) found unexpected "
                        "token {1}.  The text of the {3} tokens up to "
                        "the error is: {3}"
                        .format(peeklevel, str(lex.peek(peeklevel)), err_msg_tokens,
                            lex.last_n_tokens_original_text(err_msg_tokens)))
            if not retval and raise_on_fail:
                    raise ParserException(
                        "Function match_next (with peeklevel={0}) expected token "
                        "with label '{1}' but found token {2}.  The text parsed "
                        "from the tokens up to the error is: {3}"
                        .format(peeklevel, token_label_to_match,
                                str(lex.peek(peeklevel)),
                                lex.last_n_tokens_original_text(err_msg_tokens)))
            return retval

        def in_ignored_tokens(self, token_label_to_match,
                              raise_on_fail=False, raise_on_true=False):
            """A utility function to test if a particular token label is among the
            tokens ignored before the current token.  Returns a boolean value.  Can
            be set to raise an exception on success or failure."""
            lex = self.token_table.lex
            retval = False
            ignored_token_labels = [t.token_label for t in lex.peek().ignored_before_list]
            if token_label_to_match in ignored_token_labels:
                retval = True

            if retval and raise_on_true:
                    raise ParserException(
                        "Function in_ignored_tokens found unexpected token with "
                        "label '{0}' before the current token {1}."
                        .format(token_label_to_match, str(lex.token)))
            if not retval and raise_on_fail:
                    raise ParserException(
                        "Function in_ignored_tokens expected token with label "
                        "'{0}' before the current token {1}, but it was not found."
                        .format(token_label_to_match, str(lex.token)))
            return retval

        def no_ignored_after(self, raise_on_fail=False, raise_on_true=False):
            """Boolean function to test if any tokens were ignored between current token
            and lookahead.  Can be set to raise an exception on success or failure."""
            lex = self.token_table.lex
            retval = True
            if lex.peek().ignored_before():
                retval = False

            if retval and raise_on_true:
                    raise ParserException(
                        "Function no_ignored_after expected tokens between the current "
                        "token {0} and the following token {1}, but there were none."
                        .format(str(lex.token), str(lex.peek())))
            if not retval and raise_on_fail:
                raise ParserException(
                        "Function no_ignored_after expected nothing between the "
                        "current token {0} and the following token {1}, but there "
                        "were ignored tokens."
                        .format(str(lex.token), str(lex.peek())))
            else:
                return False
            return retval

        def no_ignored_before(self, raise_on_fail=False, raise_on_true=False):
            """Boolean function to test if any tokens were ignored between
            previous token and current token.  Can be set to raise an exception
            on success or failure."""
            lex = self.token_table.lex
            retval = True
            if lex.token.ignored_before():
                retval = False

            if retval and raise_on_true:
                    raise ParserException(
                        "Function no_ignored_before expected ignored tokens before "
                        " the current token {0}, but none were found."
                        .format(str(lex.token)))
            if not retval and raise_on_fail:
                raise ParserException(
                        "Function no_ignored_before expected no ignored tokens "
                        "before the current token {0}, but at least one was found."
                        .format(str(lex.token)))
            return retval

        #
        # The main recursive_parse function.
        #

        def get_jop_token_instance(self, lex, processed_left, lookbehind, subexp_prec):
            """Returns an instance of the jop token if one should be inferred in the
            current context; otherwise returns `None`."""
            parser_instance = self.parser_instance
            # Not if jop undefined.
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

            # Infer a jop, but only if 1) its prec would satisfy the while loop
            # in `recursive_parse` as an ordinary token, 2) the next token has
            # a head handler defined in the conditions when the jop will need
            # to run its head handler, and 3) the next token similarly has no
            # tail handler in the context.
            if parser_instance.jop_token_subclass.prec() > subexp_prec:
                # Provisionally infer a jop; create a subclass instance for its token.
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

            # Need a way to pass the subexp_prec argument to null-string
            # handlers, so they can relay it -- a way that works with recursive
            # calls, too.
            #
            # Since different INSTANCES of the null-string token are always
            # used then can paste it onto that -- the handler fun is passed
            # tok.  Currently done like this.
            #
            # How about setting it as an attribute of the looked-up handler
            # function itself?  Handler fun (only for null-string) can then
            # look at its own attribute.  Token instance probably better.
            #
            # TODO: consider "virtual null-string tokens" which only execute
            # their handlers (such as to change pstate) but are not treated
            # as actual tokens.  Some possible uses, but is there enough
            # real advantage?
            #
            # To do backtracking stuff you can handle productions like this
            #   <prod> = <prod1> | <prod2> | <prod3>
            # with virtual tokens by going in order (of def or priority) and
            # setting the state to, say <prod1>, looking up the handler,
            # calling the handler.  Inside the handler, call *this routine*
            # recursively until a non-fail result returned or all fail.  Raise
            # a special exception on fail, and continue the loop on the next
            # item if one fails.  Until one succeeds or possibilities
            # exhausted.
            #
            # Each production needs to declare whether it is a head or tail,
            # though (should be same for whole thing).
            #
            # ---> Above probably works for general backtracking... also for
            # formulaic evaluation of production rules.  Just need to register
            # a special handler for head token of each production case.  (Could
            # conceivably even do optimization and search the full tree once --
            # such as for G-normal-form rules -- setting improved preconditions
            # for the tokens...)
            #
            # Note this routine now returns a (possible) actual token.  So the
            # handlers can call this routine *and* the dispatch_handler routine
            # inside them, until all fail or an actual handler returned.  They
            # need to call lex.next to get the actual token, so it would also be
            # easy to substitute-in an instance of the null-string token.

            # DEFINE: this routine must return nothing or an actual token and
            # handler.  So, job of handlers in null-string tokens is to return
            # an actual token and handler to call to fill the "slot" in the
            # grammar, NOT to return a processed-left... Kind of inelegant that
            # they behave differently... UNLESS this routine actually makes the
            # call, too... 
            #   processed_left = self.get_next_token_call_handler_return_processed(...)
            # Note that this comes about because of the need to repeatedly call
            # the handler in some cases (not all).  Instead, could just use jump
            # into middle of the recursive_parse (or break it into two functions
            # and have recursive_parse call both).
            #
            # Jumping into recursive_parse in middle may be better solution...
            # as originally done.  To the handlers it is all the same loop,
            # since they get the same info.

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
                            # Below parameters only used in null-string handler funs.
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
            passed as an argument.
            
            If `processed_left` is set (evaluates to true) then the first part
            of `recursive_parse` is skipped and it jumps right to the
            tail-handling loop part using the passed-in values of
            `processed_left` and `lookbehind`.  These parameters should
            generally not be set.  They are only used by certain null-string
            tokens tail-handlers so they can relay their call as a tail-handler
            to the actual token (which does the real work)."""
            # NOTE that with a good, efficient pushback function the modifiable
            # prec for different handler functions might be doable: just do a
            # next then evaluate the prec, then pushback.

            # NOTE it is tempting to define a jop and null-string token
            # instance and only use it when necessary (and replace it only when
            # used).  But, some things need to be considered.  What if new head
            # handlers are dynamically registered?  Is there any special
            # attribute or other thing that is assigned on creation which
            # changes?  (Either way, should probably add extra things like line
            # numbers to mimic ordinary tokens.) Note that currently you only
            # really pay the creation cost if you actually use the
            # corresponding feature, but you incur it more than necessary.

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
        # Shortcut for copying instances of tokens (deep copies should not be used).
        #

        def copy(self):
            """Return a shallow copy."""
            # TODO, consider whether to reset parent and children and whether to
            # overload __copy__ magic method.
            return copy.copy(self)

        #
        # Some representations that apply to the subclasses.
        #

        def summary_repr_with_types(self):
            return ("<" + str(self.token_label) + 
                    "," + str(self.value) + 
                    "," + str(self.val_type) + ">")

        def tree_repr_with_types(self, indent=""):
            string = indent + self.summary_repr_with_types() + "\n"
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
    DEFAULT_BEGIN_TOKEN_LABEL = "k_begin" # Default label for begin token.
    DEFAULT_END_TOKEN_LABEL = "k_end" # Default label for end token.

    def __init__(self, num_lookahead_tokens=2,
                       lexer = None,
                       default_begin_end_tokens=True,
                       type_table = None,
                       skip_type_checking=False,
                       overload_on_arg_types=True,
                       overload_on_ret_types=False,
                       multi_expression=False):
        """Initialize the parser.
        
        If a Lexer is passed in the parser will use
        that lexer and its token table, otherwise a new lexer is created.
        
        No default begin and end functions will be set if a lexer is passed in,
        regardless of the value of `default_begin_end_tokens`.  Otherwise,
        default begin and end tokens will be defined unless
        `default_begin_end_tokens` is set false (note that creating them by
        default is the opposite of the default behavior for the lower-level
        Lexer class).

        Setting `skip_type_checking=True` is slightly faster if typing is not
        being used at all.  Setting `overload_on_ret_types` requires an extra
        walk of the token tree, and implies overloading on argument types.
        
        If `multi_expression` is set then multiple expressions will be parsed
        from the token stream until it reaches the end, and a list of token
        trees will be returned, one for each expression."""

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
                                num_lookahead_tokens=num_lookahead_tokens,
                                default_begin_end_tokens=False)
            # Set the begin and end tokens unless the user specified not to.
            if default_begin_end_tokens:
                self.def_begin_end_tokens(self.DEFAULT_BEGIN_TOKEN_LABEL,
                                          self.DEFAULT_END_TOKEN_LABEL)
        if type_table:
            self.type_table = type_table
        else:
            self.type_table = TypeTable(self)
        self.num_lookahead_tokens = num_lookahead_tokens
        self.jop_token_label = None # Label of the jop token, if any.
        self.jop_token_subclass = None # The actual jop token, if defined.
        self.null_string_token_label = None # Label of the null-string token, if any.
        self.null_string_token_subclass = None # The actual null-string token, if any.
        self.multi_expression = False # Whether to parse multiple expressions.

        # Type-checking options below; these can be changed between calls to `parse`.
        self.skip_type_checking = skip_type_checking # Skip all type checks, faster.
        self.overload_on_arg_types = overload_on_arg_types # Raise error on mult defs?
        self.overload_on_ret_types = overload_on_ret_types # Requires extra processing.
        if overload_on_ret_types:
            self.overload_on_arg_types = True

    #
    # Methods defining tokens.
    #

    token_kinds = ("regular", "ignored", "begin", "end", "jop", "null-string")

    def def_token_master(self, token_label, regex_string=None, on_ties=0, ignore=False,
                         ignored_token_label=None, token_kind="regular"):
        """The master method for defining tokens that all the convenience methods
        must call.  Allows for factoring out some common code and keeping the
        attributes of all the different kinds of tokens up-to-date."""
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
                raise ParserException("Called head of begin token.")
            def begin_tail(self, lex, left):
                raise ParserException("Called tail of begin token.")
            tok = self.modify_token_subclass(
                                   token_label, head=begin_head, tail=begin_tail)
            self.begin_token_subclass = tok

        elif token_kind == "end":
            token_table.def_end_token(token_label)
            self.end_token_label = token_label
            # Define dummy handlers for the begin-token.
            def end_head(self, lex):
                raise ParserException("Called head of end token.")
            def end_tail(self, lex, left):
                raise ParserException("Called tail of end token.")
            tok = self.modify_token_subclass(
                                      token_label, head=end_head, tail=end_tail)
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

    def def_begin_end_tokens(self, begin_token_label, end_token_label):
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
        token is required; usually it will be a token for spaces and tabs."""
        # TODO: update docs above, ignored_token_label now not always used....
        return self.def_token_master(jop_token_label,
                                     ignored_token_label=ignored_token_label,
                                     token_kind="jop")

    def def_null_string_token(self, null_string_token_label):
        """Define the null-string token.  This token has no regex pattern.  An
        instance is inserted in `recursive_parse` when it is inferred to be
        present based.  It can only ever have head handlers, and is not even
        tested for tail handlers.  This method must be called before a
        null-string can be used.  The parameter `null_string_token_label` is
        the label for the newly-created tok representing it."""
        return self.def_token_master(null_string_token_label,
                                     token_kind="null-string")

    #
    # Undefine tokens.
    #

    def undef_token(self, token_label):
        """A method for undefining any token defined by the `PrattParser` methods.
        Since the `token_kind` was set for all tokens when they were defined
        it knows how to undelete any kind of token."""
        token_table = self.token_table
        tok = token_table.get_token_subclass(token_label)
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
    # Methods to modify tokens.
    #

    def modify_token_subclass(self, token_label, prec=None, head=None, tail=None, 
                       precond_label=None, precond_fun=None,
                       precond_priority=0, val_type=None, arg_types=None,
                       eval_fun=None, ast_label=None):
        """Look up the subclass of base class `TokenNode` corresponding to the
        label `token_label` (in the token table) and modify its properties.  A
        token with that label must already be in the token table, or an
        exception will be raised.
        
        Returns the modified class. Saves any given `head` or `tail` functions
        in the handler dict for the token.  Both can be passed in one call, but
        they must have the same preconditions function and priority.
        
        If `tail` is set then the prec will also be set unless `prec` is
        `None`.  For a head the `prec` value is ignored.  If `tail` is set and
        `prec` is `None` then the prec value defaults to zero.  If `head` or
        `tail` is set and `precond_label` is also set then the corresponding
        preconditions function will be looked up and the head or tail function
        will be associated that function.  If `precond_fun` is also set then it
        will first be registered with the label `precond_label` (which must be
        present in that case).
        
        The `eval_fun` and the `ast_label` arguments are saved as attributes of
        the type signature (and all defined type signatures for a token are
        saved with that token).  This is so different overloads of the token
        can have different AST labels and/or evaluation functions."""

        if isinstance(arg_types, str):
            raise ParserException("The arg_types argument to token_subclass must"
                    " be None or an iterable returning type labels (e.g., a list"
                    " or tuple).")
        if tail and prec is None: prec = 0

        if self.token_table.has_key(token_label):
            TokenSubclass = self.token_table.get_token_subclass(token_label)
        else:
            raise ParserException("In call to mod_token_subclass: subclass for"
                    " token labeled '{0}' has not been defined.  Maybe try"
                    " calling `def_token` first.".format(token_label))
            # This used to just create a subclass, but that can mask errors.
            #TokenSubclass = self.token_table.create_token_subclass(token_label)

        # Save a reference to the PrattParser, so nodes can access it if they need to.
        TokenSubclass.parser_instance = self # maybe weakref later

        if tail: TokenSubclass.static_prec = prec # Ignore prec for heads; it will stay 0.

        # Get the type sig.
        type_sig = TypeSig(val_type, arg_types, eval_fun=eval_fun, ast_label=ast_label) 
        
        if head:
            TokenSubclass.register_handler_fun(HEAD, head,
                               precond_label=precond_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig)
        if tail:
            TokenSubclass.register_handler_fun(TAIL, tail,
                               precond_label=precond_label, precond_fun=precond_fun,
                               precond_priority=precond_priority, type_sig=type_sig)
        return TokenSubclass

    def undef_handler(self, token_label, head_or_tail, precond_label=None,
                         val_type=None, arg_types=None, all_handlers=False):
        """Undefine a head or tail function with the given `token_label`,
        `precond_label` and type signature.  The `head_or_tail` value should be
        `HEAD` or `TAIL`.  If `all_precond` is set then all heads and tails for all
        preconditions will be undefined.  If `all_overloads` then all
        overloaded type signatures will be undefined.  The token itself is
        never undefined; use the `undef_token` method for that."""
        TokenSubclass = self.token_table.get_token_subclass(token_label)
        TokenSubclass.unregister_handler_fun(head_or_tail,
                                         precond_label=precond_label,
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

    # TODO these define and undefine methods each need a corresponding
    # undefine method (or one that does all); should be easy with undef_handler method.
    # Is it necessary to call undef_handler, or does undef the token do that
    # too?  Look at undef_handler and see........ use below if needed.

    def def_literal(self, token_label, val_type=None, eval_fun=None, ast_label=None):
        """Defines the token with label `token_label` to be a literal in the
        syntax of the language being parsed.  This method adds a head handler
        function to the token.  Literals are the leaves of the parse tree; they
        are things like numbers and variable names in a numerical expression.
        They always occur as the first (and only) token in a subexpression
        being evaluated by `recursive_parse`, so they need a head handler but not
        a tail handler."""
        def head_handler_literal(tok, lex):
            tok.process_and_check_node(head_handler_literal)
            return tok
        return self.modify_token_subclass(token_label, head=head_handler_literal,
                                          val_type=val_type, arg_types=(),
                                          eval_fun=eval_fun, ast_label=ast_label)

    def def_multi_literals(self, tuple_list):
        """An interface to the `def_literal` method which takes a list of
        tuples.  The `def_literal` method will be called for each tuple, unpacked
        in the order in the tuple.  Unspecified optional arguments get their default
        values."""
        return multi_funcall(self.def_literal, tuple_list)

    def def_infix_multi_op(self, operator_token_labels, prec,
                                    assoc, repeat=False, in_tree=True,
                                    val_type=None, arg_types=None, eval_fun=None,
                                    ast_label=None):
        # TODO only this type currently supports "in_tree" kwarg.  General and easy
        # mechanism, though.  Test more and add to other methods.
        # Does in-tree keep the first one? how is it defined for this thing?
        # Comma operator is example of in_tree=False, but how does it handle
        # the root??
        # TODO: How about in-tree that works at root iff the node only has one child?
        """Takes a list of operator token labels and defines a multi-infix
        operator.  If `repeat=True` it will accept any number of repetitions of
        the list of operators (but type-checking for that is not implemented
        yet).  For a single operator, repeating just has the effect of putting
        the arguments in a flat argument/child list instead of as nested binary
        operations based on left or right association.  Any argument-checking
        is done after any node removal, which may affect the types that should
        be passed-in in the list arg_types of parent constructs."""
        if assoc not in ["left", "right"]:
            raise ParserException('Argument assoc must be "left" or "right".')
        recurse_bp = prec # TODO: does late-binding affect this?????
        # TODO also above, change name... is it stored with token as tok.prec?
        # See the DEBUG statement below... should be settable, but if prec is
        # fixed for a token it probably doesn't matter!
        if assoc == "right": recurse_bp = prec - 1
        def tail_handler(tok, lex, left):
            tok.append_children(left, tok.recursive_parse(recurse_bp))
            while True:
                for op in operator_token_labels[1:]:
                    tok.match_next(op, raise_on_fail=True)
                    assert tok.prec() == recurse_bp or tok.prec()-1 == recurse_bp # DEBUG
                    tok.append_children(tok.recursive_parse(recurse_bp))
                if not repeat: break
                # Peek ahead and see if we need to loop another time.
                if lex.peek().token_label != operator_token_labels[0]: break
                tok.match_next(operator_token_labels[0], raise_on_fail=True)
                tok.append_children(tok.recursive_parse(recurse_bp))
            tok.process_and_check_node(tail_handler, in_tree=in_tree,
                                                     repeat_args=repeat)
            return tok
        return self.modify_token_subclass(operator_token_labels[0], prec=prec,
                                tail=tail_handler, val_type=val_type,
                                arg_types=arg_types, eval_fun=eval_fun,
                                ast_label=ast_label)

    def def_infix_op(self, operator_token_label, prec, assoc, in_tree=True,
                     val_type=None, arg_types=None, eval_fun=None, ast_label=None):
        """This just calls the more general method `def_multi_infix_op`."""
        return self.def_infix_multi_op([operator_token_label], prec,
                              assoc, in_tree=in_tree,
                              val_type=val_type, arg_types=arg_types,
                              eval_fun=eval_fun, ast_label=ast_label)

    def def_prefix_op(self, operator_token_label, prec, val_type=None, arg_types=None,
                      eval_fun=None, ast_label=None):
        """Define a prefix operator."""
        def head_handler(tok, lex):
            tok.append_children(tok.recursive_parse(prec))
            tok.process_and_check_node(head_handler)
            return tok
        return self.modify_token_subclass(operator_token_label, head=head_handler,
                            val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                            ast_label=ast_label)

    def def_postfix_op(self, operator_token_label, prec, allow_ignored_before=True,
                       val_type=None, arg_types=None, eval_fun=None,
                       ast_label=None):
        """Define a postfix operator.  If `allow_ignored_before` is false then
        no ignored token (usually whitespace) can appear immediately before the
        operator."""
        def tail_handler(tok, lex, left):
            if not allow_ignored_before:
                tok.no_ignored_before(raise_on_fail=True)
            tok.append_children(left)
            tok.process_and_check_node(tail_handler)
            return tok
        return self.modify_token_subclass(operator_token_label, prec=prec, tail=tail_handler, 
                            val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                            ast_label=ast_label)

    def def_bracket_pair(self, lbrac_token_label, rbrac_token_label,
                                               eval_fun=None, ast_label=None):
        """Define a matching bracket grouping operation.  The returned type is
        set to the type of its single child (i.e., the type of the contents of
        the brackets).  Defines a head handler for the left bracket token, so
        effectively gets the highest evaluation precedence.  As far as types,
        it is treated as a function that takes one argument of wildcard type
        and returns whatever type the argument has."""
        # Define a head for the left bracket of the pair.
        def head_handler(tok, lex):
            tok.append_children(tok.recursive_parse(0))
            tok.match_next(rbrac_token_label, raise_on_fail=True)
            tok.process_and_check_node(head_handler,
                    typesig_override=TypeSig(tok.children[0].val_type, (None,)))
            return tok
        return self.modify_token_subclass(lbrac_token_label, head=head_handler,
                                   eval_fun=eval_fun, ast_label=ast_label)

    def def_stdfun(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label,
                      precond_priority=1,
                      val_type=None, arg_types=None, eval_fun=None, ast_label=None,
                      num_args=None):
        """This definition of stdfun uses lookahead.  This will take
        arbitrarily many arguments if `arg_types` is `None`.  To check the
        number of arguments when types are not used, set `arg_types` to, for
        example, `[None]*3` for three arguments.
        
        The `num_args` parameter is optional for specifying the number of
        arguments when typing is not being used.  If it is set to a nonnegative
        number then it will automatically set `arg_types` to the corresponding
        list of `None` values; if `arg_types` is set then it is ignored."""
        if num_args is not None and arg_types is None:
            arg_types = [None]*num_args

        def preconditions(lex, lookbehind):
            """Must be followed by a token with label 'lpar_token_label', with no
            whitespace in-between."""
            peek_tok = lex.peek()
            if peek_tok.ignored_before(): return False
            if peek_tok.token_label != lpar_token_label: return False
            return True
        precond_label = "lpar after, no whitespace between" # Should be a unique label.

        def head_handler(tok, lex):
            # Below match is for a precondition, so it will match and consume.
            tok.match_next(lpar_token_label, raise_on_fail=True)
            # Read comma-separated subexpressions until the peek is rpar_token_label.
            while not tok.match_next(rpar_token_label, consume=False):
                tok.append_children(tok.recursive_parse(0))
                if not tok.match_next(comma_token_label):
                    break
                else:
                    tok.match_next(rpar_token_label, raise_on_true=True)
            tok.match_next(rpar_token_label, raise_on_fail=True)
            tok.process_and_check_node(head_handler)
            return tok
        return self.modify_token_subclass(fname_token_label, prec=0,
                     head=head_handler, precond_label=precond_label,
                     precond_fun=preconditions, precond_priority=precond_priority,
                     val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                     ast_label=ast_label)

    def def_stdfun_lpar_tail(self, fname_token_label, lpar_token_label,
                      rpar_token_label, comma_token_label, prec_of_lpar,
                      val_type=None, arg_types=None, eval_fun=None, ast_label=None,
                      num_args=None):
        """This is an alternate version of stdfun that defines lpar as an infix
        operator (with a tail).  This function works in the usual cases but
        the current version without preconditions may have problems distinguishing
        "b (" from "b(" when a multiplication jop is set.  The lookahead version
        `def_stdfun` is usually preferred."""
        if num_args is not None and arg_types is None: arg_types = [None]*num_args

        def tail_handler(tok, lex, left):
            # Nothing between fun name and lpar_token.
            tok.no_ignored_before(raise_on_fail=True)
            while not tok.match_next(rpar_token_label, consume=False):
                left.append_children(tok.recursive_parse(prec_of_lpar))
                if not tok.match_next(comma_token_label):
                    break
                else:
                    tok.match_next(rpar_token_label, raise_on_true=True)
            tok.match_next(rpar_token_label, raise_on_fail=True)
            left.process_and_check_node(tail_handler)
            return left
        return self.modify_token_subclass(lpar_token_label,
                                         prec=prec_of_lpar, tail=tail_handler,
                                         val_type=val_type, arg_types=arg_types,
                                         eval_fun=eval_fun, ast_label=ast_label)

    def def_jop(self, prec, assoc,
                      precond_label=None, precond_fun=None, precond_priority=None,
                      val_type=None, arg_types=None, eval_fun=None,
                      ast_label=None):
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
        return self.modify_token_subclass(self.jop_token_label, prec=prec, tail=tail_handler,
                            precond_label=precond_label, precond_fun=precond_fun,
                            precond_priority=precond_priority,
                            val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                            ast_label=ast_label)

    def def_production(self, pstate_label, token_label, prec=0, precond_priority=None,
                       # Below not used yet.... need to know cases of production...
                       precond_label=None, precond_fun=None,
                       val_type=None, arg_types=None, eval_fun=None,
                       ast_label=None):
        """Define a production in the sense of a recursive-descent parser,
        using the null-string token.  The handler functions basically just change
        the state and relay the arguments that the null-string token was called with.
        
        It is assumed that the start state will be initially pushed on the
        `pstack` attribute of the parser whenever productions are being
        used."""

        # TODO: In order to do the backtracking this routine will need to know
        # all the cases of the production.  Then, it should iterate over them,
        # call each one (by priority order) and catch special exception
        # BacktrackFailedBranch on failure.  On failure move to next one, or
        # raise the fail again if out of choices.
        #
        # In future optimizations this "tree" of cases coule be searched to find
        # the tokens for, say LL(1) grammars which can be set as preconditions
        # to avoid the backtracking by using a lookahead.
        
        # Use a default argument to preconditions to get early binding of closure.
        def preconditions(lex, lookbehind, state_label=state_label):
            if not lex.token_table.parser_instance.pstate[-1] == state_label:
                return False
            # TODO may also need to look ahead at peeks sometimes??
        precond_label = "production-{0}-peeks-" + "-".join(peek_list)

        def head_handler(tok, lex):
            """Just push the state, relay the call, and pop afterwards."""
            # Push the new state onto the pstack.
            tok.parser_instance.pstack.append("pstate_label")
            # Get the next subexpression, relaying the precedence.
            processed = tok.recursive_parse(tok.subexp_prec)
            # Pop the state off the pstack.
            tok.parser_instance.pop()
            return processed

        def tail_handler(tok, lex, left):
            """Just push the state, relay the call, and pop afterwards."""
            # Push the new state onto the pstack.
            tok.parser_instance.pstack.append("pstate_label")
            # Get the next subexpression, relaying the precedence.
            processed = tok.recursive_parse(tok.subexp_prec,
                                processed_left=left, lookbehind=tok.lookbehind)
            # Pop the state off the pstack.
            tok.parser_instance.pop()


            return processed

        return self.modify_token_subclass(fname_token_label, prec=prec,
                     head=head_handler, tail=tail_handler, precond_label=precond_label,
                     precond_fun=preconditions, precond_priority=precond_priority,
                     val_type=val_type, arg_types=arg_types, eval_fun=eval_fun,
                     ast_label=ast_label)


    #
    # The main parse routines.
    #

    def parse_from_lexer(self, lex):
        """The same as the `parse` method, but a lexer is already assumed to be
        initialized.  This is used when one parser instance calls another
        parser instance (implicitly, via handler functions of its tokens)."""
        # TODO: Set the lexer to look at the token_table of this parser
        # instance, then return the setting afterward back to original.  Use
        # the lexer's `set_token_table` method.
        # 
        # NOTE you do not need to set the TypeTable, since the tokens already
        # know it.
        #
        # Consider if instead the recursive_parse routine should be called for
        # the token from a special method, maybe parse_from_lexer.
        # 
        # Just copy the `parse` method and remove the stuff it doesn't need
        # to do.

    def parse(self, program):
        """The main routine for parsing a full program or expression.  Users of
        the class should call this method to perform the parsing operations
        (after defining a grammar, of course).  Returns a token tree or a list
        of token trees if `multi_expression` is set."""

        parse_tree_list = []
        while True:
            self.lex.set_text(program)
            begin_tok = self.lex.token # Get begin token to access recursive_parse.
            output = begin_tok.recursive_parse(0)
            parse_tree_list.append(output)

            # Finalize type-checking for root when overloading on return types.
            if self.overload_on_ret_types: 
                output.check_types_in_tree_second_pass(root=True)

            # See if we reached the end of the token stream.
            if self.lex.peek().is_end_token():
                # TODO: Note that we never shut down the lexer's generator.
                break
            if self.multi_expression:
                continue
            else:
                raise IncompleteParseException("Parsing never reached the end of"
                    " the text.  Parsing stopped with tokens still in the lexer."
                    " No syntax element was recognized. The last-parsed token had"
                    " label '{0}' and value '{1}'.  Parsing stopped before a token"
                    " in the lexer with label '{2}' and value '{3}'."
                    .format(self.lex.token.token_label, self.lex.token.value, 
                            self.lex.peek().token_label, self.lex.peek().value))

        if self.multi_expression:
            return parse_tree_list
        else:
            return output

def sort_handler_dict(d):
    """Return the sorted `OrderedDict` version of the dict `d` passed in,
    sorted by the precondition priority in the items.  Used in
    `PrattParser.register_handler_fun` to keep handlers sorted by priority."""
    # https://docs.python.org/3/library/collections.html#ordereddict-examples-and-recipes
    return OrderedDict(sorted(
           d.items(), key=lambda item: item[1].precond_priority, reverse=True))

#
# Named tuples for storing data.
#

# These named tuples are stored in the ordered dicts handler_funs[HEAD] and
# handler_funs[TAIL] for each token subclass.
HandlerData = namedtuple("HandlerData",
                         ["precond_fun", "precond_priority", "handler_fun"])

#
# Exceptions
#

class NoHandlerFunctionDefined(ParserException):
    """Only raised by dispatcher function, and only when it fails to find a
    handler function (head or tail, whichever it was looking for)."""
    pass

class IncompleteParseException(ParserException):
    """Only raised at the end of the `PrattParser` function `parse` if tokens
    remain in the lexer after the parser finishes its parsing (unless the
    `multi_expression` flag is set)."""
    pass

