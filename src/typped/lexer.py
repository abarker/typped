# -*- coding: utf-8 -*-
"""

A `Lexer` class instance is a general lexer/scanner/tokenizer.  It was designed
to be used by the `PrattParser` class, but it can also be used for other
lexical scanning applications.

The general purpose of the `Lexer` is to take a string of text and produce a
corresponding sequence of tokens from the text.  The set of possible tokens is
defined by the user, with a string label and a regex pattern that is searched
for in the program text.  Once initialized with text a `Lexer` instance is a
generator which sequentially produces tokens with its `next` method.  It is
also an iterator, so it can be used in loops, etc.

With some lexers the order in which tokens are defined is significant.  They
match regexes from a list of regexes, taking the first match without regard
to the length of the match.  The
`Lexer` class was designed to function independently of the order in which
tokens are defined.  The longest match is always returned, with ties broken
by an explicit priority mechanism.  This allows token definitions to be
organized in various ways.   They can all be done in one
place or spread around in the code in any order, however the programmer wants
to do it.

Defining tokens
===============

This section describes the low-level definition of tokens when using `Lexer` as
a standalone application.  To use tokens with a `PrattParser` instance, though,
you need to use the corresponding `def_token` method of the `PrattParser`
class.  That class adds extra attributes, methods, etc., to the tokens.  The
interface is generally the same.

A token for a left parenthesis would be defined like this::

    lex = typped.Lexer()
    lex.def_token("k_lpar", r"lpar")

The string `k_lpar` is a label for the token.  The use of the string prefix
"`k_`" is a naming convention for token labels.  An identifier token could be
defined like this::

    lex.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

Notice that in the definition of the identifier the keyword argument `on_ties`
is set to -1.  The lexer will by default always choose the longest string which
matches a defined regex pattern for a token.  If there is a tie then, by
default, an exception will be raised.  The `on_ties` value is used to break
ties; strings of the same length are sorted by that value and the
highest-priority string is chosen.  The default `on_ties` value is `0`.  Suppose
you also wanted a token for the string `mod`, and defined it as::

    lex.def_token("k_mod", r"mod")

Since this token has a higher `on_ties` value, it will always take precedence over
the identifier token, even though both match and have the same length.

Begin and end tokens
====================

The lexer uses sentinel begin-token and end-token tokens for the beginning and
the end of the token sequence for text.  These tokens must be explicitly
defined (i.e., given string labels) either by calling `def_begin_end_tokens`::

    lex.def_begin_end_tokens("k_begin", "k_end")

or else by setting the `default_begin_end_tokens` flag to `True` when
initializing the lexer::

    lex = Lexer(def_begin_end_tokens=True)

The default tokens have the labels `k_begin` and `k_end`.

The begin-token is never explicitly returned.  After the call to `set_text` to
define the text to tokenize and before any calls to `next` the begin-token is
the current token `lexer.token`.  So `lex.token` and `lex.peek(0)` would both
return the begin token.

After the end of the text the `next` method explicitly returns one end-token.
Calling `next` again raises `StopIteration` and halts the lexing of the
currently-set text.  All peeks beyond the end of the text are reported as
end-tokens.

Using the lexer
===============

This is a simple example of using the lexer.  Notice that multiple token definitions
can be combined using the `def_multi_tokens` method.  It is usually better to
define a shorter alias for the function call, however.::

    lex = Lexer()

    lex.def_begin_end_tokens("k_begin", "k_end")
    lex.def_token("k_space", r"[ \\t]+", ignore=True) # note + NOT *
    lex.def_token("k_newline", r"[\\n\\f\\r\\v]+", ignore=True) # note + NOT *
    tokens = [
        ("k_identifier", r"[a-zA-Z_](?:\w*)")
        ("k_plus", r"\+")
        ]
    lex.def_multi_tokens(tokens)

    lex.set_text("x  + y")

    for t in lex:
        print(t)

The result is as follows:
::

    <k_identifier,x>
    <k_plus,+>
    <k_identifier,y>
    <k_end,None>

Notice that the end-token is actually returned, but the begin-token is not.
The method `def_default_whitespace` could alternately be used to define the
whitespace tokens.

User-accessible methods and attributes of `Lexer`
=================================================

The lexer class has many utility methods and user-accessible attributes.  Some
of the main ones are listed here.  One of the most commonly-accessed attributes
of a lexer `lex` is the current token, `lex.token`.

General methods:

* `next` --- return the next token
* `peek` --- peek at the next token without consuming it
* `go_back` --- go back in the text stream by some number of tokens

Helper methods:

* `match_next` --- matches the specified token, with various options
* `in_ignored_tokens` --- test if some particular token was ignored before the current one
* `no_ignored_after` --- true if no ignored tokens immediately follow current token
* `no_ignored_before` --- true if no ignored tokens immediately preceed current token

Some boolean-valued informational methods:

* `curr_token_is_first` --- true if the current token is the first returned
* `text_is_set` --- true only when text is currently set for scanning

Other attributes:

* `token` --- the current token (the most recent one returned by `next`)
* `all_tokens_count` --- num of tokens since text was set (begin and end not counted)
* `default_helper_exception` --- the default exception for helpers like `match_next`
* `text_is_set` --- whether or not text has been set for the lexer

TODO, list more, and why not make some of these methods of `TokenNode` instead?

User-accessible attributes of tokens
====================================

The tokens returned by the lexer are instances of a subclass of the class
`TokenNode` (named that since the parser combines them into the nodes of a
parse tree).  The subclasses themselves represent the general kind of token,
for example if `k_identifier` was defined as a token label then a particular
subclass of `TokenNode` would be created to represent identifiers in general.
The particular instances of identifiers, found in the lexed text with their
actual string values, are represented by instances of the general class for
identifiers.

User-accessible methods of tokens.

* `is_begin_token` --- true when tokens is a begin token
* `is_end_token` --- true when tokens is a end token
* `is_begin_or_end_token` --- true when tokens is a begin_or_end_token
* `ignored_before_labels` --- just the token labels of the tokens ignored before

For a token named `t`, these attributes are available:

* `t.token_label` --- the string label of the token (which was defined with it)
* `t.value` --- the string value for the token, found in the lexed text
* `t.is_first` --- true iff this is the first non-begin token in the text
* `t.is_first_on_line` --- true iff this is the first token returned for a line
* `t.parent` --- can be set to the parent in a tree; set by the lexer to `None`
* `t.children` --- can be set to a list of children; set by the lexer to `[]`
* `t.original_matched_string` --- the original text that was consumed for this token
* `t.line_and_char` --- tuple of line number and character where the token started
* `t.char_index_in_program` --- the index of this token into the text set via `set_text`
* `t.ignored_before` --- a tuple of all tokens ignored immediately before this one

TODO, list other methods, too.

Initialization options
======================

There are several options that can be set on initialization, including the
level of token lookahead that is supported.

TODO

Code
====

"""

# TODO: consider adding a method to return the processed part and the unprocessed
# part of the orginal text.  Shouldn't be too hard when up-to-speed on the Lexer
# workings (just calculate the right slices into self.program).  Slight complication
# with lookahead and how to define it.

# TODO: Look at the shlex package and see if anything that could be used or ideas to
# borrow.  https://docs.python.org/3.6/library/shlex.html

# TODO: implement move_back, the equivalent of push_back except you think of it
# as moving in the token buffer rather than pushing back a token (it is still a
# peek token).

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_pratt_parser.py",
                              "../../test/test_matcher.py",
                              "../../test/test_lexer.py",
                              ],
                              pytest_args="-v")

from typped import cythonize
if cythonize:
    #import pyximport
    #pyximport.install()
    from . import token_buffer
    TB = token_buffer.TokenBuffer
    from .matcher import Matcher

import collections
from .matcher import Matcher
from .shared_settings_and_exceptions import LexerException, is_subclass_of

#
# TokenNode
#

class TokenNode(object):
    """The base class for token objects.  Each different kind of token is represented
    by a subclass of this class.  Instances of the tokens in the program text are
    represented by instances of the subclass for that kind of token.

    The attribute `token_label` is the string token label for the kind of token
    represented by an instance.  The attribute `value` is set to the actual
    string value in the lexed text which matched the regex of the token.  The
    attribute `ignored_before` is a tuple of all tokens ignored just
    before the lexer got this token.

    The attribute `children` is a list of the child nodes, and `parent` is the
    parent.  Indexing a `TokenNode` class also returns the corresponding child
    node, i.e. `t_node[0]` would be the leftmost child."""

    token_label = None # A label for subclasses representing kinds of tokens.
    original_matched_string = "" # Default, for begin- and end-tokens.

    def __init__(self):
        """Initialize the TokenNode."""
        self.ignored_before = [] # Values ignored by lexer just before.
        self.value = None # The actual parsed text string for the token.
        self.children = [] # Args to functions are their children in parse tree.
        self.parent = None # The parent in a tree of nodes.

    def original_text(self):
        """Return the original text that was read in lexing the token, including
        any ignored text."""
        #ignored_strings = [ s.value for s in self.ignored_before ]
        #joined = "".join(ignored_strings) + self.value
        #assert joined == self.original_matched_string # A debugging test.
        return self.original_matched_string

    def ignored_before_labels(self):
        """Return the list of token labels of tokens which were ignored just
        before this token."""
        return [t.token_label for t in self.ignored_before]

    def append_children(self, *token_nodes):
        """Append all the arguments as children, also setting their parent to self."""
        for t in token_nodes:
            self.children.append(t)
            t.parent = self

    def __getitem__(self, index):
        """Use indexing to access the children.  No check is made, however, to see
        if the correct number of children exist."""
        return self.children[index]

    def convert_to_AST(self, convert_TokenNode_to_AST_node_fun):
        """Call this on the root node.  Converts the token tree to an abstract
        syntax tree.  This basically converts the nodes one-to-one to a more
        convenient type of node for the AST of a given application.  The
        function `convert_TokenNode_to_AST_node_fun` should take one argument,
        a `TokenNode` instance, and return an AST node instance for the
        corresponding AST node.  The only requirement for the AST nodes is that
        they have a method called `append_children`.  The `ast_data` attribute
        of a node can be used to save information useful in the transformation."""
        ast_node = convert_TokenNode_to_AST_node_fun(self)
        for child in self.children:
            ast_node.append_children(
                    child.convert_to_AST(convert_TokenNode_to_AST_node_fun))
        return ast_node

    #
    # Informational methods.
    #

    def is_begin_token(self):
        """Test whether this token is the begin-token."""
        return self.token_label == self.token_table.begin_token_label

    def is_end_token(self):
        """Test whether this token is the end-token."""
        return self.token_label == self.token_table.end_token_label

    def is_begin_or_end_token(self):
        """Test whether this token is either the begin- or end-token."""
        return self.is_begin_token() or self.is_end_token()

    #
    # Various representations.  Note a token can be considered a node or a subtree.
    # Coming straight from the Lexer, though, they do not yet have any children.
    #

    def traditional_repr(self):
        """Representation as a string that looks like class initialization."""
        return "TokenNode()"

    def value_repr(self):
        """Token representation as its value."""
        return str(self.value)

    def label_repr(self):
        """Token representation as its token label."""
        return str(self.token_label)

    def summary_repr(self):
        """Token representation as a summarizing string containing both the label and
        the value."""
        value_str = str(self.value)
        if isinstance(self.value, str):
            value_str = "'" + value_str + "'"
        return "<{0},{1}>".format(self.token_label, value_str)

    def tree_repr(self, indent=0):
        """Token representation as the root of a parse subtree, with formatting.
        The optional `indent` parameter can be either an indent string or else
        an integer for the number of spaces to indent."""
        try:
            num_indent = int(indent)
        except ValueError:
            pass
        else:
            indent = " " * num_indent
        string = indent + self.summary_repr() + "\n"
        for c in self.children:
            string += c.tree_repr(indent=indent+" "*4)
        return string

    def string_tree_repr(self, only_vals=False, only_labels=False):
        """Token representation as the root of a parse subtree, in a string format.
        This is the default representation, used for `__repr__`."""
        string = self.summary_repr()
        if only_vals: string = self.value_repr()
        if only_labels: string = self.label_repr()
        if self.children:
            string += "("
            string += ",".join(c.string_tree_repr() for c in self.children)
            string += ")"
        return string

    def old_repr(self):
        """This old representation is kept *only* because it is used in some tests."""
        if self.token_label == "k_number":
            return "[literal {0}]".format(self.value)
        if self.token_label == "k_lpar":
            if self.children:
                return "[k_lpar {0} k_rpar]".format(self.children[0].old_repr())
            else:
                return "[literal k_lpar]"
        else:
            str_val = "[" + str(self.value)
            for a in self.children: str_val += " " + a.old_repr()
            str_val += "]"
            return str_val
    __repr__ = string_tree_repr


def basic_token_subclass_factory():
    """Create and return a new token subclass representing tokens with label
    `token_label`.  This function is called from the `_create_token_subclass`
    method  of `TokenTable` when it needs to create a new one to
    start with.  This function **should not be called directly**, since
    additional attributes (such as the token label and a new subclass name)
    also need to be added to the generated subclass.

    This function is the default argument to the `token_subclassing_fun`
    keyword argument of the initializer for `TokenTable`.  Users
    can define their own such function in order to add methods to token objects
    which are particular to their own application (the `PrattParser` class does
    this, for example).

    Note that using a separate subclass for each token label allows for
    attributes and methods specific to a kind of token to be pasted onto the
    class itself without conflicts.  For example, the `PrattParser` subclass
    adds head handler and tail handler methods which are specific to a given
    token label."""
    # If we instead used a metaclass to generate the token subclasses instead
    # of a factory then it would be possible to define a __repr__ that controls
    # how the token-representing classes themselves are printed (they are ugly
    # now).  How much would this complicate things for users who wanted to
    # create their own factory?  Kind of an advanced topic for many people.  If
    # they could simply declare a metaclass that would be OK, but might need
    # args passed, etc.  See version in PrattParser module.
    class TokenSubclass(TokenNode):
        """This is the class returned by the factory function."""
        def __init__(self, value):
            super(TokenSubclass, self).__init__() # Call base class __init__.
            self.value = value # Passed in for instances by the Lexer token generator.

    return TokenSubclass

#
# Token table.
#

class TokenTable(object):
    """A symbol table holding subclasses of the `TokenNode` class for each token label
    defined in a `Lexer` instance.  Also has methods for operating on tokens.
    Each `Lexer` instance contains an instance of this class to save the subclasses for
    the kinds of tokens which have been defined for it."""
    def __init__(self, token_subclass_factory_fun=basic_token_subclass_factory,
                       pattern_matcher_instance=None):
        """Initialize the token table.

        The parameter `token_subclass_factory_fun` can be passed a function to
        be used to generate token subclasses, taking a token label as an
        argument.  The default is `basic_token_subclass_factory`.

        The parameter `pattern_matcher_instance` can be passed an empty pattern
        matcher instance, which will be used instead of the default one.  In
        this way users can define their own matchers, or pass in whatever options
        they choose to the initializer of the default one."""
        self.token_subclassing_fun = token_subclass_factory_fun
        self.token_subclass_dict = {}
        self.lex = None # The lexer currently associated with this token table.

        self.begin_token_label = None
        self.begin_token_subclass = None
        self.end_token_label = None
        self.end_token_subclass = None
        if pattern_matcher_instance is None:
            pattern_matcher_instance = Matcher()
        self.pattern_matcher = pattern_matcher_instance

    def __contains__(self, token_label):
        """Test whether a token subclass for `token_label` has been stored."""
        return token_label in self.token_subclass_dict

    def __getitem__(self, token_label):
        """Look up the subclasses of base class `TokenNode` corresponding to
        `token_label` in the token table and return it.  Raises a
        `LexerException` if no subclass is found for the token label."""
        if token_label in self.token_subclass_dict:
            TokenSubclass = self.token_subclass_dict[token_label]
        else:
            raise LexerException("No token with label '{0}' is in the token table."
                                   .format(token_label))
        return TokenSubclass

    def _create_token_subclass(self, token_label, store_in_dict=True):
        """Create a subclass for tokens with label `token_label` and store it
        in the token table.  Return the new subclass.  Raises a `LexerException`
        if a subclass for `token_label` has already been created.  If
        `store_in_dict` is `False` then the token is not stored."""
        if token_label in self.token_subclass_dict:
            raise LexerException("In `_create_token_subclass`, already created the"
                    " token subclass for token_label '{0}'.".format(token_label))
        # Create a new token subclass for token_label and add some attributes.
        TokenSubclass = self.token_subclassing_fun()
        TokenSubclass.token_label = token_label
        TokenSubclass.__name__ = "TokenClass_" + token_label # For debugging.
        # Store the newly-created subclass in the token_dict.
        if store_in_dict:
            self.token_subclass_dict[token_label] = TokenSubclass
        return TokenSubclass

    def undef_token_subclass(self, token_label):
        """Un-define the token with label token_label.  The `TokenNode` subclass
        previously associated with that label is removed from the dictionary."""
        try:
            del self.token_subclass_dict[token_label]
        except KeyError:
            return # Not saved in dict, ignore.

    def undef_token(self, token_label):
        """Undefine the token corresponding to `token_label`."""
        # Remove from the list of defined tokens and from the token table.
        self.undef_token_subclass(token_label)
        self.pattern_matcher.undef_pattern(token_label)

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False,
                  matcher_options=None):
        """Define a token and the regex to recognize it.  Returns the new
        token subclass.

        The label `token_label` is the label for the kind of token.

        The label `regex_string` is a Python regular expression defining the
        text strings which match for the token.  If `regex_string` is set to
        `None` then a dummy token will be created which is never searched for
        in the lexed text.  To better catch errors it does not have a default
        value, so setting it to `None` must be done explicitly.

        Setting `ignore=True` will cause all such tokens to be ignored (except
        that they will be placed on the `ignored_before` list of the
        non-ignored token that they precede).

        In case of ties for the longest match in scanning, the integer
        `on_ties` values are used to break the ties.  If any two are still
        equal an exception will be raised.

        The `option` parameter takes a string value, which is then passed to
        the `insert_pattern` method of whatever matcher is being used."""
        if token_label in self:
            raise LexerException("A token with label '{0}' is already defined.  It "
            "must be undefined before it can be redefined.".format(token_label))

        if regex_string is not None:
            self.pattern_matcher.insert_pattern(token_label, regex_string,
                         on_ties, ignore=ignore, matcher_options=matcher_options)

        # Initialize and return a bare-bones, default token_subclass.
        tok = self._create_token_subclass(token_label)
        tok.token_table = self
        return tok

    def def_begin_token(self, begin_token_label):
        """Define the begin-token.  The lexer's `def_begin_end_tokens` method
        should usually be called instead."""
        tok = self.def_token(begin_token_label, None)
        self.begin_token_label = begin_token_label
        self.begin_token_subclass = tok
        return tok

    def def_end_token(self, end_token_label):
        """Define the end-token.  The `def_begin_end_tokens` method should usually
        be called instead."""
        tok = self.def_token(end_token_label, None)
        self.end_token_label = end_token_label
        self.end_token_subclass = tok
        return tok

    def get_next_token_label_and_value(self, program, prog_unprocessed,
                                          ERROR_MSG_TEXT_SNIPPET_SIZE):
        """Return the next token label for the start of the current program
        text, as in the string `program` and indexed by the numbers in
        the ordered-pair tuple `prog_unprocessed`."""
        return self.pattern_matcher.get_next_token_label_and_value(
                                              program, prog_unprocessed,
                                              ERROR_MSG_TEXT_SNIPPET_SIZE)

    def ignored_tokens(self):
        """Return the set of ignored tokens."""
        return self.pattern_matcher.ignore_tokens

#
# Lexer
#

class TokenBuffer(object):
    """An abstraction of the token buffer.  This is used internally by the
    `Lexer` class and should not usually be accessed by users.  It is basically
    a nice wrapper over an underlying deque, but this is complicated by the
    need to save persistent state pointers into the buffer even in fixed-size
    buffers when tokens at the front get dropped.

    Previous tokens are stored in the same deque as the current token and any
    lookahead tokens.  The default indexing is relative to the current token,
    at `current_offset`, which is zero for the current token.  (The current
    offset is itself relative to a reference point, but users do not need to
    know that detail)."""

    def __init__(self, token_getter_fun, max_peek=None, max_deque_size=None,):
        """Initialize the buffer."""
        self.token_getter_fun = token_getter_fun
        self.max_deque_size = max_deque_size
        self.max_peek = max_peek
        # Any popleft operations are done explicitly, so maxlen=None.
        self.token_buffer = collections.deque(maxlen=None)

        # Indices are relative to current_offset, and current_offset is
        # relative to reference_point.  This is because a fixed-size deque can
        # drop elements.  The current_offset is not relative to 0 because then
        # a saved offset would become invalid when items are popped off the
        # left of the deque.  The reference point is decremented for every
        # deque element popped off the left, and at no other time.  That way,
        # the current offset can be saved and remain valid until the
        # `TokenBuffer` is reset (though the referenced item may get deleted).
        self.current_offset = 0
        self.reference_point = 0

    def init(self, begin_token):
        """Initialize the token buffer, or clear an reset it.  Any saved
        offsets are no longer valid, but no check is made for that."""
        self.current_offset = 0
        self.reference_point = 0
        self.token_buffer.clear()
        self._append(begin_token)

    def state_to_offset(self, state):
        """Return the offset into the current deque that corresponds to what
        was the offset (absolute index to the current token) at the time when
        the state was saved."""
        return state - self.reference_point

    def get_state(self):
        """Return a buffer state indicator that can be returned to later.  The
        `go_back` or `push_back` methods of the lexer use this."""
        return self._offset_to_absolute(self.current_offset)

    def __getitem__(self, index):
        """Index the buffer relative to the current offset.  Zero is the
        current token.  Negative indices go back in the buffer.  They **do not**
        index from the end of the buffer, as with ordinary Python indexing."""
        # Slices are CURRENTLY NOT allowed.  What should they return?  How
        # do you know where the zero point is?
        #if isinstance(index, slice): # Handle slices recursively.
        #    start = index.start
        #    stop = index.stop
        #    step = index.step
        #    # The indices call returns the above three in a tuple.
        #    return [self[i] for i in range(*index.indices(len(self)))]
        if isinstance(index, int): # The ordinary case.
            if self.max_peek is not None and index > self.max_peek:
                raise LexerException("User-set maximum peeking level of {0} was"
                                     " exceeded.".format(self.max_peek))
            while self._index_to_absolute(index) >= len(self.token_buffer):
                if self.token_buffer[-1].is_end_token():
                    return self.token_buffer[-1]
                self._append()
            return self.token_buffer[self._index_to_absolute(index)]
        else:
            raise TypeError("Invalid argument type in __getitem__ of TokenBuffer.")

    def num_saved_previous_tokens(self):
        """Return the number of tokens before the current token that are saved."""
        return self._offset_to_absolute(self.current_offset)

    def num_tokens_after_current(self):
        """An informational method.  Returns the number of tokens from
        the current token to the end of the token buffer.  Some may have been
        read past the position of the current token due to peeks or
        pushbacks."""
        begin_point = self._offset_to_absolute(self.current_offset) + 1
        return len(self.token_buffer) - begin_point

    def move_forward(self, num_toks=1):
        """Move the current token (i.e., the offset) forward by one.  This is
        the token buffer's equivalent of `next`, except that it returns
        previously-buffered tokens if possible.  The `Lexer` method `next`
        should always be called by users of that class, because it also handles
        some other things.

        Attempts to move past the first end-token leave the current offset at
        the first end-token.  No new tokens are added to the buffer.  The
        end-token is returned."""
        for _ in range(num_toks):
            if self[0].is_end_token():
                break
            self.current_offset += 1
            self._fill_to_current_offset()
        return self[0]

    def move_back(self, num_toks=1):
        """Move the current token (i.e., offset) back `num_toks` tokens.  Will
        always stop at the begin-token.  Users should check the condition if it
        matters.  If the move attempts to move back to before the
        currently-saved tokens, but the begin-token is no longer saved, then a
        `LexerException` is raised."""
        self.current_offset -= num_toks
        absolute_index = self._offset_to_absolute(self.current_offset)
        if absolute_index < 0:
            self.current_offset += abs(absolute_index)
            curr_token = self[0]
            if not curr_token.is_begin_token():
                raise LexerException("Not enough saved tokens to move back to the"
                                     " begin-token in the `move_back` method.")
        else:
            curr_token = self[0]
        return curr_token

    #
    # Internal utility methods below.
    #

    def _index_to_absolute(self, index):
        """Convert an index into an absolute index into the current deque.
        Note that any changes to the current offset or to the reference
        point (the latter via _append) will invalidate the absolute reference.
        In those cases it will need to be re-calculated."""
        return index + self.current_offset + self.reference_point

    def _offset_to_absolute(self, offset):
        """Convert an offset into an absolute index into the current deque.
        Note that calls to `_append` can modify the reference point and
        invalidate the absolute index.  Needs to be re-calculated after
        such a call."""
        return offset + self.reference_point

    def _fill_to_current_offset(self):
        """If the current offset points past the end of the token buffer then
        get tokens and append them until it is a valid index.  Note this
        calls `_append`.  If the current offset is past the first end token
        in the text then the current offset point is reset to the first end
        token.  Only one end token is ever stored in the buffer."""
        while self._offset_to_absolute(self.current_offset) >= len(self.token_buffer):
            self._append() # Note this call can change self.reference_point.

    def _pop(self):
        """Users should not call.  Pop off the rightmost item and return it.  Moves
        the current token backward if necessary."""
        retval = self.token_buffer.pop()
        if self._offset_to_absolute(self.current_offset) >= len(self.token_buffer):
            self.current_offset -= 1
        return retval

    def _append(self, tok=None):
        """Append to buffer and fix current index if necessary.  Users should not
        call.  If `tok` is not set then the token to append is obtained from the
        `token_getter_fun` function.

        Note that this is the **only** method that ever gets tokens directly
        from the token getter function."""
        # TODO: should this fill with end tokens, or stop at the first end token?
        if tok is None:
            tok = self.token_getter_fun()
            # TODO: below causes a FAIL with go_back hanging... probably __getitem__
            # calling but not compensating for this behavior...
            if tok.is_end_token() and self.token_buffer[-1].is_end_token():
                return tok
        self.token_buffer.append(tok)
        if (self.max_deque_size is not None
                and len(self.token_buffer) > self.max_deque_size):
            self.reference_point -= 1
            self.token_buffer.popleft() # Do an explicit popleft.
            if self._offset_to_absolute(self.current_offset) < 0:
                raise LexerException("Error in TokenBuffer:"
                    " Maximum buffer size is too small for the amount of peeking."
                    " Current token was deleted.")
            assert len(self.token_buffer) == self.max_deque_size

if cythonize:
    TokenBuffer = TB

class GenTokenState(object):
    """The state of the token_generator program execution."""
    ordinary = 1
    end = 2
    uninitialized = 3

# The beginnings of a state tuple for the Lexer.  NOT YET USED AT ALL, but it would
# be more elegant than the current ad hoc state restoration approach.
LexerState = collections.namedtuple("LexerState", [
                           "x",
                           "y",
                        ])

# TODO: Consider if it is a good idea to have a method like `next_raw` which
# would just return tokens for raw characters.  This would be useful in
# parsing, say, C-style comments using the parser rather than a complicated
# regex.  Would need a special token kind to return for it.  This effectively
# modifies the token set scanned for, and would require flushing the buffer
# with go_back.  Other than that it should work. Not especially efficient to
# create a token for each char, but still linear in text size.

class Lexer(object):
    """Scans text and returns tokens, represented by instances of `TokenNode`
    subclass instances. There is one subclass for each kind of token, i.e., for
    each token label.  These subclasses themselves are assumed to have been
    created before any scanning operation, via the `def_token` method.

    Token sequences are assumed to have both a begin-token and an end-token
    sentinel, defined via the `def_begin_end_tokens` method.  Exactly one
    end-token will be returned by `next`; any further calls to `next` raise
    `StopIteration`.

    The scanning is independent of the order in which tokens are defined.  The
    longest match over all token patterns will always be the one selected.  In
    case of ties the `on_ties` value (passed to `def_token`) is used to
    break it.  If that fails a `LexerException` is raised.

    If no token table is passed into `__init__` the `Lexer` will create its
    own empty one."""

    ERROR_MSG_TEXT_SNIPPET_SIZE = 40 # Number of characters to show for context.
    DEFAULT_BEGIN = "k_begin" # Default label for begin-token.
    DEFAULT_END = "k_end" # Default label for end-token.

    #
    # Initialization methods
    #

    def __init__(self, token_table=None, max_peek_tokens=None,
                 max_deque_size=None, default_begin_end_tokens=False,
                 final_mod_function=None):

        """Initialize the Lexer.  Optional arguments set the `TokenTable` to be
        used (default creates a new one), the maximum number of lookahead
        tokens (default is no fixed maximum), and the maximum deque size, which
        determines how far `go_back` operations will work (the default is
        unlimited).

        If `default_begin_end_tokens` is true then begin- and end-tokens will
        be defined using the default token labels.  By default, though, the user
        must call the `def_begin_end_tokens` method to define the begin and
        end tokens (using whatever labels are desired).

        If `final_mod_function` is passed a function taking a two arguments
        then any time a token instance is created by the lexer that function
        will be called with the parser and the token itself as the two
        arguments.  It should return the modified token."""
        self.reset(token_table=token_table, max_peek_tokens=max_peek_tokens,
                   max_deque_size=max_deque_size,
                   default_begin_end_tokens=default_begin_end_tokens,
                   final_mod_function=final_mod_function)

    def reset(self, token_table=None, max_peek_tokens=None,
                    max_deque_size=None, default_begin_end_tokens=False,
                    final_mod_function=None):
        """Return the lexer to the initial state.  Takes the same arguments as
        the initializer."""
        self.text_is_set = False
        self.token = None
        self.all_token_count = None

        if token_table is None:
            token_table = TokenTable()
        self.set_token_table(token_table)

        if cythonize:
            if max_peek_tokens is None: max_peek_tokens = -1
            if max_deque_size is None: max_deque_size = -1
        self.token_buffer = TokenBuffer(self._unbuffered_token_getter,
                                              max_peek=max_peek_tokens,
                                              max_deque_size=max_deque_size)

        # These line and char numbers are for raw, unprocessed tokens, not the
        # buffered ones.  Use the values set with tokens as the token attribute
        # line_and_char (such as for the current token) for that info.
        self.raw_linenumber = 1 # The line number currently being read.
        self.upcoming_raw_charnumber = 1 # Char number of first char of upcoming token.
        self.upcoming_raw_total_chars = 1 # Like above, but total num., not on line.

        self.all_token_count = 0

        self.token_generator_state = GenTokenState.uninitialized

        if default_begin_end_tokens:
            self.def_begin_end_tokens(self.DEFAULT_BEGIN, self.DEFAULT_END)

        self.final_mod_function = final_mod_function

        # The default exception raised by methods like `match_next`.
        self.default_helper_exception = LexerException

    def set_token_table(self, token_table, go_back=0):
        """Sets the current `TokenTable` instance for the lexer to
        `token_table`.  This is called on initialization, but can also be
        called at any time.  If text is being scanned at the time then it
        flushes the current and lookahead tokens and re-scans the current
        token.

        When set with this method the token table is always given the attribute
        `lex`, which points to the lexer instance that this method was called from.
        This attribute is used by tokens (which know their fixed symbol table)
        so they can find the current lexer (to call `next`, etc.)"""
        self.token_table = token_table
        token_table.lex = self
        if self.text_is_set:
            self.go_back(0) # Re-scan the current token.

    def set_text(self, program,
                 reset_linenumber=True, reset_charnumber=True):
        # TODO: redefine to take a TextStream.  Be sure to also pass back position
        # info with the returned text so that tokens have have their line/position
        # of origin pasted onto them..... or at least keep track in generating
        # tokens.
        """Users should call this method to pass in the program text (or other
        text) which is to be lexically scanned.  The parameter `program` should
        be a string."""
        if not (self.token_table.begin_token_label
                and self.token_table.end_token_label):
            raise LexerException("Begin and end tokens must be defined by calling"
                    " `def_begin_end_tokens` before set_text can be called.")

        self.already_returned_end_token = False
        self._curr_token_is_first = False # Is curr token first non-ignored in text?
        self._returned_first_token = False

        # Reset line, character, and token counts.  All counts include the buffer.
        if reset_linenumber:
            self.raw_linenumber = 1
        if reset_charnumber:
            self.upcoming_raw_charnumber = 1
            self.upcoming_raw_total_chars = 1
        self.all_token_count = 0 # Count all actual tokens (not begin and end).
        self.non_ignored_token_count = 0 # Count non-ignored actual tokens.

        self.program = program # The program text currently being scanned/lexed.
        # The prog_unprocessed list holds slice indices for the unprocessed part
        # of the program text.  The go_back routine can modify this.
        self.prog_unprocessed = [0, len(self.program)] # The unprocessed slice.
        self.token_generator_state = GenTokenState.ordinary

        # Set up the token buffer.
        self._initialize_token_buffer()
        self.token = self.token_buffer[0] # Last token returned; begin-token here.
        self.text_is_set = True

    def _initialize_token_buffer(self):
        """A utility routine to initialize (fill) the token buffer.  The
        `token_buffer[0]` slot is the current token.  The current token will be
        set to the begin-token after this routine runs (since no tokens have
        yet been read with `next`).  Any tokens in the buffer past the first
        end-token are also set to end-tokens.  The size of the token buffer is
        `self.NUM_LOOKAHEAD_TOKENS` plus one for the current token.  For
        two-token lookahead the buffer deque has the form:
            [<current_token>, <peek1>, <peek2>]
        """
        begin_tok = self.token_table.begin_token_subclass(None) # Get instance.
        if self.final_mod_function:
            begin_tok = self.final_mod_function(self, begin_tok)
        tb = self.token_buffer
        tb.init(begin_tok) # Begin token set as current; first next() returns it.
        self.token = tb[0]
        assert tb[0].is_begin_token() # DEBUG check, remove later

    #
    # Next and peek related methods
    #

    def next(self, num=1):
        """Return the next token, consuming from the token stream.  Also sets
        `self.token` to the return value.  Returns one end-token and raises
        `StopIteration` on a `next` after that end-token.

        If `num` is greater than one a list of the tokens is returned.  This
        list is cut short if the first end-token is encountered, so this
        kind of `next` call will never generate `StopIteration`."""

        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's next method when no text is set.")

        if self.already_returned_end_token:
            self.text_is_set = False
            raise StopIteration

        # Handle num > 1 case with recursion.
        if num > 1:
            ret_list = []
            for _ in range(num):
                if not self.token.is_end_token():
                    ret_list.append(self.next())
                else:
                    break
            return ret_list

        # Handle ordinary case.
        tb = self.token_buffer
        self.token = tb.move_forward()
        if self.token.is_end_token():
            self.already_returned_end_token = True
        return self.token

    __next__ = next # For Python 3.

    def __iter__(self):
        return self # Class provides its own __next__ method.

    def peek(self, num_toks=1):
        """Peek ahead in the token stream without consuming any tokens.  The
        argument `num_toks` is the number of tokens ahead to peek.  The default
        peek of `num_toks=1` peeks at the token just beyond the current token.
        Peeking zero shows the current token.  Negative peeks are allowed, and
        look back at the previous tokens (up to the number saved in the token
        buffer).

        Tokens are read into the buffer on-demand to satisfy any requested
        peek.  If `max_peek_tokens` is set then an exception will be raised on
        attempts to peek farther than that."""
        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's peek method when no text is set.")
        try:
            retval = self.token_buffer[num_toks]
        except IndexError: # Shouldn't happen.
            raise BufferIndexError
        return retval

    def move_back(self, num_toks=1, num_is_raw=False):
        """NOT YET IMPLEMENTED

        Move the current token back in the token stream.  This method is
        similar to methods commonly called `push_back`. It is similar to
        `go_back` except that tokens are not rescanned.  The current position
        in the token buffer is just moved back.  This is more efficient than
        `go_back` but it assumes that there have been no modifications,
        additions, or deletions to the token definitions.  If the parser is
        guaranteed to be static with respect to the defined tokens then this is
        the routine to use.  Otherwise, use `go_back`.

        The optional parameter `num_toks` is the number of tokens to move
        back.  Negative numbers move forward, consuming more tokens if
        necessary.  Moving forward will always stop before consuming a second
        end-token (which would raise `StopIteration` if done in `next`)."""
        # TODO: Implement.  Shouldn't be too hard, but it might need to modify
        # some of the Lexer attributes.  Remember, though, that the line and
        # char number in the lexer class are for the latest *unbuffered* token.
        #
        # TODO use the token buffer's move_forward and move_back methods;
        # finish implementing them...
        raise NotImplementedError
        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's move_back method when no text is set.")
        if self.token_generator_state == GenTokenState.uninitialized:
            raise LexerException("The token generator has not been initialized "
                  "or has reached `StopIteration` by reading past the end-token.")

        token_buffer = self.token_buffer

    def _pop_tokens(self, n):
        """Pop `n` tokens from the token buffer, resetting the slice indices in
        `self.prog_unprocessed` and other state variables.  Used by `go_back`."""
        popped_to_begin_token = False
        current_token_is_first = False
        for _ in range(n):
            token_buffer = self.token_buffer

            if token_buffer[0].is_begin_token():
                popped_to_begin_token = True
                self.token = self.token_buffer[0]
                return popped_to_begin_token, current_token_is_first

            popped = token_buffer._pop()

            if popped.is_end_token():
                continue # No actual text was read for end tokens.

            self.non_ignored_token_count -= 1
            self.all_token_count -= (1 + len(popped.ignored_before))

            # Reset the line number information.
            if popped.ignored_before:
                line_and_char = popped.ignored_before[0].line_and_char
                char_index_in_program = popped.ignored_before[0].char_index_in_program
            else:
                line_and_char = popped.line_and_char
                char_index_in_program = popped.char_index_in_program
            self.raw_linenumber, self.upcoming_raw_charnumber = line_and_char
            self.upcoming_raw_total_chars = char_index_in_program

            # Reset the slice indices into the program text.
            self.prog_unprocessed[0] -= len(popped.original_matched_string)

            if token_buffer[-1].is_begin_token():
                current_token_is_first = True

        self.token = self.token_buffer[0]
        return popped_to_begin_token, current_token_is_first

    def go_back(self, num_toks=1, num_is_raw=False):
        """This method allows the lexer to go back in time by `num_toks`
        tokens.  The call `go_back(num_to_pop)` will undo the effects of the
        last `num_to_pop` calls to `next`.  This operation is different from
        the usual pushback operations because the program text is re-scanned,
        rather than simply backing up to already-scanned tokens.

        Going back one with `go_back(1)` or just `go_back()` results in the
        current token being set back to the previous token and also re-scanned
        from the original text.  Calling `go_back(0)` just re-scans the current
        token (and flushes any tokens in the lookahead buffer).  Values greater
        than one go farther back in the token stream.  Attempts to go back
        before the beginning of the program text go back to the beginning and
        stop there.

        This method returns the current token after any re-scanning.

        Values of `num_toks` less than one apply to saved loohahead tokens (if
        any).  The call `go_back(-1)` flushes all lookahead tokens saved in the
        buffer except the one immediately following the current token.

        If `num_is_raw` is true then `num_toks` is interpreted as the actual
        number of tokens to go back, including any in the buffer (which are
        otherwise handled automatically).  This can be useful when looking at
        `lex.all_token_count` to determine how far to go back and undo
        something.

        Going back with re-scanning can be necessary when token definitions
        themselves change dynamically, such as by semantic actions.  For
        example, a declaration of the string "my_fun" as a variable might
        dynamically add a token for that new variable, which would then stop it
        from matching a general identifier with a lower on_ties value (set to,
        say, -1).  This kind of thing is also needed when swapping token
        tables, such as in parsing a sublanguage with a different parser.
        Since the sublanguage has a different collection of tokens the
        lookahead buffer must be re-scanned based on those tokens."""

        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's go_back method when no text is set.")
        if self.token_generator_state == GenTokenState.uninitialized:
            raise LexerException("The token generator has not been initialized "
                  "or has reached `StopIteration` by reading past the end-token.")

        token_buffer = self.token_buffer # Shorter alias.

        # For negative values just pop the required number off the end of token_buffer.
        if num_toks < 0:
            peekahead_num = abs(num_toks)
            self._pop_tokens(token_buffer.num_tokens_after_current() - peekahead_num)
            return self.token

        # We will re-scan at least one token, so reset `already_returned_end_token`.
        self.already_returned_end_token = False

        num_buffered_after_current = token_buffer.num_tokens_after_current()
        num_to_pop = num_toks + num_buffered_after_current + 1 # new curr is rescanned

        if num_is_raw:
            # Works with lex.all_token_count in production_rules, but why +2?
            # Setting max_peek_tokens doesn't affect it.  Clean up code.
            num_to_pop = num_toks + 2 # The added number doesn't matter except when it does...

        popped_to_begin_token, current_token_is_first = self._pop_tokens(num_to_pop)

        # Re-scan to get the new current token.
        if not popped_to_begin_token:
            self.next()

        # Reset some state variables.
        if popped_to_begin_token:
            self.peek().is_first = True
            self._returned_first_token = False
            self._curr_token_is_first = False
        elif current_token_is_first:
            self.token.is_first = True
            self._returned_first_token = True
            self._curr_token_is_first = True
        self.already_returned_end_token = self.token.is_end_token()

        return self.token

    def get_current_state(self):
        """Get a lexer state that can be returned to with `go_back_to_state`.
        States become invalid after the text is reset, but no check is made."""
        return self.token_buffer.get_state()

    def go_back_to_state(self, state):
        """Return the lexer to the state `state` saved from a previous call to
        `get_current_state`."""
        index_to_go_back_to = self.token_buffer.state_to_offset(state)
        num_to_go_back = self.token_buffer.current_offset - index_to_go_back_to
        if num_to_go_back < 0:
            self.next(-num_to_go_back)
        else:
            self.go_back(num_to_go_back)
        return self.token

    #
    # Informational methods
    #

    def curr_token_is_begin(self):
        """True if `self.token` (the last one returned by the `next` method) is
        the begin-token."""
        return self.token.is_begin_token()

    def curr_token_is_first(self):
        """True if `self.token` (the last one returned by the `next` function)
        is the first actual token in the currently-set program text.  Resetting
        the text resets this.  This value is also set as the attribute
        `is_first` on all returned tokens.  This is useful, for example, for
        finding indentation levels (along with `ignored_before_curr`)."""
        return self.token.is_first

    def ignored_before_curr(self):
        """Return the list of all tokens ignored just before `self.token` (the
        last token returned by the `next` function).  Useful for enforcing
        things like syntactic whitespace requirements, along with
        `curr_token_is_first`. This list is also set as the attribute
        `ignored_before_tokens` on all returned tokens."""
        return self.token.ignored_before

    def curr_token_is_end(self):
        """True if `self.token` (the last one returned by the `next` method) is
        the end-token."""
        return self.token.is_end_token()

    def is_defined_token_label(self, token_label):
        """Return true if `token` is currently defined as a token label."""
        return self.token_table.is_defined_token_label()

    def last_n_tokens_original_text(self, n):
        """Returns the original text parsed by the last `n` tokens (back from
        and including the current token).  This routine is mainly used to make
        error messages more helpful.  It uses the token attribute
        `original_matched_string` and the saved tokens in the token buffer.
        (which must be large enough for `n`)."""
        # TODO: Test this, code updated to use token_buffer class.
        # Could also print line numbers and stuff....
        n = min(n, self.token_buffer.num_saved_previous_tokens() + 1)
        prev_tokens = [self.token_buffer[t] for t in range(-n+1, 1)]
        string_list = [s.original_matched_string for s in prev_tokens]
        full_string = "".join(string_list)
        return full_string

    def get_unprocessed_text(self, peek=1):
        """Return all the text that is set but not yet processed.  Returns
        `None` if no text is currently set.  The current token is assumed
        to have been processed.

        By default this is relative to the token at a peek of `1`, but the
        `peek` number can be set to a previous or later one if available in the
        buffer."""
        if not self.text_is_set:
            return None
        text = self.program[self.peek(peek).char_index_in_program:]
        return text

    def get_processed_text(self, peek=1):
        """Return all the text that is set and has been processed.  Returns
        `None` if no text is currently set.  The current token is assumed
        to have been processed.

        By default this is relative to the current peek token, but the `peek`
        number can be set to a previous or later one if available in the
        buffer."""
        if not self.text_is_set:
            return None
        text = self.program[:self.peek(peek).char_index_in_program]
        return text

    #
    # Methods to define and undefine tokens
    #

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False,
                  matcher_options=None):
        """A convenience method to define a token. It calls the corresponding
        `def_token` method of the current `TokenTable` instance associated with
        the lexer, and does nothing else."""
        new_subclass = self.token_table.def_token(token_label, regex_string,
                    on_ties=on_ties, ignore=ignore, matcher_options=matcher_options)
        return new_subclass

    def undef_token(self, token_label):
        """A convenience function to call the corresponding `undef_token` of
        the current `TokenTable` instance associated with the Lexer."""
        self.token_table.undef_token(token_label)

    def def_ignored_token(self, token_label, regex_string, on_ties=0,
                          matcher_options=None):
        """A convenience function to define an ignored token without setting
        `ignore=True`.  This just calls `def_token` with the value set."""
        return self.def_token(token_label, regex_string, on_ties=on_ties, ignore=True,
                              matcher_options=matcher_options)

    def def_multi_tokens(self, tuple_list, **kwargs):
        """A convenience function, to define multiple tokens at once.  Each element
        of the passed-in list should be a tuple containing the arguments to the
        ordinary `def_token` method.  Called in the same order as the list.  Any
        keyword arguments are passed on to `def_token`.  Returns a tuple of the
        defined tokens."""
        return multi_funcall(self.def_token, tuple_list, **kwargs)

    def def_multi_ignored_tokens(self, tuple_list, **kwargs):
        """A convenience function, to define multiple tokens at once with
        `ignore=True` set.  Each element of the passed-in list should be a tuple
        containing the arguments to the ordinary `def_token` method.  Called in
        the same order as the list.  Any keyword arguments are passed on to
        `def_token`.  Returns a tuple of the defined tokens."""
        return multi_funcall(self.def_ignored_token, tuple_list, **kwargs)

    def def_begin_end_tokens(self, begin_token_label, end_token_label):
        """Define the sentinel tokens at the beginning and end of the token
        stream.  This method must be called before using the Lexer.  It will
        automatically be called using default token label values unless
        `default_begin_end_tokens` was set false on initialization.  Returns a
        tuple of the new begin- and end-token subclasses.  These tokens do not
        need to be defined with `def_token` because they are never actually
        scanned and recognized in the program text (which would also require a
        regex pattern)."""
        # TODO: consider if begin and end tokens should be created by a
        # token_table method.  Probably they should, but then in Lexer need
        # to change all the self.begin_token_label to have self.token_table
        # prefix.
        begin_tok = self.token_table.def_begin_token(begin_token_label)
        end_tok = self.token_table.def_end_token(end_token_label)
        return begin_tok, end_tok

    def def_default_whitespace(self, space_label="k_space", space_regex=r"[ \t]+",
                        newline_label="k_newline", newline_regex=r"[\n\f\r\v]+",
                        matcher_options=None):
        """Define the standard whitespace tokens for space and newline, setting
        them as ignored tokens."""
        tok = self.def_ignored_token
        tok(space_label, space_regex, matcher_options=matcher_options)
        tok(newline_label, newline_regex, matcher_options=matcher_options)

    #
    # Some helper functions when using the Lexer class.
    #

    def match_next(self, token_label_to_match, peeklevel=1, consume=True,
                   raise_on_fail=False, raise_on_success=False,
                   err_msg_tokens=3):
        # TODO: Consider a way for users to define custom error strings for
        # better error-reporting.
        """A utility function that tests whether the value of the next token
        label equals a given token label.

        This method consumes a token from the lexer if and only if there is a
        match.  Either way, a boolean is returned indicating the match status.

        If `consume` is false then no tokens will ever be consumed.  Otherwise,
        and by default, a token will be consumed if and only if it matches.

        The parameter `peeklevel` is passed to the peek function for how far
        ahead to look; the default is one.

        If `raise_on_fail` set true then a `LexerException` will be raised by
        default if the match fails.  The default can be changed by setting the
        lexer instance attribute `default_helper_exception`.  Similarly,
        `raise_on_success` raises an exception when a match is found.  Either one
        can be set to a subclass of `Exception` instead of a boolean, and then
        that exception will be called.

        The parameter `err_msg_tokens` can be set to change how many tokens
        worth of text back the error messages report (as debugging
        information) when an exception is raised.  (The count does not
        include whitespace, but it is printed, too.)"""
        retval = False
        if token_label_to_match == self.peek(peeklevel).token_label:
            retval = True
        if consume and retval:
            self.next() # Eat the token that was matched.

        if retval and raise_on_success:
            exception = return_first_exception(raise_on_success,
                                               self.default_helper_exception)
            raise exception(
                    "Function match_next (with peeklevel={0}) found unexpected "
                    "token {1}.  The text of the {2} tokens up to "
                    "the error is: {3}" # TODO fix below, fails with parser
                    .format(peeklevel, str(self.peek(peeklevel)), err_msg_tokens,
                        self.last_n_tokens_original_text(err_msg_tokens)))
        if not retval and raise_on_fail:
            exception = return_first_exception(raise_on_fail,
                                               self.default_helper_exception)
            raise exception(
                    "Function match_next (with peeklevel={0}) expected token "
                    "with label '{1}' but found token {2}.  The text parsed "
                    "from the tokens up to the error is: {3}" # TODO fix below, fails
                    .format(peeklevel, token_label_to_match,
                            str(self.peek(peeklevel)),
                            self.last_n_tokens_original_text(err_msg_tokens)))
        return retval

    # TODO document these utilities......
    def in_ignored_tokens(self, token_label_to_match,
                          raise_on_fail=False, raise_on_success=False):
        """A utility function to test if a particular token label is among
        the tokens ignored before the current token.  Returns a boolean
        value.  Like `match_next`, this method can be set to raise an
        exception on success or failure."""
        retval = False
        ignored_token_labels = [t.token_label for t in self.peek().ignored_before]
        if token_label_to_match in ignored_token_labels:
            retval = True

        if retval and raise_on_success:
            exception = return_first_exception(raise_on_success,
                                               self.default_helper_exception)
            raise exception(
                    "Function in_ignored_tokens found unexpected token with "
                    "label '{0}' before the current token {1}."
                    .format(token_label_to_match, str(self.token)))
        if not retval and raise_on_fail:
            exception = return_first_exception(raise_on_fail,
                                               self.default_helper_exception)
            raise exception(
                    "Function in_ignored_tokens expected token with label "
                    "'{0}' before the current token {1}, but it was not found."
                    .format(token_label_to_match, str(self.token)))
        return retval

    def no_ignored_after(self, raise_on_fail=False, raise_on_success=False):
        """A boolean utility function to test if any tokens were ignored
        between current token and lookahead.  Like `match_next`, this method
        can be set to raise an exception on success or failure."""
        retval = True
        if self.peek().ignored_before:
            retval = False

        if retval and raise_on_success:
            exception = return_first_exception(raise_on_success,
                                               self.default_helper_exception)
            raise exception(
                    "Function no_ignored_after expected tokens between the current "
                    "token {0} and the following token {1}, but there were none."
                    .format(str(self.token), str(self.peek())))
        if not retval and raise_on_fail:
            exception = return_first_exception(raise_on_fail,
                                               self.default_helper_exception)
            raise exception(
                    "Function no_ignored_after expected nothing between the "
                    "current token {0} and the following token {1}, but there "
                    "were ignored tokens."
                    .format(str(self.token), str(self.peek())))
        else:
            return False
        return retval

    def no_ignored_before(self, raise_on_fail=False, raise_on_success=False):
        """A boolean utility function to test if any tokens were ignored between
        previous token and current token.  Like `match_next`, this method
        can be set to raise an exception on success or failure."""
        retval = True
        if self.token.ignored_before:
            retval = False

        if retval and raise_on_success:
            exception = return_first_exception(raise_on_success,
                                               self.default_helper_exception)
            raise exception(
                    "Function no_ignored_before expected ignored tokens before "
                    " the current token {0}, but none were found."
                    .format(str(self.token)))
        if not retval and raise_on_fail:
            exception = return_first_exception(raise_on_fail,
                                               self.default_helper_exception)
            raise exception(
                    "Function no_ignored_before expected no ignored tokens "
                    "before the current token {0}, but at least one was found."
                    .format(str(self.token)))
        return retval

    #
    # Lower-level routine for token generation
    #

    def _unbuffered_token_getter(self):
        """This routine generates tokens from the program text in the attribute
        `self.program`.  It does not modify the program itself, but keeps slice
        indices in a list `self.prog_unprocessed` indexing the unprocessed
        part.  That slice can be externally modified (the `go_back` routine
        does this).

        This is a lower-level function used by `next` to do the real work.  All
        the token subclasses should have been defined and stored in the the
        `TokenTable`.  Regexes defined for tokens are repeatedly matched at the
        beinning of the string `program`.  When a winning_index is found it is
        stripped off the beginning of the unprocessed slice of `program`.  For
        each winning_index the token subclass is looked up in the `TokenTable`
        object and an instance of that subclass is returned to represent the
        token.  Every token processed is represented by a unique new instance
        of the appropriate subclass of `TokenNode`.

        This generator has two states which can be set instance-globally to
        alter the state of the generator.  The states are
        `GenTokenState.ordinary` for ordinary scanning execution, and
        `GenTokenState.end` when all the tokens have been read.  In the
        `GenTokenState.end` state the method returns nothing but end tokens.
        The end state is normally entered when the program text becomes empty.
        If that variable is later is set to have text again the state switches
        back to ordinary.  (The lexer's `next` routine handles any raising of
        `StopIteration`.)"""
        ignored_before_labels = []
        ignored_before_tokens = []
        original_matched_string = ""
        token_table = self.token_table

        while True:
            self._curr_token_is_first = not self._returned_first_token
            self._returned_first_token = True

            if self.prog_unprocessed[0] == self.prog_unprocessed[1]:
                self.token_generator_state = GenTokenState.end
            else:
                self.token_generator_state = GenTokenState.ordinary

            first_after_newline = False

            # =======================================================================
            # === Ordinary execution state ==========================================
            # =======================================================================
            if self.token_generator_state == GenTokenState.ordinary:
                # Find the token_label and token_value of the matching prefix
                # which is longest (with ties broken by the on_ties values).
                label_and_value = token_table.get_next_token_label_and_value(
                                    self.program, self.prog_unprocessed,
                                    self.ERROR_MSG_TEXT_SNIPPET_SIZE)
                token_label, token_value = label_and_value

                # Remove matched prefix of the self.prog_unprocessed argument after
                # saving the matched prefix string.
                original_matched_string += self.program[self.prog_unprocessed[0]:
                                        self.prog_unprocessed[0]+len(token_value)]
                self.prog_unprocessed[0] += len(token_value)

                # Look up the class to represent the winning_index.
                try:
                    token_subclass_for_label = token_table[token_label]
                except LexerException:
                    raise LexerException("Undefined key in token table for "
                                         "token_label '{0}'.".format(token_label))

                # Make an instance of the class to return (or at least to save
                # in the token's ignored_before if ignored).
                token_instance = token_subclass_for_label(token_value)
                if self.final_mod_function:
                    token_instance = self.final_mod_function(self, token_instance)
                self.all_token_count += 1

                # Save the line and char counts for the beginning text of the
                # token with the token from the Lexer attributes.  Then update
                # the Lexer attributes.  Remember that the Lexer class versions
                # always refer to the beginning of the next token to be read
                # (into the buffer, not as the current token).  The versions
                # stored with the tokens themselves hold the beginning of text
                # when this routine scanned that token (including any ignored
                # text before it).
                #
                # Remember that we are looping and getting tokens which may turn
                # out to be ignored tokens (tested just below this block).
                token_instance.line_and_char = (
                                 self.raw_linenumber, self.upcoming_raw_charnumber)
                token_instance.char_index_in_program = self.upcoming_raw_total_chars
                num_newlines = token_value.count("\n")
                self.raw_linenumber += num_newlines
                if num_newlines == 0:
                    self.upcoming_raw_charnumber += len(token_value)
                else:
                    first_after_newline = True
                    last_newline = token_value.rfind("\n")
                    self.upcoming_raw_charnumber = (
                            len(token_value) - (last_newline + 1) + 1)
                self.upcoming_raw_total_chars += len(token_value)

                # ------------------------------------------------------------------
                # Go to the top of the loop and get another if the token is ignored.
                # ------------------------------------------------------------------
                if token_label in token_table.ignored_tokens():
                    ignored_before_labels.append(token_label)
                    ignored_before_tokens.append(token_instance)
                    continue

                self.non_ignored_token_count += 1

            # =======================================================================
            # === Return only end-tokens state ======================================
            # =======================================================================
            elif self.token_generator_state == GenTokenState.end:
                token_subclass_for_end = token_table[token_table.end_token_label]
                token_instance = token_subclass_for_end(None)
                if self.final_mod_function:
                    token_instance = self.final_mod_function(self, token_instance)

                token_instance.line_and_char = (self.raw_linenumber,
                                                self.upcoming_raw_charnumber)
                token_instance.char_index_in_program = self.upcoming_raw_total_chars

            # Got a token to return.  Set some attributes and return it.
            # Note that the attributes below are not set on ignored tokens!
            token_instance.original_matched_string = original_matched_string
            token_instance.ignored_before = tuple(ignored_before_tokens)
            token_instance.all_token_count = self.all_token_count
            token_instance.non_ignored_token_count = self.non_ignored_token_count
            token_instance.is_first = self._curr_token_is_first
            token_instance.is_first_on_line = token_instance.is_first or first_after_newline

            return token_instance


def multi_funcall(function, tuple_list, **kwargs):
    """A convenience function that takes a function (or method) and a list of tuples
    and calls `function` with the values in those tuple as arguments.
    Any unrecognized keyword arguments are passed on to the function `function`
    as keyword arguments.  If the `exception_to_raise` keyword argument is provided with
    an exception then that exception will be called whenever a `TypeError` results from
    the attempt to call `function` (defaulting to `LexerException`)."""
    retval_list = []
    exception_to_raise = kwargs.pop("exception_to_raise", LexerException)
    print("kwargs to pass to function are", kwargs)
    for t in tuple_list:
        try:
            retval_list.append(function(*t, **kwargs))
        except TypeError:
            raise exception_to_raise(
                    "Bad multi-definition of {0}: Omitted required arguments or bad "
                    "keyword arguments passed in.  Error on this tuple:\n{1}\nwith "
                    "keyword arguments\n{2}".format(function.__name__, t, kwargs))
    return tuple(retval_list)

def return_first_exception(*args):
    """Go down the argument list and return the first object that is a
    subclass of the `Exception` class.  Arguments do not need to all be
    classes.  Returns `None` if all fail.  Used to allow an optional exception
    class to be passed to a function instead of true, with a default called
    if the passed-in value is not an exception."""
    for item in args:
        if is_subclass_of(item, Exception):
            return item
    return None

#
# Exceptions
#

class BufferIndexError(LexerException):
    """Raised on attempts to read past the beginning or the end of the buffer
    (such as in `peek` methods)."""
    pass


