# -*- coding: utf-8 -*-
"""

The `Lexer` class is a general lexer/scanner/tokenizer module.  It was meant to
be used by the `PrattParser` class, but it could also be used for other lexical
scanning applications.

The general purpose of the `Lexer` is to take a string of text and produce a
corresponding string of tokens.  The tokens themselves are defined by the user,
with a string label and a regex pattern that is searched for in the program
text.  A `Lexer` class is a generator that sequentially produces tokens.  It is
also an iterator, so after initialization with text it can be used in loops,
etc.

With some lexers the order in which tokens are defined is significant.  The
`Lexer` class was designed to function independently of the order in which
tokens are defined.  This allows token definitions to be either put in one
place or spread around in the code in any order, however the programmer wants
to organize things.  It makes priorities explicit rather than being implicitly
defined by ordering.

A token for a left parenthesis, for example, would be defined like this::

    lexer.def_token("k_lpar", r"lpar")

The string `k_lpar` is a label for the token.  (The use of the string prefix
`k_` is a naming convention for string token labels).  Similarly, an identifier
could be defined like this::

    parser.def_token("k_identifier", r"[a-zA-Z_](?:\w*)", on_ties=-1)

Notice that in the definition of the identifier the keyword argument `on_ties`
is set to -1.  The lexer will by default always match the longest string which
matches some defined regex pattern for a token.  If there is a tie then by
default an exception will be raised.  The `on_ties` value is used to break
ties; strings with the same length are sorted by that value.  The default
`on_ties` value is 0.  Suppose you also wanted a token for the string `mod`,
and defined it as::

    lexer.def_token("k_mod", r"mod")

Since this token has a higher `on_ties` value, it will always take precedence over
the identifier token (even though both match and have the same length).

The lexer assumes sentinel begin-token and end-token tokens, which must be
explicitly defined using either the `def_begin_end_tokens` method or else by
setting the `default_begin_end_tokens` flag to `True` when initializing the
lexer.  The begin-token is never explicitly returned.  After the call to
`set_text`, before any calls to `next`, the begin-token is the current token
`lexer.token`.  After the end of the text the `next` method explicitly returns
one end-token.  Calling `next` on that end-token raises `StopIteration` and
halts the lexing of the currently-set text.  All peeks beyond the end of the
text are reported as end-tokens.

Using the lexer
---------------

This is a simple example of using the lexer.  Notice that multiple token definitions
can be combined using the `def_multi_tokens` method.::

    lex = Lexer()

    lex.def_begin_end_tokens("begin", "end")
    lex.def_token("space", r"[ \\t]+", ignore=True) # note + NOT *
    lex.def_token("newline", r"[\\n\\f\\r\\v]+", ignore=True) # note + NOT *
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
    <end,None>

Notice that the end-token is actually returned, but the begin-token is not.

User-accessible attributes of tokens
------------------------------------

The tokens returned by the lexer are instances of a subclass of the class
`TokenNode` (named that since the parser combines them into the nodes of a
parse tree).  The subclasses themselves represent the general kind of token,
for example if `k_identifier` was defined as a token label then a particular
subclass of `TokenNode` would be created to represent identifiers in general.
The particular instances of identifiers, found in the lexed text with their
actual string values, are represented by instances of the general class for
identifiers.

For a token named `t`, these attributes are available:

* `t.token_label` -- the string label of the token (which was defined with it)
* `t.value` -- the string value for the token, found in the lexed text
* `t.ignored_before` -- a list of all the tokens ignored immediately before this one
* `t.is_first` -- true when this is the first token in the text, false otherwise
* `t.parent` -- can be set to the parent in a tree; set by the lexer to `None`
* `t.children` -- can be set to a list of children; set by the lexer to `[]`
* `original_matched_string` -- the original text that was consumed for this token

TODO, list other methods, too.

User-accessible methods and attributes of `Lexer`
-------------------------------------------------

There are many utility methods of the lexer that users can call.  Only the main
ones are listed here.  See the full documentation below.

General methods:

* `next` -- return the next token
* `peek` -- peek at the next token without consuming it
* `go_back` -- go back in the text stream by some number of tokens

Some boolean-valued informational methods:

* `is_first` -- true only if the token passed in is the first token
* `text_is_set` -- true only when text is currently set for scanning

Other attributes:

* `token` -- the current token (the most recent one returned by `next`)
* `line_number` -- the line number of the current token
* `char_number` -- the character number of the current character

TODO, list more, why not make some methods of `TokenNode` instead?

Initialization options
----------------------

There are several options that can be set on initialization, including the
level of token lookahead that is supported.

TODO

Code
----

"""

# TODO: why not consider a peek-on-demand lexer, that only peeks when it needs
# to?  Then users get whatever they ask for or really need.  You could still
# set a max_peek value to approximate the old way, with a default of infinity.

# TODO: consider adding a method to return the processed part and the unprocessed
# part of the orginal text.  Shouldn't be too hard when up-to-speed on the Lexer
# workings (just calculate the right slices into self.program).  Slight complication
# with lookahead and how to define it.

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_lexer.py", pytest_args="-v")

import re
import collections
from .shared_settings_and_exceptions import LexerException

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
    attribute `ignored_before_tokens` is a list of all tokens ignored just
    before the lexer got this token.
    
    The attribute `children` is a list of the child nodes, and `parent` is the
    parent.  Indexing a `TokenNode` class also returns the corresponding child 
    node, i.e. `t_node[0]` would be the leftmost child."""
    
    token_label = None # A label for subclasses representing kinds of tokens.
    original_matched_string = "" # Default, for begin- and end-tokens.

    def __init__(self):
        """Initialize the TokenNode."""
        self.ignored_before_tokens = [] # Values ignored by lexer just before.
        self.value = None # The actual parsed text string for the token.
        # TODO: consider defining children in parser code, BUT is general
        # enough that including it here is probably helpful........
        self.children = [] # Args to functions are their children in parse tree.
        self.parent = None # The parent in a tree of nodes.

    def original_text(self):
        """Return the original text that was read in lexing the token, including
        any ignored text."""
        #ignored_strings = [ s.value for s in self.ignored_before_tokens ]
        #joined = "".join(ignored_strings) + self.value
        #assert joined == self.original_matched_string
        return self.original_matched_string

    def ignored_before(self):
        """Return the list of tokens which were ignored just before this token.
        This can be used, for example, to make sure that there is whitespace
        between two tokens which require whitespace between them.  It can also
        be used to find the level of indentation before a token."""
        return self.ignored_before_tokens

    def ignored_before_labels(self):
        """Return the list of token labels of tokens which were ignored just
        before this token."""
        return [t.token_label for t in self.ignored_before_tokens]

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
        corresponding AST node.  Note that `ast_label` is an attribute of all
        `TokenNode` instances in the final tree.  Any other attributes can be
        copied over.  The AST nodes are only assumed to have an append_children
        method which appends a child AST node."""
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
    def tree_repr(self, indent=""):
        """Token representation as the root of a parse subtree, with formatting."""
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
    `token_label`.  This function is called from the `create_token_subclass`
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

    class TokenSubclass(TokenNode):
        def __init__(self, value):
            super(TokenSubclass, self).__init__() # Call base class __init__.
            self.value = value # Passed in for instances by the Lexer token generator.

    return TokenSubclass

#
# Token subclass token table
#

class TokenTable(object):
    """A symbol table holding subclasses of the `TokenNode` class for each token label
    defined in a `Lexer` instance.  Each `Lexer` instance contains an instance of this
    class to save the subclasses for the kinds of tokens which have been defined for
    it."""
    def __init__(self, token_subclass_factory_fun=basic_token_subclass_factory):
        """Initialize the token table.  The parameter `token_subclass_factory_fun`
        can be passed a function to be used to generate token subclasses,
        taking a token label as an argument.  The default is
        `basic_token_subclass_factory`."""
        self.token_subclassing_fun = token_subclass_factory_fun
        self.token_subclass_dict = {}

        # These three lists below are kept in the same order so the same index
        # will correctly index into them.  There is one entry for each token,
        # in the same order as they were defined.
        self.token_labels = [] # The list of token_labels.
        self.compiled_regexes = [] # The compiled regexes for recognizing tokens.
        self.on_ties = [] # List of int values for breaking equal-length match ties.
        self.ignore_tokens = set() # The set of tokens to ignore.
        self.lex = None # The lexer currently associated with this token table.
        self.begin_token_label = None
        self.begin_token_subclass = None
        self.end_token_label = None
        self.end_token_subclass = None

    def has_key(self, token_label):
        """Test whether a token subclass for `token_label` has been stored."""
        return token_label in self.token_subclass_dict

    def get_token_subclass(self, token_label):
        """Look up the subclasses of base class `TokenNode` corresponding to
        `token_label` in the token table and return it.  Raises a
        `LexerException` if no subclass is found for the token label."""
        if token_label in self.token_subclass_dict:
            TokenSubclass = self.token_subclass_dict[token_label]
        else: raise LexerException("No token with label '{0}' is in the token table."
                                   .format(token_label))
        return TokenSubclass

    def create_token_subclass(self, token_label, store_in_dict=True):
        """Create a subclass for tokens with label `token_label` and store it
        in the token table.  Return the new subclass.  Raises a `LexerException`
        if a subclass for `token_label` has already been created.  If
        `store_in_dict` is `False` then the token is not stored."""
        if token_label in self.token_subclass_dict:
            raise LexerException("In `create_token_subclass`, already created the"
                    " token subclass for token_label '{0}'.".format(token_label))
        # Create a new token subclass for token_label and add some attributes.
        TokenSubclass = self.token_subclassing_fun()
        TokenSubclass.token_label = token_label
        TokenSubclass.__name__ = "token_subclass-" + token_label # For debugging.
        # Store the newly-created subclass in the token_dict.
        if store_in_dict: self.token_subclass_dict[token_label] = TokenSubclass
        return TokenSubclass

    def undef_token_subclass(self, token_label):
        """Un-define the token with label token_label.  The `TokenNode` subclass
        previously associated with that label is removed from the dictionary."""
        try:
            del self.token_subclass_dict[token_label]
        except KeyError:
            return # Not saved in dict, ignore.

    def is_defined_token_label(self, token):
        """Return true if `token` is currently defined as a token label."""
        return token in self.token_labels

    def undef_token(self, token_label):
        """Undefine the token corresponding to `token_label`."""
        # Remove from the list of defined tokens and from the token table.
        self.undef_token_subclass(token_label)
        self.ignore_tokens.discard(token_label)
        try:
            tok_index = self.token_labels.index(token_label)
        except ValueError:
            return
        del self.token_labels[tok_index]
        del self.compiled_regexes[tok_index]
        del self.on_ties[tok_index]

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False):
        """Define a token and the regex to recognize it.
        
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
        
        Returns the new token subclass."""
        if self.is_defined_token_label(token_label):
            raise LexerException("A token with label '{0}' is already defined.  It "
            "must be undefined before it can be redefined.".format(token_label))
        if regex_string is not None:
            self._insert_pattern(regex_string)
            self.token_labels.append(token_label)
            self.on_ties.append(on_ties)
            if ignore:
                self.ignore_tokens.add(token_label)
        # Initialize with a bare-bones, default token_subclass.
        tok = self.create_token_subclass(token_label)
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

    def _insert_pattern(self, regex_string):
        """Insert the pattern in the list of regex patterns, after compiling it."""
        # TODO prepare for using trie for simple patterns
        # Note negative lookbehind assertion (?<!\\) for escape before
        # the strings which start Python regex special chars.
        # TODO move string below up to global space after testing.
        non_simple_regex_contains = \
                r"""(
                        ( (?<!\\)[.^$*+?{[|(] )+ # Start of special char.
                    |   ( [\\][ABdDsSwWZ] )+     # Python regex escape.
                    )"""
        compiled_non_simple_regex_contains = re.compile(
                                  non_simple_regex_contains, re.VERBOSE|re.UNICODE)
        def is_simple_pattern(regex_string):
            # TODO more complicated: could be single-char in brackets!
            # https://docs.python.org/2.0/ref/strings.html
            match_object = compiled_non_simple_regex_contains.search(regex_string)
            #matched_string = regex_string[match_object.start():match_object.end()]
            #print(" substring", matched_string)
            return not bool(match_object)

        #if is_simple_pattern(regex_string):
        #    print("simple pattern", regex_string)
        #else:
        #    print("non-simple pattern", regex_string)

        # below is actual code
        compiled_regex = re.compile(regex_string, re.VERBOSE|re.MULTILINE|re.UNICODE)
        self.compiled_regexes.append(compiled_regex)

    def _get_matched_prefixes_and_length_info(self, program, unprocessed_slice_indices):
        """A utility routine that does the actual string match on the prefix of
        `self.program`.  Return the list of matching prefixes and a list of
        (length, on_ties) data for ranking them."""
        # Note that Python's finditer finds the *first* match group and stops.
        # They are ordered by the order they occur in the regex.  It finds the
        # longest match of any particular group, but stops when it finds a
        # match of some group.  Instead of using that, this code loops over all
        # the separate patterns to find the overall longest, breaking ties with
        # on_ties values.
        matching_prefixes_list = [] # All the prefix strings that match some token.
        len_and_on_ties_list = [] # Ordered like matching_prefixes_list (len, on_ties)
        for count, patt in enumerate(self.compiled_regexes):
            match_object = patt.match(program, 
                                      unprocessed_slice_indices[0],
                                      unprocessed_slice_indices[1])
            if match_object: 
                matched_string = program[
                                 match_object.start():match_object.end()]
                matching_prefixes_list.append(matched_string)
                # Save info to compare matches by length, break ties if necessary.
                len_on_ties_tuple = (len(matched_string), self.on_ties[count])
                len_and_on_ties_list.append(len_on_ties_tuple)
            else: # Match returns None if nothing matches, not a MatchObject.
                matching_prefixes_list.append("")
                len_and_on_ties_list.append((0,self.on_ties[count]))
        return matching_prefixes_list, len_and_on_ties_list

    def _find_winning_token_label_and_value(self, program, unprocessed_slice_indices,
                                 matching_prefixes_list, len_and_on_ties_list,
                                 ERROR_MSG_TEXT_SNIPPET_SIZE):
        """Find the `(len, on_ties)` tuple in `len_and_on_ties_list` which is
        longest and wins tie breaking.  Return the token label and value of the
        matching prefix.  The list arguments should be in correspondence with
        the `self.token_labels` list."""
        # Note that tuple comparisons give the correct max value.
        winning_tuple = max(len_and_on_ties_list)
        if winning_tuple[0] == 0:
            raise LexerException("No matches in Lexer, unknown token at "
                    "the start of this unprocessed text:\n{0}"
                    .format(program[unprocessed_slice_indices[0]
                            :unprocessed_slice_indices[0] +
                                ERROR_MSG_TEXT_SNIPPET_SIZE]))

        # We know the winning tuple's value, now see if it is unique.
        winning_indices = []
        for i in range(len(len_and_on_ties_list)):
            if len_and_on_ties_list[i] == winning_tuple:
                winning_indices.append(i)

        if len(winning_indices) > 1: # Still have a tie, raise an exception.
            win_labels = [ self.token_labels[i] for i in winning_indices ]
            raise LexerException("There were multiple token-pattern matches"
                    " with the same length, found in Lexer.  Set the on_ties"
                    " keyword arguments to break ties.  The possible token "
                    " types are: {0}\nAmbiguity at the start of this "
                    " unprocessed text:\n{1}".format(win_labels,
                        program[unprocessed_slice_indices[0]
                            :unprocessed_slice_indices[0] +
                                ERROR_MSG_TEXT_SNIPPET_SIZE]))

        # Got unique winner; use its index to get corresponding winning_index.
        winning_index = winning_indices[0]
        label = self.token_labels[winning_index]
        value = matching_prefixes_list[winning_index]
        return label, value


#
# Lexer
#

class GenTokenState:
    """The state of the token_generator program execution."""
    ordinary = 1
    end = 2
    uninitialized = 3

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

    def __init__(self, token_table=None, num_lookahead_tokens=2,
                 max_go_back_tokens=None, default_begin_end_tokens=False):
        """Initialize the Lexer.  Optional arguments set the
        `TokenTable` to be used (default creates a new one), the
        number of lookahead tokens (default is two), or the maximum number of
        tokens that the `go_back` method can accept (default is unlimited).
        If `default_begin_end_tokens` is true then begin- and end-tokens will
        be defined using the default token labels.  By default, though, the user
        must call the `def_begin_end_tokens` method to define the begin and
        end tokens (using whatever labels are desired)."""
        self.text_is_set = False

        if token_table is None:
            token_table = TokenTable()
        self.set_token_table(token_table)

        # TODO: Consider integrating the lookahead tokens and the previous
        # tokens into a single buffer.  Then the lookahead is just a slice of
        # that buffer.  This allows easy push_back operations, at least.
        # Obviously next() would have to be modified to know when to use
        # generate_token or not.  Would be nice to have a push_back in addition
        # to a go_back.  This would also work with the peek-on-demand
        # enhancement idea.  Abstracting out the current buffer first would
        # probably make the change/refactoring easier.  Would also make it easier
        # to have different views of the lexer easier, such as if it were one back.
        # TODO: these properties should maybe be properties of the token_table,
        # not the lexer anymore.  Buffer is re-scanned, anyway, and lookahead level
        # is part of language... currently must use max value for all parsers/lexers.
        self.NUM_LOOKAHEAD_TOKENS = num_lookahead_tokens
        self.MAX_TOKEN_BUFFER_SIZE = self.NUM_LOOKAHEAD_TOKENS + 1
        self.token_buffer = collections.deque(maxlen=self.MAX_TOKEN_BUFFER_SIZE)

        self.MAX_GO_BACK_TOKENS = max_go_back_tokens
        if self.MAX_GO_BACK_TOKENS: 
            self.PREV_TOKEN_BUF_SIZE = (self.MAX_GO_BACK_TOKENS 
                                        + self.NUM_LOOKAHEAD_TOKENS + 2)
        else:
            self.PREV_TOKEN_BUF_SIZE = None # None gives unlimited number.
        self.previous_tokens = collections.deque(maxlen=self.PREV_TOKEN_BUF_SIZE)

        self.linenumber = 1
        self.charnumber = 1

        self.token_generator_state = GenTokenState.uninitialized

        if default_begin_end_tokens:
            self.def_begin_end_tokens(self.DEFAULT_BEGIN, self.DEFAULT_END)

    def set_token_table(self, token_table, go_back=0):
        """Sets the current `TokenTable` instance for the lexer to
        `token_table`.  This is called on initialization, but can also be
        called at any time.  If text is being scanned at the time it flushes,
        re-scans, and refills the lookahead buffer (using `go_back`, with
        the argument given by `go_back`).
        
        When set with this method the token table is always given the attribute
        `lex`, which points to the lexer instance this method was called from.
        This attribute is used by tokens (which know their fixed symbol table)
        so they can find the current lexer (to call `next`, etc.)"""
        self.token_table = token_table
        self.token_table.lex = self
        if self.text_is_set:
            self.go_back(0)

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
        self.previous_tokens.clear()
        self.ignored_before_curr = [] # Tokens ignored just before current one.

        # Reset line, character, and token counts.  All counts include the buffer.
        if reset_linenumber:
            self.linenumber = 1
        if reset_charnumber:
            self.charnumber = 1
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
        # Put a begin-token sentinel in self.previous_tokens if it is empty.
        begin_tok = self.token_table.begin_token_subclass(None)
        if not self.previous_tokens:
            self.previous_tokens.append(begin_tok)

        # Set up the buffer.
        tb = self.token_buffer
        tb.clear()
        tb.append(begin_tok) # This will be popped off on first next().
        for i in range(self.NUM_LOOKAHEAD_TOKENS): # Fill with lookaheads.
            new_token = self.token_generator() # Will generate all end-tokens at end.
            tb.append(new_token)
        self.token = self.token_buffer[0]
        assert tb[0].is_begin_token() # debug check, remove later

    #
    # Next and peek related methods
    #

    def next(self, num=1):
        """Return the next token, consuming from the token stream.  Also sets
        `self.token` to the return value.  Returns one end-token and raises
        `StopIteration` on a `next` after that end-token.  If `num` is greater
        than one a list of the tokens is returned (this list is cut short if
        the first end-token is encountered, and so will never generate
        `StopIteration`).  This method basically just adds buffering on top of
        the lower-level routine `token_generator`."""
        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's next method when no text is set.")

        if self.already_returned_end_token:
            self.text_is_set = False
            raise StopIteration

        # Handle num > 1 case with recursion.
        if num > 1:
            ret_list = []
            for i in range(num):
                if not self.token.is_end_token():
                    ret_list.append(self.next())
                else: break
            return ret_list

        # Handle ordinary case.
        tb = self.token_buffer
        tb.popleft()
        if tb[0].is_end_token():
            self.already_returned_end_token = True
        tb.append(self.token_generator())
        self.token = tb[0]
        return self.token
    __next__ = next # For Python 3.
    
    def __iter__(self): return self # Class provides its own __next__ method.

    def peek(self, num_toks=1):
        """Peek ahead in the token stream without consuming any tokens.  Note
        that the argument is the actual number of tokens ahead to peek.  I.e.,
        the indexing starts at 1. (Peeking 0 is considered to mean peek at the
        current token.)  Peeking beyond the end of the buffer raises
        `BufferIndexError`, a subclass of `IndexError`.  A peek within the
        buffer size is always valid, and returns an end-token for all peeks
        from the first end-token and beyond."""
        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's peek method when no text is set.")
        try:
            retval = self.token_buffer[num_toks]
        except IndexError:
            raise BufferIndexError
        return retval

    def go_back(self, num_toks=1):
        """This method allows the lexer to go back in time by `num_toks`
        tokens.  Going back one with `go_back(1)` or just `go_back()` results
        in the current token being set to a re-scanned version of the previous
        token.  The text being parsed is restored to the state before those
        `num_toks` previous tokens were scanned, and the farthest one back is
        immediately re-scanned.  Lookahead tokens in the buffer are also
        re-scanned.  This operation is different from the usual pushback
        operations because the program text is re-scanned, rather than simply
        backing up to already-scanned tokens.
        
        Values of `num_toks` less than one apply to the current token and
        loohahead tokens.  Calling `go_back(0)` re-scans the current token and
        all tokens in the lookahead buffer; `go_back(-1)` re-scans only the
        tokens in the buffer ahead of the current token.  Values greater than
        one go farther back in the token stream.  Attempts to go back before
        the beginning of the program text go back to the beginning and stop
        there.
        
        This method returns the current token after any re-scanning.

        Note that this kind of backing up is different from the usual
        `push_back` methods of lexers.  Those only push back parsed tokens, and
        do not recreate the original text and re-scan it.  Re-scanning can be
        necessary when token definitions themselves change dynamically, such as
        by semantic actions.  For example, a declaration of the string "my_fun"
        as a variable might dynamically add a token for that new variable,
        which would then stop it from matching a general identifier with a
        lower on_ties value (set to, say, -1).  This kind of thing is also
        needed when swapping token tables, such as in parsing a sublanguage
        with a different parser.  Since the sublanguage has a different
        collection of tokens the lookahead buffer must be re-scanned based on
        those tokens."""
        if not self.text_is_set:
            raise LexerException(
                    "Attempt to call lexer's go_back method when no text is set.")

        #def print_debug(msg=""):
        #    print(msg, "   token_buffer:", self.token_buffer, 
        #            "\n   previous_tokens:", self.previous_tokens,
        #            "\n   prog_unprocessed:", self.prog_unprocessed,
        #            "\n   linenumber, charnumber:", self.linenumber, self.charnumber)
        if self.token_generator_state == GenTokenState.uninitialized:
            raise LexerException("The token generator has not been initialized "
                  "or has reached `StopIteration` by reading past the end-token.")
        if self.MAX_GO_BACK_TOKENS and num_toks > self.MAX_GO_BACK_TOKENS:
            raise LexerException("Attempt to go back {0} tokens when `MAX_GO_BACK_LEVELS`"
                    " is set to {1}.".format(num_toks, self.MAX_GO_BACK_TOKENS))

        num_non_ends_in_buf = len([True for t in self.token_buffer
                                                 if not t.is_end_token()])
        n = num_toks + num_non_ends_in_buf

        # Pop the tokens from self.previous_tokens, resetting self.prog_unprocessed.
        peek_token_is_first = False
        current_token_is_first = False
        i = 0
        while True:
            i += 1
            if i > n: break
            popped = self.previous_tokens.pop()
            if popped.is_begin_token():
                peek_token_is_first = True
                break
            if popped.is_end_token():
                i -=  1 # end-tokens aren't actually read from the token stream.
                continue
            if popped.ignored_before():
                (self.linenumber,
                        self.charnumber) = popped.ignored_before()[0].line_and_char
            else:
                self.linenumber, self.charnumber = popped.line_and_char
            self.non_ignored_token_count -= 1
            self.all_token_count -= (1 + len(popped.ignored_before()))
            self.prog_unprocessed[0] -= len(popped.original_matched_string)

            if self.previous_tokens[-1].is_begin_token():
                current_token_is_first = True

        # Re-scan the necessary tokens in the token buffer.
        if peek_token_is_first:
            self._initialize_token_buffer()
        else:
            for i in range(max(-num_toks, 0), len(self.token_buffer)):
                self.token_buffer[i] = self.token_generator()

        # Reset some state variables.
        self.token = self.token_buffer[0]
        if peek_token_is_first:
            self.peek().is_first = True
            self._returned_first_token = False
            self._curr_token_is_first = False
        elif current_token_is_first:
            self.token.is_first = True
            self._returned_first_token = True
            self._curr_token_is_first = True
        if self.token.is_end_token():
            self.already_returned_end_token = True
        else:
            self.already_returned_end_token = False
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
        return self.token.ignored_before_tokens

    def curr_token_is_end(self):
        """True if `self.token` (the last one returned by the `next` method) is
        the end-token."""
        return self.token.is_end_token()

    def is_defined_token_label(self, token_label):
        """Return true if `token` is currently defined as a token label."""
        # TODO: maybe should be a version in token_table that is called from here.
        return token_label in self.token_table.token_labels

    def last_n_tokens_original_text(self, n):
        """Returns the original text parsed by the last `n` tokens (back from
        an including the current token).  This routine is mainly used to make
        error messages more helpful.  It uses the token attribute
        `original_matched_string` and the lexer attribute `previous_tokens`
        (which must be large enough for `n`)."""
        # TODO: clean up and debug, still flaky... also better document the stuff it
        # uses in the doc section.  Useful fun, though.  Could also print
        # line numbers and stuff....
        prev_tokens = [self.previous_tokens[i] for i in range(-n-1, 0)]
        string_list = [s.original_matched_string for s in prev_tokens]
        full_string = "".join(string_list)
        return full_string

    #
    # Methods to define and undefine tokens
    #

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False):
        """A convenience method to define a token. It calls the corresponding
        `def_token` method of the current `TokenTable` instance associated with
        the lexer, and does nothing else."""
        new_subclass = self.token_table.def_token(token_label, regex_string,
                       on_ties=on_ties, ignore=ignore)
        return new_subclass

    def def_ignored_token(self, token_label, regex_string, on_ties=0):
        """A convenience function to define an ignored token without setting
        `ignore=True`.  This just calls `def_token` with the value set."""
        return self.def_token(token_label, regex_string, on_ties=on_ties, ignore=True)

    def def_multi_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once.  Each element
        of the passed-in list should be a tuple containing the arguments to the
        ordinary `def_token` method.  Called in the same order as the list.  Returns
        a tuple of the defined tokens."""
        return multi_funcall(self.def_token, tuple_list)

    def def_multi_ignored_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once with
        `ignore=True` set.  Each element of the passed-in list should be a tuple
        containing the arguments to the ordinary `def_token` method.  Called in
        the same order as the list.  Returns a tuple of the defined tokens."""
        return multi_funcall(self.def_ignored_token, tuple_list)

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

    def undef_token(self, token_label):
        """A convenience function to call the corresponding `undef_token` of
        the current `TokenTable` instance associated with the Lexer."""
        self.token_table.undef_token(token_label)

    #
    # Lower-level routine for token generation
    #

    def token_generator(self):
        """This routine generates tokens from the program text in the attribute
        `self.program`.  It does not modify the program itself, but keeps slice
        indices in a list `self.prog_unprocessed` indexing the unprocessed
        part.  That slice can be externally modified (the `go_back` routine
        does this).
        
        This is a lower-level function used by `next` to do the real work.  All
        the token subclasses should have been defined and stored in the the
        `TokenTable`.  Regexes defined for tokens are repeatedly matched at the
        beinning of the string `program`.  When a winning_index is found it is
        stripped off the beginning of the unprocessed slice of `program` and
        the generator waits for the next call.  For each winning_index the
        token subclass is looked up in the `TokenTable` object and an instance
        of that subclass is yielded to represent the token.  Every token
        processed is represented by a unique new instance of the appropriate
        subclass of `TokenNode`.
        
        This generator has two states which can be set class-globally to alter
        the state of the generator.  The states are `GenTokenState.ordinary`
        for ordinary scanning execution, and `GenTokenState.end` when all the
        tokens have been read and the generator keeps returning nothing but end
        tokens.  The end state is normally entered when the program text
        becomes empty.  If that variable is later is set to have text again the
        state switches back to ordinary."""
        ignored_before_labels = []
        ignored_before_tokens = []
        original_matched_string = ""

        while True:
            self._curr_token_is_first = not self._returned_first_token
            self._returned_first_token = True

            if self.prog_unprocessed[0] == self.prog_unprocessed[1]:
                self.token_generator_state = GenTokenState.end
            else:
                self.token_generator_state = GenTokenState.ordinary

            # =======================================================================
            # === Ordinary execution state ==========================================
            # =======================================================================
            if self.token_generator_state == GenTokenState.ordinary:
                # Get the matching prefixes and length-ranking information.
                matching_prefixes_list, len_and_on_ties_list = \
                           self.token_table._get_matched_prefixes_and_length_info(
                                   self.program, self.prog_unprocessed)

                # Find the label and value of the matching prefix which is longest
                # (with ties broken by the on_ties values).
                label, value = self.token_table._find_winning_token_label_and_value(
                                    self.program, self.prog_unprocessed,
                                    matching_prefixes_list, len_and_on_ties_list,
                                    self.ERROR_MSG_TEXT_SNIPPET_SIZE)

                # Remove matched prefix of the self.prog_unprocessed argument after
                # saving the matched prefix string.
                original_matched_string += self.program[self.prog_unprocessed[0]
                                                :self.prog_unprocessed[0]+len(value)]
                self.prog_unprocessed[0] += len(value)

                # Look up the class to represent the winning_index.
                try:
                    token_subclass_for_label = self.token_table.get_token_subclass(label)
                except LexerException:
                    raise LexerException("Undefined key in token table for "
                                         "label '{0}'.".format(label))

                # Make an instance of the class to return (or at least to save
                # in the token's ignored_before if ignored).
                tci = token_subclass_for_label(value)
                self.all_token_count += 1

                # Save the line and char counts for the beginning of the token
                # with the token, then update them to the beginning of the next
                # token.  The Lexer class versions always hold the beginning of
                # the next token to be read (into the last buffer slot, not as
                # the current token); the versions stored with the tokens
                # themselves hold the beginning of text when this routine
                # scanned that token (including any ignored text before it).
                tci.line_and_char = (self.linenumber, self.charnumber)
                num_newlines = value.count("\n")
                self.linenumber += num_newlines
                if num_newlines == 0:
                    self.charnumber += len(value)
                else:
                    last_newline = value.rfind("\n")
                    self.charnumber = len(original_matched_strings) - (last_newline + 1) + 1
                
                # ------------------------------------------------------------------
                # Go to the top of the loop and get another if the token is ignored.
                # ------------------------------------------------------------------
                if label in self.token_table.ignore_tokens:
                    ignored_before_labels.append(label)
                    ignored_before_tokens.append(tci)
                    continue

                self.previous_tokens.append(tci)
                self.non_ignored_token_count += 1

            # =======================================================================
            # === Return only end-tokens state ======================================
            # =======================================================================
            elif self.token_generator_state == GenTokenState.end:
                token_subclass_for_end = self.token_table.get_token_subclass(
                                                  self.token_table.end_token_label)
                tci = token_subclass_for_end(None)
                tci.line_and_char = (self.linenumber, self.charnumber)
                # Only save a single end-token on previous_tokens list.
                if (self.previous_tokens and not 
                                self.previous_tokens[-1].is_end_token()):
                    self.previous_tokens.append(tci)

            # Got a token to return.  Set some attributes and return it.
            tci.original_matched_string = original_matched_string
            tci.ignored_before_tokens = tuple(ignored_before_tokens)
            tci.all_token_count = self.all_token_count
            tci.non_ignored_token_count = self.non_ignored_token_count
            tci.is_first = self._curr_token_is_first

            return tci


def multi_funcall(function, tuple_list, exceptionToRaise=LexerException):
   """A convenience function that takes a function (or method) and a list of tuples
   and calls `function` with the values in those tuple as arguments."""
   retval_list = []
   for t in tuple_list:
       try:
           retval_list.append(function(*t))
       except TypeError:
           raise exceptionToRaise(
                   "Bad multi-definition of {0}: Omitted required arguments."
                   "\nError on this tuple: {1}".format(function.__name__, t))
   return tuple(retval_list)

#
# Exceptions
#

class BufferIndexError(LexerException):
    """Raised on attempts to read past the beginning or the end of the buffer
    (such as in `peek` methods)."""
    pass


