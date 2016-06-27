# -*- coding: utf-8 -*-
"""

A general lexer/scanner module.

"""

from __future__ import print_function, division, absolute_import
import re
import sys
import types
import collections

#
# TokenNode
#

class TokenNode(object):
    """The base class for token objects."""
    token_label = None # A label for subclasses representing kinds of tokens.

    def __init__(self):
        """Initialize the TokenNode."""
        self.children = [] # Args to functions are their children in parse tree.
        self.parent = None # The parent in a tree of nodes.
        self.ignored_before_tokens = [] # Values ignored by lexer just before.
        self.evaluate = None # A method to evaluate some nodes, added dynamically.
        self.val_type = None # The type of the node's value, after processing.
        self.allowed_types = None # Set in recursion to ensure return type matches.
        self.value = None # The actual parsed text string for the token.

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
    # Various representations.
    #

    def value_repr(self):
        return str(self.value)
    def label_repr(self):
        return str(self.token_label)
    def summary_repr(self):
        return "<" + str(self.token_label) + "," + str(self.value) + ">"
    def tree_repr(self, indent=""):
        string = indent + self.summary_repr() + "\n"
        for c in self.children:
            string += c.tree_repr(indent=indent+" "*4)
        return string
    def string_repr(self, only_vals=False, only_labels=False):
        string = self.summary_repr()
        if only_vals: string = self.value_repr()
        if only_labels: string = self.label_repr()
        if self.children:
            string += "("
            string += ",".join(c.string_repr() for c in self.children)
            string += ")"
        return string
    def old_repr(self):
        """This old representation is kept *only* because it is used in some tests."""
        if self.token_label == "k_number":
            return "[literal {0}]".format(self.value)
        if self.token_label == "k_lpar":
            if self.children: return "[k_lpar {0} k_rpar]".format(self.children[0].old_repr())
            else: return "[literal k_lpar]"
        else:
            str_val = "[" + str(self.value)
            for a in self.children: str_val += " " + a.old_repr()
            str_val += "]"
            return str_val
    __repr__ = string_repr


def create_token_subclass():

    """Create and return a new token subclass representing tokens with label
    `token_label`.  This is called from the `create_token_subclass` method  of
    `TokenSubclassSymbolTable` when it needs to create a new one to start with.
    This function should not be called directly, since attributes (like the
    token label and a new subclass name) need to be added to the generated
    subclass.
    
    This function is the default argument to the `token_subclassing_fun`
    keyword argument of the initializer for `TokenSubclassSymbolTable`.  Users
    can define their own such function in order to add methods particular to
    their application (the PrattParser class does this).

    Using a separate subclass for each token label allows for attributes and
    methods specific to a kind of token to be pasted onto the class itself
    without conflicts.  For example, the PrattParser subclass adds nud and led
    methods which are specific to a given token label."""

    class TokenSubclass(TokenNode):
        def __init__(self, value):
            super(TokenSubclass, self).__init__() # Call base class __init__.
            self.value = value # Set by Lexer token generator for instances.

    return TokenSubclass

#
# Token subclass symbol table
#

class TokenSubclassSymbolTable(object):
    """This is mainly used by the `Lexer` class.  A symbol table holding
    subclasses of the `TokenNode` class for each token label defined in a `Lexer`
    instance."""
    def __init__(self, token_subclassing_fun=create_token_subclass):
        """Initialize the symbol table.  The parameter `token_subclassing_fun`
        can be passed a function to be used to generate token subclasses,
        taking a token label as an argument.  The default is
        `create_token_subclass`."""
        self.token_subclassing_fun = token_subclassing_fun
        self.token_subclass_dict = {}

    def has_key(self, token_label):
        """Test whether a token subclass for `token_label` has been stored."""
        return token_label in self.token_subclass_dict

    def get_token_subclass(self, token_label):
        """Look up the subclasses of base class `TokenNode` corresponding to
        `token_label` in the symbol table and return it.  Raises a
        `LexerException` if no subclass is found for the token label."""
        if token_label in self.token_subclass_dict:
            TokenSubclass = self.token_subclass_dict[token_label]
        else: raise LexerException("No token with label {0} is in the symbol table."
                                   .format(token_label))
        return TokenSubclass

    def create_token_subclass(self, token_label, store_in_dict=True):
        """Create a subclass for tokens with label `token_label` and store it
        in the symbol table.  Return the new subclass.  Raises a `LexerException`
        if a subclass for `token_label` has already been created.  If
        `store_in_dict` is `False` then the token is not stored."""
        if token_label in self.token_subclass_dict:
            raise LexerException("In create_token_subclass, already created the"
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
        try: del self.token_subclass_dict[token_label]
        except KeyError: return # Not saved in dict, ignore.
    
#
# Lexer
#

class GenTokenState:
    """The state of the token_generator program execution."""
    ordinary = 1
    end = 2
    uninitialized = 3

class Lexer(object):
    """Scans the program and returns the tokens, represented by instances of
    `TokenNode` subclass instances. There is one subclass for each kind of
    token, i.e., for each token label.  These subclasses themselves are assumed
    to have been created before any scanning operation which can return an
    instance, via the `def_token` method. 
    
    Token strings are assumed to have both a begin and an end token, defined
    via the `def_begin_end_tokens` method.  These token types act as
    sentinels at the beginning and end of the token stream.  Exactly one end
    token will be returned by `next`; any further calls to `next` raise
    `StopIteration`.
    
    The scanning is independent of the order in which tokens are defined.  The
    longest match over all token patterns will always be the one selected.  In
    case of ties the `on_ties` value (passed to `def_token`) is used to
    break it.  If that fails a `LexerException` is raised.
    
    If no symbol table is passed into `__init__` the `Lexer` will create its
    own empty one."""

    ERROR_MSG_TEXT_SNIPPET_SIZE = 40 # Number of characters to show for context.
    DEFAULT_BEGIN = "k_begin" # Default label for begin token.
    DEFAULT_END = "k_end" # Default label for end token.

    #
    # Initialization methods
    #

    def __init__(self, symbol_table=None, num_lookahead_tokens=2,
                 max_go_back_tokens=None, default_begin_end_tokens=False):
        """Initialize the Lexer.  Optional arguments set the
        `TokenSubclassSymbolTable` to be used (default creates a new one), the
        number of lookahead tokens (default is two), or the maximum number of
        tokens that the `go_back` method can accept (default is unlimited).
        If `default_begin_end_tokens` is true then begin and end tokens will
        be defined using the default token labels.  By default, though, the user
        must call the `def_begin_end_tokens` method to define the begin and
        end tokens (using whatever labels are desired)."""
        if symbol_table is None: self.symbol_table = TokenSubclassSymbolTable()
        else: self.symbol_table = symbol_table
        self.ignore_tokens = set()

        # These three lists below are kept in the same order so the same index
        # will correctly index into them.  There is one entry for each token,
        # in the same order as they were defined.
        self.token_labels = [] # The list of token_labels.
        self.compiled_regexes = [] # The compiled regexes for recognizing tokens.
        self.on_ties = [] # List of int values for breaking equal-length match ties.

        # Consider integrating the lookahead tokens and the previous tokens
        # into a single buffer.  Then the lookahead is just a slice of that
        # buffer.  This allows easy pushback operations, at least.  Obviously
        # next() would have to be modified to know when to use generate_token
        # or not.
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

        self.reset_linenumber = True # Reset linenumber on each set_text call.
        self.linenumber = 1
        self.reset_charnumber = True # Reset charnumber on each set_text call.
        self.charnumber = 1

        self.token_generator_state = GenTokenState.uninitialized

        if default_begin_end_tokens:
            self.def_begin_end_tokens(self.DEFAULT_BEGIN, self.DEFAULT_END)

        return

    def set_text(self, program):
        # TODO: redefine to take a TextStream.  Be sure to also pass back position
        # info with the returned text so that tokens have have their line/position
        # of origin pasted onto them..... or at least keep track in generating
        # tokens.
        """Users should call this method to pass in the program text (or other
        text) which is to be lexically scanned.  The parameter `program` should
        be a string."""
        if not (self.begin_token_label and self.end_token_label):
            raise LexerException("Begin and end tokens must be defined by calling"
                    " def_begin_end_tokens before set_text can be called.")

        self.already_returned_end_token = False
        self._curr_token_is_first = False # Is curr token first non-ignored in text?
        self._returned_first_token = False
        self.previous_tokens.clear()
        self.ignored_before_curr = [] # Tokens ignored just before current one.

        # Reset line, character, and token counts.  All counts include the buffer.
        if self.reset_linenumber: self.linenumber = 1
        if self.reset_charnumber: self.charnumber = 1
        self.all_token_count = 0 # Count all actual tokens (not begin and end).
        self.non_ignored_token_count = 0 # Count non-ignored actual tokens.

        self.program = program # The program text currently being scanned/lexed.
        # The prog_unprocessed list holds slice indices for the unprocessed part
        # of the program text.  The go_back routine can modify this.
        self.prog_unprocessed = [0, len(self.program)] # The unprocessed slice.
        self.token_generator_state = GenTokenState.ordinary

        # Set up the token buffer.
        self._initialize_token_buffer()
        self.token = self.token_buffer[0] # Last token returned; begin token here.

    def _initialize_token_buffer(self):

        """A utility routine to initialize (fill) the token buffer.  The
        `token_buffer[0]` slot is the current token.  The current token will be
        set to the begin token after this routine runs (since no tokens have
        yet been read with `next`).  Any tokens in the buffer past the first
        end token are also set to end tokens.  The size of the token buffer is
        `self.NUM_LOOKAHEAD_TOKENS` plus one for the current token.  For
        two-token lookahead the buffer deque has the form:
            [<current_token>, <peek1>, <peek2>]
        """
        # Put a begin token sentinel in self.previous_tokens if it is empty.
        begin_tok = self.begin_token_subclass(None)
        if not self.previous_tokens: self.previous_tokens.append(begin_tok)

        # Set up the buffer.
        tb = self.token_buffer
        tb.clear()
        tb.append(begin_tok) # This will be popped off on first next().
        for i in range(self.NUM_LOOKAHEAD_TOKENS): # Fill with lookaheads.
            new_token = self.token_generator() # Will generate all end tokens at end.
            tb.append(new_token)
        self.token = self.token_buffer[0]
        assert tb[0].token_label == self.begin_token_label # debug, remove

    #
    # Next and peek related methods
    #

    def next(self, num=1):
        """Return the next token, consuming from the token stream.  Also sets
        `self.token` to the return value.  Returns one end token and raises
        `StopIteration` on a `next` after that end token.  If `num` is greater
        than one a list of the tokens is returned (this list is cut short if
        the first end token is encountered, and so will never generate
        `StopIteration`).  This method adds buffering on top of the lower-level
        routine `token_generator`."""
        if self.already_returned_end_token: raise StopIteration

        # Handle num > 1 case with recursion.
        if num > 1:
            ret_list = []
            for i in range(num):
                if not self.is_end_token(self.token):
                    ret_list.append(self.next())
                else: break
            return ret_list

        # Handle ordinary case.
        tb = self.token_buffer
        tb.popleft()
        if tb[0].token_label == self.end_token_label:
            self.already_returned_end_token = True
        tb.append(self.token_generator())
        self.token = tb[0]
        return self.token
    __next__ = next # For Python 3.
    
    def __iter__(self): return self # Class provides its own __next__ method.

    def peek(self, num_toks=1):
        """Peek ahead in the token stream without consuming any tokens.  Note
        that the argument is the actual number of tokens ahead to peek.  I.e.,
        the indexing starts at 1. (You can consider 0 to mean to peek at
        the current token, and that also works.)  Peeking beyond the end
        of the buffer raises `BufferIndexError`, a subclass of `IndexError`.
        A peek within the buffer size is always valid, and returns an end
        token for all peeks from the first end token and beyond."""
        try: retval = self.token_buffer[num_toks]
        except IndexError: raise BufferIndexError
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

        This kind of backup method can be necessary when the token definitions
        themselves are dynamically changed, such as by a semantic action.  For
        example, a declaration for the string "my_fun" as a variable might
        dynamically add a token for that new variable, which would then stop it
        from matching a general identifier with an on_ties value set to, say,
        -1."""
        #def print_debug(msg=""):
        #    print(msg, "   token_buffer:", self.token_buffer, 
        #            "\n   previous_tokens:", self.previous_tokens,
        #            "\n   prog_unprocessed:", self.prog_unprocessed,
        #            "\n   linenumber, charnumber:", self.linenumber, self.charnumber)
        if self.token_generator_state == GenTokenState.uninitialized:
            raise LexerException("The token generator has not been initialized "
                  "or has reached StopIteration by reading past the end token.")
        if self.MAX_GO_BACK_TOKENS and num_toks > self.MAX_GO_BACK_TOKENS:
            raise LexerException("Attempt to go back {0} tokens when MAX_GO_BACK_LEVELS"
                    " is set to {1}.".format(num_toks, self.MAX_GO_BACK_TOKENS))

        num_non_ends_in_buf = len([True for t in self.token_buffer
                                        if t.token_label != self.end_token_label])
        n = num_toks + num_non_ends_in_buf

        # Pop the tokens from self.previous_tokens, resetting self.prog_unprocessed.
        peek_token_is_first = False
        current_token_is_first = False
        i = 0
        while True:
            i += 1
            if i > n: break
            popped = self.previous_tokens.pop()
            if popped.token_label == self.begin_token_label:
                peek_token_is_first = True
                break
            if popped.token_label == self.end_token_label:
                i -=  1 # End tokens aren't actually read from the token stream.
                continue
            if popped.ignored_before():
                (self.linenumber,
                        self.charnumber) = popped.ignored_before()[0].line_and_char
            else:
                self.linenumber, self.charnumber = popped.line_and_char
            self.non_ignored_token_count -= 1
            self.all_token_count -= (1 + len(popped.ignored_before()))
            self.prog_unprocessed[0] -= len(popped.original_matched_string)

            if self.is_begin_token(self.previous_tokens[-1]):
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
        if self.is_end_token(self.token):
            self.already_returned_end_token = True
        else:
            self.already_returned_end_token = False
        return self.token

    #
    # Informational methods
    #

    def is_begin_token(self, token):
        """Test whether the token is the end token."""
        return token.token_label == self.begin_token_label

    def curr_token_is_begin(self):
        """True if `self.token` (the last one returned by the `next` method) is
        the begin token."""
        return self.token.token_label == self.begin_token_label

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

    def is_end_token(self, token):
        """Test whether `token` is the end token."""
        return token.token_label == self.end_token_label

    def curr_token_is_end(self):
        """True if `self.token` (the last one returned by the `next` method) is
        the end token."""
        return self.token.token_label == self.end_token_label

    def is_defined_token_label(self, token):
        """Return true if `token` is currently defined as a token label."""
        return token in self.token_labels

    #
    # Methods to define and undefine tokens
    #

    def def_token(self, token_label, regex_string, on_ties=0, ignore=False):
        """Define a token and the regex to recognize it.  The label
        `token_label` is the label for the kind of token.  Setting
        `ignore=True` will cause all such tokens to be ignored (except that
        they will be placed on the `ignored_before` list of the non-ignored
        token that they precede).  In case of ties for the longest match in
        scanning, the integer `on_ties` values are used to break the ties.  If
        any two are still equal an exception will be raised.  Returns the new
        token subclass."""
        if self.is_defined_token_label(token_label):
            raise LexerException("Token {0} is already defined.  It must be undefined"
                                 " before it can be redefined.".format(token_label))
        self._insert_pattern(regex_string)
        self.token_labels.append(token_label)
        self.on_ties.append(on_ties)
        if ignore: self.ignore_tokens.add(token_label)
        # Initialize with a bare-bones, default token_subclass.
        new_subclass = self.symbol_table.create_token_subclass(token_label)
        return new_subclass

    def def_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once.  Each element
        of the passed-in list should be a tuple containing the arguments to the
        ordinary `def_token` method.  Called in the same order as the list."""
        for t in tuple_list:
            self.def_token(*t)

    def def_ignored_tokens(self, tuple_list):
        """A convenience function, to define multiple tokens at once with
        `ignore=True` set.  Each element of the passed-in list should be a tuple
        containing the arguments to the ordinary `def_token` method.  Called in
        the same order as the list."""
        for t in tuple_list:
            t = list(t)
            if len(t) == 3: t.append("True") # Has a tie breaker, no ignore set.
            elif len(t) == 2: t.extend([0, "True"]) # Has no on_ties, set to default.
            self.def_token(*t)

    def undef_token(self, token_label):
        """Undefine the token corresponding to `token_label`."""
        # Remove from the list of defined tokens and the symbol table.
        self.symbol_table.undef_token_subclass(token_label)
        self.ignore_tokens.discard(token_label)
        try: tok_index = self.token_labels.index(token_label)
        except ValueError: return
        del self.token_labels[tok_index]
        del self.compiled_regexes[tok_index]
        del self.on_ties[tok_index]

    def def_begin_end_tokens(self, begin_token_label, end_token_label):
        """Define the sentinel tokens at the beginning and end of the token
        stream.  This method must be called before using the Lexer.  It will
        automatically be called using default token label values unless
        `default_begin_end_tokens` was set false on initialization.  Returns a
        tuple of the new begin and end token subclasses.  These tokens do not
        need to be defined with `def_token` because they are never actually
        scanned in the program text (which would require the regex pattern)."""

        # Define begin token.
        self.begin_token_label = begin_token_label
        self.begin_token_subclass = self.symbol_table.create_token_subclass(
                                                                begin_token_label)
        # Define end token.
        self.end_token_label = end_token_label
        self.end_token_subclass = self.symbol_table.create_token_subclass(
                                                                end_token_label)
        return self.begin_token_subclass, self.end_token_subclass

    def define_unstored_token(self, token_label):
        """Define a token that is not stored in the symbol table dict, and which
        has no regex pattern."""
        new_subclass = self.symbol_table.create_token_subclass(
                                                token_label, store_in_dict=False)
        return new_subclass

    #
    # Lower-level methods related to token generation
    #

    def _insert_pattern(self, regex_string):
        """Insert the pattern in the list of patterns."""
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

    def _get_matched_prefixes_and_length_info(self):
        """A utility routine to do the actual string match on the prefix of
        `self.program`.  Return the list of matching prefixes and a list of
        (length, on_ties) data for ranking them."""
        # Python's finditer finds the *first* match group and stops.  They
        # are ordered by the order they occur in the regex.  It finds the
        # longest match of any particular group, but stops when it finds a
        # match of some group.  Instead of that, this code loops over the
        # separate patterns to find the overall longest, breaking ties with
        # on_ties values. 
        matching_prefixes_list = [] # All the prefix strings that match some token.
        len_and_on_ties_list = [] # Ordered like matching_prefixes_list (len, on_ties)
        for count, patt in enumerate(self.compiled_regexes):
            match_object = patt.match(self.program, 
                                      self.prog_unprocessed[0],
                                      self.prog_unprocessed[1])
            if match_object: 
                matched_string = self.program[
                                 match_object.start():match_object.end()]
                matching_prefixes_list.append(matched_string)
                # Save info to compare matches by length, break ties if necessary.
                len_on_ties_tuple = (len(matched_string), self.on_ties[count])
                len_and_on_ties_list.append(len_on_ties_tuple)
            else: # Match returns None if nothing matches, not a MatchObject.
                matching_prefixes_list.append("")
                len_and_on_ties_list.append((0,self.on_ties[count]))
        return matching_prefixes_list, len_and_on_ties_list

    def _find_winning_token_label_and_value(self, 
                                 matching_prefixes_list, len_and_on_ties_list):
        """Find the `(len, on_ties)` tuple in `len_and_on_ties_list` which is
        longest and wins tie breaking.  Return the token label and value of the
        matching prefix.  The list arguments should be in correspondence with
        the `self.token_labels` list."""
        # Note that tuple comparisons give the correct max value.
        winning_tuple = max(len_and_on_ties_list)
        if winning_tuple[0] == 0:
            raise LexerException("No matches in Lexer, unknown token at "
                    "the start of this unprocessed text:\n{0}"
                    .format(self.program[self.prog_unprocessed[0]
                            :self.prog_unprocessed[0]
                            + self.ERROR_MSG_TEXT_SNIPPET_SIZE]))

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
                        self.program[self.prog_unprocessed[0]
                            :self.prog_unprocessed[0]
                            + self.ERROR_MSG_TEXT_SNIPPET_SIZE]))

        # Got unique winner; use its index to get corresponding winning_index.
        winning_index = winning_indices[0]
        label = self.token_labels[winning_index]
        value = matching_prefixes_list[winning_index]
        return label, value

    def token_generator(self):
        """This routine generates tokens from the program text in
        `self.program`.  It does not modify the program itself, but keeps slice
        indices in a list `self.prog_unprocessed` indexing the unprocessed
        part.  That slice can be externally modified (the `go_back` routine
        does this).
        
        This is a lower-level function used by `next` to do the real work.  All
        the token subclasses should have been defined and stored in the the
        `TokenSubclassSymbolTable`.  Regexes defined for tokens are repeatedly
        matched at the beinning of the string `program`.  When a winning_index
        is found it is stripped off the beginning of the unprocessed slice of
        `program` and the generator waits for the next call.  For each
        winning_index the token subclass is looked up in the
        `TokenSubclassSymbolTable` object and an instance of that subclass is
        yielded to represent the token.  Every token processed is represented
        by a unique new instance of the appropriate subclass of `TokenNode`.
        
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
                                   self._get_matched_prefixes_and_length_info()

                # Find the label and value of the matching prefix which is longest
                # (with ties broken by the on_ties values).
                label, value = self._find_winning_token_label_and_value(
                                    matching_prefixes_list, len_and_on_ties_list)

                # Remove matched prefix of the self.prog_unprocessed argument after
                # saving the matched prefix string.
                original_matched_string += self.program[self.prog_unprocessed[0]
                                                :self.prog_unprocessed[0]+len(value)]
                self.prog_unprocessed[0] += len(value)

                # Look up the class to represent the winning_index.
                try:
                    token_subclass_for_label = self.symbol_table.get_token_subclass(label)
                except LexerException:
                    raise LexerException("Undefined key in symbol table for "
                                         "this label: {0}.".format(label))

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
                if label in self.ignore_tokens:
                    ignored_before_labels.append(label)
                    ignored_before_tokens.append(tci)
                    continue

                self.previous_tokens.append(tci)
                self.non_ignored_token_count += 1

            # =======================================================================
            # === Return only end tokens state ======================================
            # =======================================================================
            elif self.token_generator_state == GenTokenState.end:
                token_subclass_for_end = self.symbol_table.get_token_subclass(
                                                             self.end_token_label)
                tci = token_subclass_for_end(None)
                tci.line_and_char = (self.linenumber, self.charnumber)
                # Only save a single end token on previous_tokens list.
                if (self.previous_tokens and not 
                                self.is_end_token(self.previous_tokens[-1])):
                    self.previous_tokens.append(tci)

            # Got a token to return.  Set some attributes and return it.
            tci.original_matched_string = original_matched_string
            tci.ignored_before_tokens = tuple(ignored_before_tokens)
            tci.all_token_count = self.all_token_count
            tci.non_ignored_token_count = self.non_ignored_token_count
            tci.is_first = self._curr_token_is_first

            return tci

#
# Exceptions
#

class LexerException(Exception):
    pass

class BufferIndexError(IndexError):
    pass

#
# Run tests when invoked as a script.
#

if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("test/test_lexer.py", pytest_args="-v")

