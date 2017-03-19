"""

This module contains a lexer (tokenizer) which uses a `RegexTrieDictScanner`
class to identify tokens.  A `RegexTrieDictScanner` is used to scan for basic
tokens.  First the `TextStream` object is defined as a wrapper for a stream of
text characters from various sources.  Next, a class `RegexTrieDictLexer` is
defined as a very simple lexer drawing characters from a `TextStream`.  Finally,
a class `BufferedRegexTrieDictLexer` is defined using the basic
`RegexTrieDictLexer` but providing many more features such as buffering,
pushback, general whitespace ignoring, etc.  Generally the
`BufferedRegexTrieDictLexer` should be used in applictions, rather than the
lower-level lexer.

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_regex_trie_dict_lexer.py", pytest_args="-v")

import sys
import collections # for named tuple
from .regex_trie_dict import RegexTrieDict
from .regex_trie_dict_scanner import RegexTrieDictScanner, TokenData
from .text_stream import TextStream
#from basic_defs import * # DataTuple namedtuple defined here is needed a few places

# TODO remove DataTuple below and replace with the class instances for the
# various token types.  DataTuple used to just give the kind and then some
# extra info.  It is apparently only used in this module (maybe old parser.py
# indexed an instance of it, maybe not).
#
# The DataTuple namedtuple.
#
# We need a standard convention for passing parse data between modules.  We
# simply use a tuple, where the first element gives the kind of token (i.e.,
# the type, but the term "type" is already used in a different context) and the
# second element gives the data itself.  The data itself will by convention be
# either None or the associated parse-tree node, if there is such a node
# associated with that token type, perhaps without all of its elements yet
# filled-in.
#
# Instead of using a regular tuple a namedtuple called DataTuple is used.  This
# allows the fields to be indexed by names (in addition to numerical indices),
# and so produces more readable and maintainable code.  The two fields are
# "kind" and "data".
DataTuple = collections.namedtuple("DataTuple", ["kind","data"])

#
# The RegexTrieDictLexer class
#

class RegexTrieDictLexer(object):
    """A simple, nonbuffered lexer based on the RegexTrieDict data structure."""

    def __init__(self, text_stream, trie_dict, whitespace=""):
        """The `trie_dict` is assumed to define the tokens and have a stored data
        element with each token which exactly equals the data which should be
        returned when the token is recognized.  Text is taken character by
        character from the text_stream and insertSeqElem is called on a
        RegexTrieDictScanner created from the trie_dict.  When a match is found
        the (string,data) tuple is returned.

        Whitespace is a string of characters to ignore in processing tokens,
        except that they are assumed to always define an endOfQuery in the
        trieTok data structure.  The default whitespace is the empty string
        (which doesn't ignore anything)."""

        self.text_stream = text_stream
        self.trie_dict = trie_dict
        self.trie_tok = RegexTrieDictScanner(self.trie_dict)
        self.trie_tok.reset_seq() # always start fresh at the beginning of TextStream
        self.trie_tok.clear_token_data_deque() # may not always be wanted...
        self.token_data_deque = self.trie_tok.get_token_data_deque()
        self.whitespace = set(whitespace)
        return

    def set_whitespace(whitespace_chars):
        self.whitespace = set(whitespace_chars)
        return

    def __iter__(self):
        """So statements like: for tok in lexObj: ... can be used."""
        return self # implies that the next() method will be used for iterations

    def next(self, ignore_whitespace=True):
        """Get the next token.  The returned token for a string is exactly the
        matchedData element for that string, from the StringDelimitedTrieDict.
        It consists of a tuple of three elements: a boolean for whether there was
        an error, the string itself, and any data item which was stored with the
        string."""
        # TODO the above documentation is quite outdated.
        RAISE_ERROR_ON_BAD_TOKEN = False # maybe make a settable class variable
        while True:
            if len(self.token_data_deque) > 0:
                tok_data = self.token_data_deque.popleft() # process the first token
                if RAISE_ERROR_ON_BAD_TOKEN:
                    if tok_data.validToken == False:
                        raise Exception(
                            "Bad token '"+tok_data.tokenString+"' in token stream.")
                if ignore_whitespace and tok_data.tokenString in self.whitespace:
                    continue
                return tok_data
            else: # len(self.token_data_deque)==0, must insert some chars to gen. matches
                if self.text_stream.end_of_text_stream():
                    self.trie_tok.assert_end_of_seq()
                    if len(self.token_data_deque) == 0:
                        raise StopIteration(
                            "no more tokens in RegexTrieDictLexer for next()")
                else:
                    query_char = self.text_stream.next()
                    query_char_pos = self.text_stream.get_pos_of_last_next()
                    self.trie_tok.add_text_elem(query_char, query_char_pos)
        return

    def end_of_token_stream(self):
        """True if no more tokens.  Whitespace tokens are not ignored."""
        # note that order matters in the "and" on the next line
        if len(self.token_data_deque) == 0 and self.text_stream.end_of_text_stream():
            self.trie_tok.assert_end_of_seq() # see if any possible matches unflushed
            if len(self.token_data_deque) == 0:
                return True
        return False


#
# The BufferedRegexTrieDictLexer class
#


class BufferedRegexTrieDictLexer(object):
    """A buffered lexer built on a TreeLexer to allow for pushback and
    lookahead of tokens.

    The next() method raises a StopIteration when it is out of tokens, so the
    class can be used as an iterator.  The peek() method, however, returns None
    upon trying to peek past the end of the token stream.  End of the token
    stream can also be explicitly tested with end_of_token_stream().

    This class just creates an internal RegexTrieDictLexer instance and adds
    buffering."""
    # Implemented with a deque.  An index one greater than len of the deque means
    # to read from the token stream.  The only other indices are negative,
    # indexing from the right of the deque.  (This is necessary since the buffer
    # will start deleting elements at the beginning on appends once
    # MAX_BUFFERED_TOKENS is reached, messing up the indices from the left).

    def __init__(self, text_stream, regex_trie_dict, whitespace="",
                 MAX_BUFFERED_TOKENS=1024):
        self.lexer = RegexTrieDictLexer(text_stream, regex_trie_dict, whitespace)
        self.MAX_BUFFERED_TOKENS = MAX_BUFFERED_TOKENS # set to None for infinite
        self.token_data_buf = collections.deque(maxlen=self.MAX_BUFFERED_TOKENS)
        self.whitespace = set(whitespace)
        self.pos = 0 # a self.pos value of len(tokenDataBuf) means read in and append
        return

    def set_whitespace(whitespace_chars):
        self.whitespace = set(whitespace_chars)
        return

    def __iter__(self):
        """So statements like: for tok in lexObj: ... can be used."""
        return self # implies that the next() method will be used for iterations

    def next(self, ignore_whitespace=True):
        """Get next token.  If ignore_whitespace is False, i.e., the function is
        called as next(False), then whitespace tokens will be reported as regular
        tokens, regardless of any whitespace-character setting.

        Note that for correctness (over efficiency), whitespace tokens are also
        buffered.  This ensures that the results will be the same as for the
        unbuffered RegexTrieDictLexer, even after pushbacks.  For true
        equivalence any next(boolVal) results should only be pushed back with
        pushback(boolVal), for the same boolVal."""

        if self.pos == len(self.token_data_buf): # empty, need to call lower-level lexer
            # let self.lexer.next raise a StopIteration, if necessary
            new_tok = self.lexer.next(False) # call low-level lexer, don't ignore white
            self.token_data_buf.append(new_tok)
            self.pos = len(self.token_data_buf)
            if ignore_whitespace:
                while self.token_data_buf[-1].tokenString in self.whitespace:
                    return self.next(ignore_whitespace) # recurse
            return self.token_data_buf[-1]
        # at this point we know buffer is not empty, so self.pos is negative
        retval = self.token_data_buf[self.pos]
        self.pos += 1 # there won't be any appends until pos gets back to the end
        if self.pos == 0:
            self.pos = len(self.token_data_buf) # at the far right end
        if ignore_whitespace and retval.tokenString in self.whitespace:
            return self.next(ignore_whitespace) # recurse
        return retval

    def expect_char(self, char_str, ignore_whitespace=True):
        # TODO rewrite or eliminate; should look at label.  Used in old parser.py as
        # if not lex.expectChar(whitespace, False):
        #       print("Error: whitespace required after type '"+echoString+"'.")

        """A convenience function.  Does a peek() and returns True if the string
        for the returned token matches one of the characters in char_str, False
        otherwise."""
        return self.peek(ignore_whitespace).tokenString in char_str

    def pushback(self, ignore_whitespace=True):
        """Effectively pushes back the last token read.  If ignore_whitespace is
        True then the last token and any whitespace token before it is pushed
        back (this is to reverse a previous next() operation with
        ignore_whitespace==True).

        This function returns True if the pushback was successful, and False if
        it failed due to running out of buffered tokens (i.e., if more
        pushbacks than nexts have been called within the current size of the
        token buffer.).  Note that returning False does not necessarily mean it
        is back to the start of the token stream unless MAX_BUFFERED_TOKENS =
        None.  Method beginning_of_line() does assume this, however, for the
        special case of the first line.

        Note also that you cannot pushback any token which caused (as a higher
        level parse action) a definition to be made which changed the keys and
        values in the regexTrieDict of the lexer (or else the results will be
        undefined).  The lexer is dynamic, and changes according to definitions
        of its tokens in the regexTrieDict."""

        if self.pos == -len(self.token_data_buf):
            return False
        elif self.pos == len(self.token_data_buf):
            self.pos = -1
        else: self.pos = self.pos - 1
        # if ignoring whitespace, push back the preceeding whitespace
        if ignore_whitespace:
            if self.pos == -len(self.token_data_buf): return False
            if self.token_data_buf[self.pos-1].tokenString in self.whitespace:
                return self.pushback(ignore_whitespace) # recurse
        return True

    def peek(self, ignore_whitespace=True):
        """Peek ahead and return the next token without incrementing the token
        stream.  Returns None if a peek is attempted when endOfTokenStream()
        is True."""

        if self.end_of_token_stream(ignore_whitespace): return None # peeking too far
        tok = self.next(ignore_whitespace)
        self.pushback(ignore_whitespace)
        return tok

    def end_of_token_stream(self, ignore_whitespace=True):
        """True when there are no more tokens in the token stream, ignoring
        whitespace when ignore_whitespace==True.  Does not consume any tokens
        or alter the token stream."""

        count = 0
        while True:
            # if no tokens in buffer and no tokens left in low-level lexer then True
            if self.pos == len(self.token_data_buf) and self.lexer.end_of_token_stream():
                retval = True
                break
            # if whitespace isn't being ignored we're done: endOfTokenStream is False
            if not ignore_whitespace:
                retval = False
                break
            # look ahead past whitespace (these chars will be pushed back afterward)
            # if we find any non-whitespace we know we're not at endOfTokenStream
            tok = self.next(False)
            count += 1
            if tok.tokenString not in self.whitespace:
                retval = False
                break

        # push back the lookahead chars
        for i in range(count): self.pushback(False)

        return retval

    def end_of_line(self, ignore_whitespace=True):
        """True if at the end of a line, ignoring white space if ignore_whitespace
        is True.  At endOfTokenStream() this also gives True.  Does not consume
        any tokens or alter the token stream."""

        count = 0
        while True:
            if self.end_of_token_stream(False): return True
            tok = self.next(False)
            count += 1
            if tok.tokenString == "\n":
                retval = True
                break
            if ignore_whitespace and tok.tokenString in self.whitespace:
                continue
            retval = False
            break

        for i in range(count): self.pushback(False)
        return retval

    def beginning_of_line(self, ignore_whitespace=True):
        """True if at the beginning of a line.  When ignore_whitespace=True it
        assumes that the token buffer is large enough to cover any cases where
        backing up with pushback would exhaust the buffer before reaching the
        actual beginning of the token stream.  (Only files with whitespace
        regions on the order of MAX_BUFFERED_TOKENS would violate this.)"""

        # at beginning, no reads yet
        if self.lexer.text_stream.beginning_of_text_stream() == 0:
            return True

        # handle regular cases
        retval = False
        count = 0
        while True:
            if self.token_data_buf[self.pos].tokenString == "\n":
                retval = True
                break
            if not ignore_whitespace: break
            if self.token_data_buf[self.pos].tokenString in self.whitespace:
                push_val = self.pushback(False)
                if push_val: count += 1
                if push_val == False:
                    retval = True
                    break

        for i in range(count): self.next(False)
        return retval

    def get_all_to_delimiter(self, delimiter_str, ignore_whitespace=True):
        """This reads in a sequence of tokens from the current place in the token
        stream, up to a delimiter which is a character in the string
        delimiter_str.  The strings for each such token are concatenated together
        and returned as a single uncheckedString token.  Each individual
        delimiter character must be the string for a distinct token type.  The
        delimiter character itself is pushed back.  The empty string is reported
        as the TokenData string value if a delimiter is found immediately.  End
        of token stream is always a delimiter.

        The delimiter can contain whitespace characters even when whitespace is
        ignored.  In that case, if any whitespace delimiters occur before some
        non-whitespace characters then they are ignored and not treated as
        end-delimiters.  After some non-whitespace the whitespace delimiter
        counts as an end-delimiter.  For example, an identifier might begin and
        end with blanks, but only the end ones should be delimiters.

        The results of this method currently cannot be pushed back because
        multiple tokens are possibly combined (with some possibly considered as
        errors in the usual lexing) to create the undefined identifier.  So
        multiple pushbacks would be required (and the number would have to be
        saved).

        This routine can be used when new language elements like variables and
        functions are defined in known places in the grammar, but they haven't
        yet been entered into the LanguageData database.  It can also be used
        to read, say, numbers (which are recognized from seeing a leading
        digit)."""

        delimiter_set = set(delimiter_str)
        before = True
        string_val = ""
        line_info_list = []

        while True:
            if not self.peek(False): break # check for end of token stream
            tok_data = self.next(False)
            if tok_data.tokenString in delimiter_set:
                if ignore_whitespace and (tok_data.tokenString in self.whitespace) and before:
                    continue
                self.pushback(ignore_whitespace) # if ignore, put white before it back, too
                break
            if ignore_whitespace and (tok_data.tokenString in self.whitespace):
                continue
            before = False
            string_val = string_val + tok_data.tokenString
            line_info_list.extend(tok_data.elemData)

        return TokenData(True, string_val, DataTuple("uncheckedString", None), line_info_list)

    def print_token_buf_strings(self, n=0):
        """Debugging routine to print out the strings for the last n tokens in
        tokenDataBuf.  If n=0 all the tokens are printed."""
        buf_size = len(self.token_data_buf)
        if n == 0: n = buf_size
        print("TokenBuff[", end="")
        for i in range(max(0, buf_size-n), buf_size):
            string = self.token_data_buf[i].tokenString
            if string == "\n": string = "\\n"
            if i != buf_size-1: print(string+",", end="")
            else: print(string, end="")
        print("]")
        return


#
#  Run test cases.
#


if __name__ == "__main__":

    # exit(0) # comment this out to test interactive
    print("\nTest interactive...\n")
    import readline
    readline.parse_and_bind('set editing-mode vi')

    ts = TextStream()
    ts.set_raw_in()  # reads data from raw_input

    td = RegexTrieDict()

    tdlex = BufferedRegexTrieDictLexer(ts, td, " \n") # ignore space and newline

    #tdlex = BufferedRegexTrieDictLexer(ts, td)

    #for tok in tdlex: print(tok)

    # alternate getting up to delimiter and getting next
    while not tdlex.end_of_token_stream():
        print(tdlex.get_all_to_delimiter(" \n"))
        print(tdlex.next())

