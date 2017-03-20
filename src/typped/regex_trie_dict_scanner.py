"""
RegexTrieDictScanner
----------------------

TODO: Update this intro to reflect new implementation using PrefixMatcher.

The `RegexTrieDictScanner` uses a `RegexTrieDict` for tokenizing a sequence of
elements.  The underlying `RegexTrieDict`  essentially does the same thing that
`re.match` would do in a traditional scanner design, except that 1) patterns in
it can be efficiently dynamically updated, 2) it is fast for large numbers of
"simple patterns," and 3) it supports online scanning (i.e., where characters
are inserted one at a time and the longest-matching result is returned as soon
possible based on the patterns and the sequence).  The implementation is pure
Python, though, so there is a constant before any asymptotic efficiency with
increasing numbers of patterns.  Like Python's regex (but not some other
implementations) it has an exponential worst-case match time.  Memory use can
also be large, since the matching algorithm essentially does a BFS on
possible pattern matches (which is what makes the online usage work).

Elements of sequences (usually characters of strings) are inserted one by one,
from left to right, into the scanner.  Tokens are returned when their patterns
match.  (The key-sequences inserted into the underlying `RegexTrieDict` define
the tokens.)  An arbitrary sequence can then be tokenized by inserting it
element by element.  The all matches are reported, and the shortest or longest
can be matches can be detected (the longest is only defined assuming the regex
patterns in the `RegexTrieDict` reach some state where no further characters
will match cause a match).

.. code-block:: python

   # FIX THIS EXAMPLE

   td = RegexTrieDict()
   # ... insert patterns in td

   tok = RegexTreeDictScanner(td)

   for c in "eggbert":
      tok.insert_seq_elem(c)
      tok.print_token_deque()

   for c in "eggber":
      tok.insert_seq_elem(c)
      tok.print_token_deque()

   tok.insert_seq_elem("x")

The results of the tokenization are automatically placed in a deque which is
stored with the `RegexTreeDictScanner` instance.  Users can manipulate this
deque in any way they want; it is only used for reporting tokens as they are
detected (i.e., they are inserted when matched).  Note that it may be necessary
to call `tok.assert_end_of_sequence()` in order for the tokenizer to deal with
situations that are currently ambiguous as far as finding the longest match.

.. code-block:: python

   tok.assert_end_of_sequence()
   tok.print_token_deque()
   tok.clear_deque()

.. topic:: Revise this stuff later...

    Key strings can be matched as tokens from a sequential character stream,
    choosing either the longest or the shortest (first) match.  Finding the
    shortest matches (assuming no regexes) is linear in the overall query string
    length.  The time when finding the longest matches is still efficient in the
    usual cases but is not linear in the query-lengths because recursion is used to
    effectively back up when it becomes known that a recognized pattern is the
    longest (in that part of the sequence).  But it must wait for a mismatch or the
    end of the query string to know that a saved possible match was the longest.

    Worst case, suppose we have these three keys::

       aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
       a
       b

    Now, suppose the input query-stream of characters is::

       aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab

    We save the first `a` as a possible match, and then we have to go all the way
    to the final `b` to determine that the longer string is not a match (and the
    single character is the longest match).  Then we go back and start the same
    thing over from the second `a`, and so forth, getting a time which depends on
    the length of the longest string stored and the prefix properties of the stored
    strings.  When the stored strings are relatively short relative to the query
    length and are distributed in "the usual ways" this should not make much
    difference in practice.

    The tree can easily handle whitespace characters if whitespace characters are
    never valid stored strings or substrings of stored strings.  We just get an
    unrecognized string result on those characters (depending on how error handling
    is configured, with fastRecover of not).  Alternately, and better, we can
    insert one of each whitespace character into the tree and then just test and
    ignore those matches.  Or we could just do a split on whitespace before feeding
    data to the tree; that is probably best in most situations.

    For queries of fixed keys which are either in the structure or not there is
    no real advantage to using the tree algorithm (it will be slower since it
    uses a standard dicts at each node for storing child nodes).  What the tree
    algorithm can do well is recognize stored items (tokens) out of a
    continuous, sequential stream of characters (either finding the first match
    or the longest), while also allowing fast inserts and deletes of
    keys/tokens.

    Using a standard hashed dict (or REs) for the same thing we would build up
    the query string character by character and query the hash dict on the
    query string each time a character is appended to it.  That is, generate
    the prefixes of the input character string.  Then it would save possible
    matches, etc., and when a match is recognized as the longest it would
    remove that prefix and restart, just like the tree version below.  To find
    the longest match, however, we need to know when a mismatch occurs or else
    go all the way to the end of the input each time.  This would entail saving
    all the prefixes of all the keys (in another dict, perhaps), or otherwise
    coming up with some scheme to detect when no longer-match is possible
    (because the current query string is not a prefix of any key).  This scheme
    would have to be able to be quickly updated on inserts and deletes of keys.

"""

from __future__ import print_function, division, absolute_import

# Run test cases below when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_regex_trie_dict_scanner.py", pytest_args="-v")

import collections # to use deque and MutableSequence abstract base class
from .regex_trie_dict import PrefixMatcher, RegexTrieDictError
#from .shared_settings_and_exceptions import TyppedBaseException

class RegexTrieDictScanner(object):
    """This class implements a scanner using the keys of a `RegexTrieDict` as patterns
    for tokens."""

    def __init__(self, regex_trie_dict):
        """User must pass in a valid `RegexTrieDict` containing the token regex
        patterns."""
        # TODO note that at certain times changes to trie are allowed, others not...
        # after getting a token you can modify before inserting more....
        self.rtd = regex_trie_dict
        self.matcher = PrefixMatcher(self.rtd)
        self.clear()

    def clear(self):
        """Reset the scanner to its initial condition."""
        #self.match_longest = True # Whether to always look for longest match.
        self.no_invalid_tokens_found = True # Whether unstored string found on curr query.
        self.token_data_deque = collections.deque() # The deque of query matches.
        self.reset_seq()

    def reset_seq(self):
        """Reset the sequence being inserted, i.e., start the next insertion
        back at the root node of the `RegexTrieDict`.  This is called when a
        prefix is "accepted" as a token and the detection shifts to the next
        token."""
        self.matching_nodes = None # Currently-matching nodes for current prefix.
        self.last_matching_nodes = None # Last node collection containing a match.
        self.last_matching_index = None # Prefix index for last match.
        self.curr_prefix_text = [] # The list of elems in current prefix being scanned.
        self.matcher.reset() # Reset the matcher.

    def is_valid(self):
        """Return true if the current sequence being tokenized is still valid.
        Return false otherwise.  A sequence becomes invalid if there are any
        inserts or deletes in the underlying trie."""
        return self.matcher.is_valid()

    def add_text_elem(self, elem, misc_data=None):

        """Insert `elem` as the next element from the text prefix sequence
        being scanned."""
        # TODO: work on API, what to return and when?
        # MOST IMPORTANT NOW, that is the confusing thing, what is spec?

        # Currently assumes longest match.
        # TODO: for non-greedy looping we need the full NodeDataStateList,
        # and to figure out exactly how to analyze it...
        print("DEBUG inserting char in scanner:", elem)

        if not self.is_valid():
            raise TrieDictScannerError("The trie of regex has been modified since"
                    " starting this prefix search, so the search is now invalid.")

        self.curr_prefix_text.append(elem)

        self.matcher.add_key_elem(elem)
        self.matching_nodes = self.matcher.get_meta(default=[], raw_nodes=True)

        if self.matching_nodes:
            self.last_matching_nodes = self.matching_nodes
            self.last_matching_index = len(self.curr_prefix_text) - 1

        # If no more matches possible get the last match to return, remove the
        # text from the curr_prefix_text, reset the matcher, and then re-add the
        # remaining elems of text (calling this routine recursively).
        if self.matcher.cannot_match():
            final_matches = self.last_matching_nodes
            if self.last_matching_index:
                match_text = self.curr_prefix_text[:self.last_matching_index + 1]
                new_prefix = self.curr_prefix_text[self.last_matching_index + 1:]
            else:
                raise PrefixCannotMatch("RegexTrieDictScanner: It is"
                             " impossible for the text sequence to match the patterns"
                             " in the current RegexTrieDict.")
            print("DEBUG match text in scanner is", match_text)

            print("DEBUG new prefix is", new_prefix)
            self.reset_seq()
            for elem in new_prefix:
                print("DEBUG re-inserting recursively....", elem)
                self.add_text_elem(elem)
            return match_text, final_matches
        else:
            return None

    def assert_end_of_seq(self):
        """Asserts that there are no more elements in the current sequence.
        Adds an element that cannot match to force any current states to
        terminate with a match or not."""
        return self.add_text_elem(self.rgt.magic_elem)


#
# Exceptions specific to this module.
#

class TrieDictScannerError(RegexTrieDictError):
    pass

class PrefixCannotMatch(TrieDictScannerError):
    """Raised when a prefix match is determined to be impossible."""
    pass

