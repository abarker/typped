"""
RegexTrieDictScanner
----------------------

TODO: Update this intro to reflect new implementation using PrefixMatcher.
TODO: For now, see the docs to the insert_char method...

This module is mainly intended for scanning text composed of characters as
elements.  So, unlike the general `RegexTrieDict` module, it is written with
elements assumed to be characters (one-element strings).  The code itself
should still work for general elements of sequences, not just character
elements in text sequences, but this has not been tested.

========= older below, save what can be ===================================

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

   td = RegexTrieDict()
   scanner = RegexTreeDictScanner(td)

   text_string = "test string here"
   for char in text_string: # May instead be a realtime string of chars.
       prefix_match_list = scanner.insert_char(text)
       if prefix_match_list:
           for match in prefix_match_list:
               print("Matched a prefix:", match)

The results of the tokenization are automatically placed in a deque which is
stored with the `RegexTreeDictScanner` instance.  Users can manipulate this
deque in any way they want; it is only used for reporting tokens as they are
detected (i.e., they are inserted when matched).  Note that it may be necessary
to call `tok.assert_end_of_text()` in order for the tokenizer to deal with
situations that are currently ambiguous as far as finding the longest match.

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

from .regex_trie_dict import SequentialPrefixMatcher, RegexTrieDictError

class RegexTrieDictScanner(object):
    """This class implements a scanner using the keys of a `RegexTrieDict` as patterns
    for tokens."""

    def __init__(self, regex_trie_dict):
        """User must pass in a valid `RegexTrieDict` containing the token regex
        patterns."""
        # TODO note that at certain times changes to trie are allowed, others not...
        # after getting a token you can modify before inserting more....
        self.rtd = regex_trie_dict
        self.string_joiner = self.rtd.combine_elems_fun
        self.prefix_matcher = SequentialPrefixMatcher(self.rtd)
        self.reset()

    def reset(self):
        """Reset the current character sequence being inserted, i.e., start the
        next insertion back at the root node of the `RegexTrieDict`."""
        self.cannot_match = False # Set true when nothing in trie can match curr prefix.
        self.matching_nodes = None # Currently-matching nodes for current prefix.
        self.last_matching_nodes = None # Last node collection containing a match.
        self.last_matching_index = None # Prefix index for last match.
        self.last_index_inserted_in_rtd_matcher = 0 # Last index of current pattern.
        self.end_of_text_asserted = False
        self.curr_prefix_text = [] # The list of chars in current prefix being scanned.
        self.prefix_matcher.reset() # Reset the regex prefix matcher.

    def is_valid(self):
        """Return true if the current text sequence being scanned is still valid.
        Return false otherwise.  A sequence becomes invalid if there are any
        inserts or deletes in the underlying trie."""
        return self.prefix_matcher.is_valid()

    def current_prefix(self, join_chars=True):
        """Return the current (not yet matched) prefix sequence.  This is just
        the current sequence of characters which have been entered with
        `append_text`, but with any already-detected prefix matches remover.

        If `join_chars` is true the characters are joined using the default
        character joiner of the underlying `RegexTrieDict`.  Otherwise the
        result is a list of characters."""
        if join_chars:
            return self.combine_chars_fun(self.curr_prefix_text)
        return self.curr_prefix_text

    def append_text(self, text, final_text=False):
        """Append the string `text` to the current prefix."""
        if self.end_of_text_asserted:
            # TODO: could have option to delete end magic char or auto-call reset text.
            raise TrieDictScannerError("End of text was previously asserted, call"
                                       " `clear` before inserting more text.""")
        for char in text:
            if char != self.rtd.magic_elem_never_matches:
                self.curr_prefix_text.append(char)
            else:
                self.curr_prefix_text.append(char) # TODO making part of prefix for now...
                self.end_of_text_asserted = True

        if final_text:
            self.assert_end_of_text()

    def assert_end_of_text(self, join_chars=True, reinsert_on_match=True):
        """Asserts that there are no more characters in the current sequence.
        Adds a special character that cannot match anything to the current prefix."""
        if self.end_of_text_asserted:
            return
        self.end_of_text_asserted = True
        self.curr_prefix_text.append(self.rtd.magic_elem_never_matches)

    def get_prefix_matches(self, join_chars=True, only_first=False):
        """Get all the prexix matches for the currently-inserted prefix text.
        If the currently-inserted characters allow a prefix
        match to be recognized as the longest possible (with respect to the
        current patterns in the trie) then the match is returned.

        If `reinsert_on_match` is true then, after a match is found, the
        scanner is reset and all the text characters which were inserted but
        not part of the previously-recognized prefix are reinserted until
        another known-longest match is found.  This continues until no
        guaranteed longest-prefix matches can be found.  The list of
        known-longest prefix matches is then returned.  More characters can
        then be inserted, with the same behavior.  Any remaining suffix becomes
        the new list attribute `curr_prefix_text` of the scanner instance.

        When a longest match is found that match is returned.   Otherwise, the
        return values are as follows:

        * `None` -- No longest match has yet been recognized.  Note that the
           longest match might actually have been found, but it cannot be
           returned until it is recognized as being the longest possible.  This
           can require more characters to be inserted (such as for repetition groups)
           or a call to the `assert_end_of_text` method.

        * `[]` -- No matches with respect to the current trie are possible no
           matter what additional characters are inserted.  The empty list is
           recognized as the list of matches.  The attribute `cannot_match` of the
           scanner instance is also set to `True` in this case.

        If `join_chars` is true (the default) then any returned character
        sequences are joined using the character-joining operation for the
        underlying `RegexTrieDict` instance.

        Other useful user-accessible attributes of scanner:

        The `last_returned` attribute is also set to the last returned value,
        which can be a useful way to access the matches.

        The raw matching nodes in the trie are in the attribute
        `last_matching_nodes`.

        The list of appended characters which have not yet matched (and hence
        have not yet been removed from the current prefix) is stored in the
        attribute `curr_prefix_text`.  This prefix text list is a list of
        unjoined characters, and includes a magic "matches nothing" character
        if `assert_end_of_text` has been called.

        The `cannot_match` attribute is set true if no further character
        appends can cause a match."""
        # Currently assumes longest match.
        # Figure out how to do non-greedy things...
        print("DEBUG in get_prefix_matches with prefix of:", self.curr_prefix_text)
        if not self.is_valid():
            raise TrieDictScannerError("The trie of regexes has been modified since"
                    " starting this prefix search, so the search is now invalid.")

        if self.cannot_match:
            self.last_returned = []
            return self.last_returned

        # TODO: This breaks on sequential inserts!  Need to save the last insertion point
        # and start there!
        for count in range(self.last_index_inserted_in_rtd_matcher, len(self.curr_prefix_text)):
            char = self.curr_prefix_text[count]
            print("   inserting character", char, "into prefix matcher")
            self.prefix_matcher.append_key_elem(char)
            self.last_index_inserted_in_rtd_matcher += 1

            matching_nodes = self.prefix_matcher.get_meta(no_matches_retval=[],
                                                          raw_nodes=True)
            print("   matching nodes after char", char, "are:", matching_nodes)
            if matching_nodes:
                print("   Found a matching node, setting as last_matching_nodes and index")
                self.last_matching_nodes = matching_nodes
                self.last_matching_index = count
                print("   DEBUG last matching index is:", count)

            if self.prefix_matcher.cannot_match():
                # Note we do not need cannot match if test if we know the real next chars to insert!?!?
                print("   DEBUG cannot_match is true in loop with index", count, self.curr_prefix_text[count])
                self.cannot_match = True
                if self.last_matching_nodes: # Found a previous match, use it.
                    print("   breaking the for loop, had a prev match ending at", self.last_matching_index)
                    break
                else:                        # No matches were found, return empty.
                    print("   returning from inside the loop, no prev match")
                    self.last_returned = []
                    return self.last_returned

        else: # else for the for loop; finished loop without finding a match (no break)
            #if not self.cannot_match: # No prefixes found, but a match is still possible.
            print("DEBUG no matches found in whole prefix string, but still can match, returning None")
            self.last_returned = None
            return self.last_returned

        #
        # We know some match was found; remove it, reset the scanner, and recurse if needed.
        #

        # The subtraction conditional below is needed because a match of a full string ending
        # with the magic never-matches is considered to match only the non-magic characters
        # as a pattern.  So we decrement to not include the final magic character.
        if self.curr_prefix_text[self.last_matching_index] == self.rtd.magic_elem_never_matches:
            self.last_matching_index -= 1
        match_text = self.curr_prefix_text[:self.last_matching_index + 1]
        print("match text is", match_text)
        new_prefix = self.curr_prefix_text[self.last_matching_index + 1:]
        print("new_prefix is", new_prefix)
        end_of_text_asserted = self.end_of_text_asserted

        # Reset the scanner.
        self.reset()

        # Restore a few attributes.
        self.append_text(new_prefix)
        assert self.curr_prefix_text == new_prefix
        assert self.end_of_text_asserted == end_of_text_asserted

        if join_chars:
            self.last_returned = [self.string_joiner(match_text)]
        else:
            self.last_returned = [match_text]

        if only_first:
            print("DEBUG returning an only_first match of", self.last_returned)
            return self.last_returned

        #
        # Reinsert the unmatched text in the reset prefix matcher, calling recursively.
        #

        return_list = self.last_returned
        while self.get_prefix_matches(join_chars=join_chars, only_first=True):
            return_list += self.last_returned

        self.last_returned = return_list
        return self.last_returned

#
# Exceptions specific to this module.
#

class TrieDictScannerError(RegexTrieDictError):
    pass

class PrefixCannotMatch(TrieDictScannerError):
    """Raised when a prefix match is determined to be impossible."""
    pass

