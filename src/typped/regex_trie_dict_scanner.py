"""
RegexTrieDictScanner
----------------------

TODO: this is an earler class, that doesn't yet use the newer features
of RegexTrieDict like PrefixMatcher objects...

The `RegexTrieDictScanner` uses a `RegexTrieDict` for tokenizing a sequence of
elements.  This class essentially does the same thing that `re.match` would do
in a traditional scanner design, except that 1) it is easy to dynamically
update the patterns in it, 2) it is fast for large numbers of "simple
patterns," and 3) it supports on-line scanning (i.e., where characters are
inserted one at a time and the longest-matching result is returned as soon
possible based on the patterns and the sequence).  The implementation is pure
Python, though, so there is a constant before any asymptotic efficiency with
increasing numbers of patterns.  Like Python's regex (but not some other
implementations) it has an exponential worst-case match time.  Memory use can
also be large, since the underlying `RegexTrieDict` essentially does a BFS on
possible pattern matches (which makes the on-line usage work).

Elements (usually characters) are inserted one by one and tokens are returned
when their patterns are the best match.  (The key-sequences inserted into the
underlying `RegexTrieDict` are by definition the tokens.)  An arbitrary
sequence can then be tokenized by inserting it element by element, using a
special method.  The shortest or longest matches can be found.  (Note that for
generality we refer to "tokens" and the "elements" that make up both the tokens
and the sequences to be tokenized.  In lexical analysis applications the tokens
are strings and the elements are characters.)

.. code-block:: python

   td = RegexTrieDict()
   # ... insert patterns in td

   tok = RegexTreeDictScanner(td)

   for c in "eggbert":
      tok.insert_set_elem(c)
      tok.print_token_deque()

   for c in "eggber":
      tok.insert_set_elem(c)
      tok.print_token_deque()

   tok.insert_set_elem("x")

The results of the tokenization are automatically place in a deque which is
stored with the RegexTreeDictTokenizer instance.  Users can manipulate this
deque in any way they want; it is only used for reporting tokens as they are
unambiguously detected (i.e., they are inserted when matched).  Note that it
may be necessary to call tok.assert_end_of_sequence() in order for the tokenizer
to deal with situations that are currently ambiguous (as far as finding the
longest match).

.. code-block:: python

   tok.assert_end_of_sequence()
   tok.print_token_deque()
   tok.clear_deque()

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

For queries of fixed keys which are either in the structure or not there is no
real advantage to using the tree algorithm (it will be slower since it uses a
standard dicts at each node for storing child nodes).  What the tree algorithm
can do well is recognize stored items (tokens) out of a continuous, sequential
stream of characters (either finding the first match or the longest), while
also allowing fast inserts and deletes of keys/tokens.

Using a standard hashed dict (or REs) for the same thing we would build up the
query string character by character and query the hash dict on the query string
each time a character is appended to it.  That is, generate the prefixes of the
input character string.  Then it would save possible matches, etc., and when a
match is recognized as the longest it would remove that prefix and restart,
just like the tree version below.  To find the longest match, however, we need
to know when a mismatch occurs or else go all the way to the end of the input
each time.  This would entail saving all the prefixes of all the keys (in
another dict, perhaps), or otherwise coming up with some scheme to detect when
no longer-match is possible (because the current query string is not a prefix
of any key).  This scheme would have to be able to be quickly updated on
inserts and deletes of keys.

"""

#TODO: rewrite using the RegexTrieDict and PrefixMatcher classes

from __future__ import print_function, division, absolute_import

# Run test cases below when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_regex_trie_dict_scanner.py", pytest_args="-v")

import sys
import re
import collections # to use deque and MutableSequence abstract base class
from .regex_trie_dict import PrefixMatcher

#class TokenData(object):
#
#    """This class is the basic container used by the RegexTrieDictScanner.  It
#    holds a token (which may be an invalid token) and also some related data.
#    The token_data_deque of the `RegexTrieDictScanner` (such as from
#    `get_token_data_deque()` calls) contains `TokenData` objects."""
#
#    def __init__(self, is_valid, matched_seq, data, elem_data):
#        self.is_valid = is_valid # False if the "token" was not in the RegexTrieDict
#        self.matched_seq = matched_seq # the string of the token which resulted in the match
#        self.data = data # some arbitrary piece of data stored as a key/data pair
#        self.elem_data = elem_data # some arbitrary data stored on a query with each elem
#
#    def __repr__(self):
#        return "TokenData("+str(self.is_valid)+", "+str(self.matched_seq) + \
#            ", "+str(self.data)+", "+str(self.elem_data)+")"
#
#    def __bool__(self):
#        """Allow for testing a TokenData object in conditionals, converting to bool."""
#        return self.is_valid


# This `namedtuple` is the basic container used by the `RegexTrieDictScanner`.
# It holds a token (which may be an invalid token) and also some related data.
# The `token_data_deque` of the `RegexTrieDictScanner` (such as from
# `get_token_data_deque()` calls) contains `TokenData` objects."""
TokenData = collections.namedtuple("TokenData", [
            "is_valid", # False if the "token" was not in the RegexTrieDict.
            "matched_seq", # The sequence of elements (string) which resulted in the match.
            "data", # Whatever data was stored in the trie with matched pattern as key.
            "elem_data", # Some arbitrary data stored when adding an elem.
            ])


class RegexTrieDictScanner(object):
    """This class uses the keys of a RegexTrieDict as tokens."""

    def __init__(self, regex_trie_dict):
        """User must pass in a valid RegexTrieDict containing the tokens."""
        self.rtd = regex_trie_dict
        self.matcher = PrefixMatcher(self.rtd)
        self.clear()
        return

    def clear(self):
        """Reset the tokenizer to its initial condition."""
        self.match_longest = True # Whether to always look for longest match.
        self.no_invalid_tokens_found = True # Whether unstored string found on curr query.
        self.token_data_deque = collections.deque() # The deque of query matches.

        self.reset_seq()
        self.matcher.reset(self.rtd)
        return

    def reset_seq(self):
        """ Reset the sequence from previous insert_set_elem calls, i.e.,
        start the next insertion back at the root node.  All of the saved possible
        token matches are deleted and not reported: this is a cold reset.  """
        self.curr_node = self.rtd.root # the current node for current sequence elem
        self.td_insert_count = self.rtd.insertCount # to make sure Trie doesn't change
        self.td_delete_count = self.rtd.deleteCount # to make sure Trie doesn't change

        self.possible_match = False # True if a string matched which may not be longest
        self.possible_token = "" # The string for the last possible match.
        self.possible_data = None # Data stored with possibleToken
        self.possible_misc_list = [] # List of misc data saved with each element.

        self.curr_token = "" # the concatenation of all elems so far in token being found
        self.curr_misc_list = [] # List of misc data saved with each element.

        self.non_tree_match_in_progress = False # true if a number match in progress
        self.matcher.reset() # Reset the matcher.
        return

    def reset_seq_after_flushing(self):
        """This flushes out the buffer of possible saved token matches before
        resetting the sequence of elements (back to start at the root)."""
        self.assert_end_of_seq()
        self.reset_seq() # done by self.assert_end_of_seq(), but do again to be safe
        return

    def set_match_longest(self, boolVal):
        """Set True if longest matches should be found in insert_set_elem queries,
        False if shortest.  The default in initialization and after a clear()
        is True."""
        self.match_longest = boolVal
        return

    def current_seq_is_valid(self):
        """Return True if the current sequence being tokenized is still valid.
        Return False otherwise.  A sequence becomes invalid if there are any
        inserts or deletes in the underlying Trie.  This is just for informational
        purposes, since any attempt to insert an element in an invalid sequence
        will automatically call resetSeqAfterFlushing first and reset the sequence."""
        return (self.td_insert_count == self.rtd.insertCount
                and self.td_delete_count == self.rtd.deleteCount)

    #
    # Note that shortest match in insert_seq_elem works easily, but what about
    # finding the longest match?  We save the most recent possible match, and
    # make it the actual match if a mismatch occurs after it.  How do
    # we reset the tree after getting a mismatch?  We don't always learn
    # immediately on the next elem if the earlier match is good or not.
    # E.g., suppose "egg" and "eggbert" are stored and we query sequentially on
    # the characters in "eggberb".  We first find "egg" as a possible match.
    # Then when we get to the final "b" we know that it is the longest match.
    # At that point we remove the prefix "egg" from "eggberb" and re-query
    # the characters in the string "berb", starting again at the root of the
    # tree.  Fortunately, the needed data is already being saved in
    # curr_token, which has been built up to "eggberb" after the final "b"
    # has had insert_seq_elem called on it.
    #
    digits = set(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])

    def insert_seq_elem(self, elem, misc_data=None): # NOTE this is the OLD method.
        # NOTE see new_insert_seq_elem method below, after this one...
        # updating to use PrefixMatcher.
        """Insert `elem` as the next element from the sequence being scanned.
        If inserting the element results in a match (including detecting that
        the sequence is unrecognizable) then `TokenData` instances for the
        matching tokens (i.e., sequences of elements) are appended to the
        result deque, which the `get_token_data_deque` method returns.  This
        function returns `True` until some string has been recognized as not
        being stored in the tree, after which it returns False (this signals
        that some higher-level error-handling needs to be done).

        A future modification might allow multiple query instances,
        essentially a wrapper-class for pointers to nodes in the tree.

        The format of the `token_data_deque` is a deque containing `TokenData`
        namedtuple instances for each match that was found.  That is, it
        contains a data tuple for each prefix of the current sequence of
        elements that matched a pattern.

        In lexical analysis:

        The optional misc_data argument is miscellaneous data which is associated
        with the query chars (in particular, line numbers can be stored and
        "passed up" for better error reporting).

        Inserting the empty string "" is equivalent to asserting the end of the
        query string (and is how assertEndOfSeq() is implemented)."""
        # TODO: what if multiple pattern matches for query string?  Seems like you
        # need a list of TokenData namedtuples at each index of the deque....
        # Also, how are these elements being combined, if at all?  Test data on
        # strings looks like they are being added together... presumed + operator.

        # Automatically reset after flushing if underlying Trie is no longer valid.
        if not self.current_seq_is_valid() and elem != "":
            self.reset_seq_after_flushing()

        # Now handle any ordinary tree matches for the queryChar elem.
        if not elem in self.curr_node.children: # mismatch beyond current node
            # empty string queryInserts always take this path from conditional above
            if self.possible_match:
                self.token_data_deque.append(TokenData(
                                           is_valid=True,
                                           matched_seq=self.possible_token,
                                           data=self.possible_data,
                                           elem_data=self.possible_misc_list
                                           ))
                self.possible_match = False
                # remove recognized possibleMatch prefix from currToken
                # note that suffix is saved in a local var for recursion
                suffix = self.curr_token[len(self.possible_token):]
                suffix += elem
                suffixMisc = self.curr_misc_list[len(self.possible_token):]
                if elem != "": suffixMisc.append(misc_data)
                # reset query and re-query each elem of the suffix string
                self.reset_seq()
                for index in range(len(suffix)):
                    self.insert_seq_elem(suffix[index], suffixMisc[index])
                return self.no_invalid_tokens_found
            else:
                # Char doesn't match a child, no saved match, but currToken
                # matched a prefix of something up to here.  So there is
                # some error in the currToken.  There are various ways to
                # handle error recovery.
                #
                fastRecover = True # this could be a settable class variable
                #
                # To fastRecover, ditch all of the currToken rather than
                # trying to reinsert various parts to do "maximum recovery."
                # Reset and try re-querying the fail-elem if currToken had
                # nonzero length.  Slower, we can just report the first elem
                # of currToken as a fail, and reinsert all else + elem.
                if len(self.curr_token) > 0:
                    self.no_invalid_tokens_found = False # set error return flag
                    if fastRecover:
                        self.token_data_deque.append(TokenData(
                                  is_valid=False,
                                  matched_seq=self.curr_token,
                                  data=None,
                                  elem_data=self.curr_misc_list))
                    else:
                        self.token_data_deque.append(TokenData(
                                  is_valid=False,
                                  matched_seq=self.curr_token[0],
                                  data=None,
                                  elem_data=self.curr_misc_list[0]))
                    savedCurrToken = self.curr_token
                    savedCurrMiscList = self.curr_misc_list
                    self.reset_seq()
                    if fastRecover:
                        if elem != "": self.insert_seq_elem(elem, misc_data)
                    else:
                        savedCurrToken = savedCurrToken[1:] + elem
                        savedCurrMiscList = savedCurrMiscList[1:]
                        if elem != "":
                            savedCurrMiscList.append(misc_data)
                        self.insert_seq_elem(savedCurrToken, savedCurrMiscList)
                else: # only the current elem doesn't match, at root
                    if elem != "":
                        self.no_invalid_tokens_found = False # set error return flag
                        self.token_data_deque.append(
                            TokenData(
                                     is_valid=False,
                                     matched_seq=elem,
                                     data=None,
                                     elem_data=[misc_data]))
                    self.reset_seq()
                return self.no_invalid_tokens_found

        # At this point we know there is another node below the current one.
        self.curr_token = self.curr_token + elem
        self.curr_misc_list.append(misc_data)
        self.curr_node = self.curr_node.children[elem] # move currNode down tree
        if self.curr_node.is_last_elem_of_key:
            # to match longest we must wait before concluding, unless we are at a leaf
            numChildren = len(self.curr_node.children)
            if self.match_longest and numChildren != 0:
                # matchLongest and not at a leaf
                self.possible_match = True
                self.possible_token = self.curr_token
                self.possible_data = self.curr_node.data
                self.possible_misc_list = self.curr_misc_list
            else:
                # match shortest or else at a leaf
                self.token_data_deque.append(TokenData(
                               is_valid=True,
                               matched_seq=self.curr_token,
                               data=self.curr_node.data,
                               elem_data=self.curr_misc_list))
                self.reset_seq()
        return self.no_invalid_tokens_found

    def new_insert_seq_elem(self, elem, misc_data=None): # NOTE this is the NEW method.

        """Insert `elem` as the next element from the sequence currently being
        scanned.  If inserting the element results in a match (including
        detecting that the sequence is unrecognizable) then [LIST OF????]
        `TokenData` instances for the matching tokens (i.e., sequences of
        elements) are appended to the result deque, which the
        `get_token_data_deque` method returns.  This function returns `True`
        until some string has been recognized as not being stored in the tree,
        after which it returns False (this signals that some higher-level
        error-handling needs to be done).

        A future modification might allow multiple query instances,
        essentially a wrapper-class for pointers to nodes in the tree.

        The format of the `token_data_deque` is a deque containing `TokenData`
        namedtuple instances for each match that was found.  That is, it
        contains a data tuple for each prefix of the current sequence of
        elements that matched a pattern.  TODO: update when clearer.

        In lexical analysis:

        The optional misc_data argument is miscellaneous data which is associated
        with the query chars (in particular, line numbers can be stored and
        "passed up" for better error reporting).

        Inserting the empty string "" is equivalent to asserting the end of the
        query string (and is how assertEndOfSeq() is implemented)."""
        # TODO: what if multiple pattern matches for query string?  Seems like you
        # need a list of TokenData namedtuples at each index of the deque....
        # Also, how are these elements being combined, if at all?  Test data on
        # strings looks like they are being added together... presumed + operator.

        # TODO The PrefixMatcher is set up for the class.  Now update this
        # method to use it.  Update the format of the data deque to hold lists
        # or tuples of all the matches found.

        # Automatically reset after flushing if underlying Trie is no longer valid.
        if not self.current_seq_is_valid() and elem != "":
            self.reset_seq_after_flushing()

        # Now handle any ordinary tree matches for the queryChar elem.
        if not elem in self.curr_node.children: # mismatch beyond current node
            # empty string queryInserts always take this path from conditional above
            if self.possible_match:
                self.token_data_deque.append(TokenData(
                                           is_valid=True,
                                           matched_seq=self.possible_token,
                                           data=self.possible_data,
                                           elem_data=self.possible_misc_list
                                           ))
                self.possible_match = False
                # remove recognized possibleMatch prefix from currToken
                # note that suffix is saved in a local var for recursion
                suffix = self.curr_token[len(self.possible_token):]
                suffix += elem
                suffixMisc = self.curr_misc_list[len(self.possible_token):]
                if elem != "": suffixMisc.append(misc_data)
                # reset query and re-query each elem of the suffix string
                self.reset_seq()
                for index in range(len(suffix)):
                    self.insert_seq_elem(suffix[index], suffixMisc[index])
                return self.no_invalid_tokens_found
            else:
                # Char doesn't match a child, no saved match, but currToken
                # matched a prefix of something up to here.  So there is
                # some error in the currToken.  There are various ways to
                # handle error recovery.
                #
                fastRecover = True # this could be a settable class variable
                #
                # To fastRecover, ditch all of the currToken rather than
                # trying to reinsert various parts to do "maximum recovery."
                # Reset and try re-querying the fail-elem if currToken had
                # nonzero length.  Slower, we can just report the first elem
                # of currToken as a fail, and reinsert all else + elem.
                if len(self.curr_token) > 0:
                    self.no_invalid_tokens_found = False # set error return flag
                    if fastRecover:
                        self.token_data_deque.append(TokenData(
                                  is_valid=False,
                                  matched_seq=self.curr_token,
                                  data=None,
                                  elem_data=self.curr_misc_list))
                    else:
                        self.token_data_deque.append(TokenData(
                                  is_valid=False,
                                  matched_seq=self.curr_token[0],
                                  data=None,
                                  elem_data=self.curr_misc_list[0]))
                    savedCurrToken = self.curr_token
                    savedCurrMiscList = self.curr_misc_list
                    self.reset_seq()
                    if fastRecover:
                        if elem != "": self.insert_seq_elem(elem, misc_data)
                    else:
                        savedCurrToken = savedCurrToken[1:] + elem
                        savedCurrMiscList = savedCurrMiscList[1:]
                        if elem != "":
                            savedCurrMiscList.append(misc_data)
                        self.insert_seq_elem(savedCurrToken, savedCurrMiscList)
                else: # only the current elem doesn't match, at root
                    if elem != "":
                        self.no_invalid_tokens_found = False # set error return flag
                        self.token_data_deque.append(
                            TokenData(
                                     is_valid=False,
                                     matched_seq=elem,
                                     data=None,
                                     elem_data=[misc_data]))
                    self.reset_seq()
                return self.no_invalid_tokens_found

        # At this point we know there is another node below the current one.
        self.curr_token = self.curr_token + elem
        self.curr_misc_list.append(misc_data)
        self.curr_node = self.curr_node.children[elem] # move currNode down tree
        if self.curr_node.is_last_elem_of_key:
            # to match longest we must wait before concluding, unless we are at a leaf
            numChildren = len(self.curr_node.children)
            if self.match_longest and numChildren != 0:
                # matchLongest and not at a leaf
                self.possible_match = True
                self.possible_token = self.curr_token
                self.possible_data = self.curr_node.data
                self.possible_misc_list = self.curr_misc_list
            else:
                # match shortest or else at a leaf
                self.token_data_deque.append(TokenData(
                               is_valid=True,
                               matched_seq=self.curr_token,
                               data=self.curr_node.data,
                               elem_data=self.curr_misc_list))
                self.reset_seq()
        return self.no_invalid_tokens_found

    def get_token_data_deque(self):
        """Get the deque of matches generated by insert_set_elem calls."""
        return self.token_data_deque

    def clear_token_data_deque(self):
        """Sets the current deque of matches empty.  This may be useful in an
        algorithm, or to free memory in a long sequence.  Does not alter anything
        else, including the current query and any saved possible-match value."""
        self.token_data_deque.clear()

    def assert_end_of_seq(self):
        """Asserts that there are no more elements in the current sequence.
        Will empty out the buffer of elements and of possible matches."""
        # TODO do we still want null string, or special end token?
        return self.insert_seq_elem("")

    def print_token_deque(self):
        """Debugging routine, print out all the strings in `token_data_deque`."""
        #print("token data deque is", self.token_data_deque)
        print("TokenDeque[", end="")
        for i in range(len(self.token_data_deque)):
            if i != len(self.token_data_deque)-1:
                print(self.token_data_deque[i].matched_seq+",", end="")
            else:
                print(self.token_data_deque[i].matched_seq, end="")
        print("]")



