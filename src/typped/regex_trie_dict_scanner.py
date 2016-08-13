"""
RegexTrieDictScanner
----------------------

The RegexTrieDictScanner uses a RegexTrieDict for tokenizing a sequence of
elements.  This class essentially does the same thing that re.findall would do
in a traditional scanner design, except that it is easy to dynamically update
it (though it is less efficient at the static matching).

The elements (usually characters) are inserted one by one and tokens
are returned when they are recognized.  The key-sequences inserted into the
RegexTrieDict are by definition the tokens.  An arbitrary sequence can then be
tokenized by inserting it element by element into the TrieDict, using a special
method.  The shortest or longest matches can be found.  (Note that for
generality we refer to "tokens" and the "elements" that make up both the tokens
and the sequences to be tokenized.  In lexical analysis applications the tokens
are strings and the elements are characters.)

   tok = RegexTreeDictTokenizer(td)

   for i in "eggbert":
      tok.insertSeqElem(i)
      tok.printTokenDeque()
   for i in "eggber":
      tok.insertSeqElem(i)
      tok.printTokenDeque()
   tok.insertSeqElem("x")

The results of the tokenization are automatically place in a deque which is
stored with the RegexTreeDictTokenizer instance.  Users can manipulate this
deque in any way they want; it is only used for reporting tokens as they are
unambiguously detected (i.e., they are inserted when matched).  Note that it
may be necessary to call tok.assertEndOfSequence() in order for the tokenizer
to deal with situations that are currently ambiguous (as far as finding the
longest match).

   tok.assertEndOfSequence()
   tok.printTokenDeque()
   tok.clearDeque()

Key strings can be matched as tokens from a sequential character stream,
choosing either the longest or the shortest (first) match.  Finding the
shortest matches (assuming no regexes) is linear in the overall query string
length.  The time when finding the longest matches is still efficient in the
usual cases but is not linear in the query-lengths because recursion is used to
effectively back up when it becomes known that a recognized pattern is the
longest (in that part of the sequence).  But it must wait for a mismatch or the
end of the query string to know that a saved possible match was the longest.

Worst case, suppose we have these three keys:
   aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
   a
   b

Now, suppose the input query-stream of characters is:
   aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab

We save the first a as a possible match, and then we have to go all the way to
the final b to determine that the longer string is not a match (and the single
character is the longest match).  Then we go back and start the same thing over
from the second a, and so forth, getting a time which depends on the length of
the longest string stored and the prefix properties of the stored strings.
When the stored strings are relatively short relative to the query length and
are distributed in "the usual ways" this should not make much difference in
practice.

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

#TODO: rewrite using the RegexTrieDict and Matcher classes

from __future__ import print_function, division, absolute_import

# Run test cases below when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_regex_trie_dict_scanner.py", pytest_args="-v")

import sys
import re
import collections # to use deque and MutableSequence abstract base class
from . import regex_trie_dict


class TokenData(object):
    """This class is the basic container used by the RegexTrieDictScanner.  It
    holds a token (which may be an invalid token) and also some related data.
    The tokenDataDeque of the RegexTrieDictScanner (such as from getTokenDataDeque()
    calls) contains TokenData objects.

    A convention which is used in the language parsing application is that the
    self.data field contains a tuple of information where the first, 0th item in
    the tuple is a string giving the sort or type of language element, and the
    second or later elements contain data dependent of the sort of element.  The
    current module does not set any of these values, however, and so does have
    any need to know about or apply that convention."""

    def __init__(self, validToken, tokenString, data, elemData):
        self.validToken = validToken # False if the "token" was not in the RegexTrieDict
        self.tokenString = tokenString # the string of the token which resulted in the match
        self.data = data # some arbitrary piece of data stored as a key/data pair
        self.elemData = elemData # some arbitrary data stored on a query with each elem

    def __repr__(self):
        return "TokenData("+str(self.validToken)+", "+str(self.tokenString) + \
            ", "+str(self.data)+", "+str(self.elemData)+")"

    def __bool__(self):
        """Allow for testing a TokenData object in conditionals, converting to bool."""
        return self.validToken


class RegexTrieDictScanner(object):
    """This class uses the keys of a RegexTrieDict as tokens."""

    def __init__(self, regexTrieDict):
        """User must pass in a valid RegexTrieDict containing the tokens."""
        self.td = regexTrieDict
        self.clear()
        return

    def clear(self):
        """Reset the tokenizer to its initial condition."""
        self.matchLongest = True # whether to always look for longest match
        self.noInvalidTokensFound = True # whether unstored string found on curr query
        self.tokenDataDeque = collections.deque() # the deque of query matches

        self.resetSeq()
        return

    def resetSeq(self):
        """ Reset the sequence from previous insertSeqElem calls, i.e.,
        start the next insertion back at the root node.  All of the saved possible
        token matches are deleted and not reported: this is a cold reset.  """
        self.currNode = self.td.root # the current node for current sequence elem
        self.tdInsertCount = self.td.insertCount # to make sure Trie doesn't change
        self.tdDeleteCount = self.td.deleteCount # to make sure Trie doesn't change

        self.possibleMatch = False # True if a string matched which may not be longest
        self.possibleToken = "" # The string for the last possible match.
        self.possibleData = None # Data stored with possibleToken
        self.possibleMiscList = [] # List of misc data saved with each element.

        self.currToken = "" # the concatenation of all elems so far in token being found
        self.currMiscList = [] # List of misc data saved with each element.

        self.nonTreeMatchInProgress = False # true if a number match in progress
        return

    def resetSeqAfterFlushing(self):
        """This flushes out the buffer of possible saved token matches before
        resetting the sequence of elements (back to start at the root)."""
        self.assertEndOfSeq()
        self.resetSeq() # done by self.assertEndOfSeq(), but do again to be safe
        return

    def setMatchLongest(self, boolVal):
        """Set True if longest matches should be found in insertSeqElem queries,
        False if shortest.  The default in initialization and after a clear()
        is True."""
        self.matchLongest = boolVal
        return

    def currentSeqIsValid(self):
        """Return True if the current sequence being tokenized is still valid.
        Return False otherwise.  A sequence becomes invalid if there are any
        inserts or deletes in the underlying Trie.  This is just for informational
        purposes, since any attempt to insert an element in an invalid sequence
        will automatically call resetSeqAfterFlushing first and reset the sequence."""
        return (self.tdInsertCount == self.td.insertCount
                and self.tdDeleteCount == self.td.deleteCount)

    #
    # Note that shortest match in insertSeqElem works easily, but what about
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
    # currToken, which has been built up to "eggberb" after the final "b"
    # has had insertSeqElem called on it.
    #
    digits = set(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])

    def insertSeqElem(self, char, miscData=None): # TODO change to not depend on chars
        """ Insert the next element from the sequence being tokenized.  If
        inserting the element results in a match (including detecting an
        unrecognizable token) the matching token (or sequence of elements) is
        appended to the result deque, which the getTokenDataDeque method returns.
        This function returns True until some string has been recognized as not
        being stored in the tree, after which it returns False (this signals
        that some higher-level error-handling needs to be done).

        A future modification might allow multiple query instances,
        essentially a wrapper-class for pointers to nodes in the tree.

        The format of the tokenDataDeque is a deque of TokenData class instances.

        In lexical analysis:

        The optional miscData argument is miscellaneous data which is associated
        with the query chars (in particular, line numbers can be stored and
        "passed up" for better error reporting).

        Inserting the empty string "" is equivalent to asserting the end of the
        query string (and is how assertEndOfSeq() is implemented)."""

        # Automatically reset after flushing if underlying Trie is no longer valid.
        if not self.currentSeqIsValid() and char != "": # TODO: don't use insert empty char for endOfSeq
            self.resetSeqAfterFlushing()

        # Now handle any ordinary tree matches for the queryChar char.
        if not char in self.currNode.children: # mismatch beyond current node
            # empty string queryInserts always take this path from conditional above
            if self.possibleMatch:
                self.tokenDataDeque.append(TokenData(
                                           True, self.possibleToken, self.possibleData, self.possibleMiscList
                                           ))
                self.possibleMatch = False
                # remove recognized possibleMatch prefix from currToken
                # note that suffix is saved in a local var for recursion
                suffix = self.currToken[len(self.possibleToken):]
                suffix += char
                suffixMisc = self.currMiscList[len(self.possibleToken):]
                if char != "": suffixMisc.append(miscData)
                # reset query and re-query each char of the suffix string
                self.resetSeq()
                for index in range(len(suffix)):
                    self.insertSeqElem(suffix[index], suffixMisc[index])
                return self.noInvalidTokensFound
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
                # Reset and try re-querying the fail-char if currToken had
                # nonzero length.  Slower, we can just report the first char
                # of currToken as a fail, and reinsert all else + char.
                if len(self.currToken) > 0:
                    self.noInvalidTokensFound = False # set error return flag
                    if fastRecover:
                        self.tokenDataDeque.append(TokenData(
                                                   False, self.currToken, None, self.currMiscList))
                    else:
                        self.tokenDataDeque.append(TokenData(
                                                   False, self.currToken[0], None, self.currMiscList[0]))
                    savedCurrToken = self.currToken
                    savedCurrMiscList = self.currMiscList
                    self.resetSeq()
                    if fastRecover:
                        if char != "": self.insertSeqElem(char, miscData)
                    else:
                        savedCurrToken = savedCurrToken[1:] + char
                        savedCurrMiscList = savedCurrMiscList[1:]
                        if char != "":
                            savedCurrMiscList.append(miscData)
                        self.insertSeqElem(savedCurrToken, savedCurrMiscList)
                else: # only the current char doesn't match, at root
                    if char != "":
                        self.noInvalidTokensFound = False # set error return flag
                        self.tokenDataDeque.append(
                            TokenData(False, char, None, [miscData]))
                    self.resetSeq()
                return self.noInvalidTokensFound

        # At this point we know there is another node below the current one.
        self.currToken = self.currToken + char
        self.currMiscList.append(miscData)
        self.currNode = self.currNode.children[char] # move currNode down tree
        if self.currNode.isLastElemOfKey:
            # to match longest we must wait before concluding, unless we are at a leaf
            numChildren = len(self.currNode.children)
            if self.matchLongest and numChildren != 0:
                # matchLongest and not at a leaf
                self.possibleMatch = True
                self.possibleToken = self.currToken
                self.possibleData = self.currNode.data
                self.possibleMiscList = self.currMiscList
            else:
                # match shortest or else at a leaf
                self.tokenDataDeque.append(TokenData(
                                           True, self.currToken, self.currNode.data, self.currMiscList
                                           ))
                self.resetSeq()
        return self.noInvalidTokensFound

    def getTokenDataDeque(self):
        """Get the deque of matches generated by insertSeqElem calls."""
        return self.tokenDataDeque

    def clearTokenDataDeque(self):
        """Sets the current deque of matches empty.  This may be useful in an
        algorithm, or to free memory in a long sequence.  Does not alter anything
        else, including the current query and any saved possible-match value."""
        self.tokenDataDeque.clear()

    def assertEndOfSeq(self):
        """Asserts that there are no more elements in the current sequence.
        Will empty out the buffer of elements and of possible matches."""
        # TODO do we still want null string, or special end token?
        return self.insertSeqElem("")

    def printTokenDeque(self):
        """Debugging routine, print out all the strings in tokenDataDeque."""
        print("TokenDeque[", end="")
        for i in range(len(self.tokenDataDeque)):
            if i != len(self.tokenDataDeque)-1:
                print(self.tokenDataDeque[i].tokenString+",", end="")
            else:
                print(self.tokenDataDeque[i].tokenString, end="")
        print("]")



