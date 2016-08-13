"""

Pattern Matching in RegexTrieDict
---------------------------------

The RegexTrieDict also simple built-in pattern-matching capabilities.  To use
these capabilities, you first create a key which contains some special
meta-elements (see below).  Then you insert that key (meta-key) and an
associated value into the TrieDict.  These meta-keys are still treated simply
as ordinary keys by the usual TrieDict operations.  But the TrieDict also has
the special methods has_key_meta, get_meta which interpret the meta-elements.

Calling has_key_meta for a key (an ordinary, non-meta key) returns True if any
current key matches or if any meta-key pattern happens to match as a pattern.
Similarly, get_meta finds any key or meta-key pattern which matches and returns
a tuple of all the data associated with matching items (i.e., associated with
the ordinary key or with the pattern meta-key it matches).

The special meta-symbols are user-defined.  By default the values are defined
for keys which are strings of characters.  The default definitions are:

<TODO update below, some changed>

   define_meta_elems(escape="\\", repetition="*", lGroup="(", rGroup=")",
                     lWildcard="[", rWildcard="]", range="-", rangeTestFun=None)

In order for an element of a key to be interpreted as a meta-symbol it must be
preceded by the defined escape element.  So in a string key the meta-symbols
with their default definitions above would always appear as "\\*", "\\(", "\\)",
"\\[", "\\]", and "\\-".  This is intended to minimize interference with ordinary
key elements.  All specially-interpreted characters must be escaped, without
exception.  As usual a double escape such as "\\\\" (or r"\\") reverts to the
original symbol, as does an escape not followed by one of the defined
meta-elements.

Keep in mind that if a TrieDict is to be used with escaped elements as
meta-elements then all the literal escape-elements in it must be escaped.  Even
the non-pattern keys which are inserted into the TrieDict in that case must
have any escape elements escaped (they are still patterns, just simple ones).
In the queries sequences themselves, however, escape elements are always simply
treated as literals (i.e., no meta-interpretation is ever performed on the
query-key sequences).  So a single escape on a key-query matches an escaped
escape in the stored key-sequences in the Trie.

The meta-level and the object-level are intentionally kept distinct in order to
minimize some of the confusions that can occur (even though there always are
some).  The ordinary dict methods always operate on trie keys as if they were
literals.  To get an extra level of interpretation, the special meta-level
methods must be used.  These operations, however, can be freely mixed.  Patterns
can be inserted and deleted, etc.

Pattern scanning is left-to-right in a key's elements (i.e., with string keys
the characters are scanned from left to right).

The language allows single-character wildcards.  As an example with strings,
consider using these patterns as keys:

   patt1 = "abc\\[123\\]def"
   patt2 = "abc\\[1\\-3\\]def"

The first pattern, patt1, matches abc1def, abd2def, and abc3def.  The second
pattern, patt2, matches the same strings but uses a range specifier.

<TODO note we now let them define the whole wildcard-processing...>
The
boolean-valued function rangeTestFun will be called for the first and last
argument of the range, along with the element to test.  The default
range-function (when the values is set to None in the call to defineMetaElems)
only works for character ranges.

<TODO note that Python patterns are allowed, and test some.>
<TODO note that user can essentially redefine the processing of the part inside
the brackets in any way desired.>

Repetition patterns match zero or more occurrences of the pattern group.  Here
is an example with strings as keys:

   patt1 = "abc\\*\\(DD\\)efg"

This would match "abcefg", "abcDDefg", abcDDDDefg", etc.  The repetition
pattern can also take optional numeric arguments, each separated by another
asterick.  A single numeric argument, like in

   patt = "abc\\*10\\(DD\\)efg

specifies a minimum number of repetitions.  The previous example must have
ten or more occurrences of "dd" in it.  So "abcDDefg" would not match,
but "abcDDDDDDDDDDDDDDDDDDDDefg" would match.  When two numbers are given
they represent the minimum and the maximum number of repetitions, respectively.
So the pattern

   patt = "abc\\*2\\*3\\(DD\\)efg"

would not match "abcDDefg", would match "abcDDDDefg" and "abcDDDDDDefg", and
would not match abcDDDDDDDDefg".

TODO below para not implemented
The grouping meta-elements must occur just after the start and at the end of
the repetition pattern itself.  For efficiency, repetition can be limited such
that it always "breaks out" of the "loop" at the first chance it gets.   This
occurs when, at the end of a loop, the next element scanned matches the next
pattern element after the closing repetition meta-element.  So the shortest
valid repetition sequence followed by some other valid element is always
chosen.  This restriction essentially requires the end of any repeated-pattern
segment to be unambiguous (or else no looping-back will occur).

Pattern-matching Implementation Details
---------------------------------------

As noted, meta-keys are inserted and stored in a TrieDict simply as ordinary
keys.  It is the traversal algorithm in the routine get_nodes_meta which differs
from the usual get_node algorithm (which finds the node in the trie
corresponding to a key).  Note first off that the get_nodes_meta routine can
returns a list of nodes, not just a single node.  This is because multiple
patterns can match the same query-key.

The traversal algorithm is the same as usual except when meta-characters are
encountered.  In this case the state can split into a list of states (similar
to a nondeterministic automata).  That is, instead of just keeping one
current-node, we keep a list of them and update each one for each input
query-key element.  If any current node in the list matches then the key
matches.  If the set becomes empty without a match then the pattern fails to
match.

In the case of a wildcard pattern, suppose the child list of a current node
includes a begin-wildcard element, i.e., a wildcard is one possible
continuation.  Keep in mind that there may be multiple different wildcard
patterns stored in the subtree of the begin-wildcard element.  Now suppose the
next element of the query-key comes in, and we want to move down the trie.  The
state splits.  One new state is just the usual child-node from any literal
continuation which is stored in the tree (as in the non-meta version).  But
also, the algorithm fast-forwards from the current-node's begin-wildcard child
node to the first end-wildcard node in each branch of the subtree beneath it.
Along the way, it is noted whether the current query-key element matches the
wildcard.  For each end-wildcard node which matched the query-key element, the
end-wildcard node is added to the current-node list (and is also checked to see
if it is an end-node of some pattern).

In the case of a repetition pattern, suppose the child list of a current node
includes a begin-repetition element.  In this case the state splits (as before
with wildcards), with the usual literal-character child node becoming one new
state.  But also, the algorithm fast forwards to the corresponding
end-repetition element of each subtree and makes that a new state (we start at
the end to get zero repetitions).  They also remember their begin-repetition
point.  On the next query-key element both states continue, one skipping and
one looping back.

To handle uniqueness issues (if we want each distinct pattern to be represented
by the same pattern-string, with the same associated value) the meta-key
patterns can be preprocessed before they are entered into the TrieDict, to
canonicalize them.  For character wildcards, at least, the characters can be
sorted and all continuous sequences can be turned into character ranges.

Note that we can freely add and remove keys from the tree in the usual way,
since they are stored just as if they were ordinary strings.

The pattern-matching algorithm is currently not optimized at all.  For example,
each time a repetition-loop is encountered it re-processes the entire thing,
searching forward for the end-group element.  This data could instead be cached
(such as by having a dict map begin-repetition nodes to the cached data, valid
until an insertion or deletion invalidates, or the space needs to be freed).
Overriding the children on repetition loops can use up a lot of space for long
repetition patterns.  It is hard to avoid going to the end in the
zero-repetition match form, since the end is a valid continuation At-least-one
repetition patterns could potentially process from the beginning.  Overriding
the insert (__setitem__) method to keep pointers to the loop-ends and
or-sections would speed things up.  A common dict indexed by node ids could be
used, but deletion would have to del the deleted-node entries.  Then we could
start zero-repetition states but just set the stacks for the first loop, fixing
the children in appendChildNode if the stack is not empty.

How should the next version be implemented?  Based on current understanding
from this implementation, the insert method for RegexTrieDict should be
modified to create a virtual trie.  (The delete method also needs to be
modified to fix the virtual trie on deletes.)  This can be hashed on the ids of
nodes, for example, to avoid pasting things onto the actual nodes which must
later be deleted.  The virtual trie can be created by overloading the child
function for nodes.  Like for states currently, but it can be global and saved.
The virtual trie should add virtual nodes for close-repetition and open-group
nodes for 'or's.  This allows the processing for repetitions to keep track of
the loop stacks, and the processing for 'or' sections to keep track of which
paths were originally together in a common pattern, but otherwise the
repetitions should just virtually both loop back and break out, and the 'or'
patterns should be flattened down to an 'or' for each section starting at the
initial open-group and then converging back to a common close-group node (but
note the importance of keeping track of which ones were initially in the same
pattern to avoid crosstalk amongst the patterns).

"""

from __future__ import print_function, division, absolute_import

# Run test cases below when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_regex_trie_dict.py", pytest_args="-v")

import sys
import re
import collections # to use deque and MutableSequence abstract base class
from .trie_dict import TrieDict, TrieDictNode

class NodeStateData(object):

    """This class is used in pattern-searches.  It is just a fancy data-record,
    essentially a named tuple.  It holds one state of a multi-state recognizer.
    The state consists of a node in the trie as well as some extra information
    for keeping track of loops.  These states locally (for the particular state
    and search) redefine the children for each node they have visited and
    followed a child link from.  That particular child is made into the only
    child.

    Each branch in the trie represents a different pattern, connected by "or".
    The pattern-prefix for a node is always the same; a pattern with a different
    prefix would end at a different node.  When a choice of moving down the trie
    is made the prefix is further restricted.  So NodeStateData stores a dict
    for looking up the children of any visited state, and they are all
    restricted to the single path that they took previously.  This way, looping
    backward in a repetition always gives the same pattern prefix.  (States are
    also split into two or more states, as necessary, and run in parallel
    essentially as an NFA.)  Patterns within 'or' groups inside a repetition
    need to be treated similarly, since they may match a different section on
    each repetition.

    The node attribute holds a node in the trie.  The boolean nodeIsEscape is
    true for nodes representing the escape element self.escape.  The stacks are
    used to keep track of looping in repetition patterns.  The
    boundNodeChildDict is used to overload the children of the node, so that the
    same path down the trie is always followed in a repetition loop.  The
    visitedRepNodeIdSet set is a set of the ids all the loopback repetition nodes
    visited, but it is reset each time a literal element is matches the query
    element (i.e., on a literal or a wildcard match).  This is used to avoid
    infinite recursion in processing repetition patterns which match zero
    elements.

    A NodeStateData is initialized by passing all the stored items to the
    initializer, just like initializing a tuple.  Alternately, you can use
    keyword arguments or just assign values to the fields."""

    # Note that we do not inherit from collections.Sequence because then
    # arbitrary attribute assignments are the allowed.  This misses spelling
    # errors and presumably defeats the purpose of __slots__ by having a dict
    # per instance.  Note that the implemented methods are sufficient to loop
    # over NodeStateData objects in for-loops, convert to a tuple or list, etc.
    # If slots are causing problems, like with pickling, you can just globally
    # substitute some other variable name, like _slots, for __slots__ and
    # things should still work (but more space will be used in the trie).
    __slots__ = ["node", "nodeIsEscape", "loopbackStack", "loopCounterStack",
                 "loopBoundsStack", "boundNodeChildDict", "visitedRepNodeIdSet"]

    def __init__(self, *valList, **kwVals):
        self.setVals(*valList, **kwVals)
        return

    def setVals(self, *valList, **kwVals):
        if valList: # either list or kwargs, not both
            if len(valList) != 7:
                raise IndexError
            for count, var in enumerate(self.__slots__):
                self.__setattr__(var, valList[count])
        else:
            for key in kwVals.iterkeys():
                self.__setattr__(key, kwVals[key])
        return

    def __getitem__(self, index):
        return self.__getattribute__(self.__slots__[index])

    def __setitem__(self, index, value):
        return self.__setattr__(self.__slots__[index], value)

    def __len__(self): return len(self.__slots__)

    def copy(self):
        """Return a copy of the state (not a deep copy)."""
        return NodeStateData(self.node, self.nodeIsEscape, self.loopbackStack[:],
                             self.loopCounterStack[:], self.loopBoundsStack[:],
                             self.boundNodeChildDict.copy(), self.visitedRepNodeIdSet.copy())

    def setChild(self, childElem, node=None):
        """Assign the node (default to self.node) to have single child childElem."""
        if node is None: node = self.node
        nodeId = id(node)
        self.boundNodeChildDict[nodeId] = {childElem: node.children[childElem]}
        return

    def children(self, node=None):
        """During patern matches the children of a node are temporarily modified,
        so that ordinary characters in a repetition loop are bound until the
        loop exits.  To do this, the pattern-match routines use the NodeStateData
        children method, rather than the node's children method.  If node is
        set then that node's children dict is returned; default is self.node."""
        # If the trie is not modified, the ids of nodes will not change.  If it
        # is modified then any search-in-progress becomes invalid anyway.
        if node is None: node = self.node
        nodeId = id(node)
        if nodeId in self.boundNodeChildDict:
            return self.boundNodeChildDict[nodeId]
        else:
            return node.children


class NodeStateDataList(collections.MutableSequence):

    """This is essentially just a list, used to hold a collection of
    NodeStateData objects representing the full (nondeterministic) state.  A
    derived class is used so that additional information can be saved.  The
    initialization arguments, if any, are the same as for a list.  In
    particular, this class allows for checking whether or not the state-data is
    still valid in the underlying trie (there might have been insertions and/or
    deletions."""

    def __init__(self, regexTrieDict, *arg, **kwds):
        # The list nodeDataList does the real work.
        self.nodeDataList = list(*arg, **kwds)
        self.insertCount = regexTrieDict.insertCount
        self.deleteCount = regexTrieDict.deleteCount
        self.regexTrieDictInstanceId = id(regexTrieDict)
        self.regexTrieDict = regexTrieDict
        return

    def isValidIn(self, regexTrieDict):
        """Test whether or not the state-data stored in a node data list is still
        valid in the current trie.  If any insertions or deletions have occurred
        since its creation this routine returns False, otherwise True."""
        return (self.regexTrieDictInstanceId == id(regexTrieDict) and
                regexTrieDict.insertCount == self.insertCount and
                regexTrieDict.deleteCount == self.deleteCount)

    def __delitem__(self, index): del self.nodeDataList[index]

    def __getitem__(self, index): return self.nodeDataList[index]

    def __setitem__(self, index, value): self.nodeDataList[index] = value

    def __len__(self): return len(self.nodeDataList)

    def insert(self, index, obj): return self.nodeDataList.insert(index, obj)

    def append(self, item): return self.nodeDataList.append(item)

    def appendChildNode(self, queryElem, nodeData, nodeIsEscape=None):
        """A utility routine that appends a NodeStateData object for a child of
        the node in the one passed in.  The child is the one corresponding to the
        key queryElem.  All other data is kept the same.  Return True if anything
        actually added.  If the nodeIsEscape argument is not None then the
        nodeIsEscape value of the nodeDataState is set to that value.  Note that
        if a NodeStateData instance is added outside of this loop (such as in
        processing wildcards) that routine must take care of any necessary
        setChild calls for binding in repetition loops."""
        # TODO add a copy=True flag to turn off when not needed, after debug

        childrenDict = nodeData.children()
        if queryElem not in childrenDict: return False

        nodeDataCopy = nodeData.copy() # make a copy

        # Fix the overridden children dict of states inside repetition loops
        # to the single child path which is actually traveled down (so later
        # repetitions are for the same pattern in the trie).  Only needed on
        # the first time through, and not for outer-level loops with max of
        # one repetition (which is how 'or' groups are treated).  The latter
        # optimization is not currently implemented.  (It doesn't hurt
        # correctness to always run setChild, but a lot of unnecessary state
        # data is then saved and copied.)
        if nodeData.loopbackStack:  # debug below test, put back later
            #if nodeData.loopCounterStack[-1] == 1 and len(childrenDict) > 1:
            nodeDataCopy.setChild(queryElem) # fix to one child
        else:
            nodeDataCopy.boundNodeChildDict = {} # can't loop back, free the memory
        nodeDataCopy.node = childrenDict[queryElem]

        # Set nodeIsEscape if that arg is given.
        if nodeIsEscape is not None: nodeDataCopy.nodeIsEscape = nodeIsEscape

        self.nodeDataList.append(nodeDataCopy)
        return True


class MagicElem(object):
    """A special element considered unique (checked by id) and used to
    represent a null element that doesn't match anything.  Type doesn't matter
    (since no real comparisons are needed).  Not the same as a null string."""
    def __repr__(self): return "(magicElem)"


# TODO maybe add a non-greedy flag which will work on prexix-matches
# and return the first matches only.  Maybe.  Still might be nice to
# have a break-as-soon-as-possible flag, though if the syntax is
# unambiguous the other way won't branch, anyway.

# TODO add a reset method to the both triedicts (or whatever python dict
# uses for same, if any)

class RegexTrieDict(TrieDict):

    """Subclass of the TrieDict class which adds regex processing for patterns
    stored in the trie."""

    magicElem = MagicElem # A static element that is considered unique (by its id).

    def __init__(self, *args, **kwds):
        super(RegexTrieDict, self).__init__(*args, **kwds)
        self.insertCount = 0 # Used to test if inserts were done.
        self.deleteCount = 0 # Used to test if deletes were done.
        self.nodeDataList = None # Used in sequential meta mode to persist states.
        self.define_meta_elems() # Set the meta-elems to their default definitions.

    def testPatternSequence(self, keySeq, raiseErrors=False):
        """Runs some basic tests on pattern sequences.  This routine is always
        called by the insert method, with raiseErrors=True.  Users can use it to
        test patterns before inserting them.  By default the routine will just
        return a boolean value specifying whether the string passes or not.  If
        raiseErrors=True then the escape-processed version of keySeq is returned
        (assuming no errors are raised).  Note that simply passing the tests is
        no guarantee that the pattern is correct, just that it passes these
        tests."""

        def foundError(errorString, exception=PatternMatchError):
            """Utility function to raise errors or return bool as appropriate."""
            if not raiseErrors: return False
            else: raise exception(errorString)

        def getStringForKeySeq():
            """Try to get a string representation for keySeq for more-helpful
            error messages."""
            try: errPatt = "\n   " + str(keySeq)
            except: errPatt = ""
            return errPatt

        def testMatchedOpenClose(escapedKeyElemList, closeElem, noNest=False):
            """Simple test for mismatched open and close group parens."""
            if escapedKeyElemList[-1][2] != 0:
                if not (escapedKeyElemList[-1][2] == 1 and
                        escapedKeyElemList[-1][0] == closeElem):
                    return False
                if noNest and [t[2] for t in escapedKeyElemList if t[2] > 1]:
                    return False
            return True

        def testNoEmptyOpenClose(escapedKeyElemList):
            """Simple test for open-group paren followed immediately by close-group.
            Any parLevel in escapedKeyElemList must have at least three elements."""
            lastParLevel = -1
            for index, (elem, isEscaped, parLevel) in enumerate(escapedKeyElemList):
                if parLevel > lastParLevel: count = 1
                elif parLevel == lastParLevel: count += 1
                if (parLevel < lastParLevel
                   or (parLevel == 1 and index == len(escapedKeyElemList)-1)):
                    if count < 3:
                        return False
                lastParLevel = parLevel
            return True

        #
        # Begin the main body of the method.
        #

        if len(keySeq) == 0:
            foundError("The empty element is not a valid key.", exception=KeyError)

        # Process the escapes counting open and close wildcard brackets.
        escapedKeyElemList = processElemListForEscapes(keySeq, self.escape,
                                 openGroup=self.lWildcard, closeGroup=self.rWildcard)

        if not testMatchedOpenClose(escapedKeyElemList, self.rWildcard, noNest=True):
            foundError("Mismatched open and close wildcard brackets in pattern."
                       + getStringForKeySeq())

        if not testNoEmptyOpenClose(escapedKeyElemList):
            foundError("Empty open and close wildcard brackets in pattern."
                       + getStringForKeySeq())

        # Process the escapes counting open and close group parens.
        escapedKeyElemList = processElemListForEscapes(keySeq, self.escape,
                                 openGroup=self.lGroup, closeGroup=self.rGroup)

        if not testMatchedOpenClose(escapedKeyElemList, self.rGroup):
            foundError("Mismatched open and close group elements in pattern."
                       + getStringForKeySeq())

        if not testNoEmptyOpenClose(escapedKeyElemList):
            foundError("Empty open and close group elements in pattern."
                       + getStringForKeySeq())

        if raiseErrors: return escapedKeyElemList
        else: return True

    def insert(self, keySeq, data=None):
        """Store the data item in the dict with the key keySeq.  Any existing
        data at that key is overwritten.  This method is aliased to __setitem__.
        It overrides the superclass definition and adds some syntax checking on
        the input patterns."""

        if self.canonicalizeFun: keySeq = self.canonicalizeFun(keySeq)

        escapedKeyElemList = self.testPatternSequence(keySeq, raiseErrors=True)

        node = self.root
        for elem, escaped, _parenCount in escapedKeyElemList:
            if escaped:
                if self.escape in node.children: node = node.children[self.escape]
                else:
                    node.children[self.escape] = TrieDictNode()
                    node = node.children[self.escape]
            if elem in node.children:
                node = node.children[elem]
            else:
                node.children[elem] = TrieDictNode()
                node = node.children[elem]
        if not node.isLastElemOfKey: # Don't increment if just resetting data.
            self.numKeys += 1
            self.insertCount += 1
        node.isLastElemOfKey = True # End of keySeq, isLastElemOfKey is True.
        node.data = data
        return

    def delitem(self, key):
        """Delete the stored key and its data.  Raises KeyError if the key wasn't
        found in the trie.  If d is a dict, the syntax del d[key] also invokes
        this function.  This overrides the delete method of the base class to
        update a deletion counter to test pattern-match state validity."""
        self.deleteCount -= 1
        TrieDict.delitem(self, key)
        return

    def define_meta_elems(self, escape="\\", repetition="*", lGroup="(", rGroup=")",
                        lWildcard="[", rWildcard="]", rangeElem="-",
                        orElem="|", wildcardPattMatchFun=None, elemToDigitFun=None,
                        canonicalizeFun=None):
        """Define the meta-elements in pattern-matching."""
        self.escape = escape
        self.repetition = repetition
        self.lGroup = lGroup
        self.rGroup = rGroup
        self.lWildcard = lWildcard
        self.rWildcard = rWildcard
        self.rangeElem = rangeElem
        self.orElem = orElem
        self.canonicalizeFun = canonicalizeFun
        if wildcardPattMatchFun: self.wildcardPattMatchFun = wildcardPattMatchFun
        else: self.wildcardPattMatchFun = charPatternMatchTest
        if elemToDigitFun: self.elemToDigitFun = elemToDigitFun
        else: self.elemToDigitFun = charElemToInt
        self.escapeMetaElems = {
            repetition, lGroup, rGroup, lWildcard, rWildcard, rangeElem}
        return


    def getDfsGen(self, subtreeRootNode, funToApply=None, includeRoot=False,
                  yieldOnLeaves=True, yieldOnMatch=False, copies=True,
                  stopAtElems=[], stopAtEscapedElems=[],
                  stopAtDepth=False, onlyFollowElems=[],
                  stopIfParenLevelZero=[], firstParenLevel=0,
                  subtreeRootEscaped=False, sortChildren=False,
                  subtreeRootElem=None, childFun=None):
        """Returns a generator which will do a depth-first traversal of the trie,
        starting at node subtreeRootNode.  On each call it returns a list of
        (nodeElem, node) pairs for each node on some path from the root to a leaf
        of the tree.  It generates such a list for each path from the root to a
        leaf (one on each call).  If yieldOnMatch is set True then the current
        list being constructed on a path down the tree is returned on the first
        time any match-marked node is encountered, even if the node is not a
        leaf.  If yieldOnLeaves is set False then yields will only be done on
        matches.  (If both are False then the routine returns nothing.)

        If the list stopAtElems contains any elements then nodes for those
        elements are treated as leaves.  Similarly, stopAtEscapedElems treats
        escaped nodes for an element in the list to be like leaf nodes.  If
        stopAtDepth has a positive integer value then nodes at that depth are
        treated as leaves.  The onlyFollowElems list is like the negation for
        stopAtElems: it treats everything not on the list like a leaf node (i.e.,
        it only folows child-links which are on the list).

        If firstParenLevel is set to a positive integer then that integer will be
        incremented on each open-group meta-elem (self.lGroup) encountered on a
        path and decremented on each close-group meta-elem (self.rGroup)
        encountered on the path.  The default value is zero.  If
        stopIfParenLevelZero is non-empty then any elements in the list will be
        treated as leaves if they are encountered when the paren-count equals
        zero.  Note that paren-counts are updated after the comparison with zero.
        If the root is a node for a left-paren and firstParenLevel=0 then the
        matching right-paren is at paren-level zero.

        If funToApply is defined it will be called for each (nodeElem, node) pair
        on the returned lists.  The function should take two arguments; the list
        will contain the function's return value.  A copy of the node list is
        returned on each generation, but the nodes are always the actual nodes in
        the trie.  If includeRoot is True then output from the subtreeRootNode
        itself will be included in the output (with None as the corresponding
        nodeElem).

        If copies is set False then a single node list is used; this may be a
        little faster, but the returned list will change after each
        generation-cycle.  If sortChildren is True then the children of
        each node will be sorted in the dfs search ordering.

        Setting subtreeRootElem to an element will set that as the element on
        the returned list corresponding to the subtree root (otherwise it is
        None.  Sometimes the value is known when the function call is made,
        and it can be convenient to have a uniform list pattern.

        If childFun is set to a function then the children of a node are obtained
        by calling that function with the node as the argument.  This is helpful,
        for example, in pattern-matches where the child dict is locally modified
        per state."""

        def dfsRecursion(currNodeElem, currNode, depth, isEscaped, parenCount):
            # Put the node on the running list of nodes (down current tree path).
            if depth > 0 or includeRoot:
                if funToApply: nodeList.append(funToApply(currNodeElem, currNode))
                else: nodeList.append((currNodeElem, currNode))
            # Get the current node's child list and modify it.
            if childFun is not None: childDict = childFun(currNode)
            else: childDict = currNode.children
            children = childDict.keys()
            for elem in stopAtElems:
                if elem == currNodeElem: children = []
            for elem in stopAtEscapedElems:
                if isEscaped and elem == currNodeElem: children = []
            if (isEscaped and currNodeElem in stopIfParenLevelZero and
               parenCount == 0):
                children = []
            if stopAtDepth and depth == maxDepth: children = []
            if onlyFollowElems:
                children = [c for c in children if c in onlyFollowElems]
            if sortChildren: children = sorted(children)
            # Update the paren counter.
            if currNodeElem == self.lGroup: parenCount += 1
            if currNodeElem == self.rGroup: parenCount -= 1
            # Yield the results, according to the selected criteria.
            yieldedAlready = False
            if copies: yieldValue = nodeList[:]
            else: yieldValue = nodeList
            if yieldOnLeaves and not children: # match only leaves (or pseudo-leaves)
                yield yieldValue
                yieldedAlready = True
            if yieldOnMatch and currNode.isLastElemOfKey:
                if not yieldedAlready: yield yieldValue
            # Set the escape-value to pass to the recursive calls.
            if currNodeElem == self.escape and not isEscaped: isEscaped = True
            else: isEscaped = False
            # Recurse for each child.
            for elem in children:
                for value in dfsRecursion(elem, childDict[elem], depth+1,
                                          isEscaped, parenCount):
                    yield value
                if nodeList: nodeList.pop() # each child adds one, so pop one

        nodeList = []
        if stopAtDepth != False:
            maxDepth = stopAtDepth
            stopAtDepth = True # because 0 evals to False as a bool

        return dfsRecursion(subtreeRootElem, subtreeRootNode, 0, subtreeRootEscaped,
                            firstParenLevel)

    def get_root_node_data_list(self):
        """This routine returns the initial node data list, for the root node.
        This list is used by getNextNodesMeta to save the state of the
        pattern-matching between iterations.  Multiple instances are allowed."""
        # The tuple of state-information on a NodeDataList is:
        #     (node, nodeIsEscape, loopbackStack, loopCounterStack, loopBoundsStack,
        #                                                       boundNodeChildDict)
        nodeDataList = NodeStateDataList(
            self, [NodeStateData(self.root, False, [], [], [], {}, set())])
        return nodeDataList

    def has_key_meta(self, keySeq):
        """Test of whether the sequence of elements keySeq matches any of the
        regexp patterns stored in the RegexTrieDict.  Returns the number of
        matches.  Remember that any literal escapes in the trie must be
        escaped, but escapes in keySeq are always treated as literal."""
        mat = Matcher(self)
        for elem in keySeq:
            mat.next_key_elem(elem)
            if mat.cannot_match(): 
                mat.reset()
                return 0 # No more nodes, can't match.
        retval = mat.has_key()
        mat.reset() # Ends match and frees memory.
        return retval
        #### This is the earlier implementation, not using seqmeta mode.
        # self.nodeDataList = self.get_root_node_data_list()
        # 
        # for elem in keySeq:
        #     self.nodeDataList = self.getNextNodesMeta(elem, self.nodeDataList)
        #     if not self.nodeDataList: return 0 # no nodes, can't match

        # tmpNodeDataList = self.getNextNodesMeta(self.magicElem, self.nodeDataList)
        # matchedNodes = [nodeData for nodeData in tmpNodeDataList
        #                                       if nodeData[0].isLastElemOfKey]
        # return len(matchedNodes)

    def get_meta(self, keySeq, default=[]):
        """Return a list of the data items of all the stored strings which
        match the sequence of elements keySeq (based on the regexp patterns
        stored in the RegexTrieDict).  The default with no matches is to
        return the empty list.  Remember that any literal escapes in the
        trie must be escaped, but escapes in keySeq are always treated as
        literal."""
        mat = Matcher(self)
        for elem in keySeq:
            mat.next_key_elem(elem)
            if mat.cannot_match(): 
                mat.reset()
                return default # No more nodes, can't match.
        retval = mat.get(default=default)
        mat.reset() # Ends match and frees memory.
        return retval

    # def insert_until_match(self, elem, longest=True, join_elems=True):
    #     """This method is useful for using the RegexTrieDict as part of a
    #     lexical scanner.  Returns False on each insertion until it has
    #     identified the longest prefix of the sequence of inserted elements
    #     which matches a pattern stored in the trie.  (If longest=False it stops
    #     after the first match).  When a match is identified it returns a
    #     two-tuple.  The first component contains the sequence of elements that
    #     gave the longest match (i.e., the matching prefix).  If join_elems is
    #     True these elements will be combined using the addition operator;
    #     otherwise it returns a tuple of the elements.  The second component of
    #     the returned tuple will contain a tuple of each data item stored in the
    #     trie for each pattern that the sequence matched.
    #     
    #     This function resets sequential meta mode and uses it to do the
    #     scanning, so using other sequential meta methods during the process
    #     will invalidate the results."""

    #     # TODO also consider hybrid approaches with Python regex.  Maybe
    #     # use abstract interface to swap implementation and compare...

    #     # TODO have to save the matched elements and return, or change description above...
    #     # Maybe just return the length of the match as first component?  Could
    #     # save the elements themselves, but that adds complexity and may not
    #     # belong in this class... may return multiple matches if elems are saved
    #     # and re-entered automatically after a match.

    #     if not self.scannerMatchMode:
    #         self.scannerMatchMode = True
    #         self.lastMatchedNodes = []
    #         self.reset_seqmeta()

    #     # Insert the element as a key in sequential meta mode.
    #     self.seqmeta_next_key_elem(elem)

    #     # Get any matches at the current length of inserted elements.
    #     tmpNodeDataList = self.getNextNodesMeta(self.magicElem, self.nodeDataList)

    #     # If no more active patterns, we can return the longest found.
    #     if not tmpNodeDataList:
    #         self.scannerMatchMode = False
    #         self.lastMatchedNodes = None
    #         self.reset_seqmeta()
    #         return [n.node.data for n in self.lastMatchedNodes]

    #     # Find any pattern matches at this length of elements; save nodes if found.
    #     matchedNodes = [nodeData for nodeData in tmpNodeDataList
    #                                           if nodeData[0].isLastElemOfKey]
    #     if matchedNodes: self.lastMatchedNodes = matchedNodes
    #     return False

    def _skipNodeDataListEscapes(self, nodeDataList):
        """This routines handles pattern-escapes in a list of node-data tuples.
        This routine is always called by processNodeData as the first step
        (unless it is called with skipEscapes=False).  For each node on
        nodeDataList, a parallel node is added for each escape-element child,
        since that will have to be interpreted specially on the next iteration.
        We are skipping the escape itself but setting a bool in the nodeData
        state to remember it.  If escape is the only child of a node in a
        nodeData tuple then the original nodeData tuple is removed from the list.
        An ordinary list of NodeData instances is OK to pass as nodeDataList.
        The return value is a NodeStateDataList."""
        escapedNodeDataList = NodeStateDataList(self, [])
        for nodeData in nodeDataList:
            if self.escape in nodeData.children():
                # Note copies of the loop states are used.
                nodeDataCopy = nodeData.copy() # TODO can skip copy when single child
                nodeDataCopy.nodeIsEscape = True
                escapedNodeDataList.appendChildNode(self.escape, nodeDataCopy)
                if len(nodeData.children()) == 1: continue # escape is only child
            escapedNodeDataList.append(nodeData) # copy the node over unchanged
        return escapedNodeDataList


    def getNextNodesMeta(self, queryElem, nodeDataList, ignoreValidity=False):
        """Return the list of next nodes for each node on currNodeList,
        interpreting any stored pattern-matching meta-elements, when the
        query-key element queryElem is received.  The nodeDataList should be a
        NodeStateDataList object.  It stores a list of NodeDataState tuples, each
        representing a "live" state of the nondeterministic search.  The tuple
        contains a node in the trie as well as some additional state information.

        See the routine has_key_meta for a simple example of how this method is
        used.

        When queryElem is set to the special value self.magicElem this routine
        has special behavior defined.  It will simply fast-forward up to the
        point where that character would have been compared to the next one in
        the query-pattern.  Then it stops, returning those stop-nodes.  This
        turns out to be very convenient for skipping closing right-group elements
        as well as zero-repetition-matching patterns at the end of a larger key
        pattern.  Recall that to check for a match we need to look at the
        isLastElemOfKey values at the very end of the stored patterns.  Any
        nodeDataList elements which immediately precede a comparison with an
        element or set of elements (a literal character or a wildcard) are left
        unchanged.  Any others move forward to such a point.  Any well-defined
        pattern has such an endpoint."""

        #print("\ndebug getNextNodesMeta call, processing query char", queryElem)
        #print("*"*30)
        #for nd in nodeDataList:
        #   self.printTree(childFun=nd.children)
        #print("*"*30)

        if not ignoreValidity and not nodeDataList.isValidIn(self):
            raise ModifiedTrieError("Invalid nodeDataList, trie has changed.")

        # If we are starting a magicElem search, save the current node with the
        # state.  This is to avoid infinite recursion in processing repetitions
        # that match zero times.  The visitedSet is emptied in processNodeData
        # when a valid end-point is reached.  The appendChildNode routine adds
        # any nodes to the set that it processes, and drops any that would
        # loop.
        # TODO consider just turning off all looping-back in handleEndRepetition...
        # just set a switch here and turn back off at end.  No valid endpoint is
        # at beginning of a loop, anyway.
        self.magicElemNoLoop = False # debug xxx
        if queryElem == self.magicElem:
            self.magicElemNoLoop = True # debug xxx

        # Define the list that will be built-up with next states during the
        # processing.  This object will be the return value of the function.
        nextNodeDataList = NodeStateDataList(self, [])

        for nodeData in nodeDataList:
            # Process the nodeData
            self.processNodeData(queryElem, nodeData, nextNodeDataList)

        return nextNodeDataList


    def processNodeData(self, queryElem, nodeData, nextNodeDataList, skipEscapes=True):
        """Process one nodeData instance, usually from the nodeDataList.  Put the
        results on nextNodeDataList.  This large routine does most of the work in
        the processing, and is called recursively when necessary.  Escapes are
        skipped (generally producing a list of NodeStateData classes) unless
        skipEscapes is set False."""

        #
        # If escapes are to be skipped, recursively process all the resulting nodes.
        #

        if skipEscapes:
            nodeDataList = self._skipNodeDataListEscapes([nodeData])
            for nd in nodeDataList:
                self.processNodeData(queryElem, nd, nextNodeDataList, skipEscapes=False)
            return

        #
        # Handle ordinary, non-escaped nodes in the pattern trie.
        #

        if not nodeData.nodeIsEscape:

            nodeData.visitedRepNodeIdSet = set() # got a literal elem, reset

            if id(queryElem) == id(self.magicElem):
                # The "magic" element doesn't match anything, see header comments.
                nextNodeDataList.append(nodeData) # just keep the node itself

            elif queryElem == self.escape:
                # Can't match because node is neither an escaped escape char nor
                # a meta-pattern.
                pass

            elif queryElem in nodeData.children():
                nextNodeDataList.appendChildNode(queryElem, nodeData,
                                                 nodeIsEscape=False)

        #
        # Handle escaped nodes in the pattern trie, which have special meaning.
        #

        elif nodeData.nodeIsEscape:
            # At a node for an escape-element, see if any meta-element patterns
            # are among its keys.  After the first escape in a pattern we can get
            # a repetition, a left-wildcard bracket, or another escape (to be
            # treated as a literal).  Multiple are possible.  Inside a
            # repetition, which are evaluates as normal sequences, we can also
            # encounter a close-group metacharacter and we have to decide
            # whether or not to loop back.  All other escaped characters at
            # this level are errors.

            nextMetaElems = nodeData.children().keys()

            # Loop through the meta-elems stored at the node and handle each one.
            for metaElem in nextMetaElems:

                #
                # Double escape in a patern matches a single escape in query-key.
                #
                if metaElem == self.escape:
                    if queryElem == self.escape:
                        print("got escaped escape, adding to trie as element")
                        nextNodeDataList.appendChildNode(queryElem, nodeData,
                                                         nodeIsEscape=False)
                    else: # An escaped escape just doesn't match, no new nodeData.
                        continue

                #
                # Handle begin-repetitions.
                #
                elif metaElem == self.repetition:
                    print("debug processing repetition")
                    # Add the repetition subtree leaves to the nextNodeDataList, and
                    # process them as end-repetition events (generating the loop
                    # back and the continuation.  Note that repetition generally
                    # matches zero times, so the end of every repetition pattern is
                    # a valid continuation point.  So we have to go out to the end
                    # nodes and start states for those possibilities.  We only need
                    # one new state for going through the characters inside the
                    # repetition group, however, so after the first one the others
                    # are processed with noLoop=True.

                    # A repetition element must be followed by an open-group element on
                    # each path following it.  Get a generator for those nodes.
                    dfsGenOpen = self.getDfsGen(nodeData.children()[self.repetition],
                                     includeRoot=True, copies=False,
                                     stopAtEscapedElems=[self.lGroup],
                                     childFun=nodeData.children)

                    for treePathToOpenGroup in dfsGenOpen:
                        # Check some error conditions.
                        if len(treePathToOpenGroup) < 3: # root, esc, lGroup
                            raise PatternMatchError(
                                "No open-group following a repetition.")
                        lGroupElem, lGroupNode = treePathToOpenGroup[-1]
                        lGroupEsc, lGroupEscNode = treePathToOpenGroup[-2]
                        #print("   debug treePathToOpenGroup", [ t[0] for t in
                        #                                          treePathToOpenGroup])
                        if lGroupElem != self.lGroup or lGroupEsc != self.escape:
                            raise PatternMatchError(
                                "No open-group element following a repetition element.")
                        iterBounds = self.processRepetitionParams([t[0] for t in
                                                                   treePathToOpenGroup[1:-2]])

                        # If at least one iteration is required then we can avoid having
                        # to find all the closing-group elements for the subtree and
                        # launching new states for each one (they are the zero-repetition
                        # states). We can just start a state and wait and later see which
                        # ones ever reach their end-group loopback points.
                        if iterBounds[0] >= 1: # At least one iteration is required.
                            pass # TODO, just an optimization of what already works

                        # Now, for each opening paren, get a generator for all the closing
                        # parens corresponding to it.
                        dfsGenClose = self.getDfsGen(lGroupNode, includeRoot=True,
                                             copies=False,
                                             stopIfParenLevelZero=[self.rGroup, self.orElem],
                                             firstParenLevel=0, childFun=nodeData.children)
                        setNoLoop = False # whether to only create a break-state, no loop-state
                        for treePathToCloseGroup in dfsGenClose:
                            # Do some error checks.
                            if treePathToCloseGroup[-1][0] == self.orElem:
                                raise PatternMatchError(
                                    "Or element without opening paren.")
                            if len(treePathToCloseGroup) <= 3: # root, esc, rGroup
                                raise PatternMatchError("Illegal empty repetition group "
                                                        "or no close-group for repetition.")
                            rGroupElem, rGroupNode = treePathToCloseGroup[-1]
                            rGroupEsc, rGroupEscNode = treePathToCloseGroup[-2]
                            #print("      debug treePathToCloseGroup", [ t[0] for t in
                            #                                      treePathToCloseGroup ])
                            if rGroupElem != self.rGroup or rGroupEsc != self.escape:
                                raise PatternMatchError(
                                    "No close-group matching a repetition open-group.")

                            #
                            # Push the loopback node on the stack and treat as end repetition.
                            #

                            pushedNodeData = nodeData.copy()
                            # Fix the children of nodes for original repetition and beginning
                            # nodes skipped by dfs, for new state, in case nested
                            # repetitions.
                            pushedNodeData.setChild(
                                self.repetition) # do before node reassign!
                            for i in range(len(treePathToOpenGroup)-1):
                                pushedNodeData.setChild( # elem of next, node of current
                                    treePathToOpenGroup[i+1][0], node=treePathToOpenGroup[i][1])

                            pushedNodeData.node = rGroupNode
                            pushedNodeData.nodeIsEscape = False
                            pushedNodeData.loopbackStack.append(lGroupNode)
                            pushedNodeData.loopCounterStack.append(0)
                            pushedNodeData.loopBoundsStack.append(iterBounds)
                            pushedNodeData.visitedRepNodeIdSet.add(id(lGroupNode))

                            # Process the new pushedNodeData as an end-repetition.
                            self.handleEndRepetition(
                                queryElem, pushedNodeData, nextNodeDataList,
                                noLoop=setNoLoop)
                            setNoLoop = True # We only need one new state at the beginning.

                #
                # Handle end-repetitions.
                #
                elif metaElem == self.rGroup:
                    nodeDataCopy = nodeData.copy() # debug, unnecessary? xxx
                    nodeDataCopy.setChild(self.rGroup) # debug, unnecessary? xxx
                    nodeDataCopy.node = nodeDataCopy.children()[self.rGroup]
                    self.handleEndRepetition(queryElem, nodeDataCopy, nextNodeDataList,
                                             refuseRevisits=True)

                #
                # Handle beginning of an "or" group.
                #
                elif metaElem == self.lGroup:
                    print("debug processing an 'or' group")
                    # TODO if we're not inside a loop or guaranteed to break out then
                    # we can just have a single state at each start point, and not
                    # set the children to single-children, either (consider)...

                    # Treat this entire 'or' group as a one-repetition loop.  This is
                    # so we know what to do then final rGroup is encountered.  The actual
                    # loopback point isn't required and is set to a dummy node.  The
                    # continuation point will either be the next node in the tree when the final
                    # rGroup element is processed as the end of a one-repetition loop,
                    # or else it will be calculated when an orElem ("|") is encountered.

                    # Make a copy of the nodeData state for this start-point.
                    pushedNodeData = nodeData.copy()
                    pushedNodeData.nodeIsEscape = False
                    pushedNodeData.loopbackStack.append(
                        TrieDictNode()) # dummy, popped later
                    pushedNodeData.loopCounterStack.append(1)
                    pushedNodeData.loopBoundsStack.append((1, 1))
                    pushedNodeData.setChild(self.lGroup)

                    # Get a generator generating each path to a corresponding close-group.
                    # Note the current implementation is inefficient.
                    currNode = pushedNodeData.children()[self.lGroup]
                    dfsGenOrGroupEnd = self.getDfsGen(currNode, includeRoot=True,
                                           copies=False, stopIfParenLevelZero=[self.rGroup],
                                           firstParenLevel=-1,
                                           childFun=pushedNodeData.children,
                                           subtreeRootElem=self.lGroup)

                    # Iterate the generator, creating states for each section of each tree
                    # path.
                    for treePathToEndGroup in dfsGenOrGroupEnd:
                        print("debug", [i[0] for i in treePathToEndGroup])
                        print("debug, queryElem is", queryElem)
                        # Do some error checks.
                        if len(treePathToEndGroup) <= 3: # root, esc, rGroup # TODO check
                            raise PatternMatchError("Illegal empty 'or' group "
                                                    "or no close-group for 'or'.")
                        if (treePathToEndGroup[-2][0] != self.escape
                           or treePathToEndGroup[-1][0] != self.rGroup):
                            raise PatternMatchError(
                                "No close-group matching an 'or' open-group.")

                        # Fix all the nodes on the path to have one child (for these states).
                        # We make a new copy for each path to the closing rGroup, though.
                        pushedNodeDataCopy = pushedNodeData.copy()
                        for i in range(len(treePathToEndGroup)-1):
                            pushedNodeDataCopy.setChild( # elem of next, node of current
                                treePathToEndGroup[i+1][0], node=treePathToEndGroup[i][1])

                        # Find all the begin-section markers inside the 'or' group and start a
                        # new state for each one (i.e., opening paren and all '|'
                        # elements).
                        escaped = True # The first lGroup is escaped.
                        parenCount = 0
                        for i in range(len(treePathToEndGroup)):
                            elem, node = treePathToEndGroup[i]
                            # TODO check that each one has at least something in it....
                            if not escaped and elem == self.escape:
                                escaped = True
                                continue
                            # Always escaped past here.
                            if elem == self.lGroup: parenCount += 1
                            elif elem == self.rGroup: parenCount -= 1
                            # If not a valid open-group section then continue.
                            if parenCount != 1 or (
                                          elem != self.orElem and elem != self.lGroup):
                                continue
                            # Create a state for currNode (copy of general state) and
                            # process it.
                            orSectionBegin = pushedNodeDataCopy.copy()
                            orSectionBegin.node = node
                            self.processNodeData(
                                queryElem, orSectionBegin, nextNodeDataList)
                            escaped = False

                #
                # Handle end of non-final end-section of an or-group (an orElem "|").
                #
                elif metaElem == self.orElem:
                    print("debug processing non-final 'or' group section")
                    print(
                        "   debug nodeData.loopCounterStack is", nodeData.loopCounterStack)
                    # Get a generator for all final rGroup elems at this point.  There
                    # should only be one, since the state was fixed to single-children
                    # when the 'or' was first processed.
                    dfsGenOrSectionEnd = self.getDfsGen(nodeData.children()[self.orElem],
                                                        includeRoot=True, copies=False,
                                                        stopIfParenLevelZero=[
                                                            self.rGroup], firstParenLevel=0,
                                                        childFun=nodeData.children)
                    for count, treePath in enumerate(dfsGenOrSectionEnd):
                        print("      debug treePathToEndSection in process 'or' elem",
                              [t[0] for t in treePath])
                        # Errors checked earlier, when lGroup of the 'or' was processed.
                        rGroupElem, rGroupNode = treePath[-1]
                        nodeDataCopy = nodeData.copy() # debug, unneeded?? xxx
                        nodeDataCopy.setChild(self.orElem) # debug, unnecessary?? xxx
                        self.handleEndRepetition(
                            queryElem, nodeDataCopy, nextNodeDataList,
                            replaceNode=rGroupNode)
                        # When 'or' is set it should give all nodes for state
                        # single-children
                        if count > 0: # This is just a consistency-check assertion.
                            raise PatternMatchError("Failure in 'or' child-setting.")

                #
                # Handle wildcards.
                #
                elif metaElem == self.lWildcard:
                    print("debug processing wildcard")
                    # Generate all the subtree rWildcard nodes, checking that the
                    # pattern matches.

                    nodeData.visitedRepNodeIdSet = set() # got an actual elem, reset

                    # Magic elem doesn't match any char; just put current node on
                    # nextNodeDataList (so isLastElemOfKey can be checked).
                    if id(queryElem) == id(self.magicElem):
                        nextNodeDataList.append(nodeData) # just keep the node itself
                        continue # process the next metaElem in the loop

                    wildcardPattGen = self.getDfsGen(nodeData.children()[self.lWildcard],
                                          includeRoot=True, copies=False,
                                          childFun=nodeData.children,
                                          stopAtEscapedElems=[self.rWildcard])

                    for wildcardPatt in wildcardPattGen:
                        # print("      debug wildcard patt", [ t[0] for t in wildcardPatt
                        # ])
                        if len(wildcardPatt) <= 3: # root, some char, esc, rWildcard
                            raise PatternMatchError("No closing bracket for wildcard.")
                        rWildcardElem, rWildcardNode = wildcardPatt[-1]
                        escapeElem, escapeNode = wildcardPatt[-2]
                        if (rWildcardElem != self.rWildcard or escapeElem != self.escape):
                            raise PatternMatchError("No closing bracket for wildcard.")
                        pattern = [p[0] for p in wildcardPatt[1:-2]]
                        # If the character matches the wildcard pattern:
                        # 1) Get a copy NodeStateData.
                        # 2) In the NodeDataState, fix all the nodes on the path to the
                        #    end-element to have one child (the new NodeStateData now
                        #    represents just one pattern instance, not the full subtree).
                        # 3) Append the node to nextNodeDataList.
                        if self.wildcardPattMatchFun(queryElem, pattern,
                                                     self.rangeElem, self.escape):
                            nodeDataCopy = nodeData.copy()
                            nodeDataCopy.setChild(self.lWildcard)

                            # Fix the children on the wildcardPatt list to only have
                            # one child.
                            for i in range(len(wildcardPatt)-1):
                                nodeDataCopy.setChild( # elem of next, node of current
                                    wildcardPatt[i+1][0], node=wildcardPatt[i][1])

                            # Set the other elements and append to nextNodeDataList.
                            nodeDataCopy.node = rWildcardNode # stacks do not change
                            nodeDataCopy.nodeIsEscape = False
                            nextNodeDataList.append(nodeDataCopy)

                #
                # Error condition otherwise.
                #
                else:
                    # An escape in some pattern isn't followed by a valid meta-elem,
                    # like r"\Z".
                    raise PatternMatchError(
                        "Invalid meta-element (unknown escaped element) encountered."
                        "\nQuery element is: " + str(queryElem) +
                        "\nNode's children are:\n   " + str(nextMetaElems))

        return


    def handleEndRepetition(self, queryElem, closeParenNodeData, nextNodeDataList,
                            replaceNode=None, noLoop=False, noBreak=False,
                            refuseRevisits=False):
        """Handle reaching the close of a repetition group.  The closeParenNode
        should be the node corresponding to the closing repetition-group.  If
        replaceNode is set to a node then it replaces the node in closeParenNodeData
        as the new node to jump to after a breaking a loop (used in inside sections
        of 'or' patterns)."""

        """
        TODO, consider
        Greedy loops would be useful.  They always match as many loops around
        as possible, even if that causes the larger pattern to fail.  This avoids
        some worst-case scenarios.
  
        To implement: we need to link/entangle the two states that are produced
        at the end of a loop.  Then, if the loopback state makes it through
        another iteration, back to here in handleEndRepetition, it somehow signals
        that other state to die.  But, if it doesn't complete another loop, that
        state goes on.  (Note zero-repetition loops might be a problem...)
  
        Similarly, if two 'or' sections match we only need to keep one state... NO,
        it will NOT necessarily result in the same pattern in the end.  The sections
        can have different numbers of elements, and can have loops, etc.  We'd need
        to use a greedy or non-greedy rule again.  So the last-exiting match or
        first-exiting match could kill all the others.  We always come here to exit
        the 'or' sections, since they are treated as loops at the outer level.
  
        See partial start commented out below.... one problem is that we need
        unique IDs for NodeStateData instances, even across copy operations...
        but do *some* copy operations need to get a new ID????
  
  
        What if we just set the IDs here for the generated NodeStateData copies?
        Then copy could just preserve it.  Presumably anything derived from a state
        that is killed should also be killed!  So then we just put the ID on a kill
        list and at the end of the main routine we go through and remove those states.
  
        """

        print("debug in handleEndRepetition")

        if not (closeParenNodeData.loopbackStack and closeParenNodeData.loopCounterStack
                and closeParenNodeData.loopBoundsStack):
            raise PatternMatchError(
                "IndexError on a stack pop, probably mismatched parentheses.")

        # Here we branch the state into one branch that repeats the loop,
        # and one that breaks out of the loop.  Note that ordinary lists are
        # OK here, we have already checked validity of the original list and
        # any new NodeStateData instances will be added to nextNodeDataList,
        # which is already a NodeStateDataList.

        # Get some preliminary values needed for conditional tests.
        openParenNode = closeParenNodeData.loopbackStack[
            -1] # loop back to open-group node
        loopCount = closeParenNodeData.loopCounterStack[-1]
        loopBoundMin, loopBoundMax = closeParenNodeData.loopBoundsStack[-1]

        # The nodeData for breaking out of the loop.  If loopCount is below
        # loopBoundMin then we cannot break the loop yet.
        # TODO: free up the stored boundNodeChildDict for breaks to empty stack level 0.
        # TODO when the boundNodeChildList is set in repetition we don't need to copy it.
        if loopCount >= loopBoundMin and not noBreak:
            breakNodeData = closeParenNodeData.copy()
            if replaceNode is not None: breakNodeData.node = replaceNode
            breakNodeData.nodeIsEscape = False
            breakNodeData.loopbackStack.pop()
            breakNodeData.loopCounterStack.pop()
            breakNodeData.loopBoundsStack.pop()
            breakNodeDataList = [breakNodeData]
        else:
            breakNodeDataList = []

        if self.magicElemNoLoop or (refuseRevisits and
                                    id(closeParenNodeData.loopbackStack[-1])
                                    in closeParenNodeData.visitedRepNodeIdSet):
            noLoop = True

        # The nodeData for continuing the loop.  If loopCount is above
        # loopBoundMax then we cannot continue the loop and can only break.
        if (loopCount < loopBoundMax or loopBoundMax == -1) and not noLoop:
            loopNodeData = closeParenNodeData.copy()
            loopNodeData.nodeIsEscape = False
            loopNodeData.node = loopNodeData.loopbackStack[
                -1] # loop back to open-group node
            loopNodeData.loopCounterStack[-1] += 1 # increment the loop counter
            loopNodeData.visitedRepNodeIdSet.add(id(loopNodeData.node))
            loopNodeDataList = [loopNodeData]
        else:
            loopNodeDataList = []

        """
        # "Entangle" any pairs, for greedy repetition-matching.
        if breakNodeDataList and loopNodeDataList:
           pairTuple = (id(breakNodeData), id(loopNodeData))
           if pairTuple in self.entangledStatePairsSet:
              self.stateKillList.append(....)......consider
           self.entangledStatePairsSet.add( (id(breakNodeData), id(loopNodeData)) )
        """

        # Combine any nodes generated.
        nodeDataList = breakNodeDataList + loopNodeDataList

        # Detect problem with ill-formed pattern of a repetition as the only
        # thing inside another repetition.  If the loop's break-point leads to
        # a close-group and its loopback leads to a repetition then it is the
        # open-repetition for the loop we just broke out of (because open and
        # close groups are strictly nested).  So that is one way to detect an
        # illegal nested repetition with no other elements in the outer
        # repetition.  If not caught, this will cause an infinite recursion.
        # (The infinite recursion occurs because the processing below will
        # immediately process the repetition again, fast-forward to the end,
        # and we're right back here again.)
        #
        # Note this could be allowed now, since infinite recursions are
        # limited, but keep it as an error since it likely isn't intended.
        if breakNodeDataList:
            try:
                closeParenNode = closeParenNodeData.node
                closeParenNode.children[self.escape].children[self.rGroup]
                breakNodeData.loopbackStack[-1].children[
                    self.escape].children[self.repetition]
                raise PatternMatchError("Illegal nested repetition pattern with no"
                                        " characters in the outer repetition.")
            except KeyError:
                pass
            except IndexError:
                raise PatternMatchError(
                    "Index error on stack pop, probably mismatched parentheses.")

        # The repetition meta-characters do not count as a character of the
        # literal pattern being matched to queryElem, so run processNodeData on the
        # computed nodes.
        for nodeData in nodeDataList:
            self.processNodeData(queryElem, nodeData, nextNodeDataList)

        return

    def processRepetitionParams(self, seq):
        """Process the sequence between the begin-repetition and the open-group
        that necessarily follows it.  Return a tuple (minIter, maxIter), with
        -1 for infinite maxIter."""
        tupleList = processElemListForEscapes(seq, self.escape)
        val = 0 # The first value if only one, or the second otherwise.
        firstVal = 0 # The second value, if there is one.
        valSet = False
        for elem, escaped in tupleList:
            if escaped:
                if elem == self.repetition:
                    if not valSet:
                        # Note first value could default to zero if empty, but that would
                        # add a redundant representation for such patterns.
                        raise PatternMatchError("No value in first slot of two-place"
                                                " repetition bounds specification.")
                    firstVal = val # a second value will follow
                    val = 0
                else:
                    raise PatternMatchError("Bad escaped character in repetition"
                                            " bounds specification.")
            else:
                digit = self.elemToDigitFun(elem)
                val = 10*val + digit
                valSet = True
        if not firstVal: return (val, -1)
        return (firstVal, val)

    #
    # Define a few aliases/synonyms for certain methods above.
    #

    """Synonym for delitem."""
    __delitem__ = delitem

    """Synonyms for insert."""
    __setitem__ = setitem = insert


class Matcher(object):
    """Insert elements sequentially, and check whether they match any regex
    patterns stored in the trie.  No trie modifications can be made between
    inserting any key elements and testing for matches, or ModifiedTrieError
    will be raised."""
    # Should resets be automatic when we get a ModifiedTrieError, or should we
    # just let the error go?  Flag auto_reset_on_triemod?
    def __init__(self, regexTrieDict):
        """Initialize with a particular RegexTrieDict instance."""
        # Note that we have to deal with patterns that match the empty string.
        # We also want to wait until the last possible chance to do a reset,
        # so that we get a NodeStateDataList for the root that is fresh with
        # respect to any inserts which were done after the initialization.
        self.reset(regexTrieDict)

    def next_key_elem(self, elem):
        """ Inserts elem as the next elem of the key sequence.  (Note elem is
        usually a character if string patterns are stored in the tree.)"""

        if not self.matchInProgress: self._set_to_root()
        # Update the list of node data states according to the element elem.
        self.nodeDataList = self.rtd.getNextNodesMeta(elem, self.nodeDataList)
        return

    def cannot_match(self, insert_magic=False):
        """Return True if no matches are possible with further elements inserted
        with next_key_elem.  This is determined by whether or not there are any
        active patterns in the current state."""
        return not bool(self.nodeDataList)

    def reset(self, regexTrieDict=None):
        """Resets the Matcher and frees any state memory.  A new trie can
        optionally be passed in."""
        if regexTrieDict is not None: self.rtd = regexTrieDict
        self.matchInProgress = False
        self.nodeDataList = None # Free any memory for the garbage collector.

    def _set_to_root(self):
        """Utility routine to set the state back to the root of the trie."""
        self.matchInProgress = True
        self.nodeDataList = self.rtd.get_root_node_data_list()

    def has_key(self):
        """Tests whether the sequence of elements inserted by the
        next_key_elem function match any of the regexp patterns stored
        in the RegexTrieDict.  Returns the number of matches.  Remember that
        any literal escapes in the trie must be escaped, but escapes in keySeq
        are always treated as literal."""
        # See get method for comments on what's going on here.
        if not self.matchInProgress: self._set_to_root()
        tmpNodeDataList = self.rtd.getNextNodesMeta(self.rtd.magicElem, self.nodeDataList)
        matchedNodes = [nodeData for nodeData in tmpNodeDataList
                                              if nodeData[0].isLastElemOfKey]
        return len(matchedNodes)

    def get(self, default=[]):
        """Return a list of the data items of all the stored strings which
        match the sequence of elements which have been inserted with the
        next_key_elem function.  That defines the keySeq and the match
        is based on the regexp patterns stored in the RegexTrieDict.  The
        default with no matches is to return the empty list.  Remember that any
        literal escapes in the trie must be escaped, but escapes in keySeq are
        always treated as literal."""
        if not self.matchInProgress: self._set_to_root()
        # We insert a null magic element, which by definition does not match
        # any element actually in the string.  This has the side-effect of
        # moving us past any self.rGroup closing elements, as well as past any
        # patterns which can match zero times.  Note that inserting the empty
        # keySeq will skip the loop above and go directly to the magic element
        # queryElem below.  Get a temporary state after inserting the
        # magicElem, so that we don't mess up the persistent self.nodeDataList
        # for later searches.
        tmpNodeDataList = self.rtd.getNextNodesMeta(self.rtd.magicElem, self.nodeDataList)
        matchedNodes = [nodeData for nodeData in tmpNodeDataList
                                              if nodeData[0].isLastElemOfKey]
        if not matchedNodes:
            return default
        return [n.node.data for n in matchedNodes]


def charElemToInt(elem):
    """This routine is set in defineMetaElems as the default value of
    elemToDigitFun, which converts elements to digit values.  Used in calculating
    repetition bounds.  It is the default setting for elemToDigitFun, when
    the elements are characters."""
    try:
        intVal = int(elem)
    except ValueError:
        raise PatternMatchError("Bad digit in repetition bounds specification")
    return intVal


def charPatternMatchTest(queryElem, pattList, rangeElem, escapeElem):
    """This utility routine does a pattern-match for characters in the wildcard
    brackets.  It does depend on the elements being characters, since it calls
    a Python regexp.  This has the advantage of allowing all the
    special-characters in Python regexp wildcards to be used.  This is the
    default routine set in defineMetaElems as wildcardPattMatchFun, for when
    elements are characters."""
    if not pattList:
        raise PatternMatchError("No pattern in wildcard brackets.")

    # print("debug charPatternMatchTest, query elem is", queryElem, "pattList
    # is", pattList)

    pattTupleList = processElemListForEscapes(pattList, escapeElem)
    #print("debug elemList processed for escapes is", pattTupleList)
    pythonString = "^["
    firstChar = True
    for elem, escaped in pattTupleList:
        if escaped:
            if firstChar and elem == "^":
                pythonString += "^"
                continue
            if elem != "-": pythonString += "\\"
        pythonString += elem
        firstChar = False
    pythonString += "]$"
    retval = re.match(pythonString, queryElem)
    # print("debug charPatternMatchTest, pythonString is", pythonString,
    # "returning", bool(retval))
    return retval


def charRangeTest(charLower, charUpper, testChar):
    """Return True if testChar is in the range from char1 to char2, inclusive.
    Used in testing wildcard patterns in the default with character elements."""
    print("debug in charRangeTest, comparing queryElem",
          testChar, "with lower range", charLower)
    if ord(charLower) > ord(charUpper):
        raise PatternMatchError("Second element in character range greater than lower.")
    return ord(charLower) <= ord(testChar) and ord(testChar) <= ord(charUpper)


def genericWildcardMatchFun(queryElem, pattList, rangeElem, escapeElem,
                            rangeTestFun=charRangeTest):
    """This utility routine does a generic pattern-match in the wildcard
    brackets.  This routine is for general sequences of elements and does not
    depend on the elements being characters.  Only the function rangeTestFun
    needs to be defined.  The argument pattList is the content of a wildcard
    bracket, as a list of elements.  This function tests whether queryElem
    matches the character pattern.  To simply redefine the range-test function
    for elements, use something like:

       def myPattMatchFun(queryElem, pattList, rangeElem, escapeElem):
          return genericWildcardMatchFun(queryElem, pattList, rangeElem, escapeElem,
                                         rangeTestFun=myRangeTestFun)

    Then in calling defineMetaElems define wildcardPattMatchFun=myPattMatchFun.
    """

    #print("debug processing pattern pattList", pattList, "for queryElem", queryElem)
    if not pattList:
        raise PatternMatchError("No pattern in wildcard brackets.")

    pattTupleList = processElemListForEscapes(pattList, escapeElem)
    foundRangeElem = False
    stack = []
    for elem, escaped in pattTupleList:
        if escaped:
            if elem == rangeElem:
                foundRangeElem = True
                continue
            else:
                raise PatternMatchError("Invalid escaped elem in wildcard pattern.")
        else:
            if foundRangeElem:
                foundRangeElem = False
                if not stack:
                    raise PatternMatchError("No elem before range elem in wildcard.")
                if rangeTestFun(stack.pop(), elem, queryElem): return True
            else:
                stack.append(elem)
    for elem in stack:
        if rangeTestFun(elem, elem, queryElem): return True
    return False


def processElemListForEscapes(elemList, escapeChar, openGroup=None, closeGroup=None):
    """A utility routine which takes a list of possibly-escaped elements as an
    argument and returns a list of two-tuples.  The first element of a two-tuple
    is the actual character, and the second a boolean for whether or not it is
    escaped.  If openGroup and/or closeGroup is set it returns a three-tuple,
    where the last element gives the level of parenthesis nesting, starting at zero
    and increasing.  An open and its corresponding close have the same level."""
    escaped = False
    tupleList = []
    pCount = 0
    for elem in elemList:
        if elem == escapeChar and not escaped:
            escaped = True
            continue
        if escaped:
            boolVal = True
            if openGroup and elem == openGroup: pCount += 1
        else:
            boolVal = False
        if openGroup or closeGroup: tupleList.append((elem, boolVal, pCount))
        else: tupleList.append((elem, boolVal))
        if escaped:
            escaped = False
            if closeGroup and elem == closeGroup: pCount -= 1
    return tupleList


#
# Exception classes for RegexTrieDict.
#


class RegexTrieDictError(Exception):
    pass

class PatternMatchError(RegexTrieDictError):
    pass

class ModifiedTrieError(RegexTrieDictError):
    pass



