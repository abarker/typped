"""

TODO: document the MatchObject interface.

Introduction to `RegexTrieDict`
===============================

The `RegexTrieDict` class is a subclass of `TrieDict` which adds
pattern-matching capabilities.  To use these capabilities, you first create a
key which contains some special meta-elements (which are meta-characters when
the keys are strings).  Then you insert that key (meta-key) and an associated
value into the `RegexTrieDict`.  These meta-keys are treated simply as ordinary
keys by the usual `TrieDict` operations.  The `RegexTrieDict` adds some special
methods, `has_key_meta` and `get_meta`, which interpret the meta-keys as
regex patterns on queries, rather than treating them as ordinary saved items.

The `RegexTrieDict` class implements a full (though bare-bones) regex
pattern-matching language.  The syntax of the language is slightly modified due
to the tree structure in which the patterns are stored.  Basically, the special
characters are made into prefix operators rather than postfix operators so that
they are encountered first when going down the trie from the root.  The syntax
is described in a later section.

Terminology
===========

The system is defined to be very general, working on **sequences** made up of
**elements**.  Sequences are just iterable objects which return some fixed,
hashable kind of object as elements when iterated over.  Both the stored regex
patterns and the query keys are sequences.

The most common case is that the sequences are Python strings, and the elements
are the characters of the strings.  In reading the documentation it can be
helpful to think of the example where sequences=strings and
elements=characters.  Sequences do not need to be strings, however.  For
example, the sequences could be lists of strings.  In that case the elements
would be the strings on those lists.  Regex patterns would then be lists of
strings with certain of those strings designated as escape elements and
pattern-defining elements.

Meta-methods
============

As mentioned above, the `RegexTrieDict` adds some methods to the ordinary
`TrieDict` to allow for keys stored in the dict to be intepreted at a
meta-level, as regex patterns.  The two main methods are `has_key_meta` and
`get_meta`.

Every `RegexTrieDict` is also a `TrieDict`, and items can be stored in and
retrieved from it using the methods of the `TrieDict` class.  A `RegexTrieDict`
used for pattern matching is basically a `TrieDict` which has had regex
patterns inserted into it as the "ordinary" keys.  It then provides special
meta-methods which interpret those stored keys as regex patterns during
queries, rather than as ordinary sequences.

The `has_key_meta` and `get_meta` methods take ordinary sequences as arguments,
i.e., they to not take pattern-sequences as arguments.  Instead, they match the
ordinary sequence as a key against the patterns in the trie, interpreting the
sequences stored in the trie as regex patterns.  So calling `has_key_meta` for
an ordinary, non-meta key returns `True` if any current key in the trie matches
exactly *or* if any meta-key pattern in the trie matches as a pattern.
Similarly, `get_meta` finds any key or meta-key pattern in the trie which
matches and returns a tuple containing all the data associated with those
matching items.

The special meta-symbol elements which make up patterns can be user-defined,
but their interpretations as meta-symbols are fixed.  By default the values are
defined assuming that keys are strings of characters.  (In general, keys can be
sequences of any kind of hashable elements.)  The default definitions are as
follows::

   define_meta_elems(escape="\\", repetition="*", lGroup="(", rGroup=")",
                     lWildcard="[", rWildcard="]", rangeElem="-", orElem="|")

In order for an element of a key to be interpreted as a meta-symbol it **must**
be preceded by the defined escape element.  So if the key is a string the
meta-symbols with their default definitions above would always appear as
`"\\*"`, `"\\("`, `"\\)"`, `"\\["`, `"\\]"`, `"\\-"`, and `"\\|"`.  If the
backslash character is the escape then it is convenient to use raw strings like
`r"\*"` and `"r\("`.  (But remember that in Python raw strings cannot end with
a single backslash.)

The requirement that all meta-elements must be escaped is intended to minimize
interference with ordinary key elements.  There are no exceptions, so it is a
consistent rule which does not require memorization of which elements need to
be escaped and which do not.  As usual, a double escape such as `"\\\\"` (or
`r"\\"`) reverts to the original escape symbol, as does an escape not followed
by any of the defined meta-elements.

Keep in mind that when a `RegexTrieDict` is used with escaped elements in the
keys, to be treated as meta-elements, all the literal escape-elements in the
keys must be escaped as described above.  Even keys consisting of ordinary text
which are inserted into the `RegexTrieDict` to be used as a pattern with the
meta routines must have any escape elements escaped.  The escape element as a
literal must also be escaped.  Ordinary strings as regexes are still regex
patterns, just simple ones.  In the query sequences (query keys), however,
escape elements are always simply treated as literals.  That is, no
meta-interpretation is ever performed on the query-key sequences and the escape
character has no special meaning.  So a single escape on a key-query matches an
escaped escape in the stored key-sequences in the Trie.

The meta-level and the object-level are intentionally kept distinct in order to
minimize some of the confusions that can occur.  The ordinary dict methods
always operate on trie keys as if they were literals.  To get an extra level of
interpretation, the special meta-level methods must be used.  These operations,
however, can be freely mixed.  Patterns can be inserted and deleted on the fly,
etc.

Pattern scanning is left-to-right in a key's elements.  For example, with
strings as the keys the characters of the key strings are scanned from left to
right.

Syntax of the regex language
============================

The `RegexTrieDict` class implements all the basic regex patterns (though not
the fancier ones that some regex implementations allow for).  Because it needs
to be implemented in a trie, however, some of the usual syntax is modified.  In
particular, the operations like `*` which are usually postfix operations are
instead prefix operations so they are encountered first when walking down the
trie.

The language allows for single-character wildcards, i.e., wildcards which match
a single character from some set of possibilities.  As an example with strings,
consider these patterns are valid meta-keys using wildcards::

   patt1 = "abc\\[123\\]def"
   patt2 = "abc\\[1\\-3\\]def"

The first pattern, `patt1`, matches `abc1def`, `abd2def`, and `abc3def` on
meta-queries.  The second pattern, `patt2`, matches the same strings but uses a
range specifier.

<TODO note we now let them define the whole wildcard-processing...>

The boolean-valued function `rangeTestFun` will be called for the first and
last argument of the range, along with the element to test.  The default
range-function (when the values is set to `None` in the call to
`defineMetaElems`) only works for character ranges.

<TODO note that Python patterns are allowed, and test some.>

<TODO note that user can essentially redefine the processing of the part inside
the brackets in any way desired.>

Repetition patterns match zero or more occurrences of the pattern group.  Here
is an example with strings as keys::

   patt1 = "abc\\*\\(DD\\)efg"

This would match "abcefg", "abcDDefg", abcDDDDefg", etc.  The repetition
pattern can also take optional numeric arguments, each separated by another
asterick.  A single numeric argument, like in ::

   patt = "abc\\*10\\(DD\\)efg

specifies a minimum number of repetitions.  The previous example must have
ten or more occurrences of `"dd"` in it.  So `"abcDDefg"` would not match,
but `"abcDDDDDDDDDDDDDDDDDDDDefg"` would match.  When two numbers are given
they represent the minimum and the maximum number of repetitions, respectively.
So the pattern ::

   patt = "abc\\*2\\*3\\(DD\\)efg"

would not match `"abcDDefg"`, would match `"abcDDDDefg"` and `"abcDDDDDDefg"`, and
would not match `"abcDDDDDDDDefg"`.

<TODO below para not implemented>

The grouping meta-elements must occur just after the start and at the end of
the repetition pattern itself.  For efficiency, repetition can be limited such
that it always "breaks out" of the "loop" at the first chance it gets.   This
occurs when, at the end of a loop, the next element scanned matches the next
pattern element after the closing repetition meta-element.  So the shortest
valid repetition sequence followed by some other valid element is always
chosen.  This restriction essentially requires the end of any repeated-pattern
segment to be unambiguous (or else no looping-back will occur).

Pattern-matching implementation details
=======================================

As noted, meta-keys are inserted and stored in a `TrieDict` simply as ordinary
keys.  It is the traversal algorithm in the routine `get_nodes_meta` which
differs from the usual `get_node` algorithm of an ordinary `TrieDict` (which
finds the node in the trie corresponding to a key).  Note first off that the
`get_nodes_meta` routine can returns a list of nodes, not just a single node.
This is because multiple patterns can match the same query-key.

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
end-repetition element of each subtree and makes that a new state (the state
starts at the end in order to match zero repetitions).  They also remember
their begin-repetition point.  On the next query-key element both states
continue, one skipping and one looping back.

To handle uniqueness issues (if we want each distinct pattern to be represented
by the same pattern-string, with the same associated value) the meta-key
patterns can be preprocessed before they are entered into the `TrieDict`, to
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
zero-repetition match form, since the end is a valid continuation.
At-least-one repetition patterns could potentially process from the beginning.
Overriding the insert (`__setitem__`) method to keep pointers to the loop-ends
and or-sections would speed things up.  A common dict indexed by node ids could
be used, but deletion would have to del the deleted-node entries.  Then
zero-repetition states could be started but just set the stacks for the first
loop, fixing the children in `append_child_node` if the stack is not empty.

.. note::

    If this algorithm were to be rewritten (partly or in whole), based on
    experience from this implementation, what should change?  The insert method
    for `RegexTrieDict` should probably be modified to create a virtual trie.
    (The delete method would also need to be modified to fix the virtual trie
    on deletes.) This can be hashed on the ids of nodes, for example, to avoid
    pasting things onto the actual nodes which must later be deleted.  The
    virtual trie can be created by overloading the child function for nodes.
    Like for states currently, but it can be global and saved.  The virtual
    trie should add virtual nodes for close-repetition and open-group nodes for
    'or's.  This would allow the processing for repetitions to keep track of
    the loop stacks, and the processing for 'or' sections to keep track of
    which paths were originally together in a common pattern, but otherwise the
    repetitions should just virtually both loop back and break out, and the
    'or' patterns should be flattened down to an 'or' for each section starting
    at the initial open-group and then converging back to a common close-group
    node (but note the importance of keeping track of which ones were initially
    in the same pattern to avoid crosstalk amongst the patterns).

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
    The pattern-prefix for a node is always the same; a pattern with a
    different prefix would end at a different node.  When a choice of moving
    down the trie is made the prefix is further restricted.  So `NodeStateData`
    stores a dict for looking up the children of any visited state, and they
    are all restricted to the single path that they took previously.  This way,
    looping back in a repetition always gives the same pattern prefix.  (States
    are also split into two or more states, as necessary, and run in parallel
    essentially as an NFA.)  Patterns within 'or' groups inside a repetition
    need to be treated similarly, since they may match a different section on
    each repetition.

    The node attribute holds a node in the trie.  The boolean `node_is_escape` is
    true for nodes representing the escape element `self.escape`.  The stacks are
    used to keep track of looping in repetition patterns.  The
    `bound_node_child_dict` is used to overload the children of the node, so that
    the same path down the trie is always followed in a repetition loop.  The
    `visited_rep_node_id_set` set is a set of the ids all the loopback repetition
    nodes visited, but it is reset each time a literal element is matches the
    query element (i.e., on a literal or a wildcard match).  This is used to
    avoid infinite recursion in processing repetition patterns which match zero
    elements.

    A `NodeStateData` is initialized by passing all the stored items to the
    initializer, just like initializing a tuple.  Alternately, you can use
    keyword arguments or just assign values to the fields."""

    # Note that this does not inherit from collections.Sequence because then
    # arbitrary attribute assignments are the allowed.  That misses spelling
    # errors and presumably defeats the purpose of __slots__ by having a dict
    # per instance.  Note that the implemented methods are sufficient to loop
    # over NodeStateData objects in for-loops, convert to a tuple or list, etc.
    # If slots are causing problems (such as with pickling) you can just globally
    # substitute some other variable name, like _slots, for __slots__ and
    # things should still work (but more space will be used in the trie).
    __slots__ = ["node", "node_is_escape", "loopback_stack",
                 "loop_counter_stack", "loop_bounds_stack",
                 "bound_node_child_dict", "visited_rep_node_id_set"]

    def __init__(self, *valList, **kwVals):
        self.set_vals(*valList, **kwVals)
        return

    def set_vals(self, *val_list, **kw_vals):
        if val_list: # either list or kwargs, not both
            if len(val_list) != 7:
                raise IndexError
            for count, var in enumerate(self.__slots__):
                self.__setattr__(var, val_list[count])
        else:
            for key in kw_vals.iterkeys():
                self.__setattr__(key, kw_vals[key])
        return

    def __getitem__(self, index):
        return self.__getattribute__(self.__slots__[index])

    def __setitem__(self, index, value):
        return self.__setattr__(self.__slots__[index], value)

    def __len__(self): return len(self.__slots__)

    def copy(self):
        """Return a copy of the state (not a deep copy)."""
        return NodeStateData(self.node, self.node_is_escape, self.loopback_stack[:],
                             self.loop_counter_stack[:], self.loop_bounds_stack[:],
                             self.bound_node_child_dict.copy(),
                             self.visited_rep_node_id_set.copy())

    def set_child(self, child_elem, node=None):
        """Assign the node (default to self.node) to have single child child_elem."""
        if node is None: node = self.node
        node_id = id(node)
        self.bound_node_child_dict[node_id] = {child_elem: node.children[child_elem]}
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
        if nodeId in self.bound_node_child_dict:
            return self.bound_node_child_dict[nodeId]
        else:
            return node.children


class NodeStateDataList(collections.MutableSequence):
    """This is essentially just a list, used to hold a collection of
    `NodeStateData` objects representing the full (nondeterministic) state.  A
    derived class is used so that additional information can be saved.  The
    initialization arguments, if any, are the same as for a list.  In
    particular, this class allows for checking whether or not the state-data is
    still valid in the underlying trie (there might have been insertions and/or
    deletions."""
    def __init__(self, regex_trie_dict, *arg, **kwds):
        # The list node_data_list does the real work.
        self.node_data_list = list(*arg, **kwds)
        self.insert_count = regex_trie_dict.insertCount
        self.delete_count = regex_trie_dict.deleteCount
        self.regex_trie_dict_instance_id = id(regex_trie_dict)
        self.regex_trie_dict = regex_trie_dict
        return

    def is_valid_in(self, regex_trie_dict):
        """Test whether or not the state-data stored in a node data list is still
        valid in the current trie.  If any insertions or deletions have occurred
        since its creation this routine returns False, otherwise True."""
        return (self.regex_trie_dict_instance_id == id(regex_trie_dict) and
                regex_trie_dict.insertCount == self.insert_count and
                regex_trie_dict.deleteCount == self.delete_count)

    def __delitem__(self, index): del self.node_data_list[index]

    def __getitem__(self, index): return self.node_data_list[index]

    def __setitem__(self, index, value): self.node_data_list[index] = value

    def __len__(self): return len(self.node_data_list)

    def insert(self, index, obj): return self.node_data_list.insert(index, obj)

    def append(self, item): return self.node_data_list.append(item)

    def append_child_node(self, query_elem, node_data, node_is_escape=None):
        """A utility routine that appends a NodeStateData object for a child of
        the node in the one passed in.  The child is the one corresponding to the
        key query_elem.  All other data is kept the same.  Return True if anything
        actually added.  If the node_is_escape argument is not None then the
        node_is_escape value of the nodeDataState is set to that value.  Note that
        if a NodeStateData instance is added outside of this loop (such as in
        processing wildcards) that routine must take care of any necessary
        set_child calls for binding in repetition loops."""
        # TODO add a copy=True flag to turn off when not needed, after debug

        children_dict = node_data.children()
        if query_elem not in children_dict: return False

        node_data_copy = node_data.copy() # make a copy

        # Fix the overridden children dict of states inside repetition loops
        # to the single child path which is actually traveled down (so later
        # repetitions are for the same pattern in the trie).  Only needed on
        # the first time through, and not for outer-level loops with max of
        # one repetition (which is how 'or' groups are treated).  The latter
        # optimization is not currently implemented.  (It doesn't hurt
        # correctness to always run set_child, but a lot of unnecessary state
        # data is then saved and copied.)
        if node_data.loopback_stack:  # debug below test, put back later
            #if node_data.loop_counter_stack[-1] == 1 and len(children_dict) > 1:
            node_data_copy.set_child(query_elem) # fix to one child
        else:
            node_data_copy.bound_node_child_dict = {} # can't loop back, free the memory
        node_data_copy.node = children_dict[query_elem]

        # Set node_is_escape if that arg is given.
        if node_is_escape is not None: node_data_copy.node_is_escape = node_is_escape

        self.node_data_list.append(node_data_copy)
        return True


class MagicElem(object):
    """A special element considered unique (checked by id) and used to
    represent a null element that doesn't match anything.  Type doesn't matter
    (since no real comparisons are needed).  Not the same as a null string."""
    def __repr__(self): return "(magic_elem)"


# TODO maybe add a non-greedy flag which will work on prexix-matches
# and return the first matches only.  Maybe.  Still might be nice to
# have a break-as-soon-as-possible flag, though if the syntax is
# unambiguous the other way won't branch, anyway.

# TODO add a clear method to the both TrieDicts.

class RegexTrieDict(TrieDict):
    """Subclass of the TrieDict class which adds regex processing for patterns
    stored in the trie."""

    magic_elem = MagicElem # A static element that is considered unique (by its id).

    def __init__(self, *args, **kwds):
        super(RegexTrieDict, self).__init__(*args, **kwds)
        self.insertCount = 0 # Used to test if inserts were done.
        self.deleteCount = 0 # Used to test if deletes were done.
        self.node_data_list = None # Used in sequential meta mode to persist states.
        self.define_meta_elems() # Set the meta-elems to their default definitions.

    def test_pattern_sequence(self, key_seq, raise_errors=False):
        """Runs some basic tests on pattern sequences.  This routine is always
        called by the insert method, with `raise_errors=True`.  Users can use it
        to test patterns before inserting them.  By default the routine will
        just return a boolean value specifying whether the string passes or
        not.  If raise_errors=True then the escape-processed version of key_seq
        is returned (assuming no errors are raised).  Note that simply passing
        the tests is no guarantee that the pattern is correct, just that it
        passes these tests."""

        def found_error(errorString, exception=PatternMatchError):
            """Utility function to raise errors or return bool as appropriate."""
            if not raise_errors: return False
            else: raise exception(errorString)

        def get_string_for_key_seq():
            """Try to get a string representation for key_seq for more-helpful
            error messages."""
            try: errPatt = "\n   " + str(key_seq)
            except: errPatt = ""
            return errPatt

        def test_matched_open_close(escapedKeyElemList, closeElem, noNest=False):
            """Simple test for mismatched open and close group parens."""
            if escapedKeyElemList[-1][2] != 0:
                if not (escapedKeyElemList[-1][2] == 1 and
                        escapedKeyElemList[-1][0] == closeElem):
                    return False
                if noNest and [t[2] for t in escapedKeyElemList if t[2] > 1]:
                    return False
            return True

        def test_no_empty_open_close(escapedKeyElemList):
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

        if len(key_seq) == 0:
            found_error("The empty element is not a valid key.", exception=KeyError)

        # Process the escapes counting open and close wildcard brackets.
        escaped_key_elem_list = process_elem_list_for_escapes(key_seq, self.escape,
                                 openGroup=self.lWildcard, closeGroup=self.rWildcard)

        if not test_matched_open_close(escaped_key_elem_list, self.rWildcard, noNest=True):
            found_error("Mismatched open and close wildcard brackets in pattern."
                       + get_string_for_key_seq())

        if not test_no_empty_open_close(escaped_key_elem_list):
            found_error("Empty open and close wildcard brackets in pattern."
                       + get_string_for_key_seq())

        # Process the escapes counting open and close group parens.
        escaped_key_elem_list = process_elem_list_for_escapes(key_seq, self.escape,
                                 openGroup=self.lGroup, closeGroup=self.rGroup)

        if not test_matched_open_close(escaped_key_elem_list, self.rGroup):
            found_error("Mismatched open and close group elements in pattern."
                       + get_string_for_key_seq())

        if not test_no_empty_open_close(escaped_key_elem_list):
            found_error("Empty open and close group elements in pattern."
                       + get_string_for_key_seq())

        if raise_errors: return escaped_key_elem_list
        else: return True

    def insert(self, key_seq, data=None):
        """Store the data item in the dict with the key key_seq.  Any existing
        data at that key is overwritten.  This method is aliased to __setitem__.
        It overrides the superclass definition and adds some syntax checking on
        the input patterns."""

        if self.canonicalize_fun:
            key_seq = self.canonicalize_fun(key_seq)

        escaped_key_elem_list = self.test_pattern_sequence(key_seq,
                                                           raise_errors=True)

        node = self.root
        for elem, escaped, _parenCount in escaped_key_elem_list:
            if escaped:
                if self.escape in node.children:
                    node = node.children[self.escape]
                else:
                    node.children[self.escape] = TrieDictNode()
                    node = node.children[self.escape]
            if elem in node.children:
                node = node.children[elem]
            else:
                node.children[elem] = TrieDictNode()
                node = node.children[elem]
        if not node.is_last_elem_of_key: # Don't increment if just resetting data.
            self.numKeys += 1
            self.insertCount += 1
        node.is_last_elem_of_key = True # End of key_seq, isLastElemOfKey is True.
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
        self.canonicalize_fun = canonicalizeFun
        if wildcardPattMatchFun: self.wildcardPattMatchFun = wildcardPattMatchFun
        else: self.wildcardPattMatchFun = char_pattern_match_test
        if elemToDigitFun: self.elemToDigitFun = elemToDigitFun
        else: self.elemToDigitFun = char_elem_to_int
        self.escapeMetaElems = {
            repetition, lGroup, rGroup, lWildcard, rWildcard, rangeElem}
        return


    def get_dfs_gen(self, subtree_root_node, fun_to_apply=None, include_root=False,
                  yield_on_leaves=True, yield_on_match=False, copies=True,
                  stop_at_elems=[], stop_at_escaped_elems=[],
                  stop_at_depth=False, only_follow_elems=[],
                  stop_if_paren_level_zero=[], first_paren_level=0,
                  subtree_root_escaped=False, sort_children=False,
                  subtree_root_elem=None, child_fun=None):
        """Returns a generator which will do a depth-first traversal of the trie,
        starting at node `subtree_root_node`.  This is a Swiss Army knife routine
        which is used in many places to do the real work.  This definition
        overrides the corresponding definition in the superclass `TrieDict` and
        adds new options that apply to regex tries.
        
        On each call this method returns a list of `(nodeElem, node)` pairs for
        each node on some path from the root to a leaf of the tree.  It
        generates such a list for each path from the root to a leaf (one on
        each call).  If `yield_on_match` is set `True` then the current list
        being constructed on a path down the tree is returned on the first time
        any match-marked node is encountered, even if the node is not a leaf.
        If `yield_on_leaves` is set `False` then yields will only be done on
        matches.  (If both are `False` then the routine returns nothing.)

        If the list `stop_at_elems` contains any elements then nodes for those
        elements are treated as leaves.  Similarly, `stop_at_escaped_elems` treats
        escaped nodes for an element in the list to be like leaf nodes.  If
        `stop_at_depth` has a positive integer value then nodes at that depth are
        treated as leaves.  The `only_follow_elems` list is like the negation for
        `stop_at_elems`: it treats everything not on the list like a leaf node (i.e.,
        it only folows child-links which are on the list).

        If `first_paren_level` is set to a positive integer then that integer will be
        incremented on each open-group meta-elem (`self.lGroup`) encountered on a
        path and decremented on each close-group meta-elem (`self.rGroup`)
        encountered on the path.  The default value is zero.  If
        `stop_if_paren_level_zero` is non-empty then any elements in the list will be
        treated as leaves if they are encountered when the paren-count equals
        zero.  Note that paren-counts are updated after the comparison with zero.
        If the root is a node for a left-paren and `first_paren_level==0` then the
        matching right-paren is at paren-level zero.

        If `fun_to_apply` is defined it will be called for each `(nodeElem, node)` pair
        on the returned lists.  The function should take two arguments; the list
        will contain the function's return value.  A copy of the node list is
        returned on each generation, but the nodes are always the actual nodes in
        the trie.  If `include_root` is `True` then output from the `subtree_root_node`
        itself will be included in the output (with `None` as the corresponding
        `nodeElem`).

        If copies is set `False` then a single node list is used; this may be a
        little faster, but the returned list will change after each
        generation-cycle.  If `sort_children` is `True` then the children of
        each node will be sorted in the DFS search ordering.

        Setting `subtree_root_elem` to an element will set that as the element on
        the returned list corresponding to the subtree root (otherwise it is
        None.  Sometimes the value is known when the function call is made,
        and it can be convenient to have a uniform list pattern.

        If `child_fun` is set to a function then the children of a node are obtained
        by calling that function with the node as the argument.  This is helpful,
        for example, in pattern-matches where the child dict is locally modified
        per state."""

        def dfs_recursion(curr_node_elem, curr_node, depth, is_escaped, paren_count):
            # Put the node on the running list of nodes (down current tree path).
            if depth > 0 or include_root:
                if fun_to_apply: nodeList.append(fun_to_apply(curr_node_elem, curr_node))
                else: nodeList.append((curr_node_elem, curr_node))
            # Get the current node's child list and modify it.
            if child_fun is not None: child_dict = child_fun(curr_node)
            else: child_dict = curr_node.children
            children = child_dict.keys()
            for elem in stop_at_elems:
                if elem == curr_node_elem: children = []
            for elem in stop_at_escaped_elems:
                if is_escaped and elem == curr_node_elem: children = []
            if (is_escaped and curr_node_elem in stop_if_paren_level_zero and
               paren_count == 0):
                children = []
            if stop_at_depth and depth == maxDepth: children = []
            if only_follow_elems:
                children = [c for c in children if c in only_follow_elems]
            if sort_children: children = sorted(children)
            # Update the paren counter.
            if curr_node_elem == self.lGroup: paren_count += 1
            if curr_node_elem == self.rGroup: paren_count -= 1
            # Yield the results, according to the selected criteria.
            yielded_already = False
            if copies: yield_value = nodeList[:]
            else: yield_value = nodeList
            if yield_on_leaves and not children: # match only leaves (or pseudo-leaves)
                yield yield_value
                yielded_already = True
            if yield_on_match and curr_node.is_last_elem_of_key:
                if not yielded_already: yield yield_value
            # Set the escape-value to pass to the recursive calls.
            if curr_node_elem == self.escape and not is_escaped: is_escaped = True
            else: is_escaped = False
            # Recurse for each child.
            for elem in children:
                for value in dfs_recursion(elem, child_dict[elem], depth+1,
                                          is_escaped, paren_count):
                    yield value
                if nodeList: nodeList.pop() # each child adds one, so pop one

        nodeList = []
        if stop_at_depth != False:
            maxDepth = stop_at_depth
            stop_at_depth = True # because 0 evals to False as a bool

        return dfs_recursion(subtree_root_elem, subtree_root_node, 0, subtree_root_escaped,
                            first_paren_level)

    def get_root_node_data_list(self):
        """This routine returns the initial node data list, for the root node.
        This list is used by get_next_nodes_meta to save the state of the
        pattern-matching between iterations.  Multiple instances are allowed."""
        # The tuple of state-information on a NodeDataList is:
        #     (node, node_is_escape, loopback_stack, loop_counter_stack, loop_bounds_stack,
        #                                                       bound_node_child_dict)
        node_data_list = NodeStateDataList(
            self, [NodeStateData(self.root, False, [], [], [], {}, set())])
        return node_data_list

    def has_key_meta(self, keySeq):
        """Test of whether the sequence of elements keySeq matches any of the
        regexp patterns stored in the RegexTrieDict.  Returns the number of
        matches.  Remember that any literal escapes in the trie must be
        escaped, but escapes in keySeq are always treated as literal."""
        mat = PrefixMatcher(self)
        for elem in keySeq:
            mat.add_key_elem(elem)
            if mat.cannot_match(): 
                mat.reset()
                return 0 # No more nodes, can't match.
        retval = mat.has_key()
        mat.reset() # Ends match and frees memory.
        return retval
        #### This is the earlier implementation, not using seqmeta mode.
        # self.node_data_list = self.get_root_node_data_list()
        # 
        # for elem in keySeq:
        #     self.node_data_list = self.get_next_nodes_meta(elem, self.node_data_list)
        #     if not self.node_data_list: return 0 # no nodes, can't match

        # tmpNodeDataList = self.get_next_nodes_meta(self.magic_elem, self.node_data_list)
        # matchedNodes = [nodeData for nodeData in tmpNodeDataList
        #                                       if nodeData[0].is_last_elem_of_key]
        # return len(matchedNodes)

    def get_meta(self, keySeq, default=[]):
        """Return a list of the data items of all the stored strings which
        match the sequence of elements keySeq (based on the regexp patterns
        stored in the RegexTrieDict).  The default with no matches is to
        return the empty list.  Remember that any literal escapes in the
        trie must be escaped, but escapes in keySeq are always treated as
        literal."""
        mat = PrefixMatcher(self)
        for elem in keySeq:
            mat.add_key_elem(elem)
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
    #     tmpNodeDataList = self.get_next_nodes_meta(self.magic_elem, self.node_data_list)

    #     # If no more active patterns, we can return the longest found.
    #     if not tmpNodeDataList:
    #         self.scannerMatchMode = False
    #         self.lastMatchedNodes = None
    #         self.reset_seqmeta()
    #         return [n.node.data for n in self.lastMatchedNodes]

    #     # Find any pattern matches at this length of elements; save nodes if found.
    #     matchedNodes = [nodeData for nodeData in tmpNodeDataList
    #                                           if nodeData[0].is_last_elem_of_key]
    #     if matchedNodes: self.lastMatchedNodes = matchedNodes
    #     return False

    def _skip_node_data_list_escapes(self, node_data_list):
        """This routines handles pattern-escapes in a list of node-data tuples.
        This routine is always called by `processNodeData` as the first step
        (unless it is called with `skipEscapes=False`).  For each node on
        `node_data_list`, a parallel node is added for each escape-element child,
        since that will have to be interpreted specially on the next iteration.
        We are skipping the escape itself but setting a bool in the `nodeData`
        state to remember it.  If escape is the only child of a node in a
        `nodeData` tuple then the original `nodeData` tuple is removed from the list.
        An ordinary list of `NodeData` instances is OK to pass as `node_data_list`.
        The return value is a `NodeStateDataList`."""
        escapedNodeDataList = NodeStateDataList(self, [])
        for nodeData in node_data_list:
            if self.escape in nodeData.children():
                # Note copies of the loop states are used.
                nodeDataCopy = nodeData.copy() # TODO can skip copy when single child
                nodeDataCopy.node_is_escape = True
                escapedNodeDataList.append_child_node(self.escape, nodeDataCopy)
                if len(nodeData.children()) == 1: continue # escape is only child
            escapedNodeDataList.append(nodeData) # copy the node over unchanged
        return escapedNodeDataList


    def get_next_nodes_meta(self, query_elem, node_data_list,
                                                       ignore_validity=False):
        """Return the list of next nodes for each node on currNodeList,
        interpreting any stored pattern-matching meta-elements, when the
        query-key element query_elem is received.  The node_data_list should be
        a NodeStateDataList object.  It stores a list of NodeDataState tuples,
        each representing a "live" state of the nondeterministic search.  The
        tuple contains a node in the trie as well as some additional state
        information.

        See the routine has_key_meta for a simple example of how this method is
        used.

        When query_elem is set to the special value self.magic_elem this routine
        has special behavior defined.  It will simply fast-forward up to the
        point where that character would have been compared to the next one in
        the query-pattern.  Then it stops, returning those stop-nodes.  This
        turns out to be very convenient for skipping closing right-group elements
        as well as zero-repetition-matching patterns at the end of a larger key
        pattern.  Recall that to check for a match we need to look at the
        is_last_elem_of_key values at the very end of the stored patterns.  Any
        node_data_list elements which immediately precede a comparison with an
        element or set of elements (a literal character or a wildcard) are left
        unchanged.  Any others move forward to such a point.  Any well-defined
        pattern has such an endpoint."""

        #print("\ndebug get_next_nodes_meta call, processing query char", query_elem)
        #print("*"*30)
        #for nd in node_data_list:
        #   self.print_tree(childFun=nd.children)
        #print("*"*30)

        if not ignore_validity and not node_data_list.is_valid_in(self):
            raise ModifiedTrieError("Invalid node_data_list, trie has changed.")

        # If we are starting a magic_elem search, save the current node with the
        # state.  This is to avoid infinite recursion in processing repetitions
        # that match zero times.  The visitedSet is emptied in processNodeData
        # when a valid end-point is reached.  The append_child_node routine adds
        # any nodes to the set that it processes, and drops any that would
        # loop.
        # TODO consider just turning off all looping-back in handleEndRepetition...
        # just set a switch here and turn back off at end.  No valid endpoint is
        # at beginning of a loop, anyway.
        self.magic_elem_no_loop = False # debug xxx
        if query_elem == self.magic_elem:
            self.magic_elem_no_loop = True # debug xxx

        # Define the list that will be built-up with next states during the
        # processing.  This object will be the return value of the function.
        next_node_data_list = NodeStateDataList(self, [])

        for node_data in node_data_list:
            # Process the node_data
            self.process_node_data(query_elem, node_data, next_node_data_list)

        return next_node_data_list


    def process_node_data(self, query_elem, node_data, next_node_data_list,
                                                            skip_escapes=True):
        """Process the instance `node_data`, usually from the `node_data_list`.
        Put the results on `next_node_data_list`.  This large routine does most
        of the work in the processing, and is called recursively when
        necessary.  Escapes are skipped (generally producing a list of
        `NodeStateData` instances) unless `skip_escapes` is set `False`."""

        #
        # If escapes are to be skipped, recursively process all the resulting nodes.
        #

        if skip_escapes:
            node_data_list = self._skip_node_data_list_escapes([node_data])
            for nd in node_data_list:
                self.process_node_data(query_elem, nd, next_node_data_list,
                                                          skip_escapes=False)
            return

        #
        # Handle ordinary, non-escaped nodes in the pattern trie.
        #

        if not node_data.node_is_escape:

            node_data.visited_rep_node_id_set = set() # got a literal elem, reset

            if id(query_elem) == id(self.magic_elem):
                # The "magic" element doesn't match anything, see header comments.
                next_node_data_list.append(node_data) # just keep the node itself

            elif query_elem == self.escape:
                # Can't match because node is neither an escaped escape char nor
                # a meta-pattern.
                pass

            elif query_elem in node_data.children():
                next_node_data_list.append_child_node(query_elem, node_data,
                                                 node_is_escape=False)

        #
        # Handle escaped nodes in the pattern trie, which have special meaning.
        #

        elif node_data.node_is_escape:
            # At a node for an escape-element, see if any meta-element patterns
            # are among its keys.  After the first escape in a pattern we can get
            # a repetition, a left-wildcard bracket, or another escape (to be
            # treated as a literal).  Multiple are possible.  Inside a
            # repetition, which are evaluates as normal sequences, we can also
            # encounter a close-group metacharacter and we have to decide
            # whether or not to loop back.  All other escaped characters at
            # this level are errors.

            next_meta_elems = node_data.children().keys()

            # Loop through the meta-elems stored at the node and handle each one.
            for meta_elem in next_meta_elems:

                #
                # Double escape in a patern matches a single escape in query-key.
                #
                if meta_elem == self.escape:
                    if query_elem == self.escape:
                        print("got escaped escape, adding to trie as element")
                        next_node_data_list.append_child_node(query_elem, node_data,
                                                         node_is_escape=False)
                    else: # An escaped escape just doesn't match, no new node_data.
                        continue

                #
                # Handle begin-repetitions.
                #
                elif meta_elem == self.repetition:
                    self.handle_begin_repetitions(
                                      node_data, query_elem, next_node_data_list)

                #
                # Handle end-repetitions.
                #
                elif meta_elem == self.rGroup:
                    nodeDataCopy = node_data.copy() # debug, unnecessary? xxx
                    nodeDataCopy.set_child(self.rGroup) # debug, unnecessary? xxx
                    nodeDataCopy.node = nodeDataCopy.children()[self.rGroup]
                    self.handle_end_repetitions(query_elem, nodeDataCopy,
                                        next_node_data_list, refuse_revisits=True)

                #
                # Handle beginning of an "or" group.
                #
                elif meta_elem == self.lGroup:
                    self.handle_beginning_of_or_group(node_data, query_elem,
                                                              next_node_data_list)

                #
                # Handle end of non-final end-section of an or-group (an orElem "|").
                #
                elif meta_elem == self.orElem:
                    #print("debug processing non-final 'or' group section")
                    #print(
                    #    "   debug node_data.loop_counter_stack is",
                    #        node_data.loop_counter_stack)
                    # Get a generator for all final rGroup elems at this point.  There
                    # should only be one, since the state was fixed to single-children
                    # when the 'or' was first processed.
                    dfs_gen_or_section_end = self.get_dfs_gen(
                                  node_data.children()[self.orElem], include_root=True,
                                  copies=False, stop_if_paren_level_zero=[self.rGroup],
                                  first_paren_level=0, child_fun=node_data.children)
                    for count, treePath in enumerate(dfs_gen_or_section_end):
                        #print("      debug treePathToEndSection in process 'or' elem",
                        #                                  [t[0] for t in treePath])
                        # Errors checked earlier, when lGroup of the 'or' was processed.
                        rGroupElem, rGroupNode = treePath[-1]
                        nodeDataCopy = node_data.copy() # debug, unneeded?? xxx
                        nodeDataCopy.set_child(self.orElem) # debug, unnecessary?? xxx
                        self.handle_end_repetitions(
                            query_elem, nodeDataCopy, next_node_data_list,
                            replace_node=rGroupNode)
                        # When 'or' is set it should give all nodes for state
                        # single-children
                        if count > 0: # This is just a consistency-check assertion.
                            raise PatternMatchError("Failure in 'or' child-setting.")

                #
                # Handle wildcards.
                #
                elif meta_elem == self.lWildcard:
                    #print("debug processing wildcard")
                    # Generate all the subtree rWildcard nodes, checking that the
                    # pattern matches.

                    self.handle_wildcards(node_data, query_elem, next_node_data_list)

                #
                # Error condition otherwise.
                #
                else:
                    # An escape in some pattern isn't followed by a valid meta-elem,
                    # like r"\Z".
                    raise PatternMatchError(
                        "Invalid meta-element (unknown escaped element) encountered."
                        "\nQuery element is: " + str(query_elem) +
                        "\nNode's children are:\n   " + str(next_meta_elems))

        return

    def handle_wildcards(self, node_data, query_elem, next_node_data_list):
        """Handle wildcard patterns in meta-processing the trie."""
        #print("debug processing wildcard")
        # Generate all the subtree rWildcard nodes, checking that the
        # pattern matches.

        node_data.visited_rep_node_id_set = set() # got an actual elem, reset

        # Magic elem doesn't match any char; just put current node on
        # next_node_data_list (so is_last_elem_of_key can be checked).
        if id(query_elem) == id(self.magic_elem):
            next_node_data_list.append(node_data) # just keep the node itself
            return #continue # process the next metaElem in the loop TODO

        wildcard_patt_gen = self.get_dfs_gen(node_data.children()[self.lWildcard],
                              include_root=True, copies=False,
                              child_fun=node_data.children,
                              stop_at_escaped_elems=[self.rWildcard])

        for wildcard_patt in wildcard_patt_gen:
            # print("      debug wildcard patt", [ t[0] for t in wildcard_patt
            # ])
            if len(wildcard_patt) <= 3: # root, some char, esc, rWildcard
                raise PatternMatchError("No closing bracket for wildcard.")
            rWildcardElem, rWildcardNode = wildcard_patt[-1]
            escapeElem, escapeNode = wildcard_patt[-2]
            if (rWildcardElem != self.rWildcard or escapeElem != self.escape):
                raise PatternMatchError("No closing bracket for wildcard.")
            pattern = [p[0] for p in wildcard_patt[1:-2]]
            # If the character matches the wildcard pattern:
            # 1) Get a copy NodeStateData.
            # 2) In the NodeDataState, fix all the nodes on the path to the
            #    end-element to have one child (the new NodeStateData now
            #    represents just one pattern instance, not the full subtree).
            # 3) Append the node to next_node_data_list.
            if self.wildcardPattMatchFun(query_elem, pattern,
                                         self.rangeElem, self.escape):
                nodeDataCopy = node_data.copy()
                nodeDataCopy.set_child(self.lWildcard)

                # Fix the children on the wildcard_patt list to only have
                # one child.
                for i in range(len(wildcard_patt)-1):
                    nodeDataCopy.set_child( # elem of next, node of current
                        wildcard_patt[i+1][0], node=wildcard_patt[i][1])

                # Set the other elements and append to next_node_data_list.
                nodeDataCopy.node = rWildcardNode # stacks do not change
                nodeDataCopy.node_is_escape = False
                next_node_data_list.append(nodeDataCopy)

    def handle_begin_repetitions(self, node_data, query_elem, next_node_data_elem):
        """Called when a begin-repetition node (i.e. for '*') is reached in the
        trie, to handle the repetition."""
        #print("debug processing repetition")
        # Add the repetition subtree leaves to the next_node_data_elem, and
        # process them as end-repetition events (generating the loop
        # back and the continuation.  Note that repetition generally
        # matches zero times, so the end of every repetition pattern is
        # a valid continuation point.  So we have to go out to the end
        # nodes and start states for those possibilities.  We only need
        # one new state for going through the characters inside the
        # repetition group, however, so after the first one the others
        # are processed with noLoop=True.

        # A repetition element must be followed by an open-group element on
        # each path following it.  Get a generator for those nodes.  We only
        # need one new state at the beginning.

        dfs_gen_open = self.get_dfs_gen(node_data.children()[self.repetition],
                         include_root=True, copies=False,
                         stop_at_escaped_elems=[self.lGroup],
                         child_fun=node_data.children)

        for tree_path_to_open_group in dfs_gen_open:
            # Check some error conditions.
            if len(tree_path_to_open_group) < 3: # root, esc, lGroup
                raise PatternMatchError(
                    "No open-group following a repetition.")
            lGroupElem, lGroupNode = tree_path_to_open_group[-1]
            lGroupEsc, lGroupEscNode = tree_path_to_open_group[-2]
            #print("   debug tree_path_to_open_group", [ t[0] for t in
            #                                          tree_path_to_open_group])
            if lGroupElem != self.lGroup or lGroupEsc != self.escape:
                raise PatternMatchError(
                    "No open-group element following a repetition element.")
            iterBounds = self.process_repetition_params([t[0] for t in
                                                       tree_path_to_open_group[1:-2]])

            # If at least one iteration is required then we can avoid having
            # to find all the closing-group elements for the subtree and
            # launching new states for each one (they are the zero-repetition
            # states). We can just start a state and wait and later see which
            # ones ever reach their end-group loopback points.
            if iterBounds[0] >= 1: # At least one iteration is required.
                pass # TODO, just an optimization of what already works

            # Now, for each opening paren, get a generator for all the closing
            # parens corresponding to it.
            dfs_gen_close = self.get_dfs_gen(lGroupNode, include_root=True,
                                 copies=False,
                                 stop_if_paren_level_zero=[self.rGroup, self.orElem],
                                 first_paren_level=0, child_fun=node_data.children)
            set_no_loop = False # whether to only create a break-state, no loop-state
            for tree_path_to_close_group in dfs_gen_close:
                # Do some error checks.
                if tree_path_to_close_group[-1][0] == self.orElem:
                    raise PatternMatchError(
                        "Or element without opening paren.")
                if len(tree_path_to_close_group) <= 3: # root, esc, rGroup
                    raise PatternMatchError("Illegal empty repetition group "
                                            "or no close-group for repetition.")
                rGroupElem, rGroupNode = tree_path_to_close_group[-1]
                rGroupEsc, rGroupEscNode = tree_path_to_close_group[-2]
                #print("      debug tree_path_to_close_group", [ t[0] for t in
                #                                      tree_path_to_close_group ])
                if rGroupElem != self.rGroup or rGroupEsc != self.escape:
                    raise PatternMatchError(
                        "No close-group matching a repetition open-group.")

                #
                # Push the loopback node on the stack and treat as end repetition.
                #

                pushed_node_data = node_data.copy()
                # Fix the children of nodes for original repetition and beginning
                # nodes skipped by dfs, for new state, in case nested
                # repetitions.
                pushed_node_data.set_child(
                    self.repetition) # do before node reassign!
                for i in range(len(tree_path_to_open_group)-1):
                    pushed_node_data.set_child( # elem of next, node of current
                        tree_path_to_open_group[i+1][0],
                        node=tree_path_to_open_group[i][1])

                pushed_node_data.node = rGroupNode
                pushed_node_data.node_is_escape = False
                pushed_node_data.loopback_stack.append(lGroupNode)
                pushed_node_data.loop_counter_stack.append(0)
                pushed_node_data.loop_bounds_stack.append(iterBounds)
                pushed_node_data.visited_rep_node_id_set.add(id(lGroupNode))

                # Process the new pushed_node_data as an end-repetition.
                self.handle_end_repetitions(query_elem, pushed_node_data,
                                            next_node_data_elem, no_loop=set_no_loop)
                set_no_loop = True # We only need one new state at the beginning.


    def handle_end_repetitions(self, query_elem, close_paren_node_data,
                           next_node_data_list, replace_node=None,
                           no_loop=False, no_break=False, refuse_revisits=False):
        """Handle reaching the close of a repetition group.  The close_paren_node
        should be the node corresponding to the closing repetition-group.  If
        replace_node is set to a node then it replaces the node in close_paren_node_data
        as the new node to jump to after a breaking a loop (used in inside sections
        of 'or' patterns)."""
        # TODO, consider
        # Greedy loops would be useful.  They always match as many loops around
        # as possible, even if that causes the larger pattern to fail.  This avoids
        # some worst-case scenarios.
        #
        # To implement: we need to link/entangle the two states that are produced
        # at the end of a loop.  Then, if the loopback state makes it through
        # another iteration, back to here in handleEndRepetition, it somehow signals
        # that other state to die.  But, if it doesn't complete another loop, that
        # state goes on.  (Note zero-repetition loops might be a problem...)
        #
        # Similarly, if two 'or' sections match we only need to keep one state... NO,
        # it will NOT necessarily result in the same pattern in the end.  The sections
        # can have different numbers of elements, and can have loops, etc.  We'd need
        # to use a greedy or non-greedy rule again.  So the last-exiting match or
        # first-exiting match could kill all the others.  We always come here to exit
        # the 'or' sections, since they are treated as loops at the outer level.
        #
        # See partial start commented out below.... one problem is that we need
        # unique IDs for NodeStateData instances, even across copy operations...
        # but do *some* copy operations need to get a new ID????
        #
        # What if we just set the IDs here for the generated NodeStateData copies?
        # Then copy could just preserve it.  Presumably anything derived from a state
        # that is killed should also be killed!  So then we just put the ID on a kill
        # list and at the end of the main routine we go through and remove those states.

        #print("debug in handleEndRepetition")

        if not (close_paren_node_data.loopback_stack and close_paren_node_data.loop_counter_stack
                and close_paren_node_data.loop_bounds_stack):
            raise PatternMatchError(
                "IndexError on a stack pop, probably mismatched parentheses.")

        # Here we branch the state into one branch that repeats the loop,
        # and one that breaks out of the loop.  Note that ordinary lists are
        # OK here, we have already checked validity of the original list and
        # any new NodeStateData instances will be added to next_node_data_list,
        # which is already a NodeStateDataList.

        # Get some preliminary values needed for conditional tests.
        open_paren_node = close_paren_node_data.loopback_stack[
            -1] # loop back to open-group node
        loop_count = close_paren_node_data.loop_counter_stack[-1]
        loop_bound_min, loop_bound_max = close_paren_node_data.loop_bounds_stack[-1]

        # The node_data for breaking out of the loop.  If loop_count is below
        # loop_bound_min then we cannot break the loop yet.
        # TODO: free up the stored bound_node_child_dict for breaks to empty stack level 0.
        # TODO when the boundNodeChildList is set in repetition we don't need to copy it.
        if loop_count >= loop_bound_min and not no_break:
            break_node_data = close_paren_node_data.copy()
            if replace_node is not None: break_node_data.node = replace_node
            break_node_data.node_is_escape = False
            break_node_data.loopback_stack.pop()
            break_node_data.loop_counter_stack.pop()
            break_node_data.loop_bounds_stack.pop()
            break_node_data_list = [break_node_data]
        else:
            break_node_data_list = []

        if self.magic_elem_no_loop or (refuse_revisits and
                                    id(close_paren_node_data.loopback_stack[-1])
                                    in close_paren_node_data.visited_rep_node_id_set):
            no_loop = True

        # The node_data for continuing the loop.  If loop_count is above
        # loop_bound_max then we cannot continue the loop and can only break.
        if (loop_count < loop_bound_max or loop_bound_max == -1) and not no_loop:
            loop_node_data = close_paren_node_data.copy()
            loop_node_data.node_is_escape = False
            loop_node_data.node = loop_node_data.loopback_stack[
                -1] # loop back to open-group node
            loop_node_data.loop_counter_stack[-1] += 1 # increment the loop counter
            loop_node_data.visited_rep_node_id_set.add(id(loop_node_data.node))
            loop_node_data_list = [loop_node_data]
        else:
            loop_node_data_list = []

        """
        # "Entangle" any pairs, for greedy repetition-matching.
        if break_node_data_list and loop_node_data_list:
           pairTuple = (id(break_node_data), id(loop_node_data))
           if pairTuple in self.entangledStatePairsSet:
              self.stateKillList.append(....)......consider
           self.entangledStatePairsSet.add( (id(break_node_data), id(loop_node_data)) )
        """

        # Combine any nodes generated.
        node_data_list = break_node_data_list + loop_node_data_list

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
        if break_node_data_list:
            try:
                close_paren_node = close_paren_node_data.node
                close_paren_node.children[self.escape].children[self.rGroup]
                break_node_data.loopback_stack[-1].children[
                    self.escape].children[self.repetition]
                raise PatternMatchError("Illegal nested repetition pattern with no"
                                        " characters in the outer repetition.")
            except KeyError:
                pass
            except IndexError:
                raise PatternMatchError(
                    "Index error on stack pop, probably mismatched parentheses.")

        # The repetition meta-characters do not count as a character of the
        # literal pattern being matched to query_elem, so run processNodeData on the
        # computed nodes.
        for node_data in node_data_list:
            self.process_node_data(query_elem, node_data, next_node_data_list)

        return

    def process_repetition_params(self, seq):
        """Process the sequence between the begin-repetition and the open-group
        that necessarily follows it.  Return a tuple (minIter, maxIter), with
        -1 for infinite maxIter."""
        tupleList = process_elem_list_for_escapes(seq, self.escape)
        val = 0 # The first value if only one, or the second otherwise.
        first_val = 0 # The second value, if there is one.
        val_set = False
        for elem, escaped in tupleList:
            if escaped:
                if elem == self.repetition:
                    if not val_set:
                        # Note first value could default to zero if empty, but that would
                        # add a redundant representation for such patterns.
                        raise PatternMatchError("No value in first slot of two-place"
                                                " repetition bounds specification.")
                    first_val = val # a second value will follow
                    val = 0
                else:
                    raise PatternMatchError("Bad escaped character in repetition"
                                            " bounds specification.")
            else:
                digit = self.elemToDigitFun(elem)
                val = 10*val + digit
                val_set = True
        if not first_val: return (val, -1)
        return (first_val, val)

    def handle_beginning_of_or_group(self, node_data, query_elem,
                                                        next_node_data_list):
        """Handle reaching the beginning of an "or" group in the trie."""
        #print("debug processing an 'or' group")
        # TODO if we're not inside a loop or guaranteed to break out then
        # we can just have a single state at each start point, and not
        # set the children to single-children, either (consider)...

        # Treat this entire 'or' group as a one-repetition loop.  This is so we
        # know what to do then final rGroup is encountered.  The actual
        # loopback point isn't required and is set to a dummy node.  The
        # continuation point will either be the next node in the tree when the
        # final rGroup element is processed as the end of a one-repetition
        # loop, or else it will be calculated when an orElem ("|") is
        # encountered.

        # Make a copy of the node_data state for this start-point.
        pushed_node_data = node_data.copy()
        pushed_node_data.node_is_escape = False
        pushed_node_data.loopback_stack.append(
            TrieDictNode()) # dummy, popped later
        pushed_node_data.loop_counter_stack.append(1)
        pushed_node_data.loop_bounds_stack.append((1, 1))
        pushed_node_data.set_child(self.lGroup)

        # Get a generator generating each path to a corresponding close-group.
        # Note the current implementation is inefficient.
        curr_node = pushed_node_data.children()[self.lGroup]
        dfs_gen_or_group_end = self.get_dfs_gen(curr_node, include_root=True,
                               copies=False, stop_if_paren_level_zero=[self.rGroup],
                               first_paren_level=-1,
                               child_fun=pushed_node_data.children,
                               subtree_root_elem=self.lGroup)

        # Iterate the generator, creating states for each section of each tree
        # path.
        for tree_path_to_end_group in dfs_gen_or_group_end:
            #print("debug", [i[0] for i in tree_path_to_end_group])
            #print("debug, query_elem is", query_elem)
            # Do some error checks.
            if len(tree_path_to_end_group) <= 3: # root, esc, rGroup # TODO check
                raise PatternMatchError("Illegal empty 'or' group "
                                        "or no close-group for 'or'.")
            if (tree_path_to_end_group[-2][0] != self.escape
               or tree_path_to_end_group[-1][0] != self.rGroup):
                raise PatternMatchError(
                    "No close-group matching an 'or' open-group.")

            # Fix all the nodes on the path to have one child (for these states).
            # We make a new copy for each path to the closing rGroup, though.
            pushed_node_data_copy = pushed_node_data.copy()
            for i in range(len(tree_path_to_end_group)-1):
                pushed_node_data_copy.set_child( # elem of next, node of current
                    tree_path_to_end_group[i+1][0], node=tree_path_to_end_group[i][1])

            # Find all the begin-section markers inside the 'or' group and start a
            # new state for each one (i.e., opening paren and all '|'
            # elements).
            escaped = True # The first lGroup is escaped.
            paren_count = 0
            for i in range(len(tree_path_to_end_group)):
                elem, node = tree_path_to_end_group[i]
                # TODO check that each one has at least something in it....
                if not escaped and elem == self.escape:
                    escaped = True
                    continue
                # Always escaped past here.
                if elem == self.lGroup: paren_count += 1
                elif elem == self.rGroup: paren_count -= 1
                # If not a valid open-group section then continue.
                if paren_count != 1 or (
                              elem != self.orElem and elem != self.lGroup):
                    continue
                # Create a state for curr_node (copy of general state) and
                # process it.
                or_section_begin = pushed_node_data_copy.copy()
                or_section_begin.node = node
                self.process_node_data(
                    query_elem, or_section_begin, next_node_data_list)
                escaped = False

    #
    # Define a few aliases/synonyms for certain methods above.
    #

    """Synonym for delitem."""
    __delitem__ = delitem

    """Synonyms for insert."""
    __setitem__ = setitem = insert


class PrefixMatcher(object):
    """Initialized with an instance of a `RegexTrieDict`.  Allows for
    sequential processing of elements from sequence-keys, and checks of whether
    any regex patterns stored in the trie have been matched.  No trie
    modifications are allowed between adding an element to the current key and
    testing for matches of the current key, or `ModifiedTrieError` will
    be raised.
    
    Testing for `cannot_match` will indicate when no patterns can possibly
    match by adding new elements.  This can be used for on-line matching to get
    the longest pattern match as soon as possible based on the prefixes of the
    text."""
    # Should resets be automatic when we get a ModifiedTrieError, or should we
    # just let the error go?  Add a flag auto_reset_on_triemod?
    def __init__(self, regex_trie_dict):
        """Initialize with a particular `RegexTrieDict` instance."""
        # Note that we have to deal with patterns that match the empty string.
        # We also want to wait until the last possible chance to do a reset,
        # so that we get a NodeStateDataList for the root that is fresh with
        # respect to any inserts which were done after the initialization.
        self.reset(regex_trie_dict)

    def reset(self, regex_trie_dict=None):
        """Reset the `PrefixMatcher` and free any state memory.  A new trie can
        optionally be passed in."""
        if regex_trie_dict is not None:
            self.rtd = regex_trie_dict
        self.match_in_progress = False
        self.node_data_list = None # Free any memory for the garbage collector.

    def add_key_elem(self, elem):
        """Inserts elem as the next elem of the current key sequence.  (Note
        elem is usually a character if string patterns are stored in the
        tree.)"""
        if not self.match_in_progress:
            self._set_to_root()
        # Update the list of node data states according to the element elem.
        self.node_data_list = self.rtd.get_next_nodes_meta(elem, self.node_data_list)

    def cannot_match(self, insert_magic=False):
        """Return `True` if no matches are possible with further elements
        inserted with `next_key_elem`.  This is determined by whether or not
        there are any active patterns in the current state."""
        return not self.node_data_list

    def _set_to_root(self):
        """Utility routine to set the state back to the root of the trie."""
        self.match_in_progress = True
        self.node_data_list = self.rtd.get_root_node_data_list()

    def has_key(self):
        """Tests whether the sequence of elements inserted by the
        `add_key_elem` method matches any of the regexp patterns stored in the
        `RegexTrieDict` instance.  Returns the number of matches.  Remember
        that any literal escapes in the trie must be escaped, but escapes in
        query keys are always treated as literal."""
        # See the get method for comments on what's going on here.
        if not self.match_in_progress: self._set_to_root()
        tmp_node_data_list = self.rtd.get_next_nodes_meta(
                                      self.rtd.magic_elem, self.node_data_list)
        matched_nodes = [node_data for node_data in tmp_node_data_list
                                              if node_data[0].is_last_elem_of_key]
        return len(matched_nodes)

    def get(self, default=[]):
        """Return a list of the data items of all the stored strings which
        match the sequence of elements which have been added via the
        `add_key_elem` method.  That defines the current key sequence and the
        match is based on the regexp patterns stored in the `RegexTrieDict`.
        The default with no matches is to return the empty list.  Remember that
        any literal escapes in the trie must be escaped, but escapes in query
        keys are always treated as literal."""
        if not self.match_in_progress:
            self._set_to_root()
        # First use `get_next_nodes_meta` to "insert" a null magic element in
        # the trie (which by definition does not match any element actually in
        # the string).  Note that the trie itself is not modified.  Save the
        # resulting node data list in a temporary list (not affecting the real,
        # persistent `self.node_data_list` for this `PrefixMatcher` instance).
        # one for this object).  This has the side-effect of moving us past any
        # `self.rGroup` closing elements, as well as past any patterns which can
        # match zero times.  (Note that inserting the empty keySeq will skip the
        # loop above and go directly to the magic element queryElem below.)
        tmp_node_data_list = self.rtd.get_next_nodes_meta(
                                       self.rtd.magic_elem, self.node_data_list)
        matched_nodes = [nodeData for nodeData in tmp_node_data_list
                                              if nodeData[0].is_last_elem_of_key]
        if not matched_nodes:
            return default
        return [n.node.data for n in matched_nodes]

def char_elem_to_int(elem):
    """This routine is set in defineMetaElems as the default value of
    elemToDigitFun, which converts elements to digit values.  Used in calculating
    repetition bounds.  It is the default setting for elemToDigitFun, when
    the elements are characters."""
    try:
        int_val = int(elem)
    except ValueError:
        raise PatternMatchError("Bad digit in repetition bounds specification")
    return int_val


def char_pattern_match_test(query_elem, patt_list, range_elem, escape_elem):
    """This utility routine does a pattern-match for characters in the wildcard
    brackets.  It does depend on the elements being characters, since it calls
    a Python regexp.  This has the advantage of allowing all the
    special-characters in Python regexp wildcards to be used.  This is the
    default routine set in defineMetaElems as wildcardPattMatchFun, for when
    elements are characters."""
    if not patt_list:
        raise PatternMatchError("No pattern in wildcard brackets.")

    # print("debug char_pattern_match_test, query elem is", query_elem, "patt_list
    # is", patt_list)

    patt_tuple_list = process_elem_list_for_escapes(patt_list, escape_elem)
    #print("debug elemList processed for escapes is", patt_tuple_list)
    pythonString = "^["
    firstChar = True
    for elem, escaped in patt_tuple_list:
        if escaped:
            if firstChar and elem == "^":
                pythonString += "^"
                continue
            if elem != "-": pythonString += "\\"
        pythonString += elem
        firstChar = False
    pythonString += "]$"
    retval = re.match(pythonString, query_elem)
    # print("debug char_pattern_match_test, pythonString is", pythonString,
    # "returning", bool(retval))
    return retval


def char_range_test(charLower, charUpper, testChar):
    """Return True if testChar is in the range from char1 to char2, inclusive.
    Used in testing wildcard patterns in the default with character elements."""
    #print("debug in char_range_test, comparing queryElem",
    #      testChar, "with lower range", charLower)
    if ord(charLower) > ord(charUpper):
        raise PatternMatchError("Second element in character range greater than lower.")
    return ord(charLower) <= ord(testChar) and ord(testChar) <= ord(charUpper)


def generic_wildcard_match_fun(queryElem, pattList, rangeElem, escapeElem,
                            rangeTestFun=char_range_test):
    """This utility routine does a generic pattern-match in the wildcard
    brackets.  This routine is for general sequences of elements and does not
    depend on the elements being characters.  Only the function rangeTestFun
    needs to be defined.  The argument pattList is the content of a wildcard
    bracket, as a list of elements.  This function tests whether queryElem
    matches the character pattern.  To simply redefine the range-test function
    for elements, use something like:

       def myPattMatchFun(queryElem, pattList, rangeElem, escapeElem):
          return generic_wildcard_match_fun(queryElem, pattList, rangeElem, escapeElem,
                                         rangeTestFun=myRangeTestFun)

    Then in calling defineMetaElems define wildcardPattMatchFun=myPattMatchFun.
    """

    #print("debug processing pattern pattList", pattList, "for queryElem", queryElem)
    if not pattList:
        raise PatternMatchError("No pattern in wildcard brackets.")

    pattTupleList = process_elem_list_for_escapes(pattList, escapeElem)
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


def process_elem_list_for_escapes(elemList, escapeChar, openGroup=None, closeGroup=None):
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



