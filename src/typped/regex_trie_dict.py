"""

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
is described in a later section.  It implements a subset of the many features
of the built-in Python regex module.

The pattern match for a call to `has_key_meta` with a single pattern in the
trie is close to the function/method `fullmatch` in the Python regex module
(modulo the minor syntactic variation).  Equivalently, it is as if all the
Python regex patterns start with `^` and end with `$`.  The key that is queried
must match the full pattern, from beginning to end, in order to return a value
that evaluates `True`.

The actual value returned by `has_key_meta` is the number of matches against
patterns in the trie.  When the trie contains multiple regex patterns the query
key is tested against all the patterns in it; the result is like a giant "or"
against all the stored patterns.

The `get` method of a `RegexTrieDict` is like the corresponding method of an
ordinary Python `dict` in that it returns any data elements which were stored
with the matching patterns in the trie.  The difference is that it can return
more than one match.  If you need the data stored with matches then call this
instead of `has_key_meta`.  You can check for the number of matches by
looking at the `len` of the returned deque.

There is no method to get the actual matching patterns themselves, but since
the query keys must match from beginning to end they are always known
beforehand.

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
would be the strings in those lists.  Regex patterns would then be lists of
strings with certain of those strings designated as escape elements and the
pattern-defining elements such as parentheses, the repetition "*", and so
forth.

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

   define_meta_elems(escape="\\\\", repetition="*", l_group="(", r_group=")",
                     l_wildcard="[", r_wildcard="]", range_elem="-",
                     or_elem="|", dot=".", wildcard_patt_match_fun=None,
                     elem_to_digit_fun=None, canonicalize_fun=None)

In order for an element of a key to be interpreted as a meta-symbol it **must**
be preceded by the defined escape element.  So, assuming the keys are strings,
pattern strings containing the above meta-symbols with their default
definitions above would always appear as `"\\\\\\\\\\*"`, `"\\\\\\\\\\("`,
`"\\\\\\\\\\)"`, `"\\\\\\\\\\["`, `"\\\\\\\\\\]"`, `"\\\\\\\\\\-"`, and
`"\\\\\\\\\\|"`.  When the backslash character is the escape it is convenient
to use raw strings, but remember that Python raw strings cannot end with a
single backslash.

There are no exceptions to the requirement that all meta-elements must be
escaped, so it is a consistent rule which does not require memorization of
which elements need to be escaped and which do not.  As usual, a double escape
such as `r"\\\\\\\\\\"` or `"\\\\\\\\\\\\\\\\"` reverts to the literal escape
symbol, as does an escape not followed by any of the defined meta-elements.

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

Wildcards
~~~~~~~~~

The language allows for single-character wildcards, i.e., wildcards which match
a single character from some set of possibilities.  Like Python regexes, this
language also recognizes an escaped dot as a metacharacter that matches
anything (including newlines, as if `DOTALL` were set in Python).  For strings
the default dot symbol is set to `r"\\."`.

As an example of character ranges with strings, consider these patterns are
valid meta-keys using wildcards::

   patt1 = r"abc\\[123\\]def"
   patt2 = r"abc\\[1\\-3\\]def"

The first pattern, `patt1`, matches `abc1def`, `abd2def`, and `abc3def` on
meta-queries.  The second pattern, `patt2`, matches the same strings but uses a
range specifier.

<TODO note we now let them define the whole wildcard-processing...>

The boolean-valued function `rangeTestFun` will be called for the first and
last argument of the range, along with the element to test.  The default
range-function (when the values is set to `None` in the call to
`define_meta_elems`) only works for character ranges.

<TODO note that Python patterns are allowed, and test some.>

<TODO note that user can essentially redefine the processing of the part inside
the brackets in any way desired.>

Repetition patterns
~~~~~~~~~~~~~~~~~~~

Repetition patterns match zero or more occurrences of the pattern group.  Here
is an example with strings as keys::

   patt1 = r"abc\\*\\(DD\\)efg"

This would match "abcefg", "abcDDefg", abcDDDDefg", etc.  The repetition
pattern can also take optional numeric arguments, each separated by another
asterick.  A single numeric argument, like in ::

   patt = r"abc\\*10\\(DD\\)efg

specifies a minimum number of repetitions.  The previous example must have
ten or more occurrences of `"dd"` in it.  So `"abcDDefg"` would not match,
but `"abcDDDDDDDDDDDDDDDDDDDDefg"` would match.  When two numbers are given
they represent the minimum and the maximum number of repetitions, respectively.
So the pattern ::

   patt = r"abc\\*2\\*3\\(DD\\)efg"

would not match `"abcDDefg"`, would match `"abcDDDDefg"` and `"abcDDDDDDefg"`, and
would not match `"abcDDDDDDDDefg"`.

If groups
~~~~~~~~~

<TODO, document>

Greediness and non-greediness in Python regexes
===============================================

In Python regexes, the "or" cases are always non-greedy and the repetition
patterns are optionally non-greedy.  The greediness of this sort of regex
algorithm tends to be a side-effect of search algorithm, which is basically a
depth-first search.  For example, the "or" cases can all be searched in order
and if any path eventually succeeds then the rest of the search can be cut off.
The optional greediness vs. non-greediness for repetition looping can be
implemented by choosing whether first expand the loops as far as possible or
whether to start at the smallest expansion.  If anything path succeeds in
matching the rest of the search of the search can be cut off.

As an example, the two greedy search groups in `r"(.*)(B+)"` will always match
`"BBBBB"` by eating up the all the 'B' characters but one in the first group,
and the last one in the second group.  If the repetitions are made non-greedy
then the first repetition group will match nothing and the second will match a
single 'B'.  If the '$' character is added to the end of the pattern then the
first group matches nothing and the second group matches the full string.

Unlike the search in the Python library, a `RegexTrieDict` essentially uses a
breadth-first search.  This allows for online recognition of patterns, but does
not naturally map into Python's definition of greedy vs. non-greedy.  Patterns
either match the full query sequence or they do not.  Data for all the matching
patterns is returned.  To find a prefix you use a `PrefixMatcher` object and
sequentially insert elements of the sequence.  You can look at the results
along the way and take the shortest (first) match or the longest match.  The
longest match is determined by taking the last valid match before the
`PrefixMatcher` method `cannot_match` returns true (which happens when all the
parallel pattern-matching states have "died" due to mismatches).  Other
criteria can be defined based on analyzing the list of matched states.

Computational complexity
========================

The time and space complexity of this particular implementation has not been
formally analyzed.  It should be similar to other implementations of regexes
(i.e., other non-optimal ones with respect to time and space and ignoring
insert and delete time).  The worst case is exponential in the length of the
query key, like most such implementations.

This implementation is like a nondeterministic finite automata in the sense
that it can have multiple states in parallel.  The states are `NodeStateData`
instances, which point to a node and also hold extra state data.  The full
state of the system is kept in a `NodeStateDataList` instance, which is
basically a list of `NodeStateData` instances.  These states are all walked down
the trie, one step with each element of the query sequence, checking for
matches and looping back and/or splitting when necessary.  When such a state
fails to match it dies, i.e., it is not propagated to be considered against the
next element in the sequence.

While traversing the trie, the repetition and "or" groups can cause branching which
increases the number of these parallel states.  Assuming a single item in the
trie, branching of states occurs in these cases:

1. For repetitions like `r"\*\(123\)"` each time the end of the loop is reached
the single state inside the loop splits into two states.  One loops back to the
beginning of the loop, and one breaks out of the loop and continues.  So the
worst-case number of states from such a loop is linear in the number of
repetition of that loop so far in the query key-string (starting at two, since
empty patterns can match).

2. When a state reaches an "or" group like `r"\(A\|B\|C\)"` it splits into
new states for each sub-group inside the group.  So that one state becomes as
many states as there are sub-groups in the "or" group.

When there are multiple patterns in the trie patterns with identical prefixes
are treated the same up until the point where they diverge.  For ordinary
elements in pattern sequences the traversal is
the same as before, since the element either matches or it does not.

When special meta-elements are encountered in the trie traversal then the state
can split into multiple states.  This is because these points possibly allow
multiple possible matches.  Consider pattern wildcard brackets on character
string sequences as an example.  With only one pattern in the trie no new
states would ever be created by a wildcard.  The single character either
matches or it does not, so the state either proceeds or dies.  But suppose
there are two different patterns stored in the trie, containing different
wildcards that happen to have the starting '[' symbol at the same index-point
into the string.  In that case when the '[' element is reached in walking the
trie (assuming it is) the state walking that path needs to split.  It splits
into a new state for each stored pattern which was identical up the '['
character and which then differs inside that wildcard bracket.

Repetition and "or" special elements are handled similarly.  Keep in mind that
this state-splitting is due to multiple possibly-matching patterns, and is in
addition to any state-splitting due to the repetition groups or "or" groups
themselves.  The number of states created this way is at most equal to the
number of different patterns stored in the trie.

In many cases this latter kind of state-splitting does not happen very often,
and when it does most of the new states die off quickly.  For example, when the
patterns are strings containing mostly ordinary characters (which can happen
when they define the tokens of a lexical scanner).  In those cases the
`RegexTrieDict` has an advantage over alternative methods that save a list of
individual patterns and loop over all the patterns, checking each pattern for a
match.

Another alternative approach is to form a giant regex which is the "or" of all
the regexes that need to be checked.  (There is example code for this in the
Python 3 regex documentation.)  Because of the non-greedy way Python evaluates
"or" cases this gives "first not longest" behavior based on the ordering in the
giant "or."  Ignoring insertion and deletion of patterns, though, this method
will tend to be faster than using a `RegexTrieDict`.  Insertion and deletion
costs can be significant when many different patterns need to be compiled into
the large regex each time.

Neither one of the alternative approaches using Python regexes works for online
applications, where the goal is to produce a token as soon as possible, from as
little text as possible.  One approach might be to successively match on longer
and longer prefixes, starting from length 0, until an "acceptable" match is
found (which already repeats work).  But without more information about the
internal pattern state or the class of patterns you never know until the end of
the string if some greedy repetition group is still active but has not yet
finished consuming characters.  In cases where there is no such group or they
have all failed the result can be returned sooner.

[Note that the hidden Python `Scanner` class apparently does "keep going", so
also consider that or slight modifications to its code.]

The usual Python routines also only work for strings, rather than general
sequences of elements.

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

The code implementing the pattern-matching algorithm is currently not optimized
(though asymptotically the algorithm shouldn't be bad except for the known
worst cases for this kind of implementation).  For example, due to the
difficulties of debugging the initial algorithm some copies are made of items
which do not need to be copied.  As far as the looping implementation for
repetition patterns, each time a repetition-loop is encountered it re-processes
the entire thing, searching forward for the end-group element.  This data could
instead be cached (such as by having a dict map begin-repetition nodes to the
cached data, valid until an insertion or deletion invalidates, or the space
needs to be freed).  Overriding the children on repetition loops can use up a
lot of space for long repetition patterns.  It is hard to avoid going to the
end in the zero-repetition match form, since the end is a valid continuation.
At-least-one repetition patterns could potentially process from the beginning,
though.  Overriding the insert (`__setitem__`) method to keep precalculated
pointers to the loop-ends and different sections sections of each "or" group
would speed things up at the cost of some memory.  A common dict indexed by
node ids could be used, but deletion would have to del the deleted-node
entries.  Then zero-repetition states could be started but just set the stacks
for the first loop, fixing the children in `append_child_node` if the stack is
not empty.

.. TODO: move any "future implementation" stuff to comments in the code itself.
   This stuff below is questionable for the docs proper but still useful.

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

Prefix matcher class
====================

The class `PrefixMatcher` is closely related to the `RegexTrieDict` class.
Every instance of a `PrefixMatcher` is associated with a particular instance of
a `RegexTrieDict`.  The `PrefixMatcher` class is used to search for prefix
matches in the keys of the `RegexTrieDict`, where the keys are treated as
meta-sequences representing regex patterns.

To use the `PrefixMatcher` class you sequentially add elements (characters when
strings are stored in the trie) to an instance using the `add_key_elem` method.
You can then use the `get_meta` method to get the data associated with a
meta-pattern, if any.  The data associated with the matching pattern string is
returned.  The `has_key_meta` method is similar, but returns the number of
matches (which acts as `False` when it is zero).

In order to find the first prefix match in an ongoing sequence, such as one
typed by a user, you need to determine when no further prefix matches are
possible.  The `cannot_match` boolean method determines this.  When all of the
parallel match searches are exhausted, i.e., when there are no more active
match states, it returns true.

In order to get the equivalent of the Python regex function/method `match` --
which always matches from the beginning of the pattern but not necessarily to
the end -- you use a `PrefixMatcher` object initialized with the
`RegexTrieDict` instance containing the pattern sequences.  By checking
`cannot_match` after each element is added and returning the last result when
`cannot_match` is `True` you get the equivalent of `match` on the prefix.  You
can also look at the matches along the way using the `get` method.  The
separate `RegexTrieDictScanner` module does this.

In order to match the next prefix when using a `PrefixMatcher` you need to
clear the `PrefixMatcher` and start again after removing the matched prefix
removed from the query key sequence.  While this seems inefficient, it works
online and returns the prefix as spoon as it can be determined based on the
patterns in the trie and the key to be matched.  Remember, also, that it is
efficiently comparing against a large "or" of many different patterns such as
in a lexical scanning/tokenizing situation.  While doing this it retains the
ability to insert/delete patterns very efficiently.  The `TrieDictScanner` is
an implementation of a scanner using this method.

"""

from __future__ import print_function, division, absolute_import

# Run test cases below when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_regex_trie_dict.py",
                             pytest_args="-v")

import re
import collections # to use deque and MutableSequence abstract base class
from .trie_dict import TrieDict, TrieDictNode
from .shared_settings_and_exceptions import TyppedBaseException

# Todo: Consider a preprocessing routine like the Python regex one `escape`.
# This one would take an unescaped string and escape all the special chars.
# Then people who prefer it that way could do it that way.  The default could
# be easily changed (even as an init parameter).

class NodeStateData(object):
    """This class is used in pattern-searches.  It is just a fancy data-record,
    essentially a mutable named tuple.  Each state of a multi-state recognizer
    is represented by an instance of this class.  The main component is a node
    in the trie, but other data is also kept.  The state "moves" in the trie
    as part of pattern matching by updating its current node.

    A `NodeStateData` is initialized by passing all the stored items to the
    initializer, just like initializing a tuple.  Alternately, you can use
    keyword arguments or just assign values to the attributes.

    A `NodeStateData` instance consists of a node in the trie as well as some
    extra information for keeping track of loops.  These states locally (for
    the particular state and search) redefine the children for each node they
    have visited and followed a child link from.  That particular child is made
    into the only child.

    Each branch in the trie represents a different pattern, connected by "or".
    The pattern-prefix for a node is always the same; a pattern with a
    different prefix would end at a different node.  When a choice of moving
    down the trie is made the prefix is further restricted.  So `NodeStateData`
    stores a dict for looking up the children of any visited state, and they
    are all restricted to the single path that they took previously.  This way,
    looping back in a repetition always gives the same pattern prefix.  (States
    can also split into two or more states, as necessary, and run in parallel
    essentially as an NFA.)  Patterns within 'or' groups inside a repetition
    need to be treated similarly, since they may match a different section on
    each repetition.

    The `node` slot/attribute holds a node in the trie.

    The boolean `node_is_escape` is true for nodes representing the escape
    element `self.escape`.

    The stacks attributes are used to keep track of looping in repetition
    patterns.  A stack is used because the loops can be nested.  The
    `loopback_stack` contains a pointer to the place in the trie where the
    loop should loop back to, like with a goto.  The `loop_counter_stack`
    contains the current loop count.  The `loop_bounds_stack` contains the
    defined bounds for the loop.

    The `bound_node_child_dict` is used to override the children of the node,
    so that the same path down the trie is always followed in a repetition
    loop.  With only one regex this would not be needed, but remember that the
    trie contains many regexes being searched in parallel with an implicit "or"
    between them.  A repetition loop which goes back in the trie must be fixed
    to a single pattern, i.e., the path it took the first time through the
    loop.  Otherwise there will be "crosstalk" between the different stored
    patterns.

    The `visited_repetition_node_id_set` set is a set of the ids of all the loopback
    repetition nodes visited, but it is reset each time a literal element
    matches the query element (i.e., on a literal or a wildcard match).  This
    is used to avoid infinite recursion in processing repetition patterns which
    match zero elements, i.e., which match the empty string."""
    # If slots are causing problems (such as with pickling) you can just globally
    # substitute some other variable name, like `_slots`, for `__slots__` and
    # things should still work (but more space will be used in pattern matches).
    __slots__ = ["node", "node_is_escape", "loopback_stack",
                 "loop_counter_stack", "loop_bounds_stack",
                 "bound_node_child_dict", "visited_repetition_node_id_set"]

    def __init__(self, *val_list, **kw_vals):
        """Initialize the instance.  This just sets the values to the ones passed
        in by calling `set_vals`."""
        self.set_vals(*val_list, **kw_vals)

    def set_vals(self, *val_list, **kw_vals):
        """Set the values in the `NodeStateData` instance."""
        if val_list: # Either args or kwargs, not both.  If args then all args.
            if len(val_list) != 7:
                raise IndexError("Need seven arguments for NodeStateData object, only"
                        " {0} found.".format(len(val_list)))
            for count, var in enumerate(self.__slots__):
                self.__setattr__(var, val_list[count])
        else:
            for key in kw_vals:
                self.__setattr__(key, kw_vals[key])

    def __getitem__(self, index):
        return self.__getattribute__(self.__slots__[index])

    def __setitem__(self, index, value):
        return self.__setattr__(self.__slots__[index], value)

    def __len__(self):
        return len(self.__slots__)

    def copy(self):
        """Return a copy of the state (not a deep copy)."""
        return NodeStateData(self.node, self.node_is_escape, self.loopback_stack[:],
                             self.loop_counter_stack[:], self.loop_bounds_stack[:],
                             self.bound_node_child_dict.copy(),
                             self.visited_repetition_node_id_set.copy())

    def set_child(self, child_elem, node=None):
        """Assign the node (default to self.node) to have single child child_elem."""
        if node is None: node = self.node
        node_id = id(node)
        self.bound_node_child_dict[node_id] = {child_elem: node.children[child_elem]}

    def children(self, node=None):
        """During patern matches the children of a node are temporarily modified,
        so that ordinary characters in a repetition loop are bound until the
        loop exits.  To do this, the pattern-match routines use the NodeStateData
        children method, rather than the node's children method.  If node is
        set then that node's children dict is returned; default is self.node."""
        # If the trie is not modified, the ids of nodes will not change.  If it
        # is modified then any search-in-progress becomes invalid anyway.
        if node is None:
            node = self.node
        node_id = id(node)
        if node_id in self.bound_node_child_dict:
            return self.bound_node_child_dict[node_id]
        else:
            return node.children

    def __repr__(self):
        string = "NodeStateData("
        string += ", ".join("{0}={1}".format(slotname,
                    getattr(self, slotname).__repr__()) for slotname in self.__slots__)
        return string + ")"


class NodeStateDataList(collections.MutableSequence):
    """This is essentially just a list, used to hold a collection of
    `NodeStateData` objects representing the full (nondeterministic) state.  A
    derived class is used so that extra attributes and methods can be added.
    The initialization arguments, if any, are the same as for a list.

    Using this class instead of a list allows for checking whether or not the
    state-data is still valid in the underlying trie (since there might have
    been insertions and/or deletions)."""
    def __init__(self, regex_trie_dict, *arg, **kwds):
        # The list node_data_list does the real work.
        self.node_data_list = list(*arg, **kwds)
        self.insert_count = regex_trie_dict.insert_count
        self.delete_count = regex_trie_dict.delete_count
        self.regex_trie_dict_instance_id = id(regex_trie_dict)
        self.regex_trie_dict = regex_trie_dict

    def is_valid_in(self, regex_trie_dict):
        """Test whether or not the state-data stored in a node data list is still
        valid in the current trie.  If any insertions or deletions have occurred
        since its creation this routine returns False, otherwise True."""
        return (self.regex_trie_dict_instance_id == id(regex_trie_dict) and
                    regex_trie_dict.insert_count == self.insert_count and
                    regex_trie_dict.delete_count == self.delete_count)

    def __delitem__(self, index):
        del self.node_data_list[index]

    def __getitem__(self, index):
        return self.node_data_list[index]

    def __setitem__(self, index, value):
        self.node_data_list[index] = value

    def __len__(self):
        return len(self.node_data_list)

    def insert(self, index, obj):
        return self.node_data_list.insert(index, obj)

    def append(self, item):
        return self.node_data_list.append(item)

    def append_child_node(self, query_elem, node_data, node_is_escape=None, copy=True):
        """A utility routine that appends a `NodeStateData` instance for a
        child of the node in the one passed in.  The child is the one
        corresponding to the key `query_elem`.  All other data is kept the
        same.  Return `True` if anything actually added.

        If the `node_is_escape` argument is not `None` then the
        `node_is_escape` value of the `NodeStateData` instance is set to that
        value.  Note that if a `NodeStateData` instance is added outside of
        this loop (such as in processing wildcards) that routine must take care
        of any necessary set_child calls for binding in repetition loops."""
        children_dict = node_data.children()
        if query_elem not in children_dict:
            return False

        if copy:
            node_data_copy = node_data.copy()
        else:
            node_data_copy = node_data

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
            node_data_copy.bound_node_child_dict = {} # Can't loop back, free the memory.
        node_data_copy.node = children_dict[query_elem]

        # Set node_is_escape if that arg is given.
        if node_is_escape is not None:
            node_data_copy.node_is_escape = node_is_escape

        self.node_data_list.append(node_data_copy)
        return True

    def __repr__(self):
        string = "NodeStateDataList(["
        string += ", ".join(state.__repr__() for state in self.node_data_list)
        return string + "])"

class MagicElem(object):
    """Instances are special elements, considered unique (and checked by id).
    They are currently used to represent an element which doesn't match
    anything and one which matches everything.  Type doesn't matter (since no
    real comparisons are needed).  Not the same as a null string."""
    def __init__(self, type_of_magic):
        self.type_of_magic = type_of_magic
    def __repr__(self):
        return "({0})".format(self.type_of_magic)

magic_elem_never_matches = MagicElem("never_matches")
# TODO: Always match is partially implemented, either get working or delete in all places.
# Had bugs that need to be worked out...
magic_elem_always_matches = MagicElem("always_matches")

# TODO maybe add a non-greedy flag which will work on prexix-matches
# and return the first matches only.  Maybe.  Still might be nice to
# have a break-as-soon-as-possible flag, though if the syntax is
# unambiguous the other way won't branch, anyway.

class RegexTrieDict(TrieDict):
    """Subclass of the `TrieDict` class which adds regex processing for patterns
    stored in the trie."""
    magic_elem_never_matches = magic_elem_never_matches # Convenient in class namespace.
    magic_elem_always_matches = magic_elem_always_matches # Convenient in class namespace.

    def __init__(self, *args, **kwds):
        """Initialize the `RegexTrieDict` instance.  All arguments are simply passed
        to the base `TrieDict` initializer."""
        super(RegexTrieDict, self).__init__(*args, **kwds)
        self.clear()

    def clear(self):
        """Clear the `RegexTrieDict` instance, starting at a new root."""
        self.insert_count = 0 # Used to test if inserts were done.
        self.delete_count = 0 # Used to test if deletes were done.
        self.node_data_list = None # Used in sequential meta mode to persist states.
        self.define_meta_elems() # Set the meta-elems to their default definitions.
        super(RegexTrieDict, self).clear() # Call superclass clear method.

    def test_pattern_sequence(self, key_seq, raise_errors=False):
        """Runs some basic tests on a pattern sequence.  This routine is always
        called by the insert method, with `raise_errors=True`.  Users can use
        it to test patterns before inserting them.  By default the routine will
        just return a boolean value specifying whether the string passes or
        not.  If `raise_errors=True` then, assuming no errors are raised, the
        escape-processed version of `key_seq` is returned.  Note that simply
        passing the tests is no guarantee that the pattern is correct, just
        that it passes these tests."""

        def found_error(error_string, exception=PatternMatchError):
            """Utility function to raise errors or return bool as appropriate."""
            if not raise_errors:
                return False
            else:
                raise exception(error_string)

        def get_string_for_key_seq():
            """Try to get a string representation for key_seq for more-helpful
            error messages."""
            try:
                err_patt = "\n   " + str(key_seq)
            except ValueError:
                err_patt = ""
            return err_patt

        def test_matched_open_close(escaped_key_elem_list, close_elem, no_nest=False):
            """Simple test for mismatched open and close group parens."""
            if escaped_key_elem_list[-1][2] != 0:
                if not (escaped_key_elem_list[-1][2] == 1 and
                        escaped_key_elem_list[-1][0] == close_elem):
                    return False
                if no_nest and [t[2] for t in escaped_key_elem_list if t[2] > 1]:
                    return False
            return True

        def test_no_empty_open_close(escaped_key_elem_list):
            """Simple test for open-group paren followed immediately by close-group.
            Any parLevel in escaped_key_elem_list must have at least three elements."""
            last_par_level = -1
            for index, (elem, is_escaped, par_level) in enumerate(escaped_key_elem_list):
                if par_level > last_par_level: count = 1
                elif par_level == last_par_level: count += 1
                if (par_level < last_par_level
                   or (par_level == 1 and index == len(escaped_key_elem_list)-1)):
                    if count < 3:
                        return False
                last_par_level = par_level
            return True

        #
        # Begin the main body of the method.
        #

        if len(key_seq) == 0:
            found_error("The empty element is not a valid key.", exception=KeyError)

        # Process the escapes counting open and close wildcard brackets.
        escaped_key_elem_list = process_elem_list_for_escapes(key_seq, self.escape,
                                 open_group=self.l_wildcard, close_group=self.r_wildcard)

        if not test_matched_open_close(escaped_key_elem_list, self.r_wildcard, no_nest=True):
            found_error("Mismatched open and close wildcard brackets in pattern."
                       + get_string_for_key_seq())

        if not test_no_empty_open_close(escaped_key_elem_list):
            found_error("Empty open and close wildcard brackets in pattern."
                       + get_string_for_key_seq())

        # Process the escapes counting open and close group parens.
        escaped_key_elem_list = process_elem_list_for_escapes(key_seq, self.escape,
                                 open_group=self.l_group, close_group=self.r_group)

        if not test_matched_open_close(escaped_key_elem_list, self.r_group):
            found_error("Mismatched open and close group elements in pattern."
                       + get_string_for_key_seq())

        if not test_no_empty_open_close(escaped_key_elem_list):
            found_error("Empty open and close group elements in pattern."
                       + get_string_for_key_seq())

        if raise_errors:
            return escaped_key_elem_list
        else:
            return True

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
        for elem, escaped, paren_count in escaped_key_elem_list:
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
            self.num_keys += 1 # This is an attribute of the underlying TrieDict.
            self.insert_count += 1
        node.is_last_elem_of_key = True # End of key_seq, isLastElemOfKey is True.
        node.data = data

    __setitem__ = setitem = insert # Alias for insert, to use `rtd[patt] = val`

    def delitem(self, key):
        """Delete the stored key and its data.  Raises KeyError if the key wasn't
        found in the trie.  If d is a dict, the syntax del d[key] also invokes
        this function.  This overrides the delete method of the base class to
        update a deletion counter to test pattern-match state validity."""
        self.delete_count -= 1
        TrieDict.delitem(self, key)

    __delitem__ = delitem # Alias for delitem, to use `del rtd[patt]`

    def define_meta_elems(self, escape="\\", repetition="*", l_group="(", r_group=")",
                        l_wildcard="[", r_wildcard="]", range_elem="-",
                        or_elem="|", dot=".", wildcard_patt_match_fun=None,
                        elem_to_digit_fun=None, canonicalize_fun=None):
        """Define the meta-elements in pattern-matching."""
        self.escape = escape
        self.repetition = repetition
        self.l_group = l_group
        self.r_group = r_group
        self.l_wildcard = l_wildcard
        self.r_wildcard = r_wildcard
        self.range_elem = range_elem
        self.or_elem = or_elem
        self.dot = dot
        self.canonicalize_fun = canonicalize_fun
        if wildcard_patt_match_fun:
            self.wildcard_patt_match_fun = wildcard_patt_match_fun
        else:
            self.wildcard_patt_match_fun = char_pattern_match_test
        if elem_to_digit_fun:
            self.elem_to_digit_fun = elem_to_digit_fun
        else:
            self.elem_to_digit_fun = char_elem_to_int
        self.escape_meta_elems = {
            repetition, l_group, r_group, l_wildcard, r_wildcard, range_elem, dot}

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
        incremented on each open-group meta-elem (`self.l_group`) encountered on a
        path and decremented on each close-group meta-elem (`self.r_group`)
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
                if fun_to_apply:
                    node_list.append(fun_to_apply(curr_node_elem, curr_node))
                else:
                    node_list.append((curr_node_elem, curr_node))
            # Get the current node's child list and modify it.
            if child_fun is not None:
                child_dict = child_fun(curr_node)
            else:
                child_dict = curr_node.children
            children = child_dict.keys()
            for elem in stop_at_elems:
                if elem == curr_node_elem: children = []
            for elem in stop_at_escaped_elems:
                if is_escaped and elem == curr_node_elem: children = []
            if (is_escaped and curr_node_elem in stop_if_paren_level_zero
                    and paren_count == 0):
                children = []
            if stop_at_depth and depth == max_depth:
                children = []
            if only_follow_elems:
                children = [c for c in children if c in only_follow_elems]
            if sort_children:
                children = sorted(children)
            # Update the paren counter.
            if curr_node_elem == self.l_group:
                paren_count += 1
            if curr_node_elem == self.r_group:
                paren_count -= 1
            # Yield the results, according to the selected criteria.
            yielded_already = False
            if copies:
                yield_value = node_list[:]
            else:
                yield_value = node_list
            if yield_on_leaves and not children: # match only leaves (or pseudo-leaves)
                yield yield_value
                yielded_already = True
            if yield_on_match and curr_node.is_last_elem_of_key and not yielded_already:
                yield yield_value
            # Set the escape-value to pass to the recursive calls.
            is_escaped = curr_node_elem == self.escape and not is_escaped
            # Recurse for each child.
            for elem in children:
                for value in dfs_recursion(elem, child_dict[elem], depth+1,
                                          is_escaped, paren_count):
                    yield value
                if node_list:
                    node_list.pop() # each child adds one, so pop one

        node_list = []
        if stop_at_depth != False:
            max_depth = stop_at_depth
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

    def has_key_meta(self, key_seq):
        """Test of whether the sequence of elements `key_seq` matches any of the
        regexp patterns stored in the `RegexTrieDict`.  Returns the number of
        matches.  Remember that any literal escapes in the trie must be
        escaped, but escapes in `key_seq` are always treated as literal."""
        matcher = SequentialPrefixMatcher(self)
        for elem in key_seq:
            matcher.append_key_elem(elem)
            if not matcher.node_data_list: # No more states in matcher.
                matcher.reset()
                return 0 # No more nodes, can't match.
        retval = matcher.has_key_meta()
        matcher.reset() # Ends match and frees memory.
        return retval

    def get_meta(self, key_seq, no_matches_retval=[]):
        """Return a list of the data items of all the stored strings which
        match the sequence of elements `key_seq` (based on the regex patterns
        stored in the `RegexTrieDict`).  The default with no matches is to
        return the empty list, which can be set via `no_matches_retval`.
        Remember that any literal escapes in the trie must be escaped, but
        escapes in `key_seq` are always treated as literal."""
        matcher = SequentialPrefixMatcher(self)
        for elem in key_seq:
            matcher.append_key_elem(elem)
            if not matcher.node_data_list: # No more states in matcher.
                matcher.reset()
                return no_matches_retval # No more nodes, can't match.
        retval = matcher.get_meta(no_matches_retval=no_matches_retval)
        matcher.reset() # Ends match and frees memory.
        return retval

    def match_prefixes_meta(self, elem_seq, only_first=False, longest=True,
                            join_elems=True):
        """This method is similar to the `match` method of the Python libarary's
        regex module.  Matches the longest prefix of `elem_seq`.

        When a match is identified it returns a two-tuple.  The first component
        contains the sequence of elements that gave the longest match (i.e.,
        the matching prefix).  If `join_elems` is true (the default) these
        elements will be combined using the `+=` operator; otherwise a
        tuple of elements is returned.  The second component of the returned
        tuple will contain a tuple of each data item stored in the trie for
        each pattern that the sequence matched."""
        # TODO not finished..... working on greedy stuff in PrefixMatcher and
        # FULL vs. PREFIX matches first.  Should this just be left to the scanner
        # instead?
        match_list = []
        elem_deque = collections.deque(elem_seq) # Note this copies, too.
        matcher = SequentialPrefixMatcher(self)
        while True:
            while True:
                matcher.append_key_elem(elem_deque.pop_left())
                # CHECK HERE IF WE GOT A MATCH

                #if matcher.cannot_match():
                #    matcher.reset()
                #    return default # No more nodes, can't match.
            match_list.append(matcher.get_meta())
            matcher.reset() # Ends match and frees memory.
        return match_list

    def get_next_nodes_meta(self, query_elem, node_data_list,
                                                       ignore_validity=False):
        """Return the list of next nodes for each node on `node_data_list`,
        interpreting any stored pattern-matching meta-elements, when the
        query-key element query_elem is received.  The `node_data_list` should
        be a `NodeStateDataList` object.  It stores a list of `NodeStateData`
        instances, each representing a "live" state in the nondeterministic
        search.  A state consists of a node in the trie as well as some
        additional state information.

        See the routine `has_key_meta` for a simple example of how this method
        is used.

        When `query_elem` is set to the special value
        `self.magic_elem_never_matches` this routine has special behavior
        defined.  It will simply fast-forward up to the point where that
        character *would have been* compared to the next one in the
        query-pattern.  Then it stops, returning those stop-nodes.  This turns
        out to be very convenient for skipping closing right-group elements as
        well as zero-repetition-matching patterns at the end of a larger key
        pattern.  Recall that to check for a match we need to look at the
        `is_last_elem_of_key` values at the very end of the stored pattern,
        which for some patterns includes the closing element.  Any
        `node_data_list` elements which immediately precede a comparison with
        an element or set of elements (a literal character or a wildcard) are
        left unchanged.  Any others move forward to such a point.  Any
        well-defined pattern has such an endpoint."""

        #print("\ndebug get_next_nodes_meta call, processing query char", query_elem)
        #print("*"*30)
        #for nd in node_data_list:
        #   self.print_tree(childFun=nd.children)
        #print("*"*30)

        if not ignore_validity and not node_data_list.is_valid_in(self):
            raise ModifiedTrieError("Invalid node_data_list, trie has changed.")

        # If we are starting a magic_elem search, save the current node with the
        # state.  This is to avoid infinite recursion in processing repetitions
        # that match zero times.  The visitedSet is emptied in `process_node_data`
        # when a valid end-point is reached.  The `append_child_node` routine adds
        # any nodes to the set that it processes, and drops any that would
        # loop.
        # TODO consider just turning off all looping-back in `handleEndRepetition`...
        # just set a switch here and turn back off at end.  No valid endpoint is
        # at beginning of a loop, anyway.
        self.magic_elem_no_loop = False # debug xxx
        if query_elem == self.magic_elem_never_matches:
            self.magic_elem_no_loop = True # debug xxx

        # Define the list that will be built-up with next states during the
        # processing.  This object will be the return value of the function.
        next_node_data_list = NodeStateDataList(self, [])

        for node_data in node_data_list:
            # Process the node_data
            self.process_node_data(query_elem, node_data, next_node_data_list)

        return next_node_data_list

    def _skip_node_data_list_escapes(self, node_data_list):
        """This utility routine handles pattern-escapes in a list of node-data tuples.
        It is always called by `process_node_data` as the first step
        (unless it is called with `skipEscapes=False`).  For each node on
        `node_data_list`, a parallel node is added for each escape-element child,
        since that will have to be interpreted specially on the next iteration.
        We are skipping the escape itself but setting a bool in the `nodeData`
        state to remember it.  If escape is the only child of a node in a
        `nodeData` tuple then the original `nodeData` tuple is removed from the list.
        An ordinary list of `NodeData` instances is OK to pass as `node_data_list`.
        The return value is a `NodeStateDataList`."""
        escaped_node_data_list = NodeStateDataList(self, [])
        for node_data in node_data_list:
            if self.escape in node_data.children():
                # Note copies of the loop states are used.
                node_data_copy = node_data.copy() # TODO can skip copy when single child
                node_data_copy.node_is_escape = True
                escaped_node_data_list.append_child_node(self.escape, node_data_copy)
                if len(node_data.children()) == 1: continue # escape is only child
            escaped_node_data_list.append(node_data) # copy the node over unchanged
        return escaped_node_data_list

    def process_node_data(self, query_elem, node_data, next_node_data_list,
                                                            skip_escapes=True):
        """Process the instance `node_data`, usually from the `node_data_list`.
        Put the results on `next_node_data_list`.  This large routine does most
        of the work in the processing, and is called recursively when
        necessary.

        Escapes are skipped (generally producing a list of `NodeStateData`
        instances) unless `skip_escapes` is set `False`.  Escapes are skipped
        by default, but when called recursively after finding an escape the
        value is set to `True`."""
        #
        # If escapes are to be skipped, recursively process all the resulting nodes.
        #

        if skip_escapes:
            node_data_list = self._skip_node_data_list_escapes([node_data])
            # We have to loop over the recursion because new states may get
            # added by `_skip_node_data_list_escapes`.
            for nd in node_data_list:
                self.process_node_data(query_elem, nd, next_node_data_list,
                                                          skip_escapes=False)
            return

        #
        # Handle ordinary, non-escaped nodes in the pattern trie.
        #

        if not node_data.node_is_escape:

            node_data.visited_repetition_node_id_set = set() # Got a literal elem, reset.

            if query_elem is self.magic_elem_never_matches:
                next_node_data_list.append(node_data) # Just keep the node itself.

            elif query_elem is self.magic_elem_always_matches:
                children_dict = node_data.children()
                for query_elem in children_dict:
                    # TODO: consider breaking after first if only testing for cannot_match
                    next_node_data_list.append_child_node(query_elem, node_data,
                                                          node_is_escape=False)

            elif query_elem == self.escape:
                # Can't match because node is neither an escaped escape char nor
                # a meta-pattern.  TODO, is this really needed?  Explain more if so,
                # delete otherwise.
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
            # repetition, which are evaluated as normal sequences, we can also
            # encounter a close-group metacharacter and we have to decide
            # whether or not to loop back.  All other escaped characters at
            # this level are errors.

            next_meta_elems = node_data.children().keys()
            #print("next meta elems are", next_meta_elems)

            # Loop through the meta-elems stored at the node and handle each one.
            for meta_elem in next_meta_elems:

                #
                # Double escape in a patern matches a single escape in query-key.
                #
                if meta_elem == self.escape:
                    if query_elem == self.escape:
                        #print("got escaped escape, adding to trie as element")
                        next_node_data_list.append_child_node(query_elem, node_data,
                                                              node_is_escape=False)
                    else: # An escaped escape just doesn't match, no new node_data.
                        continue

                #
                # Handle the dot pattern.
                #
                elif meta_elem == self.dot:
                    # Remember that binding in Python DOES allow resetting wildcards in loops.
                    if query_elem != self.magic_elem_never_matches:
                        next_node_data_list.append_child_node(self.dot, node_data,
                                                                  node_is_escape=False)

                #
                # Handle begin-repetitions.
                #
                elif meta_elem == self.repetition:
                    #print("DEBUG got a *, meta_elem is", meta_elem)
                    self.handle_begin_repetitions(
                                      node_data, query_elem, next_node_data_list)

                #
                # Handle end-repetitions.
                #
                elif meta_elem == self.r_group:
                    node_data_copy = node_data.copy() # debug, unnecessary copy? xxx
                    node_data_copy.set_child(self.r_group) # debug, unnecessary? xxx
                    node_data_copy.node = node_data_copy.children()[self.r_group]
                    self.handle_end_repetitions(query_elem, node_data_copy,
                                        next_node_data_list, refuse_revisits=True)

                #
                # Handle beginning of an "or" group.
                #
                elif meta_elem == self.l_group:
                    self.handle_beginning_of_or_group(node_data, query_elem,
                                                              next_node_data_list)

                #
                # Handle end of non-final end-section of an or-group (an or_elem "|").
                #
                elif meta_elem == self.or_elem:
                    #print("debug processing non-final 'or' group section")
                    #print(
                    #    "   debug node_data.loop_counter_stack is",
                    #        node_data.loop_counter_stack)
                    # Get a generator for all final r_group elems at this point.  There
                    # should only be one, since the state was fixed to single-children
                    # when the 'or' was first processed.
                    dfs_gen_or_section_end = self.get_dfs_gen(
                                  node_data.children()[self.or_elem], include_root=True,
                                  copies=False, stop_if_paren_level_zero=[self.r_group],
                                  first_paren_level=0, child_fun=node_data.children)
                    for count, treePath in enumerate(dfs_gen_or_section_end):
                        #print("      debug treePathToEndSection in process 'or' elem",
                        #                                  [t[0] for t in treePath])
                        # Errors checked earlier, when l_group of the 'or' was processed.
                        r_group_elem, r_group_node = treePath[-1]
                        node_data_copy = node_data.copy() # debug, unneeded?? xxx
                        node_data_copy.set_child(self.or_elem) # debug, unnecessary?? xxx
                        self.handle_end_repetitions(
                            query_elem, node_data_copy, next_node_data_list,
                            replace_node=r_group_node)
                        # When 'or' is set it should give all nodes for state
                        # single-children
                        if count > 0: # This is just a consistency-check assertion.
                            raise PatternMatchError("Failure in 'or' child-setting.")

                #
                # Handle wildcards.
                #
                elif meta_elem == self.l_wildcard:
                    #print("debug processing wildcard")
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

        return # from `process_node_data`

    def handle_wildcards(self, node_data, query_elem, next_node_data_list):
        """Handle wildcard patterns in meta-processing the trie.  The
        `next_node_data` list is appended to for all matches."""
        node_data.visited_repetition_node_id_set = set() # got an actual elem, reset

        # Magic elem doesn't match any char; just put current node on
        # next_node_data_list (so `is_last_elem_of_key` can be checked).
        always_match = False
        if query_elem is self.magic_elem_never_matches:
            next_node_data_list.append(node_data) # just keep the node itself
            return
        elif query_elem is self.magic_elem_always_matches:
            always_match = True

        # Generate all the wildcard patterns (several can share same prefix in trie).
        wildcard_patt_gen = self.get_dfs_gen(node_data.children()[self.l_wildcard],
                              include_root=True, copies=False,
                              child_fun=node_data.children,
                              stop_at_escaped_elems=[self.r_wildcard])

        for wildcard_patt in wildcard_patt_gen:
            # print("      debug wildcard patt", [ t[0] for t in wildcard_patt
            # ])
            if len(wildcard_patt) <= 3: # root, some char, esc, r_wildcard
                raise PatternMatchError("No closing bracket for wildcard.")
            r_wildcard_elem, r_wildcard_node = wildcard_patt[-1]
            escape_elem, escape_node = wildcard_patt[-2]
            if r_wildcard_elem != self.r_wildcard or escape_elem != self.escape:
                raise PatternMatchError("No closing bracket for wildcard.")
            pattern = [p[0] for p in wildcard_patt[1:-2]]
            # If the character matches the wildcard pattern:
            # 1) Get a copy NodeStateData.
            # 2) In the NodeStateData, fix all the nodes on the path to the
            #    end-element to have one child (the new NodeStateData now
            #    represents just one pattern instance, not the full subtree).
            # 3) Append the node to next_node_data_list.
            if always_match or self.wildcard_patt_match_fun(query_elem, pattern,
                                                    self.range_elem, self.escape):
                node_data_copy = node_data.copy()
                node_data_copy.set_child(self.l_wildcard)

                # Fix the children on the wildcard_patt list to only have
                # one child.
                for i in range(len(wildcard_patt)-1):
                    node_data_copy.set_child( # elem of next, node of current
                        wildcard_patt[i+1][0], node=wildcard_patt[i][1])

                # Set the other elements and append to next_node_data_list.
                node_data_copy.node = r_wildcard_node # stacks do not change
                node_data_copy.node_is_escape = False
                next_node_data_list.append(node_data_copy)

    def handle_begin_repetitions(self, node_data, query_elem, next_node_data_elem):
        """Called when a begin-repetition node (i.e. for '*') is reached in the
        trie, to handle the repetition."""
        # Add the repetition subtree leaves to the `next_node_data_elem`, and
        # process them as end-repetition events (generating the loop back and
        # the continuation.  Note that repetition generally matches zero times,
        # so the end of every repetition pattern is a valid continuation point.
        # So we have to go out to the end nodes and start states for those
        # possibilities.  We only need one new state for going through the
        # characters inside the repetition group, however, so after the first
        # one the others are processed with `no_loop=True`.

        # A repetition element must be followed by an open-group element on
        # each path following it.  Get a generator for those nodes.  We only
        # need one new state at the beginning.
        dfs_gen_open = self.get_dfs_gen(node_data.children()[self.repetition],
                         include_root=True, copies=False,
                         stop_at_escaped_elems=[self.l_group],
                         child_fun=node_data.children)

        for tree_path_to_open_group in dfs_gen_open:
            # Check some error conditions.
            if len(tree_path_to_open_group) < 3: # root, esc, l_group
                raise PatternMatchError(
                    "No open-group following a repetition.")
            l_group_elem, l_group_node = tree_path_to_open_group[-1]
            l_group_esc, l_group_esc_node = tree_path_to_open_group[-2]
            #print("   debug tree_path_to_open_group", [ t[0] for t in
            #                                          tree_path_to_open_group])
            if l_group_elem != self.l_group or l_group_esc != self.escape:
                raise PatternMatchError(
                    "No open-group element following a repetition element.")
            iter_bounds = self.process_repetition_params([t[0] for t in
                                                         tree_path_to_open_group[1:-2]])

            # If at least one iteration is required then we can avoid having
            # to find all the closing-group elements for the subtree and
            # launching new states for each one (they are the zero-repetition
            # states). We can just start a state and wait and later see which
            # ones ever reach their end-group loopback points.
            if iter_bounds[0] >= 1: # At least one iteration is required.
                pass # TODO, just a possible optimization of what already works.

            # Now, for each opening paren, get a generator for all the closing
            # parens corresponding to it.
            dfs_gen_close = self.get_dfs_gen(l_group_node, include_root=True,
                                 copies=False,
                                 stop_if_paren_level_zero=[self.r_group, self.or_elem],
                                 first_paren_level=0, child_fun=node_data.children)
            set_no_loop = False # whether to only create a break-state, no loop-state
            for tree_path_to_close_group in dfs_gen_close:
                # Do some error checks.
                if tree_path_to_close_group[-1][0] == self.or_elem:
                    raise PatternMatchError(
                        "Or element without opening paren.")
                if len(tree_path_to_close_group) <= 3: # root, esc, r_group
                    raise PatternMatchError("Illegal empty repetition group "
                                            "or no close-group for repetition.")
                r_group_elem, r_group_node = tree_path_to_close_group[-1]
                r_group_esc, r_group_esc_node = tree_path_to_close_group[-2]
                #print("      debug tree_path_to_close_group", [ t[0] for t in
                #                                      tree_path_to_close_group ])
                if r_group_elem != self.r_group or r_group_esc != self.escape:
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

                pushed_node_data.node = r_group_node
                pushed_node_data.node_is_escape = False
                pushed_node_data.loopback_stack.append(l_group_node)
                pushed_node_data.loop_counter_stack.append(0)
                pushed_node_data.loop_bounds_stack.append(iter_bounds)
                pushed_node_data.visited_repetition_node_id_set.add(id(l_group_node))

                # Process the new pushed_node_data as an end-repetition.
                self.handle_end_repetitions(query_elem, pushed_node_data,
                                            next_node_data_elem, no_loop=set_no_loop)
                set_no_loop = True # We only need one new state at the beginning.

    def handle_end_repetitions(self, query_elem, close_paren_node_data,
                           next_node_data_list, replace_node=None,
                           no_loop=False, no_break=False, refuse_revisits=False):
        """Handle reaching the close of a repetition group.  The `close_paren_node_data`
        should corresponding to the node for the closing repetition-group.  If
        `replace_node` is set to a node then it replaces the node in
        `close_paren_node_data` as the new node to jump to after a breaking a loop
        (used in inside sections of 'or' patterns)."""
        #print("debug in handleEndRepetition")
        if not (close_paren_node_data.loopback_stack
                and close_paren_node_data.loop_counter_stack
                and close_paren_node_data.loop_bounds_stack):
            raise PatternMatchError(
                "IndexError on a stack pop, probably mismatched parentheses.")

        # Here we branch the state into one branch that repeats the loop,
        # and one that breaks out of the loop.  Note that ordinary lists are
        # OK here, we have already checked validity of the original list and
        # any new NodeStateData instances will be added to next_node_data_list,
        # which is already a NodeStateDataList.

        # Get some preliminary values needed for conditional tests.
        #open_paren_node = close_paren_node_data.loopback_stack[-1] # loop back to open-group node
        loop_count = close_paren_node_data.loop_counter_stack[-1]
        loop_bound_min, loop_bound_max = close_paren_node_data.loop_bounds_stack[-1]

        # The node_data for breaking out of the loop.  If loop_count is below
        # loop_bound_min then we cannot break the loop yet.
        #
        # TODO: Can free up the stored bound_node_child_dict for breaks to empty
        # stack level 0.
        #
        # TODO When the bound_node_child_list is set in repetition we don't need
        # to copy it.
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
                                    in close_paren_node_data.visited_repetition_node_id_set):
            no_loop = True

        # The node_data for continuing the loop.  If loop_count is above
        # loop_bound_max then we cannot continue the loop and can only break.
        if (loop_count < loop_bound_max or loop_bound_max == -1) and not no_loop:
            loop_node_data = close_paren_node_data.copy()
            loop_node_data.node_is_escape = False
            loop_node_data.node = loop_node_data.loopback_stack[
                -1] # loop back to open-group node
            loop_node_data.loop_counter_stack[-1] += 1 # increment the loop counter
            loop_node_data.visited_repetition_node_id_set.add(id(loop_node_data.node))
            loop_node_data_list = [loop_node_data]
        else:
            loop_node_data_list = []

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
                close_paren_node.children[self.escape].children[self.r_group] # TODO see below
                break_node_data.loopback_stack[-1].children[ # TODO exception for flow control
                    self.escape].children[self.repetition]
                raise PatternMatchError("Illegal nested repetition pattern with no"
                                        " characters in the outer repetition.")
            except KeyError:
                pass
            except IndexError:
                raise PatternMatchError(
                    "Index error on stack pop, probably mismatched parentheses.")

        # The repetition meta-characters do not count as a character of the
        # literal pattern being matched to query_elem, so run process_node_data on the
        # computed nodes.
        for node_data in node_data_list:
            self.process_node_data(query_elem, node_data, next_node_data_list)

    def process_repetition_params(self, seq):
        """Process the sequence between the begin-repetition and the open-group
        that necessarily follows it.  Return a tuple (minIter, maxIter), with
        -1 for infinite maxIter."""
        tuple_list = process_elem_list_for_escapes(seq, self.escape)
        val = 0 # The first value if only one, or the second otherwise.
        first_val = 0 # The second value, if there is one.
        val_set = False
        for elem, escaped in tuple_list:
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
                digit = self.elem_to_digit_fun(elem)
                val = 10*val + digit
                val_set = True
        if not first_val:
            return (val, -1)
        return (first_val, val)

    def handle_beginning_of_or_group(self, node_data, query_elem,
                                                        next_node_data_list):
        """Handle reaching the beginning of an "or" group in the trie."""
        # TODO if we're not inside a loop or are guaranteed to break out then
        # we can just have a single state at each start point, and not
        # set the children to single-children, either... consider this.

        # Treat this entire 'or' group as a one-repetition loop.  This is so we
        # know what to do then final r_group is encountered.  The actual
        # loopback point isn't required and is set to a dummy node.  The
        # continuation point will either be the next node in the tree when the
        # final r_group element is processed as the end of a one-repetition
        # loop, or else it will be calculated when an or_elem ("|") is
        # encountered.

        # Make a copy of the node_data state for this start-point.
        pushed_node_data = node_data.copy()
        pushed_node_data.node_is_escape = False
        pushed_node_data.loopback_stack.append(
            TrieDictNode()) # dummy, popped later
        pushed_node_data.loop_counter_stack.append(1)
        pushed_node_data.loop_bounds_stack.append((1, 1))
        pushed_node_data.set_child(self.l_group)

        # Get a generator generating each path to a corresponding close-group.
        # Note the current implementation is inefficient.
        curr_node = pushed_node_data.children()[self.l_group]
        dfs_gen_or_group_end = self.get_dfs_gen(curr_node, include_root=True,
                               copies=False, stop_if_paren_level_zero=[self.r_group],
                               first_paren_level=-1,
                               child_fun=pushed_node_data.children,
                               subtree_root_elem=self.l_group)

        # Iterate the generator, creating states for each section of each tree
        # path.
        for tree_path_to_end_group in dfs_gen_or_group_end:
            #print("debug", [i[0] for i in tree_path_to_end_group])
            #print("debug, query_elem is", query_elem)
            # Do some error checks.
            if len(tree_path_to_end_group) <= 3: # root, esc, r_group # TODO check
                raise PatternMatchError("Illegal empty 'or' group "
                                        "or no close-group for 'or'.")
            if (tree_path_to_end_group[-2][0] != self.escape
               or tree_path_to_end_group[-1][0] != self.r_group):
                raise PatternMatchError(
                    "No close-group matching an 'or' open-group.")

            # Fix all the nodes on the path to have one child (for these states).
            # We make a new copy for each path to the closing r_group, though.
            pushed_node_data_copy = pushed_node_data.copy()
            for i in range(len(tree_path_to_end_group)-1):
                pushed_node_data_copy.set_child( # elem of next, node of current
                    tree_path_to_end_group[i+1][0], node=tree_path_to_end_group[i][1])

            # Find all the begin-section markers inside the 'or' group and start a
            # new state for each one (i.e., opening paren and all '|'
            # elements).
            escaped = True # The first l_group is escaped.
            paren_count = 0
            for i in range(len(tree_path_to_end_group)):
                elem, node = tree_path_to_end_group[i]
                # TODO check that each one has at least something in it....
                if not escaped and elem == self.escape:
                    escaped = True
                    continue
                # Always escaped past here.
                if elem == self.l_group: paren_count += 1
                elif elem == self.r_group: paren_count -= 1
                # If not a valid open-group section then continue.
                if paren_count != 1 or (
                              elem != self.or_elem and elem != self.l_group):
                    continue
                # Create a state for curr_node (copy of general state) and
                # process it.
                or_section_begin = pushed_node_data_copy.copy()
                or_section_begin.node = node
                self.process_node_data(
                    query_elem, or_section_begin, next_node_data_list)
                escaped = False
        return # from `handle_beginning_of_or_group`

class SequentialPrefixMatcher(object):
    """Insert prefix elements of a key sequence one by one and see what the
    currently inserted prefix-sequence matches in the trie.  Initialized with
    an instance of a `RegexTrieDict`.  No trie modifications are allowed
    between adding an element to the current key and testing for matches of the
    current key, or `ModifiedTrieError` will be raised.

    The method `cannot_match` returns true when the currently inserted sequence
    of elements cannot possibly match the current trie no matter what new
    elements are added.  This can be used for on-line matching to get the
    longest pattern match as soon as possible based on the prefixes of the
    text."""

    def __init__(self, regex_trie_dict):
        """Initialize with a particular `RegexTrieDict` instance.  An instance
        is required."""
        # Note that we have to deal with patterns that match the empty string.
        # We also want to wait until the last possible chance to do a reset,
        # so that we get a NodeStateDataList for the root that is fresh with
        # respect to any inserts which were done after the initialization.
        self.reset(regex_trie_dict)

    def reset(self, regex_trie_dict=None):
        """Reset the `PrefixMatcher` and free any state memory.  A new trie can
        optionally be passed in.  If no new trie is passed in the previous one is
        used, starting at its root."""
        if regex_trie_dict is not None:
            self.rtd = regex_trie_dict
        self.match_in_progress = False
        self.node_data_list = None # Frees any memory for the garbage collector.

    def is_valid(self):
        """Test whether the current match process is still valid.  After any elements
        are inserted in the matcher any inserts or deletes from the trie will invalidate
        the current match.  The `reset` method must be called to make return to a valid
        match.  This is called by the other methods, when necessary, so usually does not
        need to be explicitly done."""
        if not self.node_data_list:
            return True
        return self.node_data_list.is_valid_in(self.rtd)

    def append_key_elem(self, elem):
        """Inserts elem as the next elem of the current key sequence.  (Note
        elem is usually a character if string patterns are stored in the
        tree.)  The first insert will start from the root of the trie.  To
        reset the matcher to the root the `reset` method must be called."""
        if not self.match_in_progress:
            self._set_to_root()
        elif not self.is_valid():
            raise ModifiedTrieError("Matcher state is invalid due to insertions or"
                    " deletions from the trie.  The `reset` method needs to be called.")
        # Update the list of node data states according to the element elem.
        self.node_data_list = self.rtd.get_next_nodes_meta(elem, self.node_data_list)

    def cannot_match(self):
        """Return `True` if the currently inserted sequence of elements cannot
        possibly match a regex in the trie no matter what elements are inserted
        with `add_key_elem`."""

        # Consider or delete: This could be determined by whether or not
        # there are any active patterns in next state, by passing it a magic
        # element that matches anything."""

        # TODO: Need better approach to tell sooner, or at least
        # simplify the below.
        """
        for node_data in self.node_data_list:
            if node_data.node.children:
                return False # Still not at end of trie for this state.

        # Now fast-forward past all the loop ends and "if" ends and check again.
        matched_nodes = self.get_valid_match_node_state_data_list()
        for node_data in matched_nodes:
            if node_data.node.children:
                return False # Still not at end of trie for this state.
        return not matched_nodes
        """

        # Old way, doesn't catch nearly as much:
        #if not self.node_data_list: # No states at all remain; definitely cannot match.
        #    return True

        # Maybe new way:
        # Some states active, see if a magic always-match leaves any of them.
        # Some may be loops that would expire on the next iteration, for example.
        always_match_next_data_list = self.get_always_match_node_state_data_list()
        return not always_match_next_data_list

    def can_still_match(self):
        """This is just the negation of `cannot_match` for convenience."""
        return not self.cannot_match()

    def _set_to_root(self):
        """Utility routine to set the state back to the root of the trie."""
        self.match_in_progress = True
        self.node_data_list = self.rtd.get_root_node_data_list()

    def has_key_meta(self):
        """Tests whether the *full* current sequence of elements inserted by the
        `add_key_elem` method matches any of the regex patterns stored in the
        `RegexTrieDict` instance.  Returns the number of matches.  Remember
        that any literal escapes in the trie must be escaped, but escapes in
        query keys are always treated as literal."""
        if not self.match_in_progress:
            self._set_to_root()
        elif not self.is_valid():
            raise ModifiedTrieError("Matcher state is invalid due to insertions or"
                    " deletions from the trie.  The `reset` method needs to be called.")
        matched_nodes = self.get_valid_match_node_state_data_list()
        return len(matched_nodes)

    # TODO: Is greedy vs. non-greedy in `PrefixMatcher` just a matter of
    # looking at the currently "live" states and deciding when to halt the trie
    # traversal?  If you have reached the end of *any* pattern (which you must
    # to return something other than failure) then you then you can just return
    # that.  Do not need to check `cannot_match` function except to see when to
    # report failure.  That is for non-greedy repetitions and non-greedy "or"
    # groups.  What about for the greedy repetitions?  Then, you can only take
    # a prefix result if 1) There are no active repetition groups, and 2) you
    # have some match for a partial.
    #
    # So, assume that we mark the repetitions as greedy vs. non-greedy, and that
    # this can be determined from the `NodeStateData` info.  Then the rule to examine
    # the states to determine if one can be returned might be:
    #   - If there are any active greedy repetition groups then nothing can be returned.
    #   - Rule out those which are currently inside an "or" group, not at end.
    #   - Rule out those which are currently inside a repetition group, not at end.
    #   - Rule out those which are not at the end of some pattern in the trie (leaf).
    # Testing for being a leaf is just looking at `is_last_elem_of_key` attributes.
    # The leaf rule might actually subsume the two middle ones.  Consider whether to
    # offer the option to have non-greedy "or" groups.  Maybe define '|?' to turn
    # ON greedy, rather than turn it off.
    #
    # TODO clarify the FULL match versus PREFIX match in these routines.  Different
    # methods, or same methods with flags?
    def get_valid_match_node_state_data_list(self):
        """Get the list of currently-matching `NodeStateData` states (for the
        elements which have been inserted up to now).  Inserts a magic element
        that does not match anything."""
        # First use `get_next_nodes_meta` to "insert" a magic element in the
        # trie (which by definition does not match any element actually in a
        # pattern).  Note that the trie itself is not modified: The resulting
        # node data list is saved in a temporary list (not affecting the real,
        # persistent `self.node_data_list` for this `PrefixMatcher` instance).
        # This has the side-effect of moving us past any `self.r_group` closing
        # elements, as well as past any patterns which can match zero times.
        magic_elem = self.rtd.magic_elem_never_matches
        tmp_node_data_list = self.rtd.get_next_nodes_meta(magic_elem,
                                                          self.node_data_list)
        valid_match_node_data_list = []
        for node_data in tmp_node_data_list:
            node = node_data[0]
            if not node.is_last_elem_of_key:
                continue
            valid_match_node_data_list.append(node_data)
        return valid_match_node_data_list

    def get_always_match_node_state_data_list(self):
        """Get the next node state data list assuming a magic "always match"
        character is entered."""
        # First use `get_next_nodes_meta` to "insert" a magic element in the
        # trie which always matches.
        # Note that the trie itself is not modified: The resulting
        # node data list is saved in a temporary list (not affecting the real,
        # persistent `self.node_data_list` for this `PrefixMatcher` instance).
        # This has the side-effect of moving us past any `self.r_group` closing
        # elements, as well as past any patterns which can match zero times.
        magic_elem = self.rtd.magic_elem_always_matches
        new_node_data_list = self.rtd.get_next_nodes_meta(magic_elem,
                                                          self.node_data_list)
        return new_node_data_list

    def get_meta(self, no_matches_retval=[], raw_nodes=False):
        """Return a list of the data items of all the stored strings which
        match the *full* sequence of elements which have been added via the
        `add_key_elem` method.  That defines the current key sequence and the
        match is based on the regex patterns stored in the `RegexTrieDict`.
        The default with no matches is to return the empty list.  Remember that
        any literal escapes in the trie must be escaped, but escapes in query
        keys are always treated as literal.

        If `no_matches_retval` is set then its value will be returned when
        there are no matches.

        If `raw_nodes` is true then a list of `NodeStateData` instance will be
        returned rather than just the list of the data attributes of the nodes
        represented in the list.  This is a shallow copy of a subset of the
        persistent list, containing all the patterns which are currently
        matches to some pattern (i.e., they are all at leaf nodes in the
        trie)."""
        if not self.match_in_progress:
            self._set_to_root()
        elif not self.is_valid():
            raise ModifiedTrieError("Matcher state is invalid due to insertions or"
                    " deletions from the trie.  The `reset` method needs to be called.")
        valid_match_node_data_list = self.get_valid_match_node_state_data_list()
        if raw_nodes:
            return valid_match_node_data_list
        if not valid_match_node_data_list:
            return no_matches_retval
        return [n.node.data for n in valid_match_node_data_list]


def char_elem_to_int(elem):
    """This routine is set in `define_meta_elems` as the default value of
    `elemToDigitFun`, which converts elements to digit values.  Used in calculating
    repetition bounds.  It is the default setting for `elemToDigitFun` when
    the elements are characters."""
    try:
        int_val = int(elem)
    except ValueError:
        raise PatternMatchError("Bad digit in repetition bounds specification")
    return int_val


def char_pattern_match_test(query_elem, patt_list, range_elem, escape_elem):
    """This utility routine does a pattern-match for characters in the wildcard
    brackets.  It depends on the elements being characters, since it calls a
    Python regex.  This has the advantage of allowing all the special
    characters in Python regex wildcards (character sets) to be used.  This is
    the default routine set in `define_meta_elems` as `wildcard_patt_match_fun`, for
    when elements are characters."""
    if not patt_list:
        raise PatternMatchError("No pattern in wildcard brackets.")

    # print("debug char_pattern_match_test, query elem is", query_elem, "patt_list
    # is", patt_list)

    # Can this ever be passed a MagicElem?  Handle below, just in case.
    #if query_elem is magic_elem_never_matches:
    #    return False


    patt_tuple_list = process_elem_list_for_escapes(patt_list, escape_elem)
    #print("debug elemList processed for escapes is", patt_tuple_list)
    python_string = "^["
    first_char = True
    for elem, escaped in patt_tuple_list:
        if escaped:
            if first_char and elem == "^":
                python_string += "^"
                continue
            if elem != "-": python_string += "\\"
        python_string += elem
        first_char = False
    python_string += "]$"
    retval = re.match(python_string, query_elem)
    # print("debug char_pattern_match_test, python_string is", python_string,
    # "returning", bool(retval))
    return retval


def char_range_test(char_lower, char_upper, test_char):
    """Return True if `test_char` is in the range from `char_lower` to
    `char_upper`, inclusive.  Used in testing wildcard patterns in the default
    with character elements."""
    #print("debug in char_range_test, comparing queryElem",
    #      test_char, "with lower range", char_lower)
    if ord(char_lower) > ord(char_upper):
        raise PatternMatchError(
                "Second element in character range greater than lower.")
    return ord(char_lower) <= ord(test_char) and ord(test_char) <= ord(char_upper)


def generic_wildcard_match_fun(query_elem, patt_list, range_elem, escape_elem,
                            range_test_fun=char_range_test):
    """This utility routine does a generic pattern-match in the wildcard
    brackets.  This routine is for general sequences of elements and does not
    depend on the elements being characters.  Only the function `range_test_fun`
    needs to be defined.  The argument `patt_list` is the content of a wildcard
    bracket, as a list of elements.  This function tests whether query_elem
    matches the character pattern.  To simply redefine the range-test function
    for elements, use something like::

       def myPattMatchFun(query_elem, patt_list, range_elem, escape_elem):
           return generic_wildcard_match_fun(query_elem, patt_list, range_elem,
                                    escape_elem, range_test_fun=myRangeTestFun)

    Then in calling `define_meta_elems` define `wildcard_patt_match_fun=myPattMatchFun`.
    """
    #print("debug processing pattern patt_list", patt_list, "for query_elem", query_elem)
    if not patt_list:
        raise PatternMatchError("No pattern in wildcard brackets.")

    patt_tuple_list = process_elem_list_for_escapes(patt_list, escape_elem)
    found_range_elem = False
    stack = []
    for elem, escaped in patt_tuple_list:
        if escaped:
            if elem == range_elem:
                found_range_elem = True
                continue
            else:
                raise PatternMatchError("Invalid escaped elem in wildcard pattern.")
        else:
            if found_range_elem:
                found_range_elem = False
                if not stack:
                    raise PatternMatchError("No elem before range elem in wildcard.")
                if range_test_fun(stack.pop(), elem, query_elem): return True
            else:
                stack.append(elem)
    for elem in stack:
        if range_test_fun(elem, elem, query_elem): return True
    return False


def process_elem_list_for_escapes(elem_list, escape_char,
                                  open_group=None, close_group=None):
    """A utility routine which takes a list of possibly-escaped elements as an
    argument and returns a list of two-tuples.  The first element of a two-tuple
    is the actual character, and the second a boolean for whether or not it is
    escaped.  If open_group and/or close_group is set it returns a three-tuple,
    where the last element gives the level of parenthesis nesting, starting at zero
    and increasing.  An open and its corresponding close have the same level."""
    escaped = False
    tuple_list = []
    p_count = 0
    for elem in elem_list:
        if elem == escape_char and not escaped:
            escaped = True
            continue
        if escaped:
            bool_val = True
            if open_group and elem == open_group: p_count += 1
        else:
            bool_val = False
        if open_group or close_group:
            tuple_list.append((elem, bool_val, p_count))
        else:
            tuple_list.append((elem, bool_val))
        if escaped:
            escaped = False
            if close_group and elem == close_group:
                p_count -= 1
    return tuple_list


#
# Exception classes for RegexTrieDict.
#


class RegexTrieDictError(TyppedBaseException):
    pass

class PrefixMatcherError(TyppedBaseException):
    pass

class PatternMatchError(RegexTrieDictError):
    pass

class ModifiedTrieError(RegexTrieDictError):
    pass



