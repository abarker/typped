"""

The TrieDict data structure is based on keys which are sequences of elements
(i.e., the keys are iterable and can be concatenated with "+").  It provides an
efficient way to recognize or map key-sequences of (hashable) items in time
linear with the length of the sequence.  For example, a sequence of characters
forming a string can be recognized or mapped to some output in linear time.  A
TrieDict can be used either as a dict or as a set (like using only the has_key
part of a dict).  Inserting and deleting key-sequences from the tree is also
linear in the length of the key-sequence.

In the usual applications, elements are characters and sequences are strings.
This module is written generically, however, so that arbitrary hashable objects
can be used as the elements.  Iterable sequences these elements are the items
stored in the TrieDict.  Except for pattern-matching, arbitrary sequences of
arbitrary hashable elements can be inserted and deleted freely in the TrieDict.
(When regexp pattern-matching is to be used the default assumes characters and
sequences are strings, but that can be changed.)

The keys method assumes that the plus operator combines elements, but setting
as_lists=True returns the list version without attempting the "addition."

The usage of a TrieDict is as follows.  First create an empty TrieDict::

   td = TrieDict()

Now insert some items.  The insert method inserts a key and an optional second
argument (default is None) which is the value associated with the key.
Alternately, the dict-style bracket notation can be used.  Any iterable items
which iterate over hashable elements can be inserted.  The nodes of the trie
will be built up from the pieces obtained by iterating over each inserted item.
In the case of strings, iteration gives the individual characters. ::

   td.insert("egg")
   td.insert("r", "what_r_maps_to")
   td["s"] = "what_s_maps_to" # bracket indexing is supported
   td.insert("eggbert")

The above creates a tree with some reduncancy on the "egg" prefix.
To test if a key is in the trie, use the usual has_key method, as
with a dict::

   if td.has_key("egg"): print('Correct, "egg" is stored')
   else: print('Incorrect, "egg" is stored')

Many of the basic dict operations can be applied, but the full dict interface
is not currently supported.

Note that this data structure is called a "trie," usually pronounced "try."
For this application the basic implementation here is sufficient and allows for
more control over the interface and implementation (and is subclassed to
recognize regex patterns in the `RegexTrieDict`).

The data structure in this implementation makes heavy use of Python's built-in
dict.  It is basically just a dict of dicts of dicts, etc., arranged into a
tree.  In the case of a string the characters of the string are used as the
keys to walk down the tree.  Each tree node holds a value that the input which
stops there is mapped to.  In a lexical analyzer application each tree node can
holds a boolean accept/reject value according to whether a test-word is a key
in the dict (when it terminates at that node it is either recognized or not).

This is clearly not the most efficient general trie implementation.  If that is
important there are several different Python packages available (optimized for
efficiency in various ways). See, e.g., https://en.wikipedia.org/wiki/Trie or
http://kmike.ru/python-data-structures/

For usual dict applications the standard hashed dict would be faster.  The
`TrieDict` data structure is especially good for the case where keys are
concatenations of hashable objects, such as in a lexical analyzer to tokenize
character strings, when prefixes are being searched.

"""

from __future__ import print_function, division, absolute_import

# Run test cases below when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run("../../test/test_trie_dict.py", pytest_args="-v")

import sys
import re
import collections # to use deque and MutableSequence abstract base class


##
## TrieDict and supporting classes.
##


class TrieDictNode(object):

    """This class is used internally, as the nodes of the tree.  It is just used as
    a record to hold the child data, match information, and arbitrary node data."""
    __slots__ = ["children", "is_last_elem_of_key", "data"] # no __dict__, save space

    def __init__(self):
        self.children = {} # dict of the node's children
        self.is_last_elem_of_key = False # whether this node represents the end of a key
        self.data = None # some arbitrary piece of data stored as a key/data pair
        return


class TrieDict(collections.MutableMapping):

    """This is a dict where the keys are made up of sequences of elements.  It
    is stored as a tree from the root with each element of a key indexing some
    child node.  The sequence of elements is a walk down the tree.  In a lexical
    analysis application the tokens are strings and the elements (sequences of
    which make up tokens) are characters.

    The data structure can be queried element by element, finding the shortest
    or longest match in a sequence.  These results from insertSeqElem
    are stored and returned to the user as a deque, since pops on both ends may
    be useful."""

    def __init__(self):
        """Initialize the basic data elements."""
        self.clear()

    def clear(self):
        """Reset the tree to its initial condition, empty of any stored strings.
        Any meta-element redefinitions are retained."""
        self.root = TrieDictNode() # the root of the recognizer tree
        self.root.is_last_elem_of_key = False # for consistency only, give root a value
        self.numKeys = 0

    def __len__(self):
        """The number of key:data pairs stored in the dictionary.  Can be called
        like the usual len function, as len(d) for a dict d."""
        return self.numKeys

    def insert(self, key_seq, data=None):
        """Store the data item in the dict with the key key_seq.  Any existing
        data at that key is overwritten.  This method is aliased to __setitem__."""
        if len(key_seq) == 0:
            raise KeyError("The empty element is not a valid key.")
        node = self.root
        for elem in key_seq:
            if elem in node.children:
                node = node.children[elem]
            else:
                # next line is the only place where non-root nodes are added to the tree
                node.children[elem] = TrieDictNode()
                node = node.children[elem]
        if not node.is_last_elem_of_key: # don't increment if just resetting data
            self.numKeys += 1
        node.is_last_elem_of_key = True # end of key_seq, is_last_elem_of_key is True
        node.data = data

    __setitem__ = insert # Allow bracket-indexing assignment like: d["key"] = 4
    #setitem = insert # Some people may prefer this name.

    def get(self, key_seq, default=None):
        """Return the data element stored with key key_seq, returning the default
        if the key is not in the dict."""
        node = self.get_node(key_seq)
        if node == None or not node.is_last_elem_of_key: return default
        else: return node.data

    def __getitem__(self, key_seq):
        """Same as get without the default value and using the bracket-indexing
        notation.  Raises a KeyError if the key is not stored."""
        node = self.get_node(key_seq)
        if node == None or not node.is_last_elem_of_key:
            raise KeyError("key "+key_seq+" is not in the TrieDict")
        else: return node.data

    def has_key(self, key_seq):
        """A boolean for whether the string is stored; doesn't handle anything else
        such as multiple matches or longest prefix match.  Leaves the tree and all
        saved data unaltered."""
        node = self.get_node(key_seq)
        if node == None or node.is_last_elem_of_key == False: return False
        else: return True

    __contains__ = has_key # Allow use of "in" syntax for testing for keys.

    def items(self, as_lists=False):
        """Return a list of all the `(key,value)` tuples stored in the trie.  Note
        that the plus operator is assumed to be overloaded to combine separate
        elements into a single key.  Setting `as_lists=True` can be used to return
        the keys as lists of elements, without attempting to combine them."""
        return [i for i in self.iteritems(as_lists=as_lists)]

    def iteritems(self, as_lists=False):
        """An iterator over the items in the trie, see the items method for details."""
        if self.numKeys != 0:
            itemGen = self.get_dfs_gen(self.root, yield_on_match=True)
            for i in itemGen:
                elem_list = [e[0] for e in i]
                if as_lists:
                    yield (elem_list, i[-1][1].data)
                else:
                    yield (combine_key_elems(elem_list), i[-1][1].data)

    def keys(self, as_lists=False):
        """Return a list of all the keys stored in the trie.  Note that the plus
        operator is assumed to be overloaded to combine separate elements into a
        single key.  Setting as_lists=True can be used to return the keys as lists
        of elements, without attempting to combine them."""
        #return [ item[0] for item in self.iteritems(as_lists=as_lists) ]
        return [key for key in self.iterkeys(as_lists=as_lists)]

    def iterkeys(self, as_lists=False):
        """An iterator over the keys in the trie, see the keys method for details."""
        for item in self.iteritems(as_lists=as_lists): yield item[0]

    __iter__ = iterkeys # Iterators go over keys, like with Python dicts.

    def values(self):
        """Return a list of all the values stored in the trie."""
        return [val for val in self.itervalues()]

    def itervalues(self):
        """Return a list of all the values stored in the trie."""
        for item in self.iteritems(as_lists=True): yield item[1]

    def get_node(self, key_seq):
        """Return the node indexed by using the sequence key_seq as the key.  Does
        not test whether is_last_elem_of_key is True, so it also returns a node for any prefix
        of a key.  Returns None if there is no corresponding node.  This is
        mainly used by other methods to walk down the tree to find a keyed
        node."""
        node = self.root
        for elem in key_seq:
            if not elem in node.children:
                return None
            else: node = node.children[elem]
        return node

    def is_prefix_of_key(self, seq):
        """Is the sequence seq a prefix of some key-sequence in the trie?
        Equality is considered a prefix."""
        if len(seq) == 0: return True
        node = self.get_node(seq)
        return bool(node)

    def some_key_is_prefix(self, seq):
        """ Very efficient determination of whether some key in the TrieDict
        is a prefix of the sequence seq.  Equality is considered a prefix."""
        if len(seq) == 0: return False
        node = self.root
        for elem in seq:
            if not elem in node.children: return False
            else:
                node = node.children[elem]
                if node.is_last_elem_of_key: return True
        return False

    def num_children(self, node):
        """The number of children at a node in the tree, i.e., the number of child
        nodes which are indexed by keys in its dict."""
        return len(node.children)

    def delitem(self, key):
        """Delete the stored key and its data.  Raises KeyError if the key wasn't
        found in the trie.  If d is a dict, the syntax del d[key] also invokes
        this function."""

        node = self.root
        nodeList = [self.root] # Save the nodes down the tree path, to back up later.
        elemList = [e for e in key] # the elements corresponding to path nodes

        # Walk down the tree for elems in elemList, saving the nodes in nodeList.
        for elem in elemList:
            if not elem in node.children:
                raise KeyError("Key is not in the TrieDict.")
            else:
                node = node.children[elem]
                nodeList.append(node)

        # We now know that key is "in the tree," see if is_last_elem_of_key is True.
        if node.is_last_elem_of_key:
            node.is_last_elem_of_key = False # matched, turn off the flag to delete key
            self.numKeys -= 1
        else:
            # The key is a prefix of something, but is not a match.
            raise KeyError("Key is not in the TrieDict.")

        #
        # Now cut loose any unneeded nodes for the garbage collector.
        #

        # If not at a leaf node (only at a is_last_elem_of_key elem) then key is a prefix
        # of something and nothing can be deleted.
        if node.children: return

        # On the forward path after the last is_last_elem_of_key, all only-child children can be
        # deleted.  We use the backward path and find the first node that can't be
        # deleted.
        for i in reversed(range(len(nodeList))):
            node = nodeList[i]
            # The can't-kill conditions: break when such a node found, otherwide free
            # node.
            if self.num_children(node) > 1 or node.is_last_elem_of_key or node == self.root:
                killNext = node        # the node to keep but kill one child
                killElem = elemList[i] # the corresponding element to kill
                break
            else: node.children = None # just to break up the tree for garbage collector
        del killNext.children[killElem]

        return

    __delitem__ = delitem # Allows the syntax: del d[key]

    def get_next_node(self, queryElem, node): # TODO really not used, but could be, but overhead
        """Return the next node in the trie from node when the key-query element
        queryElem is received.  This routine treats meta-characters as ordinary
        characters."""
        if queryElem in node.children: return node.children[queryElem]
        else: return None

    def get_dfs_gen(self, subtree_root_node, fun_to_apply=None, include_root=False,
                    yield_on_leaves=True, yield_on_match=False, copies=True,
                    stop_at_elems=[],
                    stop_at_depth=False, only_follow_elems=[],
                    sort_children=False,
                    subtree_root_elem=None, child_fun=None):
        """Returns a generator which will do a depth-first traversal of the trie,
        starting at node subtree_root_node.  This is a Swiss Army knife routine
        which is used in many places to do the real work.

        On each call this method returns a list of (nodeElem, node) pairs for
        each node on some path from the root to a leaf of the tree.  It
        generates such a list for each path from the root to a leaf (one on
        each call).  If yield_on_match is set True then the current list being
        constructed on a path down the tree is returned on the first time any
        match-marked node is encountered, even if the node is not a leaf.  If
        yield_on_leaves is set False then yields will only be done on matches.
        (If both are False then the routine returns nothing.)

        If the list stop_at_elems contains any elements then nodes for those
        elements are treated as leaves.  If stop_at_depth has a positive integer
        value then nodes at that depth are treated as leaves.  The
        only_follow_elems list is like the negation for stop_at_elems: it treats
        everything not on the list like a leaf node (i.e., it only folows
        child-links which are on the list).

        If fun_to_apply is defined it will be called for each (nodeElem, node) pair
        on the returned lists.  The function should take two arguments; the list
        will contain the function's return value.  A copy of the node list is
        returned on each generation, but the nodes are always the actual nodes in
        the trie.  If include_root is True then output from the subtree_root_node
        itself will be included in the output (with None as the corresponding
        nodeElem).

        If copies is set False then a single node list is used; this may be a
        little faster, but the returned list will change after each
        generation-cycle.  If sort_children is True then the children of
        each node will be sorted in the dfs search ordering.

        Setting subtree_root_elem to an element will set that as the element on
        the returned list corresponding to the subtree root (otherwise it is
        None.  Sometimes the value is known when the function call is made,
        and it can be convenient to have a uniform list pattern.

        If child_fun is set to a function then the children of a node are obtained
        by calling that function with the node as the argument.  This is helpful,
        for example, in pattern-matches where the child dict is locally modified
        per state."""

        def dfs_recursion(currNodeElem, currNode, depth):
            # Put the node on the running list of nodes (down current tree path).
            if depth > 0 or include_root:
                if fun_to_apply: nodeList.append(fun_to_apply(currNodeElem, currNode))
                else: nodeList.append((currNodeElem, currNode))
            # Get the current node's child list and modify it.
            if child_fun is not None: childDict = child_fun(currNode)
            else: childDict = currNode.children
            children = childDict.keys()
            for elem in stop_at_elems:
                if elem == currNodeElem: children = []
            if stop_at_depth and depth == maxDepth: children = []
            if only_follow_elems:
                children = [c for c in children if c in only_follow_elems]
            if sort_children: children = sorted(children)
            # Yield the results, according to the selected criteria.
            yieldedAlready = False
            if copies: yieldValue = nodeList[:]
            else: yieldValue = nodeList
            if yield_on_leaves and not children: # match only leaves (or pseudo-leaves)
                yield yieldValue
                yieldedAlready = True
            if yield_on_match and currNode.is_last_elem_of_key:
                if not yieldedAlready: yield yieldValue
            # Recurse for each child.
            for elem in children:
                for value in dfs_recursion(elem, childDict[elem], depth+1):
                    yield value
                if nodeList: nodeList.pop() # each child adds one, so pop one

        nodeList = []
        if stop_at_depth != False:
            maxDepth = stop_at_depth
            stop_at_depth = True # because 0 evals to False as a bool

        return dfs_recursion(subtree_root_elem, subtree_root_node, 0)


    def print_tree(self, node=0, depth=0, separation=1, childFun=None):
        """Print out a representation of the stored tree of keys.  Useful for
        debugging.  Call with default arguments to get the full tree.
        The variable separation can be increased to increase the space
        between printed elements on a line."""
        # TODO: for fun, and to test the getDfsGen, re-do this using it (set
        # the sortChildren flag True).
        if node == 0: node = self.root; print()
        if self.num_children(node) == 0:
            return True
        doIndent = False
        if childFun is not None: children = childFun(node).keys()
        else: children = node.children.keys()
        for elem in sorted(children):
            if doIndent: print("\n"+" "*(3*depth), end=""); doIndent = False
            sepStr = " " * separation
            if node.children[elem].is_last_elem_of_key: sepStr = "+" + sepStr
            else: sepStr += " "
            print(elem+sepStr, end="")
            doIndent = self.print_tree(node.children[elem], depth+1,
                                      separation=separation, childFun=childFun)
            sys.stdout.flush()
        if node == self.root: print("\n")
        return doIndent


# TODO: Document this, and let users pass in their own version if they want to.
def combine_key_elems(elem_list):
    """Combine all the elements in the list `elem_list` into a single key using
    the plus operator (string concatenation when strings are passed in)."""
    combined = elem_list[0] # We don't know the empty element in general.
    for e in elem_list[1:]:
        combined += e # This is where plus operator on elements is assumed.
    return combined

