# -*- coding: utf-8 -*-
"""

TODO: The trie matcher is not currently fully implemented.
TODO: Note that a "first not longest" matcher can work for the subset of fixed-length
patterns, aka, simple patterns.  Just order entry by length and on-ties.  Then
do the "compilation" step on-demand when the "get" fun is actually called.

A matcher for a `TokenTable`.  It is a hybrid that stores patterns either in a
list of compiled Python regexes or else in a `RegexTrieDict` instance.  The
Python regex module is used to save each of saved Python regexes against the
beginning of the text passed in.  The trie matcher does the same, and then the
longest match is returned (taking `on_ties` values into account in case of
ties).

Using the matcher
-----------------

The trie method is still experimental, so the default is to use the Python
regexes only.  Any patterns passed to the `get_next_token_label_and_value`
with `options="trie"` will be stored instead in the trie.

The Python matcher has good insert and delete times, but can become inefficient
for large numbers of patterns.  The trie is less efficient for small numbers of
patterns, but can search many simple patterns quickly in parallel by just going
down the trie.  Simple patterns are patterns literal matches with no special
characters or patterns containing only character-range wildcards.  More complex
patterns in the trie matcher are still experimental.  They are recommended only
when the prefix matching needs to be done online, as soon as possible.

Using a custom matcher
----------------------

It is possible for users to define their own matchers.  For example, the
current matcher always takes the longest match.  More-efficient matchers can be
implemented which instead take the prefix which matches the first-defined
pattern (so more-general patterns are usually defined first).  The Typped
package tries to avoid this for semantic clarity and to avoid order-dependence,
but there is no technical reason it cannot be done.

A matcher class needs to have the following methods:

* `insert_pattern` -- insert a labeled regex pattern and priority into the matcher
* `remove_pattern` -- remove an inserted pattern
* `get_next_token_label_and_value` -- return the label and value of the next match

And the following attributes:

* `ignore_tokens` -- the set of token labels which are defined as ignored

The classes need to have the signatures of their corresponding methods below.
Note that the `insert_pattern` method can take any options it wants in the
`options` argument, and can it do whatever it chooses based on the value.

Currently to switch matchers completely you need to 1) define a new matcher
class, 2) get an empty instance of that class, 3) create an empty `TokenTable`
class with that matcher passed to the initializer as the
`pattern_matcher_instance` argument, 4) pass that token table instance as the
`token_table`` paremeter of a new `Lexer` instance, and the 5) pass that lexer
instance as the `lexer` argument to the initializer of a new `PrattParser`
class.  This could be automated by subclassing `PrattParser` and doing it in
the `__init__` method. ::

    pattern_matcher = NewMatcher()
    token_table = TokenTable(pattern_matcher=pattern_matcher)
    lexer = Lexer(token_table=token_table)
    parser = PrattParser(lexer=lexer)

"""

from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_lexer.py",
                              "../../test/test_pratt_parser.py"],
                              pytest_args="-v")

import re
import collections
from .shared_settings_and_exceptions import LexerException
from .regex_trie_dict_scanner import RegexTrieDictScanner, RegexTrieDict

# Saved Python patterns are TokenPatternTuple instances.
TokenPatternTuple = collections.namedtuple("TokenPatternTuple", [
                           "regex_string",
                           "compiled_regex",
                           "on_ties",
                           ])

# These tuples are temporarily generated during the match-finding process.
MatchedPrefixTuple = collections.namedtuple("MatchedPrefixTuple", [
                           "length", # Order matters: must be first component.
                           "on_ties", # Order matters: must be second component.
                           "matched_string",
                           "token_label",
                           ])

INFINITY = float("inf")

class MatcherPythonRegex(object):
    """A matcher class that stores pattern data and matches it."""

    def __init__(self):
        self.ignore_tokens = set() # The set of tokens to ignore.

        self.python_data_dict = {} # Data for the regexes of tokens.

        self.python_fnl_data_dict = collections.OrderedDict() # FNL regex data.
        self.python_fnl_combo_regex_is_stale = True # When to recompile big regex.
        self.python_fnl_combo_regex = True # When to recompile big regex.

        self.trie_regex_data_dict = {} # Data for the regexes stored in the trie.

        self.default_insert_options = "python"
        self.rtd = None # Trie patterns stored here; only instantiated if needed.
        self.rtd_escape_char = "\\"

    def insert_pattern(self, token_label, regex_string, on_ties=0, ignore=False,
                       options=None):
        """Insert the pattern in the list of regex patterns.

        If `ignore` is true then the pattern is treated as an ignored pattern.

        If `options="python"` then the patterns are saved as individual Python
        regexes.  Each item on the list is checked against pattern for prefix
        matches.  This is the default.

        If `options="python_fnl"` then the patterns are combined into a single
        regex whenever necessary.  This is faster, but gives "first not
        longest" semantics.  That is, the first-defined patterns take
        precedence regardless of length.  In this case any `on_ties` values are
        used to pre-sort the list instead of breaking equal-length ties.

        If `options="trie"` then the pattern is inserted in a `RegexTrieDict`
        for matching (and must be in the correct format).

        The default insert options can be changed by setting the attribute
        `default_insert_options` to the desired value."""
        # Note that this method does not check for reinsertions of the same
        # token label; the def_token that calls it is responsible for that.

        if ignore:
            self.ignore_tokens.add(token_label)
        if options is None:
            options = self.default_insert_options

        if options == "trie":
            if not self.rtd is None:
                self.rtd = RegexTrieDict()
                self.rtd.define_meta_elems(escape=self.rtd_escape_char)
            self.trie_regex_data_dict[token_label] = (regex_string, on_ties)
            self.rtd[regex_string] = token_label
        elif options == "python":
            compiled_regex = re.compile(regex_string,
                                        re.VERBOSE|re.MULTILINE|re.UNICODE)
            regex_data = TokenPatternTuple(regex_string, compiled_regex, on_ties)
            self.python_data_dict[token_label] = regex_data
        elif options == "python_fnl":
            self.python_fnl_combo_regex_is_stale = True
            regex_data = (on_ties, regex_string)
            self.python_fnl_data_dict[token_label] = regex_data
        else:
            raise MatcherException("Bad option '{0}' passed to the insert_pattern method"
                                 " of the MatcherPythonRegex instance.".format(options))

    def remove_pattern(self, token_label):
        """Remove the pattern for the token corresponding to `token_label`."""
        # TODO: Not tested.
        self.ignore_tokens.discard(token_label)

        if token_label in self.python_data_dict:
            del self.python_data_dict[token_label]
        elif token_label in self.python_fnl_data_dict:
            del self.python_fnl_data_dict[token_label]
            self.python_fnl_combo_regex_is_stale = True
        elif token_label in self.trie_regex_data_dict:
            regex = regex_data_dict[token_label].regex_string
            del self.rtd[regex]
            del self.trie_regex_data_dict[token_label]
        else:
            raise MatcherException("Attempt to remove pattern for a token that"
                                   " was never defined.")

    def get_next_token_label_and_value(self, program, slice_indices,
                                       error_msg_text_snippet_size):
        """Return the best prefix match as a tuple of the token label and the matched
        string.  The `slice_indices` are an ordered pair of indices into the string
        `program` which contain the relevant part."""
        # Regular python matches.
        if self.python_data_dict:
            best_matches = self._python_get_raw_matches(program, slice_indices)
        else:
            best_matches = []
        if best_matches:
            best_matches_len = best_matches[0].length
        else:
            best_matches_len = 0

        # Trie matches.
        if self.trie_regex_data_dict:
            best_matches_trie = self._trie_get_raw_matches(program, slice_indices)
        else:
            best_matches_trie = []
        if best_matches_trie:
            best_matches_trie_len = best_matches_trie[0].length
        else:
            best_matches_trie_len = 0

        # Python first-not-longest matches.
        if self.python_fnl_data_dict:
            best_matches_fnl = self._python_first_not_longest(program, slice_indices)
        else:
            best_matches_fnl = []
        if best_matches_fnl:
            best_matches_fnl_len = best_matches_fnl[0].length
        else:
            best_matches_fnl_len = 0

        # Pick the best.

        combo_best_matches = []
        max_len = max(best_matches_len, best_matches_trie_len, best_matches_fnl_len)
        if best_matches_len == max_len:
            combo_best_matches += best_matches
        if best_matches_trie_len == max_len:
            combo_best_matches += best_matches_trie
        if best_matches_fnl_len == max_len:
            combo_best_matches += best_matches_fnl

        if not combo_best_matches:
            raise MatcherException("No matches in Lexer, unknown token at "
                    "the start of this unprocessed text:\n{0}"
                    .format(program[slice_indices[0]
                            :slice_indices[0] +
                                error_msg_text_snippet_size]))

        if len(combo_best_matches) > 1: # An unresolved tie, raise an exception.
            # TODO: Later make a better-looking format for the data in error msg.
            winning_tuples_as_dicts = [list(t._asdict().items()) for t in combo_best_matches]
            raise MatcherException("There were multiple token-pattern matches"
                    " with the same length, found in Lexer.  Set the on_ties"
                    " keyword arguments to break ties.  The possible token"
                    " types and their matched text are: {0}\n"
                    " Ambiguity at the start of this "
                    " unprocessed text:\n{1}".format(winning_tuples_as_dicts,
                        program[slice_indices[0]
                            :slice_indices[0] +
                                error_msg_text_snippet_size]))

        final_best = combo_best_matches[0]
        return final_best.token_label, final_best.matched_string

    def _trie_get_raw_matches(self, program, unprocessed_slice_indices):
        """A utility routine that does the actual string match on the prefix of
        `self.program` using the `RegexTrieDictScanner`.  Returns a list of
        `MatchedPrefixTuple` instances for all the best matches.  (In non-error
        conditions the match must be unique, but for unresolved ties we want the
        diagnostic data, too.)"""
        pass

    def _python_get_raw_matches(self, program, unprocessed_slice_indices):
        """A utility routine that does the actual string match on the prefix of
        `self.program` using the stored Python regexes.  Returns a list of
        `MatchedPrefixTuple` instances for all the best matches.  (In non-error
        conditions the match must be unique, but for unresolved ties we want the
        diagnostic data, too.)"""
        # Note that Python's finditer finds the *first* match group and stops.
        # They are ordered by the order they occur in the regex.  It finds the
        # longest match of any particular group, but stops when it finds a
        # match of some group.  Instead of using that, this code loops over all
        # the separate patterns to find the overall longest, breaking ties with
        # on_ties values.
        match_list = [] # Dict of MatchedPrefixTuple instances keyed by token labels.
        longest_tuple = (0, -INFINITY)
        for token_label, (regex_str, compiled_regex, on_ties
                                           ) in self.python_data_dict.items():
            match_object = compiled_regex.match(program,
                                                unprocessed_slice_indices[0],
                                                unprocessed_slice_indices[1])
            if match_object:
                matched_string = program[match_object.start():match_object.end()]
                match_length = len(matched_string)
                match_len_tuple = (match_length, on_ties)
                if match_len_tuple < longest_tuple:
                    continue
                if match_len_tuple != longest_tuple:
                    # match_list.clear() # Only in Python 3.
                    match_list = []
                longest_tuple = match_len_tuple
                match_list.append(MatchedPrefixTuple(
                                                length=match_length,
                                                on_ties=on_ties,
                                                matched_string=matched_string,
                                                token_label=token_label))
        return match_list


    def _python_first_not_longest(self, program, unprocessed_slice_indices):
        """A low-level scanner that combines the regexes and has "first-defined
        not longest" matching behavior for unresolved ties.  This method of
        scanning is more efficient in recognizing the patterns, but its
        semantics depend on definition ordering and it has slow insert and
        delete time (since it builds one large regex and compiles it).  This
        implementation only assembles and compiles the combined patterns
        when it is actually called to scan text, if the current compiled
        regex is stale.

        All the patterns are first sorted on any `on_ties` values provided, in
        a stable sort so the insertion order otherwise stays the same.  So the
        `on_ties` values have different semantics with this kind of scanner
        than with the others.  The others use `on_ties` to break ties between
        the longest matching patterns.  In this case, though, the first
        matching pattern in the sorted list is chosen.  So `on_ties`
        essentially becomes a way to override the effect of definition
        ordering.

        This scanner simply returns the first match, so it does not catch
        errors due to unresolved ties!  This only applies to patterns stored in
        this matcher, however.  If a combination of matchers is used then
        some ties will still be caught.

        This code is based on the tokenizer in the Python 3 documentation at
        https://docs.python.org/3.6/library/re.html#writing-a-tokenizer

        There is also an undocumented scanner in Python (see
        http://lucumr.pocoo.org/2015/11/18/pythons-hidden-re-gems/
        but it is not used here."""
        if self.python_fnl_combo_regex_is_stale:
            # Re-sort the ordered dict by on_ties values (stable sort keeps
            # insertion order otherwise).
            self.python_fnl_data_dict = collections.OrderedDict(
                    sorted(self.python_fnl_data_dict.items(), key=lambda i: i[1][0],
                           reverse=True))
            # Build the big regex and compile it.
            regex_pieces = ["(?P<{0}>{1})".format(i[0], i[1][1])
                                         for i in self.python_fnl_data_dict.items()]
            combo_regex = "|".join(regex_pieces)
            self.python_fnl_combo_regex = re.compile(combo_regex)
            self.python_fnl_combo_regex_is_stale = False

        match_object = self.python_fnl_combo_regex.match(program,
                                            unprocessed_slice_indices[0],
                                            unprocessed_slice_indices[1])

        match_list = []
        if match_object:
            token_label = match_object.lastgroup
            matched_string = program[match_object.start():match_object.end()]
            match_length = len(matched_string)
            match_list.append(MatchedPrefixTuple(length=match_length,
                                                 on_ties=None,
                                                 matched_string=matched_string,
                                                 token_label=token_label))
        print("returned match_list is", match_list)
        return match_list

def _convert_simple_pattern(self, regex_string): # EXPERIMENTAL
    """This is EXPERIMENTAL: Consider option to recognize "simple" patterns and
    automatically put them in the trie, otherwise use Python matcher.

    Convet a simple pattern to a form that can be inserted into a
    `RegexTrieDict`, if possible.  Returns `None` if the pattern is too
    complicated.  Simple pattern is essentially defined by what this routine
    is implemented to do (and a `RegexTrieDict` can/should do)"""
    return None
    # TODO the immediate below seems to work for some very simple patterns.

    simple_regex_patt = re.compile(r"^[a-zA-Z0-9_\-]+$", re.VERBOSE|re.UNICODE)
    match = simple_regex_patt.match(regex_string)
    if match is None: return None
    return regex_string # No processing needed for very simple.

    # SCRATCH BELOW

    # Note negative lookbehind assertion (?<!\\) for escape before
    # the strings which start Python regex special chars.
    non_simple_regex_contains = \
            r"""(
                    ( (?<!\\)[.^$*+?{[|(] )+ # Start of special char.
                |   ( [\\][ABdDsSwWZ] )+     # Python regex escape.
                ))"""
    compiled_non_simple_regex_contains = re.compile(
                              non_simple_regex_contains, re.VERBOSE|re.UNICODE)
    def is_simple_pattern(regex_string):
        # Could be single-char in brackets!
        # https://docs.python.org/2.0/ref/strings.html
        match_object = compiled_non_simple_regex_contains.search(regex_string)
        #matched_string = regex_string[match_object.start():match_object.end()]
        #print(" substring", matched_string)
        return not bool(match_object)
    #if is_simple_pattern(regex_string):
    #    print("simple pattern", regex_string)
    #else:
    #    print("non-simple pattern", regex_string)

#
# Exceptions.
#

class MatcherException(LexerException):
    pass

