# -*- coding: utf-8 -*-
"""

A `Matcher` object for a `TokenTable`, which the `Lexer` uses for looking up
tokens.

Using the matcher
=================

The default matcher, simply called `"python"` uses individual Python regexes
for the tokens, stored in a list.  This is to implement the "longest not first"
matching semantics.  There are several alternative matchers which can be
specified, or users can define their own as described in a later section.

To select a different matcher to save a pattern in you can set the
`matcher_options` keyword argument in the call to `def_token`.  That value is
passed on to the matcher's `insert_pattern` method.   With the default matchers
different save options can be chosen for different tokens.  Each matcher with
defined entries will be checked when looking for matches.  The default matcher
to use can be changed by setting the `default_insert_options` attribute of the
matcher instance.

One alternative matcher uses Python regexes but with "first not longest"
semantics.  This makes it more efficient on lookup, but causes order-dependence
in the definition of tokens.  (Additions and deletions are more expensive
because it builds one large regex.)  To select it use `matcher_options="python_fnl"`.

There are also hybrid matchers, but those are still experimental.  They are an
attempt to implement more-efficient longest-not-first matching.  They are
hybrid matchers that store simple patterns in either a trie or a `"python_fnl"`
matcher, reverting to the default "python" matcher for more-complex patterns.

There is also an experimental matcher that uses only the
`RegexTrieDictScanner`.  The regex language it accepts is different from the
Python regex language, though.  (The regex-trie-dict package is not currently
up on GitHub.)

The Python matcher has good insert and delete times, but can become inefficient
for large numbers of patterns.  The trie is less efficient for small numbers of
patterns, but can search many simple patterns quickly in parallel by just going
down the trie.  Simple patterns are literal matches with no special characters
or patterns containing only character-range wildcards.  More complex patterns
in the trie matcher are still experimental.

Using a custom matcher
======================

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

Code
====

"""

#TODO: Note that a "first not longest" matcher can work for the subset of fixed-length
#patterns, aka, simple patterns.  Just order entry by length and on-ties.  Then
#do the "compilation" step on-demand when the "get" fun is actually called.

# New options to consider: "fnl_for_fixed_length" "trie_for_simple" The first
# is fairly easy: you just test first thing in the insert method and set the
# option to "python" if not fixed length, otherwise to the fnl method.  Only
# problem is that you no longer catch ties on fixed-length, so on_ties doesn't
# do anything in that case.  That case still reverts to first-not-longest.  The
# trie version would not have that limitation, but has more overhead.
#
#
# Note that this regex plugin replacement has an additional "partial" feature
# which detects partial matches:
#    https://pypi.python.org/pypi/regex/
# Might be useful for the realtime, online stuff.  The docs say: "A partial
# match is one that matches up to the end of string, but that string has been
# truncated and you want to know whether a complete match could be possible if
# the string had not been truncated."  Could even go overboard and wrap the C
# code.  Just look at that project's setup.py and do similar in Cython.

from __future__ import print_function, division, absolute_import

if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_matcher.py",
                              "../../test/test_lexer.py",
                              "../../test/test_pratt_parser.py"
                              ],
                              pytest_args="-v")

import re
import collections
from .shared_settings_and_exceptions import LexerException

has_regex_trie_dict = True
try:
    from regex_trie_dict.regex_trie_dict_scanner import RegexTrieDictScanner, RegexTrieDict
except ImportError:
    has_regex_trie_dict = False

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

class Matcher(object):
    """A matcher class that stores pattern data and matches it."""

    def __init__(self):
        self.ignore_tokens = set() # The set of tokens to ignore.

        self.python_data_dict = {} # Python method regex data.

        self.python_fnl_data_dict = collections.OrderedDict() # FNL regex data.
        self.python_fnl_combo_regex_is_stale = True # When to recompile big regex.
        self.python_fnl_combo_regex = True # When to recompile big regex.
        self.sort_python_fnl = False # Whether to sort or use insertion ordering in FNL.

        self.trie_regex_data_dict = {} # Data for the regexes stored in the trie.
        self.rtd_scanner = None
        self.rtd = None # Trie patterns stored here; only instantiated if needed.
        self.rtd_escape_char = "\\"

        self.default_insert_options = "python"
        #self.default_insert_options = "python_fnl"
        #self.default_insert_options = "python_but_trie_for_simple"
        #self.default_insert_options = "python_but_fnl_for_fixed_length"

    def insert_pattern(self, token_label, regex_string, on_ties=0, ignore=False,
                       matcher_options=None):
        """Insert the pattern in the list of regex patterns.

        If `ignore` is true then the pattern is treated as an ignored pattern.

        If `matcher_options="python"` then the patterns are saved as individual Python
        regexes.  Each item on the list is checked against pattern for prefix
        matches.  This is the default.

        If `matcher_options="python_fnl"` then the patterns are combined into a single
        regex whenever necessary.  This is faster, but gives "first not
        longest" (FNL) semantics.  That is, the first-defined patterns take
        precedence regardless of length.  In this case any `on_ties` values are
        used to pre-sort the list instead of breaking equal-length ties.
        Unlike the `python` and `trie` options this method cannot detect
        multiple token matches without an `on_ties` value to break the tie.
        The insertion ordering is implicitly used to break ties.

        TODO: Reconsider the `on_ties` semantics... maybe just for comparing across
        methods?

        If `matcher_options="trie"` then the pattern is inserted in a `RegexTrieDict`
        for matching (and must be in the correct format).

        Any of the above options can be set arbitrarily for each insertion.
        The default insert options for a `MatcherPythonRegex` instance can be
        changed by setting the attribute `default_insert_options` to the desired
        value.  The default can be changed between the above options at any time.

        Two combined options are `python_but_trie_for_simple` and
        `python_but_fnl_for_fixed_length`.  These use a hybrid approach to
        limit the number of patterns which need to be sequentially searched
        while still retaining longest-match behavior.  The latter option cannot
        be used in combination with the ordinary FNL method because it always
        sorts its regexes by length.  Like the ordinary FNL method this method
        cannot catch all multiple-matches (such as for "cat" and "ca[rt]") but
        it catches more.  It cannot detect multiple matches for same-length
        patterns which are matched by the FNL matcher (vs. the Python matcher).

        Note that this method does not check for reinsertions of the same token
        label; the higher-level `def_token` routine which calls it is
        responsible for that."""
        if ignore:
            self.ignore_tokens.add(token_label)
        if matcher_options is None:
            matcher_options = self.default_insert_options

        if matcher_options == "python_but_trie_for_simple":
            converted = convert_simple_python_regex_to_rtd_regex(regex_string,
                                                                 self.rtd_escape_char)
            if converted:
                regex_string = converted
                matcher_options = "trie"
            else:
                matcher_options = "python"
        elif matcher_options == "python_but_fnl_for_fixed_length":
            length = is_fixed_length(regex_string)
            if length:
                matcher_options = "python_fnl"
                self.sort_python_fnl = True
            else:
                matcher_options = "python"

        if matcher_options == "python":
            compiled_regex = re.compile(regex_string,
                                        re.VERBOSE|re.MULTILINE|re.UNICODE)
            regex_data = TokenPatternTuple(regex_string, compiled_regex, on_ties)
            self.python_data_dict[token_label] = regex_data
        elif matcher_options == "python_fnl":
            self.python_fnl_combo_regex_is_stale = True
            regex_data = (on_ties, regex_string)
            self.python_fnl_data_dict[token_label] = regex_data
        elif matcher_options == "trie":
            if not has_regex_trie_dict:
                raise MatcherException("The regex-trie-dict package is required to use"
                        " the 'trie' option with a Matcher.  Package not found.")
            if self.rtd is None:
                self.rtd = RegexTrieDict()
                self.rtd.define_meta_elems(escape=self.rtd_escape_char)
            self.trie_regex_data_dict[token_label] = (on_ties, regex_string)
            self.rtd[regex_string] = token_label
        else:
            raise MatcherException("Bad option '{0}' passed to the insert_pattern "
                                   "method of the MatcherPythonRegex instance."
                                   .format(matcher_options))

    def remove_pattern(self, token_label):
        """Remove the pattern for the token corresponding to `token_label`."""
        self.ignore_tokens.discard(token_label)

        if token_label in self.python_data_dict:
            del self.python_data_dict[token_label]
        elif token_label in self.python_fnl_data_dict:
            del self.python_fnl_data_dict[token_label]
            self.python_fnl_combo_regex_is_stale = True
        elif token_label in self.trie_regex_data_dict:
            regex = self.trie_regex_data_dict[token_label][1]
            del self.rtd[regex]
            del self.trie_regex_data_dict[token_label]
        else:
            raise MatcherException("Attempt to remove pattern for a token that"
                                   " was never defined.")

    def get_next_token_label_and_value(self, program, slice_indices,
                                       error_msg_text_snippet_size=20):
        """Return the best prefix match as a tuple of the token label and the matched
        string.  The `slice_indices` are an ordered pair of indices into the string
        `program` which reference the relevant part."""
        # TODO: Python uses pos and endpos kwargs for the slice indices... consider
        # using that convention.

        # Regular python matches.
        if self.python_data_dict:
            best_matches = self._python_get_raw_matches(program, slice_indices)
        else:
            best_matches = []
        if best_matches:
            best_matches_len = (best_matches[0].length, best_matches[0].on_ties)
        else:
            best_matches_len = (0, -INFINITY)

        # Trie matches.
        if self.trie_regex_data_dict:
            best_matches_trie = self._trie_get_raw_matches(program, slice_indices)
        else:
            best_matches_trie = []
        if best_matches_trie:
            best_matches_trie_len = (best_matches_trie[0].length, best_matches_trie[0].on_ties)
        else:
            best_matches_trie_len = (0, -INFINITY)

        # Python first-not-longest matches.
        if self.python_fnl_data_dict:
            best_matches_fnl = self._python_first_not_longest(program, slice_indices)
        else:
            best_matches_fnl = []
        if best_matches_fnl:
            best_matches_fnl_len = (best_matches_fnl[0].length, best_matches_fnl[0].on_ties)
        else:
            best_matches_fnl_len = (0, -INFINITY)

        #
        # Pick the longest match and return it.
        #

        combo_best_matches = []
        max_len = max(best_matches_len, best_matches_fnl_len, best_matches_trie_len)
        if best_matches_len == max_len:
            combo_best_matches += best_matches
        if best_matches_trie_len == max_len:
            combo_best_matches += best_matches_trie
        if best_matches_fnl_len == max_len:
            combo_best_matches += best_matches_fnl

        if not combo_best_matches:
            raise MatcherException("No matches in Lexer, unknown token at "
                    "the start of this unprocessed text:\n'{0}'"
                    .format(program[slice_indices[0]
                            :slice_indices[0] +
                                error_msg_text_snippet_size]))

        if len(combo_best_matches) > 1: # An unresolved tie, raise an exception.
            matched_text = combo_best_matches[0].matched_string
            #assert all(m.matched_string == matched_text for m in combo_best_matches)
            winning_tokens = [(m.token_label, m.on_ties) for m in combo_best_matches]
            #for t in combo_best_matches:
            #    print("t is:", t)
            #winning_tuples_as_dicts = [list(t._asdict().items()) for t in combo_best_matches]
            raise MatcherException("Multiple token patterns matched in the lexer, with"
                    " equal (length, on_ties) tuples.  Maybe use the on_ties keyword"
                    " argument to def_token to break ties.  The matching"
                    " (token_label, on_ties) pairs are: {0}\nThe ambiguity occurred"
                    " at the start of this" " unprocessed text:\n{1}"
                    .format(winning_tokens, program[
                            slice_indices[0]:
                            slice_indices[0]+ error_msg_text_snippet_size]))
        final_best = combo_best_matches[0]
        return final_best.token_label, final_best.matched_string

    def _trie_get_raw_matches(self, program, unprocessed_slice_indices):
        """A utility routine that does the actual string match on the prefix of
        `self.program` using the `RegexTrieDictScanner`.  Returns a list of
        `MatchedPrefixTuple` instances for all the best matches, including ties.
        (In non-error conditions the match must be unique, but for unresolved ties
        we want the diagnostic data, too.)"""
        if self.rtd_scanner is None:
            self.rtd_scanner = RegexTrieDictScanner(self.rtd)
        else:
            self.rtd_scanner.reset()
        scanner = self.rtd_scanner

        # TODO option not to get full text but pass slice indices...
        text = program[unprocessed_slice_indices[0]:unprocessed_slice_indices[1]]
        scanner.append_text(text)
        scanner.assert_end_of_text() # TODO: different for online, realtime
        match_list = scanner.get_prefix_matches(only_first=True)
        if not match_list:
            return []
        match = match_list[0]
        token_label_list = scanner.last_values[0]

        longest_tuple = (0, -INFINITY)
        for token_label in token_label_list:
            match_length = len(match)
            on_ties = self.trie_regex_data_dict[token_label][0]
            match_len_tuple = (match_length, on_ties)
            if match_len_tuple < longest_tuple:
                continue
            if match_len_tuple != longest_tuple:
                # final_match_list.clear() # Only in Python 3.
                final_match_list = []
            longest_tuple = match_len_tuple
            final_match_list.append(MatchedPrefixTuple(length=match_length,
                                                 on_ties=on_ties,
                                                 matched_string=match,
                                                 token_label=token_label))
        return final_match_list

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
        not longest" (FNL) matching behavior for unresolved ties.  This method
        of scanning is more efficient in recognizing the patterns, but its
        semantics depend on definition ordering and it has slow insert and
        delete time (since it builds one large regex and compiles it).  This
        implementation only assembles and compiles the combined patterns
        when it is actually called to scan text, if the current compiled
        regex is stale.

        The `on_ties` values have different semantics with this kind of scanner
        than with the others.  All the patterns are first sorted on any
        `on_ties` values provided.  Since the sort is stable the ordering
        within equal `on_ties` values remains by insertion order.
        The other matchers use `on_ties` to break ties between the longest matching
        patterns.  In this case, though, the first matching pattern in the
        sorted list is chosen.  So `on_ties` essentially becomes a way to
        override the effect of definition ordering *except* that if multiple
        low-level matchers are used the `on_ties` values are also used to
        break ties among the different ones used.

        This scanner simply returns the first match, so it does not catch
        errors due to unresolved ties!  This only applies to patterns stored in
        this matcher, however.  If a combination of matchers is used then
        some ties will still be caught.

        If the attribute `sort_python_fnl` is true the sorting is modified.
        Items in the dict are ordered by the tuples `(length, on_ties,
        regex_string)`.  This is used in the hybrid Python-FNL matcher.

        This code is based on the tokenizer in the Python 3 documentation at
        https://docs.python.org/3.6/library/re.html#writing-a-tokenizer

        There is also an undocumented scanner in Python (see
        http://lucumr.pocoo.org/2015/11/18/pythons-hidden-re-gems/
        but it is not used here."""
        if self.python_fnl_combo_regex_is_stale:
            # If the hybrid python-fnl method is being used, sort patterns by the tuples:
            #    (length, on_ties, regex_string)
            if self.sort_python_fnl:
                self.python_fnl_data_dict = collections.OrderedDict(
                        sorted(self.python_fnl_data_dict.items(),
                            key=lambda i: (is_fixed_length(i[1][1]), i[1][0], i[1][1]),
                               reverse=True))

            # Otherwise, re-sort the ordered dict by their on_ties values.
            else:
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
            on_ties = self.python_fnl_data_dict[token_label][0]
            match_list.append(MatchedPrefixTuple(length=match_length,
                                                 on_ties=on_ties,
                                                 matched_string=matched_string,
                                                 token_label=token_label))
        return match_list

#
# Exceptions.
#

class MatcherException(LexerException):
    pass

#
# Experimental below.
#

# https://stackoverflow.com/questions/11819059/regex-match-character-which-is-not-escaped
match_unescaped_prefix = "(?<!\\)(?:\\\\)*" # Prefix for finding unescaped character.

regex_special_chars = ".^$*+?{}|()[]"
regex_special_sequences = "0123456789AbBdDsSwWZ"
#regex_char_ranges =
standard_python_escapes = "abfnrtvx\\"

match_unescaped_special = "|".join(match_unescaped_prefix + c for c in regex_special_chars)

def process_string_for_escapes(string_or_char_list, escape_char,
                               open_group=None, close_group=None):
    """A utility routine which takes a string or character list with
    possibly-escaped elements as an argument and returns a list of two-tuples.

    The first element of a returned two-tuple is the actual character, and the
    second a boolean for whether or not it is escaped.

    If `open_group` and/or `close_group` is set to an opening or closing paren
    or bracket character the function returns a list of three-tuples, where the
    last element gives the level of parenthesis nesting, starting at zero and
    increasing.  An open and its corresponding close have the same level."""
    escaped = False
    tuple_list = []
    p_count = 0
    for elem in string_or_char_list:
        if elem == escape_char and not escaped:
            escaped = True
            continue
        if escaped:
            bool_val = True
            if open_group and elem == open_group:
                p_count += 1
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

def is_fixed_length(regex):
    """If the Python regex is fixed-length return its effective length (not its actual
    length).  Otherwise, return false.  Note that zero-length matches are not
    considered fixed-length.  Only a subset of fixed-length patterns are recognized,
    currently those consisting of regular characters and/or character sets."""
    # TODO: Needs to be more precise... can \b be use in char ranges, for example???
    # Note that fixed-length can be used with longest-matching, but it still
    # cannot catch matches that tie, i.e., those ties are still broken by
    # insertion order.
    #
    # Alternative algorithm: If you get a longest match, you then go to a
    # secondary list of regexes of that fixed length which have been inserted
    # and check them one-by-one like in the regular default algorithm.
    #
    # This can be combined with the default algorithm as follows.  Any
    # non-fixed-length patterns are stored as usual in the default algorithm.
    # Fixed-length patterns are all stored in ONE big regex, sorted by
    # effective length, and for each length you also save a list of all the
    # patterns of that length.  Then to match you match the non-fixed pattern
    # regexes separately and also match the single big fixed-length regex.  If
    # the former wins, return that.  If the latter wins, do a secondary
    # sequential search on the patterns of that length.
    effective_length = 0
    inside_charset = False
    for char, is_escaped in process_string_for_escapes(regex, "\\"):
        if char == "]" and not is_escaped:
            if not inside_charset:
                raise MatcherException("Closing character set range ']' with no `[`.")
            inside_charset = False
            continue
        if inside_charset:
            continue
        if char == "[" and not is_escaped:
            inside_charset = True
            effective_length += 1
            continue
        effective_length += 1
        if char in regex_special_chars and not is_escaped:
            return False
    return effective_length

def convert_simple_python_regex_to_rtd_regex(regex, rtd_escape=None):
    """Return the conversion to the `RegexTrieDict` version of the regex.
    Currently regexes consisting of regular characters and/or character sets are
    handled.

    The `rtd_escape` argument, if present, should be the escape character
    set as the `escape` attribute of the `RegexTrieDict` instance (defaulting to
    the Python escape character).

    Any Python regex special characters must be escaped to be treated as literals.
    All but matches brackets cause `False` to be returned.  The special characters
    are '{}'.""".format(regex_special_chars)
    # TODO This needs to be more precise in handling special sequences, escapes, etc.
    # See Python regex docs...
    print("regex to convert is", regex)
    python_escape = "\\"
    if not rtd_escape:
        rtd_escape = python_escape
    same_escape = (python_escape == rtd_escape)

    inside_charset = False
    converted_char_list = []
    for char, is_escaped in process_string_for_escapes(regex, python_escape):
        print("char is", char, "is_escaped is", is_escaped)
        if char == "]" and inside_charset and not is_escaped:
            if not inside_charset:
                raise MatcherException("Closing character set range ']' with no `[`.")
            converted_char_list.append(python_escape)
            converted_char_list.append("]")
            inside_charset = False
        elif char == "[" and not is_escaped:
            converted_char_list.append(python_escape)
            converted_char_list.append("[")
            inside_charset = True
        elif char == "-" and not is_escaped and inside_charset:
            converted_char_list.append(python_escape)
            converted_char_list.append("-")
        elif char == python_escape:
            print("got a python escape")
            converted_char_list.append(python_escape)
            if is_escaped and same_escape:
                print("python escape is escaped")
                converted_char_list.append(python_escape)
        elif char == rtd_escape and not same_escape:
            if is_escaped:
                # Python escaped RTD escapes are not allowed in
                # simple patterns when the chars differ.
                return False
            converted_char_list.append(rtd_escape)
            converted_char_list.append(rtd_escape)
        elif char in regex_special_chars and not is_escaped:
            return False
        else:
            if is_escaped and inside_charset:
                converted_char_list.append(python_escape)
            converted_char_list.append(char)
    converted_string = "".join(converted_char_list)
    print(converted_string)
    return converted_string

def test_fixed_length():
    assert is_fixed_length(r"xyz") == 3
    assert is_fixed_length(r"xy[a-z]z") == 4
    assert is_fixed_length(r"xy[a-z]*z") == False
    assert is_fixed_length(r"xy[a-z]\*z") == 5
    assert is_fixed_length(r"xy[a-z]\\*z") == False
    assert is_fixed_length(r"xy[a-z]\\") == 4
#test_fixed_length()

def test_simple_regex_conversion_python_to_rtd():
    print(convert_simple_python_regex_to_rtd_regex("water[abc]salad"))
    assert convert_simple_python_regex_to_rtd_regex("water[abc]salad") == "water\\[abc\\]salad"
    assert convert_simple_python_regex_to_rtd_regex("water[a-c]salad") == "water\\[a\\-c\\]salad"
    assert convert_simple_python_regex_to_rtd_regex("xyz") == "xyz"
    assert convert_simple_python_regex_to_rtd_regex(r"x\yz") == r"xyz" # Note \y to y alone!
    assert convert_simple_python_regex_to_rtd_regex(r"x\[yz\]") == r"x[yz]"
    assert convert_simple_python_regex_to_rtd_regex("x\\\\z") == "x\\\\z"
    assert convert_simple_python_regex_to_rtd_regex(r"[\s]") == r"\[\s\]"

    assert convert_simple_python_regex_to_rtd_regex("w[a-c]~s", "~") == "w\\[a\\-c\\]~~s"
    assert convert_simple_python_regex_to_rtd_regex("w(x|y)", "~") == False
    assert convert_simple_python_regex_to_rtd_regex("w~*", "~") == False

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


