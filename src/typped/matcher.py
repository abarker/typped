# -*- coding: utf-8 -*-
"""

TODO: The trie matcher is not currently fully implemented.

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

# These tuples are temporarily generated during the match-finding.
MatchedPrefixTuple = collections.namedtuple("MatchedPrefixTuple", [
                           "length",
                           "on_ties",
                           "matched_string",
                           ])

INFINITY = float("inf")

class MatcherPythonRegex(object):
    """A matcher class that stores pattern data and matches it."""

    def __init__(self):
        # TODO: Have an option to only use trie or only use Python regex by default
        # regardless of options.  Maybe an option to use trie only on simple
        # patterns...
        self.ignore_tokens = set() # The set of tokens to ignore.
        self.python_regex_data_dict = {} # Data for the regexes of tokens.
        self.trie_regex_data_dict = {} # Data for the regexes stored in the trie.
        self.insert_options = "python"
        self.rtd = None # Trie patterns stored here; only instantiated if needed.
        self.rtd_escape_char = "\\"

    def insert_pattern(self, token_label, regex_string, on_ties=0, ignore=False,
                       options=None):
        """Insert the pattern in the list of regex patterns.

        If `ignore` is true then the pattern is treated as an ignored pattern.

        If `options="trie"` then the pattern is inserted in a `RegexTrieDict`
        for matching (and must be in the correct format).

        If `options=None` the method will instead use the value of the class
        instance attribute `insert_options` (which defaults to `"python"` to
        use Python regexes)."""
        if ignore:
            self.ignore_tokens.add(token_label)
        if options is None:
            options = self.insert_options

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
            self.python_regex_data_dict[token_label] = regex_data
        else:
            raise MatcherException("Bad option '{0}' passed to the insert_pattern method"
                                 " of the MatcherPythonRegex instance.".format(options))

    def remove_pattern(self, token_label):
        """Remove the pattern for the token corresponding to `token_label`."""
        # Remove from the list of defined tokens and from the token table.
        self.ignore_tokens.discard(token_label)

        if token_label in self.python_regex_data_dict:
            del regex_data_dict[token_label]
        elif token_label in self.trie_regex_data_dict:
            regex = regex_data_dict[token_label].regex_string
            del self.rtd[regex]
            del regex_data_dict[token_label]
        else:
            raise MatcherException("Attempt to remove pattern for a token that"
                                   " was never defined.")

    def get_next_token_label_and_value(self, program, unprocessed_slice_indices,
                                       ERROR_MSG_TEXT_SNIPPET_SIZE):
        """Find the `(len, on_ties)` tuple in `len_and_on_ties_dict` which is
        longest and wins tie breaking.  Return the token token_label and value of the
        matching prefix.  The list arguments should be in correspondence with
        the `self.token_labels` list."""
        if self.python_regex_data_dict:
            raw_matcher = self._python_get_raw_matches
            token_label, value = self._get_next_token_label_and_value(raw_matcher,
                                           program, unprocessed_slice_indices,
                                           ERROR_MSG_TEXT_SNIPPET_SIZE)
        if self.trie_regex_data_dict:
            raw_matcher = self._trie_get_matched_prefixes_and_length_info
            trie_token_label, trie_value = self._get_next_token_label_and_value(
                                                raw_matcher, program,
                                                unprocessed_slice_indices,
                                                ERROR_MSG_TEXT_SNIPPET_SIZE)
            pass # TODO

        longest_value = value
        label_of_longest = token_label
        return label_of_longest, longest_value

    #
    # The RegexTrieDictScanner methods.
    #

    #
    # The Python regex matcher methods.
    #

    def _get_next_token_label_and_value(self, raw_matcher, program,
                                        unprocessed_slice_indices,
                                        ERROR_MSG_TEXT_SNIPPET_SIZE):
        """Find the `(len, on_ties)` tuple in `len_and_on_ties_dict` which is
        longest and wins tie breaking.  Return the token token_label and value of the
        matching prefix.  The list arguments should be in correspondence with
        the `self.token_labels` list."""
        # MatchedPrefixTuple format is: (length, on_ties, matched_string)

        # TODO: Converting to a list return value for raw_matcher causes errors!
        # Things like sin vs identifier are flagged as multiples... but really
        # the on_ties should take care of it, since it is -1 on most identifiers..

        # Get the matching prefixes and length-ranking information.
        match_dict = raw_matcher(program, unprocessed_slice_indices)

        # Note that tuple comparisons give the correct max value.
        if match_dict:
            winning_match = max(match_dict.values())
        if not match_dict or winning_match.length == 0:
            raise MatcherException("No matches in Lexer, unknown token at "
                    "the start of this unprocessed text:\n{0}"
                    .format(program[unprocessed_slice_indices[0]
                            :unprocessed_slice_indices[0] +
                                ERROR_MSG_TEXT_SNIPPET_SIZE]))

        # We know the winning tuple's value, now see if it is unique.
        winning_items = []
        for item in match_dict.items():
            token_label, (length, on_ties, matched_string) = item
            if length == winning_match.length and on_ties == winning_match.on_ties:
                winning_items.append(item)

        if len(winning_items) > 1: # Still have a tie, raise an exception.
            winning_labels = [i[0] for i in winning_items]
            raise MatcherException("There were multiple token-pattern matches"
                    " with the same length, found in Lexer.  Set the on_ties"
                    " keyword arguments to break ties.  The possible token"
                    " types and their matched text are: {0}\n"
                    " Ambiguity at the start of this "
                    " unprocessed text:\n{1}".format(winning_labels,
                        program[unprocessed_slice_indices[0]
                            :unprocessed_slice_indices[0] +
                                ERROR_MSG_TEXT_SNIPPET_SIZE]))

        # Got unique winner; use its index to get corresponding winning_index.
        winning_item = winning_items[0]
        token_label = winning_item[0]
        value = match_dict[token_label].matched_string
        return token_label, value

    def _python_get_raw_matches(self, program, unprocessed_slice_indices):
        """A utility routine that does the actual string match on the prefix of
        `self.program`.  Returns the list of best matching prefixes and corresponding
        data for the error message in case of ties."""
        # Note that Python's finditer finds the *first* match group and stops.
        # They are ordered by the order they occur in the regex.  It finds the
        # longest match of any particular group, but stops when it finds a
        # match of some group.  Instead of using that, this code loops over all
        # the separate patterns to find the overall longest, breaking ties with
        # on_ties values.
        match_dict = {} # Dict of MatchedPrefixTuple instances keyed by token labels.
        longest_tuple = (0, -INFINITY)
        for token_label, (regex_str, compiled_regex, on_ties
                                           ) in self.python_regex_data_dict.items():
            match_object = compiled_regex.match(program,
                                                unprocessed_slice_indices[0],
                                                unprocessed_slice_indices[1])
            if match_object:
                matched_string = program[match_object.start():match_object.end()]
                match_length = len(matched_string)
                match_len_tuple = (match_length, on_ties)
                if match_len_tuple < longest_tuple:
                    continue
                #elif match_len_tuple == longest_tuple:
                #    # Exception can be caught here, but we want all matches for err msg.
                #    raise MatcherException("Multiples!!!!!!!!")
                longest_tuple = match_len_tuple
                match_dict[token_label] = MatchedPrefixTuple(
                                                length=match_length,
                                                on_ties=on_ties,
                                                matched_string=matched_string)
        return match_dict


def _convert_simple_pattern(self, regex_string):
    """Convet a simple pattern to a form that can be inserted into a
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

