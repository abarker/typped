# -*- coding: utf-8 -*-
"""

A matcher for a `TokenTable` that uses a hybrid of Python regex processing for
complicated patterns and a `RegexTrieDict` for simple patterns.

"""

from __future__ import print_function, division, absolute_import

# Run tests when invoked as a script.
if __name__ == "__main__":
    import pytest_helper
    pytest_helper.script_run(["../../test/test_lexer.py",
                              "../../test/test_pratt_parser.py"],
                              pytest_args="-v")

import re
import collections
from .shared_settings_and_exceptions import LexerException

TokenPatternTuple = collections.namedtuple("TokenPatternTuple", [
                           "regex_string",
                           "compiled_regex",
                           "on_ties",
                        ])

class RegexTrieHybridMatcher(object):
    """A matcher class that stores pattern data and matches it."""

    def __init__(self):
        self.ignore_tokens = set() # The set of tokens to ignore.
        self.regex_data_dict = {} # Data for the regexes of tokens.
        self.regex_trie_dict = None # Simple patterns may be stored here.

    def insert_pattern(self, token_label, regex_string, on_ties, ignore):
        """Insert the pattern in the list of regex patterns, after compiling it."""
        if ignore:
            self.ignore_tokens.add(token_label)

        simple_pattern = self._convert_simple_pattern(regex_string)

        if simple_pattern: # Put simple pattern in a trie.
            regex_data = TokenPatternTuple(regex_string, None, on_ties)
            print("got a simple one", regex_data)
            import sys
            sys.exit(0)
            # TODO: Insert in self.regex_trie_dict, add search using it, too.
            # Be sure to modify the remove_pattern routine, too.

        else: # Use Python regexes.
            compiled_regex = re.compile(regex_string,
                                    re.VERBOSE|re.MULTILINE|re.UNICODE)
            regex_data = TokenPatternTuple(regex_string, 
                                           compiled_regex,
                                           on_ties)
            self.regex_data_dict[token_label] = regex_data

    def remove_pattern(self, token_label):
        """Remove the pattern for the token corresponding to `token_label`."""
        # Remove from the list of defined tokens and from the token table.
        self.ignore_tokens.discard(token_label)
        try:
            del regex_data_dict[token_label]
        except KeyError:
            raise LexerException("Attempt to undefine pattern for token that"
                                 " was never defined.")

    def get_winning_token_label_and_value(self, program, unprocessed_slice_indices,
                                          ERROR_MSG_TEXT_SNIPPET_SIZE):
        """Find the `(len, on_ties)` tuple in `len_and_on_ties_dict` which is
        longest and wins tie breaking.  Return the token token_label and value of the
        matching prefix.  The list arguments should be in correspondence with
        the `self.token_labels` list."""
        # Get the matching prefixes and length-ranking information.
        matching_prefixes_dict, len_and_on_ties_dict = \
                   self._get_matched_prefixes_and_length_info(
                           program, unprocessed_slice_indices)

        # Note that tuple comparisons give the correct max value.
        winning_tuple = max(len_and_on_ties_dict.values())
        if winning_tuple[0] == 0:
            raise LexerException("No matches in Lexer, unknown token at "
                    "the start of this unprocessed text:\n{0}"
                    .format(program[unprocessed_slice_indices[0]
                            :unprocessed_slice_indices[0] +
                                ERROR_MSG_TEXT_SNIPPET_SIZE]))

        # We know the winning tuple's value, now see if it is unique.
        winning_items = []
        for item in len_and_on_ties_dict.items():
            if item[1] == winning_tuple:
                winning_items.append(item)

        if len(winning_items) > 1: # Still have a tie, raise an exception.
            winning_labels = [i[0] for i in winning_items]
            raise LexerException("There were multiple token-pattern matches"
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
        value = matching_prefixes_dict[token_label]
        return token_label, value

    def _get_matched_prefixes_and_length_info(self, program, unprocessed_slice_indices):
        """A utility routine that does the actual string match on the prefix of
        `self.program`.  Return the list of matching prefixes and a list of
        (length, on_ties) data for ranking them."""
        # Note that Python's finditer finds the *first* match group and stops.
        # They are ordered by the order they occur in the regex.  It finds the
        # longest match of any particular group, but stops when it finds a
        # match of some group.  Instead of using that, this code loops over all
        # the separate patterns to find the overall longest, breaking ties with
        # on_ties values.
        matching_prefixes_dict = {} # All the prefix strings that match some token.
        len_and_on_ties_dict = {}
        for token_label, (regex_str, compiled_regex, on_ties
                                                ) in self.regex_data_dict.items():
            match_object = compiled_regex.match(program, 
                                      unprocessed_slice_indices[0],
                                      unprocessed_slice_indices[1])
            if match_object: 
                matched_string = program[
                                 match_object.start():match_object.end()]
                matching_prefixes_dict[token_label] = matched_string
                # Save info to compare matches by length, break ties if necessary.
                len_on_ties_tuple = (len(matched_string), on_ties)
                len_and_on_ties_dict[token_label] = len_on_ties_tuple
            else: # Match returns None if nothing matches, not a MatchObject.
                matching_prefixes_dict[token_label] = ""
                len_and_on_ties_dict[token_label] = (0, on_ties)
        return matching_prefixes_dict, len_and_on_ties_dict

    def _convert_simple_pattern(self, regex_string):
        """Convet a simple pattern to a form that can be inserted into a
        `RegexTrieDict`, if possible.  Returns `None` if the pattern is too
        complicated."""
        return None 
        # TODO the immediate below seems to work for some very simple patterns.
        # Test it some more.  Can put in Lexer test file!!!
        # Insert it into tree.  Still need to modify the search, too.

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

