
import re
import collections
from .shared_settings_and_exceptions import LexerException
from .regex_trie_dict_scanner import RegexTrieDictScanner, RegexTrieDict

# For the namedtuples, this discussion says that cdef classes are more lightweight:
# http://grokbase.com/t/gg/cython-users/129mpzbcy7/namedtuples-in-cython
# So consider converting the code to use that.

cdef class Matcher(object):
    #
    # Instance attributes.
    #

    cpdef public set ignore_tokens # Note that "public" is necessary!
    cpdef python_fnl_data_dict # Really an OrderedDict!
    cdef dict python_data_dict, trie_regex_data_dict
    cdef int python_fnl_combo_regex_is_stale, sort_python_fnl # int for booleans
    cpdef python_fnl_combo_regex # Holds both strings and compiled regexes...
    cpdef public default_insert_options
    cpdef rtd_scanner, rtd, rtd_escape_char
    cpdef object self

    #
    # Methods
    #

    cpdef insert_pattern(self, str token_label, str regex_string, int on_ties=*,
                         int ignore=*, matcher_options=*)

    # -----------------------------------------------------------------------

    cdef str matched_text # This line and next are needed for the assert in below fun.
    cdef list combo_best_matches # If not present they give "referenced before assignment" err.

    cdef list best_matches, best_matches_trie, best_matches_fnl, winning_tokens
    cdef tuple best_matches_len, best_matches_trie_len, best_matches_fnl_len, max_len
    cpdef get_next_token_label_and_value(self, str program, slice_indices,
                                         error_msg_text_snippet_size=*)

    # -----------------------------------------------------------------------

    cpdef insert_pattern(self, str token_label, str regex_string, int on_ties=*, int ignore=*,
                         matcher_options=*)

    # -----------------------------------------------------------------------

    #cdef str regex_str, matched_string
    #cdef str token_label
    #cdef int on_ties
    #cdef _python_get_raw_matches(self, str program, unprocessed_slice_indices)

    # -----------------------------------------------------------------------

    #cdef int matched_length #, on_ties
    #cdef _python_first_not_longest(self, str program, unprocessed_slice_indices)

    # -----------------------------------------------------------------------

cpdef is_fixed_length(str regex)

