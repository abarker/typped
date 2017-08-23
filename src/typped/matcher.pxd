"""

Cython definition file for `matcher.py` to improve the results of compiling in
pure Python mode.

"""
# For the namedtuples, this discussion says that cdef classes are more lightweight:
# http://grokbase.com/t/gg/cython-users/129mpzbcy7/namedtuples-in-cython
# Consider converting the code to use that.  But comparisons and indexing would
# also need to be implemented, so probably not.

cdef class Matcher(object):
    #
    # Instance attributes.
    #

    cpdef public set ignore_tokens # Note that "public" is necessary!
    cpdef python_fnl_data_dict # Really an OrderedDict but need a type to use here.
    cdef dict python_data_dict, trie_regex_data_dict
    cdef bint python_fnl_combo_regex_is_stale, sort_python_fnl # bint for booleans
    cpdef python_fnl_combo_regex # Holds both strings and compiled regexes...
    cpdef public default_insert_options
    cpdef rtd_scanner, rtd, rtd_escape_char
    cpdef object self

    #
    # Methods
    #

    cpdef insert_pattern(self, str token_label, regex_string, on_ties=*,
                         long ignore=*, matcher_options=*)

    # -----------------------------------------------------------------------

    cdef matched_text # This line and next are needed for the assert in below fun.
    cdef list combo_best_matches # If not present they give "referenced before assignment" err.

    cdef list best_matches, best_matches_trie, best_matches_fnl, winning_tokens
    cdef tuple best_matches_len, best_matches_trie_len, best_matches_fnl_len, max_len
    cpdef get_next_token_label_and_value(self, program, slice_indices,
                                         error_msg_text_snippet_size=*)

    # -----------------------------------------------------------------------

    cpdef insert_pattern(self, str token_label, regex_string, on_ties=*, long ignore=*,
                         matcher_options=*)

    # -----------------------------------------------------------------------

    cdef _python_get_raw_matches(self, program, unprocessed_slice_indices)

    # -----------------------------------------------------------------------

    # This definition below currently must be a cpdef, not a cdef.  Otherwise
    # it gives a compile error.  Apparently some Python construct in it gives
    # Cython problems.
    cpdef _python_first_not_longest(self, program, unprocessed_slice_indices)

    # -----------------------------------------------------------------------

cpdef is_fixed_length(regex)

