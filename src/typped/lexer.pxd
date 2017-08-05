
# Maybe use this to share token_getter_fun between Lexer and TokenBuffer.
#ctypedef object (*tok_getter_type)()

#
# TokenBuffer
#

cdef class TokenBuffer(object):
    cdef token_getter_fun
    cdef public token_buffer
    cdef public long max_deque_size, max_peek, current_offset, reference_point

    #
    # Methods.
    #

    cpdef reset(self, begin_token)

    cpdef long state_to_offset(self, long state)

    cpdef long get_state(self)

    cpdef long num_saved_previous_tokens(self)

    cpdef long num_tokens_after_current(self)

    cpdef move_forward(self, long num_toks=*)

    cpdef move_back(self, long num_toks=*)

    #
    # Internal utility methods below.
    #

    cdef inline long _index_to_absolute(self, long index)

    cdef inline long _offset_to_absolute(self, long offset)

    cdef _fill_to_current_offset(self)

    cdef _pop(self) # Must be cpdef instead of cdef until TokenBuffer decl below works.

    cdef _append(self, tok=*)

#
# TokenTable
#

from matcher cimport Matcher

cdef class Lexer(object) # Forward declaration (note no colon).

cdef class TokenTable(object):
    """A symbol table holding subclasses of the `TokenNode` class for each token label
    defined in a `Lexer` instance.  Also has methods for operating on tokens.
    Each `Lexer` instance contains an instance of this class to save the subclasses for
    the kinds of tokens which have been defined for it."""
    cpdef token_subclassing_fun
    cpdef public dict token_subclass_dict # Only public because a test case uses it...
    cpdef public Lexer lex
    cpdef public str begin_token_label, end_token_label
    cpdef public begin_token_subclass, end_token_subclass
    cpdef Matcher pattern_matcher
    cpdef dict __dict__ # Allow attributes to be set.

    #
    # Methods.
    #

    cdef _create_token_subclass(self, str token_label, bint store_in_dict=*)

    cpdef undef_token_subclass(self, str token_label)

    cpdef undef_token(self, str token_label)

    cpdef def_token(self, str token_label, regex_string, long on_ties=*, bint ignore=*,
                  matcher_options=*)

    cpdef def_begin_token(self, str begin_token_label)

    cpdef def_end_token(self, str end_token_label)

    cpdef get_next_token_label_and_value(self, program, prog_unprocessed_indices,
                                             long ERROR_MSG_TEXT_SNIPPET_SIZE)

    cpdef ignored_tokens(self)

#
# Lexer
#

cdef class GenTokenState(object):
    pass
    #cdef public long ordinary, end, uninitialized # Only works on instance attributes.

cdef class Lexer(object):
    cdef public TokenBuffer token_buffer
    cdef public TokenTable token_table
    cdef public token
    cdef public bint text_is_set
    cdef public long all_token_count, non_ignored_token_count
    cdef long raw_linenumber
    cdef long upcoming_raw_linenumber, upcoming_raw_charnumber, upcoming_raw_total_chars
    cdef long token_generator_state # Holds a state, not the class object.
    cdef public final_mod_function, default_helper_exception
    cdef bint already_returned_end_token, _curr_token_is_first, _returned_first_token
    cdef public list prog_unprocessed
    cdef public program
    cpdef dict __dict__ # Allow attributes to be set.

    #
    # Methods (not all are here).
    #

    cpdef reset(self, token_table=*, max_peek_tokens=*,
                max_deque_size=*, default_begin_end_tokens=*,
                final_mod_function=*)

    cpdef set_token_table(self, TokenTable token_table, go_back=*)

    cpdef next(self, long num=*)

    cpdef peek(self, num_toks=*)

    cdef _pop_tokens(self, long n)

    cpdef go_back(self, long num_toks=*, bint num_is_raw=*)

    cpdef curr_token_is_begin(self)
    cpdef ignored_before_curr(self)
    cpdef bint curr_token_is_end(self)
    cpdef bint is_defined_token_label(self, str token_label)

    cpdef match_next(self, str token_label_to_match, long peeklevel=*, bint consume=*,
                   bint raise_on_fail=*, bint raise_on_success=*,
                   long err_msg_tokens=*)
    cpdef in_ignored_tokens(self, str token_label_to_match,
                          bint raise_on_fail=*, bint raise_on_success=*)
    cpdef no_ignored_after(self, bint raise_on_fail=*, bint raise_on_success=*)
    cpdef no_ignored_before(self, bint raise_on_fail=*, bint raise_on_success=*)

    cpdef _unbuffered_token_getter(self)

