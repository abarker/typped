# -*- coding: utf-8 -*-
"""

NOT CURRENTLY USED.

This is the `TokenBuffer` class, extracted from `lexer.py`, done is full Cython
(rather than pure Python mode Cython).

"""

from __future__ import print_function, division, absolute_import
import collections
from .shared_settings_and_exceptions import LexerException, is_subclass_of

#ctypedef object (*tok_getter_type)()

cdef class TokenBuffer(object):
    """An abstraction of the token buffer.  This is used internally by the
    `Lexer` class and should not usually be accessed by users.  It is basically
    a nice wrapper over an underlying deque, but this is complicated by the
    need to save persistent state pointers into the buffer even in fixed-size
    buffers when tokens at the front get dropped.

    Previous tokens are stored in the same deque as the current token and any
    lookahead tokens.  The default indexing is relative to the current token,
    at `current_offset`, which is zero for the current token.  (The current
    offset is itself relative to a reference point, but users do not need to
    know that detail)."""
    cdef token_getter_fun
    cdef public token_buffer
    cdef public long max_deque_size, max_peek, current_offset, reference_point

    def __init__(self, token_getter_fun, long max_peek, long max_deque_size,):
        """Initialize the buffer."""
        self.token_getter_fun = token_getter_fun
        self.max_deque_size = max_deque_size
        self.max_peek = max_peek
        # Any popleft operations are done explicitly, so maxlen=None.
        self.token_buffer = collections.deque(maxlen=None)

        # Indices are relative to current_offset, and current_offset is
        # relative to reference_point.  This is because a fixed-size deque can
        # drop elements.  The current_offset is not relative to 0 because then
        # a saved offset would become invalid when items are popped off the
        # left of the deque.  The reference point is decremented for every
        # deque element popped off the left, and at no other time.  That way,
        # the current offset can be saved and remain valid until the
        # `TokenBuffer` is reset (though the referenced item may get deleted).
        self.current_offset = 0
        self.reference_point = 0

    cpdef reset(self, begin_token):
        """Initialize the token buffer, or clear an reset it.  Any saved
        offsets are no longer valid, but no check is made for that."""
        self.current_offset = 0
        self.reference_point = 0
        self.token_buffer.clear()
        self._append(begin_token)

    cpdef int state_to_offset(self, long state):
        """Return the offset into the current deque that corresponds to what
        was the offset (absolute index to the current token) at the time when
        the state was saved."""
        return state - self.reference_point

    cpdef int get_state(self):
        """Return a buffer state indicator that can be returned to later.  The
        `go_back` or `push_back` methods of the lexer use this."""
        return self._offset_to_absolute(self.current_offset)

    def __getitem__(self, long index):
        """Index the buffer relative to the current offset.  Zero is the
        current token.  Negative indices go back in the buffer.  They **do not**
        index from the end of the buffer, as with ordinary Python indexing."""
        # Slices are CURRENTLY NOT allowed.  What should they return?  How
        # do you know where the zero point is?
        #if isinstance(index, slice): # Handle slices recursively.
        #    start = index.start
        #    stop = index.stop
        #    step = index.step
        #    # The indices call returns the above three in a tuple.
        #    return [self[i] for i in range(*index.indices(len(self)))]
        if isinstance(index, int): # The ordinary case.
            if self.max_peek >= 0 and index > self.max_peek:
                raise LexerException("User-set maximum peeking level of {0} was"
                                     " exceeded.".format(self.max_peek))
            while self._index_to_absolute(index) >= len(self.token_buffer):
                if self.token_buffer[-1].is_end_token():
                    return self.token_buffer[-1]
                self._append()
            return self.token_buffer[self._index_to_absolute(index)]
        else:
            raise TypeError("Invalid argument type in __getitem__ of TokenBuffer.")

    cpdef int num_saved_previous_tokens(self):
        """Return the number of tokens before the current token that are saved."""
        return self._offset_to_absolute(self.current_offset)

    cpdef int num_tokens_after_current(self):
        """An informational method.  Returns the number of tokens from
        the current token to the end of the token buffer.  Some may have been
        read past the position of the current token due to peeks or
        pushbacks."""
        cdef int begin_point
        begin_point = self._offset_to_absolute(self.current_offset) + 1
        return len(self.token_buffer) - begin_point

    def move_forward(self, long num_toks=1):
        """Move the current token (i.e., the offset) forward by one.  This is
        the token buffer's equivalent of `next`, except that it returns
        previously-buffered tokens if possible.  The `Lexer` method `next`
        should always be called by users of that class, because it also handles
        some other things.

        Attempts to move past the first end-token leave the current offset at
        the first end-token.  No new tokens are added to the buffer.  The
        end-token is returned."""
        for _ in range(num_toks):
            if self[0].is_end_token():
                break
            self.current_offset += 1
            self._fill_to_current_offset()
        return self[0]

    def move_back(self, long num_toks=1):
        """Move the current token (i.e., offset) back `num_toks` tokens.  Will
        always stop at the begin-token.  Users should check the condition if it
        matters.  If the move attempts to move back to before the
        currently-saved tokens, but the begin-token is no longer saved, then a
        `LexerException` is raised."""
        cdef int absolute_index
        self.current_offset -= num_toks
        absolute_index = self._offset_to_absolute(self.current_offset)
        if absolute_index < 0:
            self.current_offset += abs(absolute_index)
            curr_token = self[0]
            if not curr_token.is_begin_token():
                raise LexerException("Not enough saved tokens to move back to the"
                                     " begin-token in the `move_back` method.")
        else:
            curr_token = self[0]
        return curr_token

    #
    # Internal utility methods below.
    #

    cdef inline int _index_to_absolute(self, long index):
        """Convert an index into an absolute index into the current deque.
        Note that any changes to the current offset or to the reference
        point (the latter via _append) will invalidate the absolute reference.
        In those cases it will need to be re-calculated."""
        return index + self.current_offset + self.reference_point

    cdef inline int _offset_to_absolute(self, long offset):
        """Convert an offset into an absolute index into the current deque.
        Note that calls to `_append` can modify the reference point and
        invalidate the absolute index.  Needs to be re-calculated after
        such a call."""
        return offset + self.reference_point

    def _fill_to_current_offset(self):
        """If the current offset points past the end of the token buffer then
        get tokens and append them until it is a valid index.  Note this
        calls `_append`.  If the current offset is past the first end token
        in the text then the current offset point is reset to the first end
        token.  Only one end token is ever stored in the buffer."""
        while self._offset_to_absolute(self.current_offset) >= len(self.token_buffer):
            self._append() # Note this call can change self.reference_point.

    cpdef _pop(self):
        """Users should not call.  Pop off the rightmost item and return it.  Moves
        the current token backward if necessary."""
        retval = self.token_buffer.pop()
        if self._offset_to_absolute(self.current_offset) >= len(self.token_buffer):
            self.current_offset -= 1
        return retval

    cpdef _append(self, tok=None):
        """Append to buffer and fix current index if necessary.  Users should not
        call.  If `tok` is not set then the token to append is obtained from the
        `token_getter_fun` function.

        Note that this is the **only** method that ever gets tokens directly
        from the token getter function."""
        # TODO: should this fill with end tokens, or stop at the first end token?
        if tok is None:
            tok = self.token_getter_fun()
            # TODO: below causes a FAIL with go_back hanging... probably __getitem__
            # calling but not compensating for this behavior...
            if tok.is_end_token() and self.token_buffer[-1].is_end_token():
                return tok
        self.token_buffer.append(tok)
        if (self.max_deque_size >= 0
                and len(self.token_buffer) > self.max_deque_size):
            self.reference_point -= 1
            self.token_buffer.popleft() # Do an explicit popleft.
            if self._offset_to_absolute(self.current_offset) < 0:
                raise LexerException("Error in TokenBuffer:"
                    " Maximum buffer size is too small for the amount of peeking."
                    " Current token was deleted.")
            assert len(self.token_buffer) == self.max_deque_size

