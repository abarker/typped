# -*- coding: utf-8 -*-
"""

The `TextStream` class.  Not currently used, may become an abstraction layer for
the stream of text to be parsed.

"""

from __future__ import print_function, division, absolute_import

import sys
import collections # for deque
import readline

if sys.version_info[0] >= 3:
    get_input = input
    import io as io_module
else:
    get_input = raw_input
    # Python 2.7 has two StringIO versions, io and StringIO.  The one in io
    # only takes unicode (it is Python 3 ready), but StringIO one takes str
    # inits too.
    import StringIO as io_module


class TextStream(object):
    """This is just a unified wrapper for a stream of text characters which may
    come from various sources."""

    def __init__(self):
        """Initialize the basic object.  Some set method must also be called to
        determine the type of input before next() can be called."""
        self.clear()

    def clear(self):
        """Clear and reset the text stream."""
        self.ts = None # the text stream object, set by the set methods
        self.EOF = True # false while a stream is open and not at end
        self.curr_line_num = 1 # count lines for error reporting, starts at 1
        self.curr_char_num = 0 # count chars on line for error reporting, first is 1
        self.increment_line_on_next_char = False # used in counting lines
        self.char_buffer = collections.deque()
        self.raw_in = False

    def set_string_in(self, str_val):
        """Set a string to be the text source."""
        self.clear()
        self.ts = io_module.StringIO(str_val) # in-memory text stream
        #self.ts = StringIO.StringIO(str_val) # in-memory text stream
        self.raw_in = False
        self.EOF = False

    def set_file_in(self, f_name):
        """Set a file to be the text source."""
        self.clear()
        self.ts = open(f_name, "r")
        self.raw_in = False
        self.EOF = False

    def set_raw_in(self):
        """Read input interactively for text source."""
        self.clear()
        print("Type ^D to leave raw input mode.")
        self.raw_in = True
        self.EOF = False

    def __iter__(self):
        """So statements like: for char in textStr: etc., and comprehensions,
        can be used."""
        return self # implies that the next() method will be used for iterations

    def next(self):
        """Get the next character in the text stream. Can be used as::

               while not ts.end_of_text_stream():
                  char = ts.next()
                  ...

        or else as::

               for char in ts:
                  ...

        """
        self.refill_char_buffer_if_empty()
        if len(self.char_buffer) == 0:
            raise StopIteration("no more chars in TextStream for next()")
        retval = self.char_buffer.popleft()
        self.__increment_counters(retval)
        return retval
    __next__ = next

    def refill_char_buffer_if_empty(self):
        """Read a line to refill the char buffer.  Return `False` if end of stream
        is encountered, `True` otherwise."""
        if len(self.char_buffer) != 0: # buffer not empty, return
            return True
        if self.raw_in:
            try:
                line = get_input("|- ")
            except: # got an EOFError or some other unspecified exception
                self.EOF = True
                self.char_buffer.clear()
                return False
            self.char_buffer = collections.deque([c for c in line])
            self.char_buffer.append("\n") # restore the newline input stripped
        else:
            line = self.ts.readline() # includes the "\n" at EOL
            if line == "": # file reads return "" at EOF
                self.EOF = True # got an EOF
                self.char_buffer.clear()
                return False
            self.char_buffer = collections.deque([c for c in line])
        return True

    def peek(self):
        """Peeks one character ahead in the text stream.  Returns empty string if
        peek is attempted after `end_of_text_stream()` is `True`."""
        self.refill_char_buffer_if_empty()
        if len(self.char_buffer) == 0:
            return ""
        return self.char_buffer[0]

    def __increment_counters(self, char):
        """A utility routine for counting the current lines and characters.  Called
        each time a character is read."""
        self.curr_char_num += 1
        if self.increment_line_on_next_char:
            self.curr_char_num = 1
            self.curr_line_num += 1
            self.increment_line_on_next_char = False
        if char == "\n":
            self.increment_line_on_next_char = True

    def get_pos_of_last_next(self):
        """Return a tuple containing the line number of the last character
        returned, numbered from 1, followed by the position of the character on
        that line, also numbered from 1.  Useful for error messages."""
        return (self.curr_line_num, self.curr_char_num)

    def end_of_text_stream(self):
        """True if the last character in the text stream has been returned."""
        self.refill_char_buffer_if_empty()
        return self.EOF

    def beginning_of_text_stream(self):
        return self.curr_line_num == 1 and self.curr_char_num == 0


#
#  Run test cases.
#

if __name__ == "__main__":

    #import py.test
    #py.test.main(["-v", "test/test_text_stream.py"]) # this needs pytest 2.0

    # exit(0) # comment this out to test interactive
    print("\nTest interactive...\n")
    readline.parse_and_bind('set editing-mode vi')

    ts = TextStream()
    ts.set_raw_in()  # reads data from raw_input

