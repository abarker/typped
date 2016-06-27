# -*- coding: utf-8 -*-
"""

The TextStream class

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
        return

    def clear(self):
        self.ts = None # the text stream object, set by the set methods
        self.EOF = True # false while a stream is open and not at end
        self.currLineNum = 1 # count lines for error reporting, starts at 1
        self.currCharNum = 0 # count chars on line for error reporting, first is 1
        self.incrementLineOnNextChar = False # used in counting lines
        self.charBuffer = collections.deque()
        self.rawIn = False
        return

    def set_string_in(self, strVal):
        self.clear()
        self.ts = io_module.StringIO(strVal) # in-memory text stream
        #self.ts = StringIO.StringIO(strVal) # in-memory text stream
        self.rawIn = False
        self.EOF = False
        return

    def set_file_in(self, fName):
        self.clear()
        self.ts = open(fName, "r")
        self.rawIn = False
        self.EOF = False
        return

    def set_raw_in(self):
        self.clear()
        print("Type ^D to leave raw input mode.")
        self.rawIn = True
        self.EOF = False
        return

    def __iter__(self):
        """So statements like: for char in textStr: etc., and comprehensions,
        can be used."""
        return self # implies that the next() method will be used for iterations

    def next(self):
        """
        Get the next character in the text stream. Can be used as
               while not ts.end_of_text_stream():
                  char = ts.next()
                  ...
        or else as
               for char in ts:
                  ...
        """
        self.refill_char_buffer_if_empty()
        if len(self.charBuffer) == 0:
            raise StopIteration("no more chars in TextStream for next()")
        retval = self.charBuffer.popleft()
        self.__incrementCounters(retval)
        return retval
    __next__ = next

    def refill_char_buffer_if_empty(self):
        """Read a line to refill the char buffer.  Return False if end of stream
        is encountered, True otherwise."""
        if len(self.charBuffer) != 0: # buffer not empty, return
            return True
        if self.rawIn:
            try: line = get_input("|- ") 
            except: # got an EOFError or some other unspecified exception
                self.EOF = True
                self.charBuffer.clear()
                return False
            self.charBuffer = collections.deque([c for c in line])
            self.charBuffer.append("\n") # restore the newline input stripped
        else:
            line = self.ts.readline() # includes the "\n" at EOL
            if line == "": # file reads return "" at EOF
                self.EOF = True # got an EOF
                self.charBuffer.clear()
                return False
            self.charBuffer = collections.deque([c for c in line])
        return True

    def peek(self):
        """Peeks one character ahead in the text stream.  Returns "" if peek is
        attempted after end_of_text_stream() is True."""
        self.refill_char_buffer_if_empty()
        if len(self.charBuffer) == 0:
            return ""
        return self.charBuffer[0]

    def __incrementCounters(self, char):
        """A utility routine for counting the current lines and characters.  Called
        each time a character is read."""
        self.currCharNum += 1
        if self.incrementLineOnNextChar:
            self.currCharNum = 1
            self.currLineNum += 1
            self.incrementLineOnNextChar = False
        if char == "\n":
            self.incrementLineOnNextChar = True

    def get_pos_of_last_next(self):
        """Return a tuple containing the line number of the last character
        returned, numbered from 1, followed by the position of the character on
        that line, also numbered from 1.  Useful for error messages."""
        return (self.currLineNum, self.currCharNum)

    def end_of_text_stream(self):
        """True if the last character in the text stream has been returned."""
        self.refill_char_buffer_if_empty()
        return self.EOF

    def beginning_of_text_stream(self):
        return self.currLineNum == 1 and self.currCharNum == 0


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
    
