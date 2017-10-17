
Lookbehind
----------

Another minor generalization to Pratt parsing is the use of "lookbehind"
information.  A Pratt parser can use lookahead information from the lexer in
defining preconditions, etc.  In some cases lookbehind information, looking at
the previous ``processed_left`` values for the current subexpression, could be
useful.  This is a simple modification, and is currently implemented.  In the
``recursive_parse`` function, whenever the ``processed_left`` variable is
assigned a new value, that value is also appended to a list called
``lookbehind``.  The ``lookbehind`` list is passed as an argument to all tail
handler functions in addition to the ``processed_left`` value, so they
can make use of it if desired.

Since the lookbehind tokens have already been processed they allow the
preconditions to make use of information such as resolved type information (not
just token label information).  Of course you could already look at the
``left`` variable in a tail handler and see the type of the subexpression for,
say, the type of the left operand of an operator.

Note that the ``lookbehind`` list contains references, not copies, and so the
previous values will generally be modified versions of what they were when they
were first appended to the list.  The main thing that the lookbehind list tells
you is how many subexpressions preceed the current one at its same level in the
recursion.  In theory, the whole head versus tail distinction could be
eliminated and replaced with preconditions on whether or not the lookbehind
list is empty.  The distinction between head and tail handlers is, however,
useful in practice and so has been kept.

Lookbehind information is not a feature which will be commonly used, but it may
have some use cases.

