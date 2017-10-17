
Lookbehind and other extra data
-------------------------------

Another minor generalization to Pratt parsing is the use of "lookbehind"
information.  A Pratt parser can use lookahead information from the lexer in
defining preconditions, etc.  In some cases lookbehind information, looking at
the previous ``processed_left`` values for the current subexpression, could be
useful.

This is a simple modification, and is currently implemented.  In the
``recursive_parse`` function, whenever the ``processed_left`` variable is
assigned a new value, that value is also appended to a list called
``lookbehind``.  This list is temporarily set as an attribute of the triggering
token, and so can be accessed as ``tok.extra_data.lookbehind`` in both the
handler functions and in the preconditions functions.

Since the lookbehind tokens have already been processed they allow the
preconditions to make use of information such as resolved type information (not
just token label information).  Of course you could already look at the
``left`` variable in a tail handler and see the type of the subexpression for,
say, the type of the left operand of an operator.

Note that the ``lookbehind`` list contains references, not copies, and so the
previous values will generally be modified versions of what they were when they
were first appended to the list.  The main thing that the lookbehind list tells
you is how many subexpressions precede the current one at its same level in the
recursion.  In theory, the whole head versus tail distinction could be
eliminated and replaced with preconditions on whether or not the lookbehind
list is empty.  The distinction between head and tail handlers is useful in
practice, however, and so has been kept.

Lookbehind information is not a feature which will be commonly used, but it may
have some use cases.

In addition to lookbehind information, a namedtuple containing other
information that users might want to access during parsing is temporarily set
as the attribute ``extra_data`` of a triggering token.  The current
subexpression precedence is available as ``subexp_prec``.  Also available is a
list ``constructs`` which contains all the constructs for all the previous
sub-subexpressions of the subexpression.  They are appended just after they are
dispatched, and so the current construct is available in head or tail handler
functions.

