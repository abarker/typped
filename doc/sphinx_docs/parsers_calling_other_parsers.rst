
Parsers calling other parsers
=============================

.. warning::

    NOT FULLY TESTED, FUTURE FEATURE.  See the test file
    ``example_parser_called_from_parser.py`` for a simple, working
    example.

It is useful for a parser instance to be able to call another parser instance
in order to parse an expression in a sublanguage that the second parser is
defined to parse.  Strictly speaking, the other parser would be called from one
of the handler functions of a token, but the tokens are always associated with
a fixed parser and make the call in implementing its ``parse`` command.

In the design for how this should work we want the associated parsers to be as
independent as possible.  Separate parsers are considered to parse sublanguages
which are disjoint in the sense that all the tokens in a language and any of
its sublanguages are distinct -- even when they represent the same thing, such
as, say, a left paren.  This separation is necessary in the underlying Pratt
parser design where tokens are associated with handler functions, and handler
functions essentially define the language being parsed.

If a final parse tree needs to be converted to a representation such that
possible overlapping tokens from different parsers are always instances of the
same class then a postprocessing step can be used.  This is a simple example of
converting the parse/expression tree to an AST.

One approach to the design would be to pass around a common text stream
instance, with each parser calling its ``parse`` function on the remaining
unprocessed part.  That would be elegant in a sense, but would require
reinitializing the text for each parser's lexer, for each subexpression parsed
by that parser.  Things like line and column numbers in the text could become
more difficult to keep track of.

Instead of passing around a text stream, the Typped parsers call other parsers
by passing around a common lexer, already initialized with the text.  The lexer
is just instructed to look at a different token table, corresponding to
whatever parser is using the lexer at the time.  It is like if one parser said
to another, "Hey, can you do me a favor and parse a subexpression of your
language from the text in this lexer that I've been using?  Use your own token
table with it (since that defines your sublanguage).  When you are done,
restore the lexer's token table and pass me the result."

From the user's point of view this operation is simple.  Suppose you are in a
handler for an atomic formula and your want to use a separate parser, named
``term_parser``, to parse terms inside the handler.  Inside the handler you can
just call ``term_parser.parse_from_lexer(self.parser.lex)``, passing in the
current lexer instead of the usual arguments to ``parse``.

Some inefficiency is introduced when calling another parser because when the
token table of the lexer is switched the lexer then needs to discard any
lookahead tokens parsed using the old token table.  This should not be
significant in most cases, especially since the lexer only does lookahead
on-demand.

