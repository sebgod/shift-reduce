%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: lexer.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Wed Nov 19 18:37:44 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.lexer.

:- interface.

:- import_module char.
:- import_module list.
:- import_module stream.
:- import_module io.
:- import_module shift_reduce.egt.
:- import_module shift_reduce.egt.grammar.
:- import_module shift_reduce.egt.state.
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

:- type lexer
    --->    lexer(
                lexer_input_stream  :: text_input_stream,
                lexer_grammar       :: grammar
            ).

:- type lexer_io
    --->    lexer_io(
                lio_io      :: io,
                lio_state   :: lexer_state
            ).

:- type token
    --->    token(
                token_index :: table_index,
                token_chars :: list(char)
            ).

:- instance stream(lexer, lexer_io).

:- instance input(lexer, lexer_io).

:- instance reader(lexer, token, lexer_io, io.error).

:- pred lex(lexer::in, lexer_io::di, lexer_io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for size/1
:- import_module require.
:- import_module string.
:- import_module shift_reduce.egt.charset.
:- import_module shift_reduce.egt.state.
:- import_module int.

%----------------------------------------------------------------------------%

lex(Lexer, !LexerIO) :-
    stream.get(Lexer, Result, !LexerIO),
    (
        Result = stream.ok(token(Index, Chars)),
        trace [io(!TraceIO)] (
            io.format("(%d, %s)\n",
                [i(Index), s(from_char_list(Chars))], !TraceIO)
        )
    ;
        Result = stream.error(Error),
        error(io.error_message(Error))
    ;
        Result = stream.eof
    ).

%----------------------------------------------------------------------------%

:- instance stream(lexer, lexer_io) where [
    pred(name/4) is lexer_name
].

:- pred lexer_name(lexer::in, name::out, lexer_io::di, lexer_io::uo).

lexer_name(Lexer, Name,
    lexer_io(!.IO, States), lexer_io(!:IO, make_unique(States))) :-
    name(Lexer ^ lexer_input_stream, Name, !IO).

:- instance input(lexer, lexer_io) where [].

:- instance reader(lexer, token, lexer_io, io.error) where [
    pred(get/4) is read_token
].

:- pred read_token(lexer::in, result(token, io.error)::out,
    lexer_io::di, lexer_io::uo).

read_token(Lexer, Result, !LexerIO) :-
    Grammar = Lexer ^ lexer_grammar,
    LexerState = !.LexerIO ^ lio_state,
    Charsets = Grammar ^ charsets,
    ( if
        is_in_charsets('9', Charsets, _),
        is_in_charsets('A', Charsets, _),
        is_in_charsets(' ', Charsets, _)
    then
        Result = ok(token(-1, []))
    else
        unexpected($file, $pred, "cannot find token in charset")
    ).

:- pragma memo(is_in_charsets/3).

:- pred is_in_charsets(char::in, table(charset)::in,
    table_index::out) is semidet.

is_in_charsets(Char, Charsets, Match) :-
    is_in_charsets(Char, Charsets, 0, Charsets ^ size, Match).

:- pred is_in_charsets(char::in, table(charset)::in,
    table_index::in, table_index::in, table_index::out) is semidet.

is_in_charsets(Char, Charsets, Index, Size, Match) :-
    Index < S ize,
    ( if
        Charset = Charsets ^ unsafe_elem(Index),
        is_in_charset(Char, Charset)
    then
        Match = Index,
        trace [io(!IO)] (
            io.write_char(Char, !IO),
            io.write_string(" is in ", !IO),
            io.write_int(Match, !IO),
            io.nl(!IO)
        )
    else
        is_in_charsets(Char, Charsets, Index+1, Size, Match)
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.lexer.
%----------------------------------------------------------------------------%
