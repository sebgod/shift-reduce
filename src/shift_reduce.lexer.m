%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
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
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

:- type lexer
    --->    lexer(
                lexer_input_stream  :: text_input_stream,
                lexer_grammar       :: grammar
            ).

:- type lexer_io
    --->    lexer_io(
                lexer_io :: io
            ).

:- type token
    --->    token(
                token_index :: table_index,
                token_chars :: list(char)
            ).

:- instance stream(lexer, lexer_io).

:- instance input(lexer, lexer_io).

:- instance reader(lexer, token, lexer_io, io.error).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

% TODO: include/import/use modules

%----------------------------------------------------------------------------%
:- instance stream(lexer, lexer_io) where [
    pred(name/4) is lexer_name
].

:- pred lexer_name(lexer::in, name::out, lexer_io::di, lexer_io::uo).

lexer_name(Lexer, Name, lexer_io(!.IO), lexer_io(!:IO)) :-
    name(Lexer ^ lexer_input_stream, Name, !IO).

:- instance input(lexer, lexer_io) where [].

:- instance reader(lexer, token, lexer_io, io.error) where [
    pred(get/4) is read_token
].

:- pred read_token(lexer::in, result(token, io.error)::out,
    lexer_io::di, lexer_io::uo).

read_token(Lexer, Result, !LexerIO) :-
    Result = ok(token(-1, [])).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.lexer.
%----------------------------------------------------------------------------%
