%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_shift_reduce.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Thu Jul 10 16:47:57 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Testing the simple grammar in ParserTest.grm
%----------------------------------------------------------------------------%

:- module test_shift_reduce.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module shift_reduce.
:- import_module shift_reduce.egt.
:- import_module shift_reduce.egt.grammar.
:- import_module shift_reduce.egt.state.
:- import_module shift_reduce.grmc.
:- import_module shift_reduce.lexer.
:- import_module dir.
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

main(!IO) :-
    progdir(ProgDir, !IO),
    compile(
        ProgDir / ".." / "tools",
        ProgDir / "ParserTest.grm",
        EgtFile,
        [force_recompile],
        !IO),
    from_file(EgtFile, Grammar, !IO),
    open_input(ProgDir / "ParserTest.txt", OpenResult, !IO),
    ( if OpenResult = ok(InputStream) then
        Lexer = lexer(InputStream, Grammar),
        InitialState = make_unique(Grammar ^ initial_state),
        lex(Lexer, lexer_io(!.IO, InitialState), lexer_io(!:IO, _)),
        close_input(InputStream, !IO)
    else if OpenResult = error(OpenError) then
        error(error_message(OpenError))
    else
        unexpected($file, $pred, "unknown `io.res(T)' value")
    ).

%----------------------------------------------------------------------------%
:- end_module test_shift_reduce.
%----------------------------------------------------------------------------%
