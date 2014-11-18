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
:- import_module shift_reduce.grmc.
:- import_module dir.
:- import_module list.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

:- use_module stream.

:- pred lex(text_input_stream::in, io::di, io::uo) is det.

lex(Reader, !IO) :-
    stream.get(Reader, Result, !IO),
    ( if Result = stream.ok(Char) then
        io.format("%c", [c(Char)], !IO)
    else if Result = stream.error(Error) then
        io.format("error: %s", [s(io.error_message(Error))], !IO)
    else if Result = stream.eof then
        true
    else
        unexpected($file, $pred, "unknown stream result")
    ).

main(!IO) :-
    progdir(ProgDir, !IO),
    compile(
        ProgDir / ".." / "tools",
        ProgDir / "ParserTest.grm", EgtFile,
        [force_recompile],
        !IO),
    from_file(EgtFile, Grammar, !IO),
    open_input(ProgDir / "ParserTest.txt", OpenResult, !IO),
    ( if OpenResult = ok(InputStream) then
        lex(InputStream, !IO),
        close_input(InputStream, !IO)
    else if OpenResult = error(OpenError) then
        unexpected($file, $pred, error_message(OpenError))
    else
        unexpected($file, $pred, "unknown `io.res(T)' value")
    ).

%----------------------------------------------------------------------------%
:- end_module test_shift_reduce.
%----------------------------------------------------------------------------%
