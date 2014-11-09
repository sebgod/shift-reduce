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
:- import_module shift_reduce.grmc.
:- import_module dir.
:- import_module list.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

main(!IO) :-
    progdir(ProgDir, !IO),
    ParserTestGrammar = "ParserTest",
    GrammarFile = ProgDir / (ParserTestGrammar ++ ".grm"),
    CompiledGrammarFile = ParserTestGrammar ++ ".egt",
    compile(ProgDir / ".." / "tools", GrammarFile, !IO),
    io.format("Testing %s\n", [s(CompiledGrammarFile)], !IO),
    io.open_binary_input(CompiledGrammarFile, OpenResult, !IO),
    ( OpenResult = ok(FileInput) ->
        read_tables(FileInput, GrammarTables, !IO),
        io.close_binary_input(FileInput, !IO)
    ; OpenResult = error(FileOpenError) ->
        unexpected($file, $pred, "cannot open " ++ CompiledGrammarFile ++
            ": " ++ io.error_message(FileOpenError))
    ;
        unexpected($file, $pred, "unknown binary input result type")
    ).

%----------------------------------------------------------------------------%
:- end_module test_shift_reduce.
%----------------------------------------------------------------------------%
