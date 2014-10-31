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
:- import_module list.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

main(!IO) :-
    command_line_arguments(Args, !IO),
    FileName = ( if Args = [Arg0 | _] then Arg0 else "ParserTest.egt"),
    CompileCmd = "../tools/grmc *.grm",
    io.call_system(CompileCmd, CompileResult, !IO),
    ( CompileResult = ok(ExitCode) ->
        io.format("%s result: %d\n", [s(CompileCmd), i(ExitCode)], !IO)
    ; CompileResult = error(CompileError) ->
        unexpected($file, $pred, "could not compile grammar file: " ++
            io.error_message(CompileError))
    ;
        unexpected($file, $pred, "unknown io.call_system/4 result")
    ),
    io.format("Testing %s\n", [s(FileName)], !IO),
    io.open_binary_input(FileName, OpenResult, !IO),
    ( OpenResult = ok(FileInput) ->
        read_tables(FileInput, GrammarTables, !IO),
        io.close_binary_input(FileInput, !IO)
    ; OpenResult = error(FileOpenError) ->
        unexpected($file, $pred, "cannot open " ++ FileName ++
            ": " ++ io.error_message(FileOpenError))
    ;
        unexpected($file, $pred, "unknown binary input result type")
    ).

%----------------------------------------------------------------------------%
:- end_module test_shift_reduce.
%----------------------------------------------------------------------------%


