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
    (   if list.index0(Args, 0, Arg0)
        then FileName = Arg0
        else FileName = "ParserTest.egt"
    ),
    io.format("Testing %s\n", [s(FileName)], !IO),
    io.open_binary_input(FileName, OpenResult, !IO),
    ( OpenResult = ok(FileInput) ->
        read_tables(FileInput, GrammarTables, !IO),
        io.close_binary_input(FileInput, !IO)
    ; OpenResult = error(IO_Error) ->
        unexpected($file, $pred, "cannot open " ++ FileName ++
            " because of " ++ io.error_message(IO_Error))
    ;
        unexpected($file, $pred, "unknown binary input result type")
    ).

%----------------------------------------------------------------------------%
:- end_module test_shift_reduce.
%----------------------------------------------------------------------------%


