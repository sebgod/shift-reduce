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

:- import_module bitmap.
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
    compile(ProgDir / ".." / "tools", ProgDir / "ParserTest.grm", EgtFile,
        [force_recompile], !IO),
    io.open_binary_input(EgtFile, OpenResult, !IO),
    ( OpenResult = ok(FileInput) ->
        read_binary_file_as_bitmap(FileInput, BitmapResult, !IO),
        ( BitmapResult = ok(Bitmap) ->
            read_tables(GrammarTables, Bitmap, _),
            GrammarTables = grammar(grammar_info(Header)),
            trace [io(!Trace)] (
                io.format("Header: %s\nBytes read: %d\n",
                    [s(Header), i(det_num_bytes(Bitmap))], !Trace)
            )
        ; BitmapResult = error(BitmapError) ->
            unexpected($file, $pred, "error reading bitmap" ++
                io.error_message(BitmapError))
        ;
            unexpected($file, $pred, "unknown bitmap error type")
        ),
        io.close_binary_input(FileInput, !IO)
    ; OpenResult = error(FileOpenError) ->
        unexpected($file, $pred, "cannot open " ++ EgtFile ++
            ": " ++ io.error_message(FileOpenError))
    ;
        unexpected($file, $pred, "unknown binary input result type")
    ).

%----------------------------------------------------------------------------%
:- end_module test_shift_reduce.
%----------------------------------------------------------------------------%
