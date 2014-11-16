%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Thu Jul 10 16:17:57 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% `egt' is responsible for loading the enhanced grammar table binary format.
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.

:- interface.

:- import_module bitmap.

%----------------------------------------------------------------------------%

:- type read_pred == pred(byte_index, byte_index, bitmap, bitmap).
:- inst read_pred == (pred(in, out, bitmap_di, bitmap_uo) is det).

%----------------------------------------------------------------------------%

:- type grammar
    --->    grammar(
                grammar_info :: grammar_info     % Meta-Information
            ).

:- type grammar_info
    --->    grammar_info(
                info_header :: string   % EGT File Header
            ).

:- pred read_tables(grammar::out,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- include_module primitive.
:- import_module io. % for tracing
:- include_module record.
:- import_module require.
:- import_module shift_reduce.egt.primitive.
:- import_module shift_reduce.egt.record.
%----------------------------------------------------------------------------%

read_tables(Grammar, !Bitmap) :-
    read_tables(Grammar, 0, _, !Bitmap).

:- pred read_tables(grammar::out)
    `with_type` read_pred `with_inst` read_pred.

read_tables(Grammar, !Index, !Bitmap) :-
    primitive.read_string(Header, !Index, !Bitmap),
    Info = grammar_info(Header),
    ( NumBytes = num_bytes(!.Bitmap) ->
        read_records(NumBytes, Records, !Index, !Bitmap),
        trace [io(!IO)] ( io.write_line(Records, !IO) ),
        Grammar = grammar(Info)
    ;
        unexpected($file, $pred, "Bitmap is not initialised!")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.
%----------------------------------------------------------------------------%
