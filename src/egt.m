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
:- import_module shift_reduce.egt.primitive.
%----------------------------------------------------------------------------%

read_tables(Grammar, !Bitmap) :-
    primitive.read_string(Header, 0, Index, !Bitmap),
    Info = grammar_info(Header),
    Grammar = grammar(Info).
%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.
%----------------------------------------------------------------------------%
