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

:- import_module array.
:- import_module bitmap.
:- include_module record.
:- import_module shift_reduce.egt.record.

%----------------------------------------------------------------------------%

:- type read_pred == pred(byte_index, byte_index, bitmap, bitmap).
:- inst read_pred == (pred(in, out, bitmap_di, bitmap_uo) is det).

%----------------------------------------------------------------------------%

:- type table(T) == array(T).

:- type grammar
    --->    grammar(
                grammar_info    :: grammar_info,  % Meta-Information
                initial_states  :: initial_states,
                charsets        :: table(charset),
                dfa_states      :: table(dfa_state),
                groups          :: table(group),
                lalr_states     :: table(lalr_state),
                symbols         :: table(symbol),
                productions     :: table(production),
                properties      :: table(property)
            ).

:- type grammar_info
    --->    grammar_info(
                info_header :: string   % EGT File Header
            ).

:- pred parse_grammar(grammar::out,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- include_module primitive.
:- import_module require.
:- import_module shift_reduce.egt.primitive.
%----------------------------------------------------------------------------%

parse_grammar(Grammar, !Bitmap) :-
    parse_grammar(Grammar, 0, _, !Bitmap).

:- pred parse_grammar(grammar::out)
    `with_type` read_pred `with_inst` read_pred.

parse_grammar(Grammar, !Index, !Bitmap) :-
    primitive.read_string(Header, !Index, !Bitmap),
    Info = grammar_info(Header),
    ( NumBytes = num_bytes(!.Bitmap) ->
        Grammar0 = grammar(Info, initial_states(-1, -1),
            make_empty_array, make_empty_array, make_empty_array,
            make_empty_array, make_empty_array, make_empty_array,
            % Size of property table fixed since no entry in table count
            init(8, property("", ""))),
        read_tables(NumBytes, Grammar0, Grammar, !Index, !Bitmap)
    ;
        unexpected($file, $pred, "Bitmap is not initialised!")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.
%----------------------------------------------------------------------------%
