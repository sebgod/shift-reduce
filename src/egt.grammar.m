%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: egt.grammar.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 14:35:49 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.grammar.

:- interface.

:- import_module bitmap. % for inst bitmap_{di|uo}
:- import_module charset. % for type charset, in mercury_misc
:- import_module io.
:- import_module shift_reduce.egt.dfa.
:- import_module shift_reduce.egt.group.
:- import_module shift_reduce.egt.lalr.
:- import_module shift_reduce.egt.production.
:- import_module shift_reduce.egt.property.
:- import_module shift_reduce.egt.state.
:- import_module shift_reduce.egt.symbol.
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

:- type grammar
    --->    grammar(
                initial_state   :: lexer_state,
                charsets        :: table(charset),
                dfa_states      :: table(dfa_state),
                groups          :: table(group),
                lalr_states     :: table(lalr_state),
                symbols         :: table(symbol),
                productions     :: table(production),
                properties      :: table(property)
            ).

:- func empty = grammar.

:- pred from_file(string::in, grammar::out, io::di, io::uo) is det.

:- pred from_bitmap(grammar::out,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- use_module array. % TODO: wrap array.init/1 in table module
:- import_module require.
:- import_module string. % for ++/2
:- import_module shift_reduce.egt.primitive.
:- import_module shift_reduce.egt.table.
:- import_module shift_reduce.egt.record.

%----------------------------------------------------------------------------%

empty = grammar(empty, empty, empty, empty, empty, empty, empty,
            % Size of property table fixed since no entry in table count
            array.init(8, empty)).

from_file(EgtFile, Grammar, !IO) :-
    io.open_binary_input(EgtFile, OpenResult, !IO),
    (
        OpenResult = ok(FileInput),
        read_binary_file_as_bitmap(FileInput, BitmapResult, !IO),
        (
            BitmapResult = ok(Bitmap),
            from_bitmap(Grammar, Bitmap, _)
        ;
            BitmapResult = error(BitmapError),
            unexpected($file, $pred, "error reading bitmap" ++
                io.error_message(BitmapError))
        ),
        io.close_binary_input(FileInput, !IO)
    ;
        OpenResult = error(FileOpenError),
        unexpected($file, $pred, "cannot open " ++ EgtFile ++
            ": " ++ io.error_message(FileOpenError))
    ).

from_bitmap(Grammar, !Bitmap) :-
    parse_grammar(Grammar, 0, _, !Bitmap).

:- pred parse_grammar(grammar::out)
    `with_type` read_pred `with_inst` read_pred.

parse_grammar(Grammar, !Index, !Bitmap) :-
    ( if NumBytes = num_bytes(!.Bitmap) then
        read_string(Header, !Index, !Bitmap),
        ( if Header = "GOLD Parser Tables/v5.0" then
            build_tables(NumBytes, empty, Grammar, !Index, !Bitmap)
        else
            sorry($file, $pred, Header)
        )
    else
        unexpected($file, $pred, "Bitmap is not initialised!")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.grammar.
%----------------------------------------------------------------------------%
