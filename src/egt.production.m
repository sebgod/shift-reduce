%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.production.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 16:49:39 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.production.

:- interface.

:- import_module bitmap. % for type word

%----------------------------------------------------------------------------%

:- type production
    --->    production(
                prod_head_index     :: word,
                prod_symbols        :: table(word) % indicies to symbol table
            ).

:- func empty = production.

:- func parse_production `with_type` parse_func(production)
    `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for make_empty_array
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

empty = production(-1, make_empty_array).

parse_production(Entries, Index) = Rule :-
    ( Entries = [word(Index0), word(HeadIndex), reserved | SymbolEntries] ->
        Index = Index0,
        generate_foldl(length(SymbolEntries),
            (pred(_Idx::in, SymbolIndex::out, in, out) is det -->
                (
                    [word(SymbolIndex0)]
                ->
                    { SymbolIndex = SymbolIndex0 }
                ;
                    { unexpected($file, $pred, "premature end of range list") }
                )
            ),
            Symbols,
            SymbolEntries,
            SymbolRest
        ),
        expect(is_empty(SymbolRest), $file, $pred,
            "still have symbol entries"),
        Rule = production(HeadIndex, Symbols)
    ;
        unexpected($file, $pred, "invalid rule record")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.production.
%----------------------------------------------------------------------------%
