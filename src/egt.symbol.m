%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.symbol.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 14:17:04 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% A symbol is a pair of a symbol and a kind used for tokenisation of input.
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.symbol.

:- interface.

:- import_module enum.

%----------------------------------------------------------------------------%

:- type symbol_kind
    --->    nonterminal     % normal nonterminal
    ;       terminal        % normal terminal
    ;       noise           % noise terminal, ignored by parser
    ;       end_of_file     % end of file character
    ;       group_start     % lexical group start
    ;       group_end       % lexical group end, can end with terminal
    ;       decremented     % used in CGT, deprecated
    ;       error.          % error terminal, emitted by parser

:- instance enum(symbol_kind).

:- type symbol
    --->    symbol(
                sym_name    :: string,
                sym_kind    :: symbol_kind
            ).

:- func empty = symbol.

:- func parse_symbol `with_type` parse_func(symbol) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap. % for type word
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

empty = symbol("", error).

parse_symbol(Entries, Index) = Symbol :-
    (
        Entries = [word(Index0), string(Name), word(SymbolConstant)],
        SymbolKind = from_int(SymbolConstant)
    ->
        Index = Index0,
        Symbol = symbol(Name, SymbolKind)
    ;
        unexpected($file, $pred, "invalid symbol record")
    ).

:- instance enum(symbol_kind) where [
    (to_int(X) = Y :- symbol_kind_to_constant(X, Y)),
    (from_int(X) = Y :- symbol_kind_to_constant(Y, X))
].

:- pred symbol_kind_to_constant(symbol_kind, word).
:- mode symbol_kind_to_constant(in, out) is det.
:- mode symbol_kind_to_constant(out, in) is semidet.

symbol_kind_to_constant(nonterminal, 0).
symbol_kind_to_constant(terminal, 1).
symbol_kind_to_constant(noise, 2).
symbol_kind_to_constant(end_of_file, 3).
symbol_kind_to_constant(group_start, 4).
symbol_kind_to_constant(group_end, 5).
symbol_kind_to_constant(decremented, 6).
symbol_kind_to_constant(error, 7).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.symbol.
%----------------------------------------------------------------------------%
