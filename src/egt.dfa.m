%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.dfa.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 16:26:33 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module egt.dfa.

:- interface.

:- import_module bitmap. % for type word
:- import_module maybe. % type maybe

%----------------------------------------------------------------------------%

:- type edge
    --->    edge(
                edge_charset_index  :: word,
                edge_target_index   :: word
            ).

:- type dfa_state
    --->    dfa_state(
                dfa_accept_index    :: maybe(word),
                dfa_edges           :: table(edge)
            ).

:- func empty = dfa_state.

:- func parse_dfa `with_type` parse_func(dfa_state) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for generate_fold/5, make_empty_array
:- import_module bool. % type bool used in parse_dfa/2
:- import_module int.
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

empty = dfa_state(no, make_empty_array).

parse_dfa(Entries, Index) = DfaState :-
    ( Entries = [word(Index0), bool(AcceptState), word(AcceptIndex),
                 reserved | EdgeEntries]
    ->
        Index = Index0,
        MaybeAcceptIndex = (AcceptState = yes -> yes(AcceptIndex) ; no),
        generate_foldl(length(EdgeEntries) // 3,
            (pred(_Idx::in, edge(CharSetIndex, TargetIndex)::out,
                in, out) is det -->
                (
                    [word(CharSetIndex0), word(TargetIndex0), reserved]
                ->
                    { CharSetIndex = CharSetIndex0,
                      TargetIndex  = TargetIndex0 }
                ;
                    { unexpected($file, $pred, "premature end of edge list") }
                )
            ),
            Edges,
            EdgeEntries,
            EdgeRest
        ),
        expect(is_empty(EdgeRest), $file, $pred, "still have edge entries"),
        DfaState = dfa_state(MaybeAcceptIndex, Edges)
    ;
        unexpected($file, $pred, "invalid DFA state record")
    ).

%----------------------------------------------------------------------------%
:- end_module egt.dfa.
%----------------------------------------------------------------------------%
