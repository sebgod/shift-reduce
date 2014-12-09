%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
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

:- import_module maybe. % type maybe
:- import_module shift_reduce.egt.entry. % for type parse_func
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

:- type edge
    --->    edge(
                edge_charset_index  :: table_index,
                edge_target_index   :: table_index
            ).

:- type dfa_state
    --->    dfa_state(
                dfa_accept_index    :: maybe(table_index),
                dfa_edges           :: table(edge)
            ).

:- func empty = dfa_state.

:- func parse_dfa `with_type` parse_func(dfa_state) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for generate_fold/5
:- import_module bool. % type bool used in parse_dfa/2
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module std_util.

%----------------------------------------------------------------------------%

empty = dfa_state(no, empty).

    % maybe_bool(BoolValue, MaybeValue).
    % <=> maybe_func((func(yes) = MaybeValue is semidet), BoolValue).
:- func maybe_bool(T, bool) = maybe(T).

maybe_bool(MaybeValue, yes) = yes(MaybeValue).
maybe_bool(_, no) = no.

parse_dfa(Entries, Index) = DfaState :-
    ( if Entries = [word(Index0), bool(AcceptState), word(AcceptIndex),
                 reserved | EdgeEntries]
    then
        Index = Index0,
        MaybeAcceptIndex = maybe_bool(AcceptIndex, AcceptState),
        generate_foldl(length(EdgeEntries) // 3,
            (pred(_Idx::in, edge(CharSetIndex, TargetIndex)::out,
                in, out) is det -->
                ( if
                    [word(CharSetIndex0), word(TargetIndex0), reserved]
                then
                    { CharSetIndex = CharSetIndex0,
                      TargetIndex  = TargetIndex0 }
                else
                    { unexpected($file, $pred, "premature end of edge list") }
                )
            ),
            Edges,
            EdgeEntries,
            EdgeRest
        ),
        expect(is_empty(EdgeRest), $file, $pred, "still have edge entries"),
        DfaState = dfa_state(MaybeAcceptIndex, Edges)
    else
        unexpected($file, $pred, "invalid DFA state record")
    ).

%----------------------------------------------------------------------------%
:- end_module egt.dfa.
%----------------------------------------------------------------------------%
