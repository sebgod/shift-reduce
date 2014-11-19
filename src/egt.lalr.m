%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.lalr.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 15:43:15 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.lalr.

:- interface.

:- import_module enum.
:- import_module shift_reduce.egt.entry.
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

:- type action_kind
    --->    shift       % shift symbol (target_index -> lalr_state_table)
    ;       reduce      % reduce rule (target_index -> rule_table)
    ;       goto        % reduce rule (jump to target_index)
    ;       accept.     % end of parsing

:- instance enum(action_kind).

:- type action
    --->    action(
                action_symbol_index   :: table_index,
                action_kind           :: action_kind,
                action_target_index   :: table_index
            ).

:- type lalr_state
    --->    lalr_state(
                lalr_actions    :: table(action)
            ).

:- func empty = lalr_state.

:- func parse_lalr `with_type` parse_func(lalr_state) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for generate_fold/5, make_empty_array
:- import_module enum. % for from_int/1
:- import_module int. % for (//)/2
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

empty = lalr_state(empty).

parse_lalr(Entries, Index) = LalrState :-
    ( Entries = [word(Index0), reserved | ActionEntries] ->
        Index = Index0,
        generate_foldl(length(ActionEntries) // 4,
            (pred(_Idx::in,
                action(SymbolIndex, ActionKind, TargetIndex)::out,
                in, out) is det -->
                (
                    [word(SymbolIndex0), word(ActionConstant),
                     word(TargetIndex0), reserved]
                ->
                    { SymbolIndex = SymbolIndex0,
                      ( if ActionKind0 = from_int(ActionConstant) then
                          ActionKind = ActionKind0
                      else
                          unexpected($file, $pred, "unknown action kind")
                      ),
                      TargetIndex  = TargetIndex0 }
                ;
                    { unexpected($file, $pred,
                        "premature end of action list") }
                )
            ),
            Actions,
            ActionEntries,
            ActionRest
        ),
        expect(is_empty(ActionRest), $file, $pred,
            "still have action entries"),
        LalrState = lalr_state(Actions)
    ;
        unexpected($file, $pred, "invalid LALR state record")
    ).

%----------------------------------------------------------------------------%

:- instance enum(action_kind) where [
    (to_int(X) = Y :- action_kind_to_constant(X, Y)),
    (from_int(X) = Y :- action_kind_to_constant(Y, X))
].

:- pred action_kind_to_constant(action_kind, table_index).
:- mode action_kind_to_constant(in, out) is det.
:- mode action_kind_to_constant(out, in) is semidet.

action_kind_to_constant(shift, 1).
action_kind_to_constant(reduce, 2).
action_kind_to_constant(goto, 3).
action_kind_to_constant(accept, 4).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.lalr.
%----------------------------------------------------------------------------%
