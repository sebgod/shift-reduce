%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: egt.group.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 16:39:51 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.group.

:- interface.

:- import_module enum.
:- import_module shift_reduce.egt.entry.
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

    % The table index of the group in the group table.
:- type group_nesting
    --->    group_nesting(
                grp_nesting_index :: table_index % zero-indexed
            ).

:- type group_advance_mode
    --->    token       % the group will advance a token
    ;       character.  % the group will advance by just one character

:- instance enum(group_advance_mode).

:- type group_ending_mode
    --->    open        % the ending symbol will be left on the input queue
    ;       closed.     % the ending symbol will be consumed

:- instance enum(group_ending_mode).

:- type group
    --->    group(
                group_name              :: string,
                group_container_index   :: table_index,
                group_start_index       :: table_index,
                group_end_index         :: table_index,
                group_advance_mode      :: group_advance_mode,
                group_ending_mode       :: group_ending_mode,
                group_nestings          :: table(group_nesting)
            ).

:- func empty = group.

:- func parse_group `with_type` parse_func(group) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%----------------------------------------------------------------------------%

empty = group("", -1, -1, -1, token, open, empty).

parse_group(Entries, Index) = (
        unexpected($file, $pred, "invalid group record")
    ).

%----------------------------------------------------------------------------%
% enum(T) instances for named constants:
%  * group_advance_mode
%  * group_ending_mode

:- instance enum(group_advance_mode) where [
    (to_int(X) = Y :- group_advance_mode_to_constant(X, Y)),
    (from_int(X) = Y :- group_advance_mode_to_constant(Y, X))
].

:- pred group_advance_mode_to_constant(group_advance_mode, table_index).
:- mode group_advance_mode_to_constant(in, out) is det.
:- mode group_advance_mode_to_constant(out, in) is semidet.

group_advance_mode_to_constant(token, 0).
group_advance_mode_to_constant(character, 1).

%----------------------------------------------------------------------------%

:- instance enum(group_ending_mode) where [
    (to_int(X) = Y :- group_ending_mode_to_constant(X, Y)),
    (from_int(X) = Y :- group_ending_mode_to_constant(Y, X))
].

:- pred group_ending_mode_to_constant(group_ending_mode, table_index).
:- mode group_ending_mode_to_constant(in, out) is det.
:- mode group_ending_mode_to_constant(out, in) is semidet.

group_ending_mode_to_constant(open, 0).
group_ending_mode_to_constant(closed, 1).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.group.
%----------------------------------------------------------------------------%
