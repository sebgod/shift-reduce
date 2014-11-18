%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.state.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 17:21:16 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.state.

:- interface.

:- import_module bitmap.

%----------------------------------------------------------------------------%

:- type initial_states
    --->    initial_states(
                init_dfa    :: word,  % initial DFA state (0)
                linit_lalr  :: word   % initial LALR state (0)
            ).

:- func parse_initial_states `with_type` parse_func(initial_states)
    `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

parse_initial_states(Entries, -1) =
    ( Entries = [word(Dfa), word(Lalr)] ->
        initial_states(Dfa, Lalr)
    ;
        unexpected($file, $pred, "invalid initial states record")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.state.
%----------------------------------------------------------------------------%
