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

:- import_module shift_reduce.egt.entry.
:- import_module shift_reduce.egt.table.

%----------------------------------------------------------------------------%

:- type lexer_state
    --->    lexer_state(
                dfa     :: table_index,  % initial DFA state  (usually 0)
                lalr    :: table_index   % initial LALR state (usually 0)
            ).

:- func empty = lexer_state.

:- func make_unique(lexer_state) = lexer_state.
:- mode make_unique(in) = uo is det.

:- func parse_initial_lexer_state `with_type` parse_func(lexer_state)
    `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

empty = lexer_state(-1, -1).

make_unique(lexer_state(Dfa, Lalr)) = lexer_state(Dfa + 0, Lalr + 0).

parse_initial_lexer_state(Entries, -1) =
    ( if Entries = [word(Dfa), word(Lalr)] then
        lexer_state(Dfa, Lalr)
    else
        unexpected($file, $pred, "invalid initial lexer_state record")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.state.
%----------------------------------------------------------------------------%
