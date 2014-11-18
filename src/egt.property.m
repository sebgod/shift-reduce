%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.property.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 16:50:56 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.property.

:- interface.

%----------------------------------------------------------------------------%

:- type property
    --->    property(
                prop_key    :: string,
                prop_value  :: string
            ).

:- func empty = property.

:- func parse_property `with_type` parse_func(property)
    `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

empty = property("", "").

parse_property(Entries, Index) = Property :-
    ( Entries = [word(Index0), string(Key), string(Value)] ->
        Index = Index0,
        Property = property(Key, Value)
    ;
        unexpected($file, $pred, "invalid property record")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.property.
%----------------------------------------------------------------------------%
