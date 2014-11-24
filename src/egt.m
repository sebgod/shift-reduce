%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: egt.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Thu Jul 10 16:17:57 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% `egt' is responsible for loading the enhanced grammar table binary format.
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.

:- interface.

:- include_module charset.
:- include_module dfa.
:- include_module entry.
:- include_module grammar.
:- include_module group.
:- include_module lalr.
:- include_module primitive.
:- include_module production.
:- include_module property.
:- include_module record.
:- include_module state.
:- include_module symbol.
:- include_module table.

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.
%----------------------------------------------------------------------------%
