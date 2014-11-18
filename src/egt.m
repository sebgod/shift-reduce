%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
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
:- include_module symbol.

%----------------------------------------------------------------------------%

:- import_module shift_reduce.egt.charset.
:- import_module shift_reduce.egt.dfa.
:- import_module shift_reduce.egt.entry.
:- import_module shift_reduce.egt.grammar.
:- import_module shift_reduce.egt.group.
:- import_module shift_reduce.egt.lalr.
:- import_module shift_reduce.egt.primitive.
:- import_module shift_reduce.egt.production.
:- import_module shift_reduce.egt.property.
:- import_module shift_reduce.egt.record.
:- import_module shift_reduce.egt.symbol.

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.
%----------------------------------------------------------------------------%
