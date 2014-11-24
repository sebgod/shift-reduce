%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: shift_reduce.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Thu Jul 10 16:11:22 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Top-level module of the shift-reduce parser library.
%----------------------------------------------------------------------------%

:- module shift_reduce.

:- interface.

:- include_module shift_reduce.egt.
:- include_module shift_reduce.grmc.
:- include_module shift_reduce.lexer.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module shift_reduce.
%----------------------------------------------------------------------------%
