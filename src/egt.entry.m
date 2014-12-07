%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: egt.entry.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 15:06:33 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.entry.

:- interface.

:- import_module bitmap. % for type byte
:- import_module bool.
:- import_module list.
:- import_module shift_reduce.egt.primitive. % for type read_pred
:- import_module shift_reduce.egt.table. % for type table_index

%----------------------------------------------------------------------------%

:- type entry
    --->    reserved
    ;       bool(bool)
    ;       byte(byte)
    ;       word(table_index)
    ;       string(string).

:- type entries == list(entry).

:- type parse_func(T) == (func(entries, table_index) = T).
:- inst parse_func == (func(in, out) = out is det).

:- pred read_entries(int::in, entries::out)
    `with_type` read_pred `with_inst` read_pred.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

read_entries(Count, Entries, !Index, !Bitmap) :-
    read_entries_acc(Count, [], RevEntries, !Index, !Bitmap),
    reverse(RevEntries, Entries).

:- pred read_entries_acc(int::in, entries::in, entries::out)
    `with_type` read_pred `with_inst` read_pred.

read_entries_acc(Count, !Entries, !Index, !Bitmap) :-
    ( Count > 0 ->
        read_ascii(Type, !Index, !Bitmap),
        ( Type = 'E' ->
            Entry = reserved
        ; Type = 'B' ->
            read_bool(Bool, !Index, !Bitmap),
            Entry = bool(Bool)
        ; Type = 'b' ->
            read_byte(Byte, !Index, !Bitmap),
            Entry = byte(Byte)
        ; Type = 'I' ->
            read_word(Word, !Index, !Bitmap),
            Entry = word(Word)
        ; Type = 'S' ->
            read_string(String, !Index, !Bitmap),
            Entry = string(String)
        ;
            unexpected($file, $pred,
                format("entry type <%c> not implemented!", [c(Type)]))
        ),
        !:Entries = [Entry | !.Entries],
        read_entries_acc(Count - 1, !Entries, !Index, !Bitmap)
    ;
        true
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.entry.
%----------------------------------------------------------------------------%
