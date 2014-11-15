%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.record.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sat Nov 15 21:16:10 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Records
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.record.

:- interface.

:- import_module bitmap.
:- import_module bool.
:- import_module list.
%----------------------------------------------------------------------------%

:- type entry
    --->    empty
    ;       bool(bool)
    ;       byte(byte)
    ;       word(word)
    ;       string(string).

:- type entries == list(entry).

:- type record
    --->    property(
                prop_index :: int,
                prop_key   :: string,
                prop_value :: string
            )
    ;       table_counts.

:- pred read_record(record::out)
    `with_type` read_pred `with_inst` read_pred.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module io. % for tracing
:- import_module string.
:- import_module require.

%----------------------------------------------------------------------------%

read_record(Record, !Index, !Bitmap) :-
    read_ascii(RecordType, !Index, !Bitmap),
    ( RecordType = 'M' ->
        read_word(Count, !Index, !Bitmap),
        read_entries(Count, [], RevEntries, !Index, !Bitmap),
        reverse(RevEntries, EntriesWithSpec),
        ( EntriesWithSpec = [byte(SpecByte) | Entries] ->
            Spec = char.det_from_int(SpecByte),
            CountNoSpec = Count - 1,
            ( Spec = 'p' ->
                read_property(Record, CountNoSpec, Entries)
            ;
                unexpected($file, $pred,
                    format("record spec <%c> not implemented!", [c(Spec)]))
            )
        ;
            unexpected($file, $pred, "entry without valid specifier!")
        )
    ;
        unexpected($file, $pred,
            format("record type <%c> not implemted!", [c(RecordType)]))
    ).

:- pred read_entries(int::in, entries::in, entries::out)
    `with_type` read_pred `with_inst` read_pred.

read_entries(Count, !Entries, !Index, !Bitmap) :-
    ( Count > 0 ->
        read_ascii(Type, !Index, !Bitmap),
        ( Type = 'E' ->
            Entry = empty
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
        read_entries(Count - 1, !Entries, !Index, !Bitmap)
    ;
        true
    ).

:- pred read_property(record::out, int::in, entries::in) is det.

read_property(Property, _Count, Entries) :-
    Property =
    ( Entries = [word(Index), string(Key), string(Value)] ->
        property(Index, Key, Value)
    ;
        unexpected($file, $pred, "Invalid property record")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.record.
%----------------------------------------------------------------------------%
