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

:- type records == list(record).

:- pred read_records(num_bytes::in, records::out)
    `with_type` read_pred `with_inst` read_pred.

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

read_records(NumBytes, Records, !Index, !Bitmap) :-
    read_records(NumBytes, [], RevRecords, !Index, !Bitmap),
    reverse(RevRecords, Records).

:- pred read_records(num_bytes::in, records::in, records::out)
    `with_type` read_pred `with_inst` read_pred.

read_records(NumBytes, !Records, !Index, !Bitmap) :-
    ( !.Index < NumBytes ->
        read_record(Record, !Index, !Bitmap),
        !:Records = [Record | !.Records],
        read_records(NumBytes, !Records, !Index, !Bitmap)
    ;
        true
    ).

read_record(Record, !Index, !Bitmap) :-
    read_ascii(RecordType, !Index, !Bitmap),
    ( RecordType = 'M' ->
        read_word(Count, !Index, !Bitmap),
        read_entries(Count, EntriesWithSpec, !Index, !Bitmap),
        ( EntriesWithSpec = [byte(SpecByte) | Entries] ->
            Spec = char.det_from_int(SpecByte),
            Reader =
                ( Spec = 'c' ->
                    read_character_set_table
                ; Spec = 'D' ->
                    read_dfa_table
                ; Spec = 'g' ->
                    read_group_table
                ; Spec = 'I' ->
                    read_initial_states
                ; Spec = 'L' ->
                    read_lalr_table
                ; Spec = 'p' ->
                    read_property
                ; Spec = 'R' ->
                    read_rule_table
                ; Spec = 'S' ->
                    read_symbol_table
                ; Spec = 't' ->
                    read_table_counts
                ;
                    unexpected($file, $pred,
                        format("record spec <%c> not implemented!",
                            [c(Spec)]))
                ),
            Record = Reader(Entries)
        ;
            unexpected($file, $pred, "entry without valid specifier!")
        )
    ;
        unexpected($file, $pred,
            format("record type <%c> not implemted!", [c(RecordType)]))
    ).

:- pred read_entries(int::in, entries::out)
    `with_type` read_pred `with_inst` read_pred.

read_entries(Count, Entries, !Index, !Bitmap) :-
    read_entries(Count, [], RevEntries, !Index, !Bitmap),
    reverse(RevEntries, Entries).

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

%----------------------------------------------------------------------------%

:- type parse_func == (func(entries) = record).
:- inst parse_func == (func(in) = out is det).

:- func read_character_set_table
    `with_type` parse_func `with_inst` parse_func.

read_character_set_table(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_dfa_table `with_type` parse_func `with_inst` parse_func.

read_dfa_table(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_group_table `with_type` parse_func `with_inst` parse_func.

read_group_table(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_initial_states `with_type` parse_func `with_inst` parse_func.

read_initial_states(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_lalr_table `with_type` parse_func `with_inst` parse_func.

read_lalr_table(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_property `with_type` parse_func `with_inst` parse_func.

read_property(Entries) =
    ( Entries = [word(Index), string(Key), string(Value)] ->
        property(Index, Key, Value)
    ;
        unexpected($file, $pred, "Invalid property record")
    ).

:- func read_rule_table `with_type` parse_func `with_inst` parse_func.

read_rule_table(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_symbol_table `with_type` parse_func `with_inst` parse_func.

read_symbol_table(Entries) =
    unexpected($file, $pred, "not implemented").

:- func read_table_counts `with_type` parse_func `with_inst` parse_func.

read_table_counts(Entries) =
    unexpected($file, $pred, "not implemented").

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.record.
%----------------------------------------------------------------------------%
