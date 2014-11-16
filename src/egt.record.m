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

:- import_module array.
:- import_module bitmap.
:- import_module bool.
:- import_module char.
:- import_module enum.
:- import_module list.
:- import_module maybe.
%----------------------------------------------------------------------------%

:- type entry
    --->    empty
    ;       bool(bool)
    ;       byte(byte)
    ;       word(word)
    ;       string(string).

:- type entries == list(entry).

:- type character_range
    --->    character_range(
                cr_start :: char,
                cr_end   :: char
            ).

:- type symbol_kind
    --->    nonterminal     % normal nonterminal
    ;       terminal        % normal terminal
    ;       noise           % noise terminal, ignored by parser
    ;       end_of_file     % end of file character
    ;       group_start     % lexical group start
    ;       group_end       % lexical group end, can end with terminal
    ;       decremented     % used in CGT, deprecated
    ;       error.          % error terminal, emitted by parser

:- instance enum(symbol_kind).

    % The table index of the group in the group table.
:- type group_nesting
    --->    group_nesting(
                grp_nesting_index :: word % zero-indexed
            ).

:- type group_advance_mode
    --->    token       % the group will advance a token
    ;       character.  % the group will advance by just one character

:- instance enum(group_advance_mode).

:- type group_ending_mode
    --->    open        % the ending symbol will be left on the input queue
    ;       closed.     % the ending symbol will be consumed

:- instance enum(group_ending_mode).

:- type edge
    --->    edge(
                edge_charset_index  :: word,
                edge_target_index   :: word
            ).

:- type action_kind
    --->    shift       % shift symbol (target_index -> lalr_state_table)
    ;       reduce      % reduce rule (target_index -> rule_table)
    ;       goto        % reduce rule (jump to target_index)
    ;       accept.     % end of parsing

:- instance enum(action_kind).

:- type action
    --->    action(
                action_symbol_index   :: word,
                action_kind           :: action_kind,
                action_target_index   :: word
            ).

:- type record
    --->    character_set(
                cs_index            :: word,
                cs_unicode_plane    :: word,
                cs_range_count      :: word,  % number of ranges
                cs_ranges           :: array(character_range)
            )
    ;       dfa_state(
                dfa_index           :: word,
                dfa_accept_index    :: maybe(word),
                dfa_edges           :: array(edge)
            )
    ;       group(
                group_index             :: word,
                group_name              :: string,
                group_container_index   :: word,
                group_start_index       :: word,
                group_end_index         :: word,
                group_advance_mode      :: group_advance_mode,
                group_ending_mode       :: group_ending_mode,
                group_nesting_count     :: word, % number of nestings
                group_nestings          :: array(group_nesting)
            )
    ;       initial_states(
                init_dfa    :: word,  % initial DFA state (0)
                linit_lalr  :: word   % initial LALR state (0)
            )
    ;       lalr_state(
                lalr_index      :: word,
                lalr_actions    :: array(action)
            )
    ;       production(
                prod_index      :: word,
                prod_head_index :: word,
                prod_symbols    :: array(word) % indicies to symbol table
            )
    ;       property(
                prop_index  :: word,
                prop_key    :: string,
                prop_value  :: string
            )
    ;       symbol(
                sym_index   :: word,
                sym_name    :: string,
                sym_kind    :: symbol_kind
            )
    ;       table_counts(
                count_symbol        :: word,
                count_character_set :: word,
                count_rule          :: word,
                count_dfa           :: word,
                count_lalr          :: word,
                count_group         :: word
            )
    ;       fake. % XXX only for debugging.

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
                    read_character_set
                ; Spec = 'D' ->
                    read_dfa
                ; Spec = 'g' ->
                    read_group
                ; Spec = 'I' ->
                    read_initial_states
                ; Spec = 'L' ->
                    read_lalr
                ; Spec = 'p' ->
                    read_property
                ; Spec = 'R' ->
                    read_rule
                ; Spec = 'S' ->
                    read_symbol
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

:- func read_character_set
    `with_type` parse_func `with_inst` parse_func.

read_character_set(Entries) = Charset :-
    (
        Entries = [word(Index), word(UnicodePlane),
                   word(RangeCount), empty | RangeEntries]
    ->
        generate_foldl(RangeCount,
            (pred(_Idx::in, character_range(Start, End)::out,
                in, out) is det -->
                (
                    [word(StartUnit), word(EndUnit)]
                ->
                    { Start = char.det_from_int(StartUnit),
                      End   = char.det_from_int(EndUnit)
                    }
                ;
                    { unexpected($file, $pred, "premature end of range list") }
                )
            ),
            Ranges,
            RangeEntries,
            _RangeRest
        ),
        Charset = character_set(Index, UnicodePlane, RangeCount, Ranges)
    ;
        unexpected($file, $pred, "invalid character set record")
    ).

:- func read_dfa `with_type` parse_func `with_inst` parse_func.

read_dfa(Entries) = fake. % XXX only for debugging
%    unexpected($file, $pred, "not implemented").

:- func read_group `with_type` parse_func `with_inst` parse_func.

read_group(Entries) = fake. % XXX only for debugging
%    unexpected($file, $pred, "not implemented").

:- func read_initial_states `with_type` parse_func `with_inst` parse_func.

read_initial_states(Entries) =
    ( Entries = [word(Dfa), word(Lalr)] ->
        initial_states(Dfa, Lalr)
    ;
        unexpected($file, $pred, "invalid initial states record")
    ).

:- func read_lalr `with_type` parse_func `with_inst` parse_func.

read_lalr(Entries) = fake. % XXX only for debugging
%    unexpected($file, $pred, "not implemented").

:- func read_property `with_type` parse_func `with_inst` parse_func.

read_property(Entries) =
    ( Entries = [word(Index), string(Key), string(Value)] ->
        property(Index, Key, Value)
    ;
        unexpected($file, $pred, "invalid property record")
    ).

:- func read_rule `with_type` parse_func `with_inst` parse_func.

read_rule(Entries) = fake. % XXX only for debugging
%    unexpected($file, $pred, "not implemented").

:- func read_symbol `with_type` parse_func `with_inst` parse_func.

read_symbol(Entries) =
    (
        Entries = [word(Index), string(Name), word(SymbolConstant)],
        SymbolKind = from_int(SymbolConstant)
    ->
        symbol(Index, Name, SymbolKind)
    ;
        unexpected($file, $pred, "invalid symbol record")
    ).

:- func read_table_counts `with_type` parse_func `with_inst` parse_func.

read_table_counts(Entries) =
    ( Entries = [word(SymbolTable), word(CharacterSetTable), word(RuleTable),
                 word(DFATable), word(LALRTable), word(GroupTable)] ->
        table_counts(SymbolTable, CharacterSetTable, RuleTable,
            DFATable, LALRTable, GroupTable)
    ;
        unexpected($file, $pred, "invalid table counts record")
    ).

%----------------------------------------------------------------------------%
% enum(T) instances for named constants:
%  * symbol_kind
%  * action_kind
%  * group_advance_mode
%  * group_ending_mode

:- instance enum(symbol_kind) where [
    (to_int(X) = Y :- symbol_kind_to_constant(X, Y)),
    (from_int(X) = Y :- symbol_kind_to_constant(Y, X))
].

:- pred symbol_kind_to_constant(symbol_kind, word).
:- mode symbol_kind_to_constant(in, out) is det.
:- mode symbol_kind_to_constant(out, in) is semidet.

symbol_kind_to_constant(nonterminal, 0).
symbol_kind_to_constant(terminal, 1).
symbol_kind_to_constant(noise, 2).
symbol_kind_to_constant(end_of_file, 3).
symbol_kind_to_constant(group_start, 4).
symbol_kind_to_constant(group_end, 5).
symbol_kind_to_constant(decremented, 6).
symbol_kind_to_constant(error, 7).

%----------------------------------------------------------------------------%

:- instance enum(action_kind) where [
    (to_int(X) = Y :- action_kind_to_constant(X, Y)),
    (from_int(X) = Y :- action_kind_to_constant(Y, X))
].

:- pred action_kind_to_constant(action_kind, word).
:- mode action_kind_to_constant(in, out) is det.
:- mode action_kind_to_constant(out, in) is semidet.

action_kind_to_constant(shift, 1).
action_kind_to_constant(reduce, 2).
action_kind_to_constant(goto, 3).
action_kind_to_constant(accept, 4).

%----------------------------------------------------------------------------%

:- instance enum(group_advance_mode) where [
    (to_int(X) = Y :- group_advance_mode_to_constant(X, Y)),
    (from_int(X) = Y :- group_advance_mode_to_constant(Y, X))
].

:- pred group_advance_mode_to_constant(group_advance_mode, word).
:- mode group_advance_mode_to_constant(in, out) is det.
:- mode group_advance_mode_to_constant(out, in) is semidet.

group_advance_mode_to_constant(token, 0).
group_advance_mode_to_constant(character, 1).

%----------------------------------------------------------------------------%

:- instance enum(group_ending_mode) where [
    (to_int(X) = Y :- group_ending_mode_to_constant(X, Y)),
    (from_int(X) = Y :- group_ending_mode_to_constant(Y, X))
].

:- pred group_ending_mode_to_constant(group_ending_mode, word).
:- mode group_ending_mode_to_constant(in, out) is det.
:- mode group_ending_mode_to_constant(out, in) is semidet.

group_ending_mode_to_constant(open, 0).
group_ending_mode_to_constant(closed, 1).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.record.
%----------------------------------------------------------------------------%
