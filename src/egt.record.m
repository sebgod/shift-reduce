%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.record.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sat Nov 15 21:16:10 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: Record module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.record.

:- interface.

:- import_module bitmap.

%----------------------------------------------------------------------------%

:- type initial_states
    --->    initial_states(
                init_dfa    :: word,  % initial DFA state (0)
                linit_lalr  :: word   % initial LALR state (0)
            ).

:- type table_counts
    --->    table_counts(
                count_symbol    :: word,
                count_charset   :: word,
                count_rule      :: word,
                count_dfa       :: word,
                count_lalr      :: word,
                count_group     :: word
            ).

:- pred build_tables(num_bytes::in, grammar::in, grammar::out)
    `with_type` read_pred `with_inst` read_pred.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for ^ 'elem :='/1
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module require.

%----------------------------------------------------------------------------%

build_tables(NumBytes, !Grammar, !Index, !Bitmap) :-
    ( !.Index < NumBytes ->
        read_ascii(RecordType, !Index, !Bitmap),
        ( RecordType = 'M' ->
            read_word(Count, !Index, !Bitmap),
            read_entries(Count, EntriesWithSpec, !Index, !Bitmap),
            (
                EntriesWithSpec = [byte(SpecByte) | Entries]
            ->
                Spec = char.det_from_int(SpecByte),
                ( Spec = 'c' ->
                    !:Grammar = !.Grammar ^ charsets ^ elem(CharsetIndex) :=
                        parse_charset(Entries, CharsetIndex)
                ; Spec = 'D' ->
                    !:Grammar = !.Grammar ^ dfa_states ^ elem(DfaIndex) :=
                        parse_dfa(Entries, DfaIndex)
                ; Spec = 'g' ->
                    !:Grammar = !.Grammar ^ groups ^ elem(GroupIndex) :=
                        parse_group(Entries, GroupIndex)
                ; Spec = 'I' ->
                    !:Grammar = !.Grammar ^ initial_states :=
                        parse_initial_states(Entries, _)
                ; Spec = 'L' ->
                    !:Grammar = !.Grammar ^ lalr_states ^ elem(LalrIndex) :=
                        parse_lalr(Entries, LalrIndex)
                ; Spec = 'p' ->
                    !:Grammar = !.Grammar ^ properties ^ elem(PropIndex) :=
                        parse_property(Entries, PropIndex)
                ; Spec = 'R' ->
                    !:Grammar = !.Grammar ^ productions ^ elem(ProdIndex) :=
                        parse_production(Entries, ProdIndex)
                ; Spec = 'S' ->
                    !:Grammar = !.Grammar ^ symbols ^ elem(SymbolIndex) :=
                        parse_symbol(Entries, SymbolIndex)
                ; Spec = 't' ->
                    parse_table_counts(Entries, _)
                        = table_counts(SymbolCount, CharsetCount, RuleCount,
                                       DfaCount, LalrCount, GroupCount),
                    !:Grammar = !.Grammar ^ symbols :=
                        init(SymbolCount, empty),
                    !:Grammar = !.Grammar ^ charsets :=
                        init(CharsetCount, empty),
                    !:Grammar = !.Grammar ^ productions :=
                        init(RuleCount, empty),
                    !:Grammar = !.Grammar ^ dfa_states :=
                        init(DfaCount, empty),
                    !:Grammar = !.Grammar ^ lalr_states :=
                        init(LalrCount, empty),
                    !:Grammar = !.Grammar ^ groups :=
                        init(GroupCount, empty)
                ;
                    unexpected($file, $pred,
                        format("record spec <%c> not implemented!",
                            [c(Spec)]))
                )
            ;
                unexpected($file, $pred, "entry without valid specifier!")
            )
        ;
            unexpected($file, $pred,
                format("record type <%c> not implemted!", [c(RecordType)]))
        ),
        build_tables(NumBytes, !Grammar, !Index, !Bitmap)
    ;
        true % read all NumBytes
    ).

%----------------------------------------------------------------------------%

:- func parse_initial_states `with_type` parse_func(initial_states)
    `with_inst` parse_func.

parse_initial_states(Entries, -1) =
    ( Entries = [word(Dfa), word(Lalr)] ->
        initial_states(Dfa, Lalr)
    ;
        unexpected($file, $pred, "invalid initial states record")
    ).

%----------------------------------------------------------------------------%

:- func parse_table_counts `with_type` parse_func(table_counts)
    `with_inst` parse_func.

parse_table_counts(Entries, -1) =
    ( Entries = [word(SymbolTable), word(CharacterSetTable), word(RuleTable),
                 word(DFATable), word(LALRTable), word(GroupTable)] ->
        table_counts(SymbolTable, CharacterSetTable, RuleTable,
            DFATable, LALRTable, GroupTable)
    ;
        unexpected($file, $pred, "invalid table counts record")
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.record.
%----------------------------------------------------------------------------%
