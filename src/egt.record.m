%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
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

:- import_module bitmap. % for type num_bytes
:- import_module shift_reduce.egt.grammar.
:- import_module shift_reduce.egt.primitive.

%----------------------------------------------------------------------------%

:- pred build_tables(num_bytes::in, grammar::in, grammar::out)
    `with_type` read_pred `with_inst` read_pred.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module array. % for ^ 'elem :='/1
:- import_module char. % for det_from_Int/1
:- import_module int.
:- import_module list.
:- import_module string. % for type poly_type
:- import_module require.
:- import_module shift_reduce.egt.charset.
:- import_module shift_reduce.egt.dfa.
:- import_module shift_reduce.egt.entry.
:- import_module shift_reduce.egt.group.
:- import_module shift_reduce.egt.lalr.
:- import_module shift_reduce.egt.production.
:- import_module shift_reduce.egt.property.
:- import_module shift_reduce.egt.state.
:- import_module shift_reduce.egt.symbol.
:- import_module shift_reduce.egt.table.

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
                Spec = det_from_int(SpecByte),
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
                    !:Grammar = !.Grammar ^ initial_state :=
                        parse_initial_lexer_state(Entries, _)
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

:- type table_counts
    --->    table_counts(
                count_symbol    :: table_index,
                count_charset   :: table_index,
                count_rule      :: table_index,
                count_dfa       :: table_index,
                count_lalr      :: table_index,
                count_group     :: table_index
            ).

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
