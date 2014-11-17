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
:- import_module sparse_bitset.
%----------------------------------------------------------------------------%

:- type entry
    --->    reserved
    ;       bool(bool)
    ;       byte(byte)
    ;       word(word)
    ;       string(string).

:- type entries == list(entry).

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

:- type chars == sparse_bitset(char).

:- type action
    --->    action(
                action_symbol_index   :: word,
                action_kind           :: action_kind,
                action_target_index   :: word
            ).

:- type charset
    --->    charset(
                cs_unicode_plane    :: word,
                cs_range_count      :: word,  % number of ranges
                cs_ranges           :: chars
            ).

:- type dfa_state
    --->    dfa_state(
                dfa_accept_index    :: maybe(word),
                dfa_edges           :: array(edge)
            ).

:- type group
    --->    group(
                group_name              :: string,
                group_container_index   :: word,
                group_start_index       :: word,
                group_end_index         :: word,
                group_advance_mode      :: group_advance_mode,
                group_ending_mode       :: group_ending_mode,
                group_nestings          :: array(group_nesting)
            ).

:- type initial_states
    --->    initial_states(
                init_dfa    :: word,  % initial DFA state (0)
                linit_lalr  :: word   % initial LALR state (0)
            ).

:- type lalr_state
    --->    lalr_state(
                lalr_actions    :: array(action)
            ).

:- type production
    --->    production(
                prod_head_index     :: word,
                prod_symbols        :: array(word) % indicies to symbol table
            ).

:- type property
    --->    property(
                prop_key    :: string,
                prop_value  :: string
            ).

:- type symbol
    --->    symbol(
                sym_name    :: string,
                sym_kind    :: symbol_kind
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

:- import_module char.
:- import_module int.
:- import_module io. % for tracing
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
                        init(SymbolCount, symbol("", error)),
                    !:Grammar = !.Grammar ^ charsets :=
                        init(CharsetCount, charset(-1, -1, init)),
                    !:Grammar = !.Grammar ^ productions :=
                        init(RuleCount, production(-1, make_empty_array)),
                    !:Grammar = !.Grammar ^ dfa_states :=
                        init(DfaCount, dfa_state(no, make_empty_array)),
                    !:Grammar = !.Grammar ^ lalr_states :=
                        init(LalrCount, lalr_state(make_empty_array)),
                    !:Grammar = !.Grammar ^ groups :=
                        init(GroupCount, group("", -1, -1, -1, token, open,
                            make_empty_array))
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
        true
    ).

%----------------------------------------------------------------------------%

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
        read_entries(Count - 1, !Entries, !Index, !Bitmap)
    ;
        true
    ).

%----------------------------------------------------------------------------%

:- type parse_func(T) == (func(entries, word) = T).
:- inst parse_func == (func(in, out) = out is det).

%----------------------------------------------------------------------------%

:- func parse_charset `with_type` parse_func(charset) `with_inst` parse_func.

parse_charset(Entries, Index) = Charset :-
    (
        Entries = [word(Index0), word(UnicodePlane),
                   word(RangeCount), reserved | RangeEntries]
    ->
        Index = Index0,
        build_charset(init, Ranges, RangeEntries, RangeRest),
        expect(is_empty(RangeRest), $file, $pred, "still have range entries"),
        Charset = charset(UnicodePlane, RangeCount, Ranges)
    ;
        unexpected($file, $pred, "invalid character set record")
    ).

:- pred build_charset(chars::in, chars::out, entries::in, entries::out).

build_charset(!Charset) -->
    ( if [word(StartUnit), word(EndUnit)] then
        {
            Chars = map(char.det_from_int, StartUnit `..` EndUnit),
            insert_list(Chars, !Charset)
        },
        build_charset(!Charset)
    else if [_] then
        { unexpected($file, $pred, "premature end of range list") }
    else
        { true }
    ).

%----------------------------------------------------------------------------%

:- func parse_dfa `with_type` parse_func(dfa_state) `with_inst` parse_func.

parse_dfa(Entries, Index) = DfaState :-
    ( Entries = [word(Index0), bool(AcceptState), word(AcceptIndex),
                 reserved | EdgeEntries]
    ->
        Index = Index0,
        MaybeAcceptIndex = (AcceptState = yes -> yes(AcceptIndex) ; no),
        generate_foldl(length(EdgeEntries) // 3,
            (pred(_Idx::in, edge(CharSetIndex, TargetIndex)::out,
                in, out) is det -->
                (
                    [word(CharSetIndex0), word(TargetIndex0), reserved]
                ->
                    { CharSetIndex = CharSetIndex0,
                      TargetIndex  = TargetIndex0 }
                ;
                    { unexpected($file, $pred, "premature end of edge list") }
                )
            ),
            Edges,
            EdgeEntries,
            EdgeRest
        ),
        expect(is_empty(EdgeRest), $file, $pred, "still have edge entries"),
        DfaState = dfa_state(MaybeAcceptIndex, Edges)
    ;
        unexpected($file, $pred, "invalid DFA state record")
    ).

%----------------------------------------------------------------------------%

:- func parse_group `with_type` parse_func(group) `with_inst` parse_func.

parse_group(Entries, Index) = (
        unexpected($file, $pred, "invalid group record")
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

:- func parse_lalr `with_type` parse_func(lalr_state) `with_inst` parse_func.

parse_lalr(Entries, Index) = LalrState :-
    ( Entries = [word(Index0), reserved | ActionEntries] ->
        Index = Index0,
        generate_foldl(length(ActionEntries) // 4,
            (pred(_Idx::in,
                action(SymbolIndex, ActionKind, TargetIndex)::out,
                in, out) is det -->
                (
                    [word(SymbolIndex0), word(ActionConstant),
                     word(TargetIndex0), reserved]
                ->
                    { SymbolIndex = SymbolIndex0,
                      ( if ActionKind0 = from_int(ActionConstant) then
                          ActionKind = ActionKind0
                      else
                          unexpected($file, $pred, "unknown action kind")
                      ),
                      TargetIndex  = TargetIndex0 }
                ;
                    { unexpected($file, $pred,
                        "premature end of action list") }
                )
            ),
            Actions,
            ActionEntries,
            ActionRest
        ),
        expect(is_empty(ActionRest), $file, $pred,
            "still have action entries"),
        LalrState = lalr_state(Actions)
    ;
        unexpected($file, $pred, "invalid LALR state record")
    ).

%----------------------------------------------------------------------------%

:- func parse_property `with_type` parse_func(property)
    `with_inst` parse_func.

parse_property(Entries, Index) = Property :-
    ( Entries = [word(Index0), string(Key), string(Value)] ->
        Index = Index0,
        Property = property(Key, Value)
    ;
        unexpected($file, $pred, "invalid property record")
    ).

%----------------------------------------------------------------------------%

:- func parse_production `with_type` parse_func(production)
    `with_inst` parse_func.

parse_production(Entries, Index) = Rule :-
    ( Entries = [word(Index0), word(HeadIndex), reserved | SymbolEntries] ->
        Index = Index0,
        generate_foldl(length(SymbolEntries),
            (pred(_Idx::in, SymbolIndex::out, in, out) is det -->
                (
                    [word(SymbolIndex0)]
                ->
                    { SymbolIndex = SymbolIndex0 }
                ;
                    { unexpected($file, $pred, "premature end of range list") }
                )
            ),
            Symbols,
            SymbolEntries,
            SymbolRest
        ),
        expect(is_empty(SymbolRest), $file, $pred,
            "still have symbol entries"),
        Rule = production(HeadIndex, Symbols)
    ;
        unexpected($file, $pred, "invalid rule record")
    ).

%----------------------------------------------------------------------------%

:- func parse_symbol `with_type` parse_func(symbol) `with_inst` parse_func.

parse_symbol(Entries, Index) = Symbol :-
    (
        Entries = [word(Index0), string(Name), word(SymbolConstant)],
        SymbolKind = from_int(SymbolConstant)
    ->
        Index = Index0,
        Symbol = symbol(Name, SymbolKind)
    ;
        unexpected($file, $pred, "invalid symbol record")
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
