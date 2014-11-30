%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: egt.charset.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 16:02:43 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.charset.

:- interface.

:- import_module char. % for det_from_int/1, type char.

:- import_module shift_reduce.egt.entry. % for type parse_func

%----------------------------------------------------------------------------%

:- type charset.

:- func empty = charset.

:- pred is_in_charset(char::in, charset::in) is semidet.

:- func parse_charset `with_type` parse_func(charset) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int. % for <</2
:- import_module list.
:- import_module sparse_bitset.
:- import_module require.

%----------------------------------------------------------------------------%

:- type charset == sparse_bitset(char).

empty = init.

parse_charset(Entries, Index) = Charset :-
    ( if
        Entries = [word(Index0), word(UnicodePlane),
                   word(RangeCount), reserved | RangeEntries],
        % security measures for build_charsets/5 ranges
        length(RangeEntries) = RangeCount * 2,
        0 =< UnicodePlane, UnicodePlane =< 0xff
      then
        Index = Index0,
        Offset = UnicodePlane << 6, % 0xPPhhhhhh
        build_charsets(Offset, empty, Charset, RangeEntries, RangeRest),
        expect(is_empty(RangeRest), $file, $pred, "still have range entries")
      else
        unexpected($file, $pred, "invalid character set record")
    ).

is_in_charset(Char, Charset) :- member(Char, Charset).

:- pred build_charsets(int::in, charset::in, charset::out,
                       entries::in, entries::out).

build_charsets(Offset, !Charset) -->
    ( if [word(StartUnit), word(EndUnit)] then
        {
            CharCodeRange = (Offset + StartUnit) `..` (Offset + EndUnit),
            CharList = map(det_from_int, CharCodeRange),
            insert_list(CharList, !Charset)
        },
        build_charsets(Offset, !Charset)
      else if [_] then
        { unexpected($file, $pred, "premature end of range list") }
      else
        { true }
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.charset.
%----------------------------------------------------------------------------%
