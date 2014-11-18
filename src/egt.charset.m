%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: egt.charset.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Nov 18 16:02:43 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% TODO: module documentation
%----------------------------------------------------------------------------%

:- module egt.charset.

:- interface.

:- import_module bitmap. % for type word

%----------------------------------------------------------------------------%

:- type chars.

:- type charset
    --->    charset(
                cs_unicode_plane    :: word,
                cs_range_count      :: word,  % number of ranges
                cs_ranges           :: chars
            ).

:- func empty = charset.

:- func parse_charset `with_type` parse_func(charset) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char. % for det_from_int/1
:- import_module list.
:- import_module sparse_bitset.
:- import_module require.

%----------------------------------------------------------------------------%

:- type chars == sparse_bitset(char).

empty = charset(-1, -1, init).

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
            Chars = map(det_from_int, StartUnit `..` EndUnit),
            insert_list(Chars, !Charset)
        },
        build_charset(!Charset)
    else if [_] then
        { unexpected($file, $pred, "premature end of range list") }
    else
        { true }
    ).

%----------------------------------------------------------------------------%
:- end_module egt.charset.
%----------------------------------------------------------------------------%
