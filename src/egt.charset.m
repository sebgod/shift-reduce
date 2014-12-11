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

:- import_module charset. % for type charset, in mercury_misc
:- import_module shift_reduce.egt.entry. % for type parse_func

%----------------------------------------------------------------------------%

:- func empty = charset.

:- func parse_charset `with_type` parse_func(charset) `with_inst` parse_func.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int. % for <</2
:- import_module pair. % for type pair
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

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

:- pred build_charsets(int::in, charset::in, charset::out,
                       entries::in, entries::out).

build_charsets(Offset, !Charset) -->
    ( if [word(StartUnit), word(EndUnit)] then
        {
            CharCodeRange = (Offset + StartUnit) - (Offset + EndUnit),
            !:Charset = union(!.Charset, charset_from_range(CharCodeRange))
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
