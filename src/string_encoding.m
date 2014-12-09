%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: string_encoding.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Sun Jun 22 11:13:20 CEST 2014
% Stability: low
%----------------------------------------------------------------------------%
% A little effort to implement UTF-16 support for strings.
%----------------------------------------------------------------------------%

:- module string_encoding.

:- interface.

:- import_module list.

%----------------------------------------------------------------------------%

:- type utf_encoding
    ---> utf7
    ;    utf8
    ;    utf16
    ;    utf32
    ;    gb18030.

    % `native_encoding' returns the encoding used for strings,
    % i.e. the encoding for a code unit.
:- func native_encoding = utf_encoding.

:- type code_unit_list == list(int).

:- type conv_pred == pred(code_unit_list, code_unit_list).
:- inst conv_pred == (pred(in, out) is det).

%----------------------------------------------------------------------------%

    % `native_code_units_to_unicode(TargetEncoding, !CodeUnits)'
    % converts a list of native code units used for encoding strings in the
    % current grade to any of the most common Unicode transformation formats,
    % and will throw an exception if the desired Unicode transformation
    % format is not supported.
    %
:- pred native_code_units_to_unicode(utf_encoding::in)
    `with_type` conv_pred `with_inst` conv_pred.

    % `native_code_units_to_unicode(SourceEncoding, !CodeUnits)'
    % converts a list of code units encoded in the `SourceEncoding' to the
    % native encoding used as the string encoding for the current grade.
    % This predicate will throw an exception if the source
    % Unicode transformation format is not supported.
    %
:- pred native_code_units_from_unicode(utf_encoding::in)
    `with_type` conv_pred `with_inst` conv_pred.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module require.

%----------------------------------------------------------------------------%
%
% `native_encoding' is implemented as a foreign procedure.
%
% It mapping for all implemented grades is:
%   * UTF-16 for the Java and C# grades
%   * UTF-8  for all others
%

:- pragma foreign_proc("Java",
    string_encoding.native_encoding = (Encoding::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Encoding = utf16;
").

:- pragma foreign_proc("C#",
    string_encoding.native_encoding = (Encoding::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Encoding = utf16;
").

native_encoding = utf8.

%----------------------------------------------------------------------------%
%
% The implementation uses the native encoding to efficiently convert from one
% encoding to another. The goal is to reduce the number of conversions to
% a mininum.
%

:- func encoding_name(utf_encoding) = string is det.

encoding_name(utf7)  = "UTF-7".
encoding_name(utf8)  = "UTF-8".
encoding_name(utf16) = "UTF-16".
encoding_name(utf32) = "UTF-32".
encoding_name(gb18030) = "GB18030".

native_code_units_to_unicode(TargetEncoding, !CodeUnits) :-
    ( native_encoding = TargetEncoding ->
        true
    ; native_encoding = utf16, TargetEncoding = utf8 ->
        utf16_to_utf8(!CodeUnits)
    ;
        unexpected($file, $pred,
            format("%s -> %s is not supported in grade %s",
                [s(encoding_name(native_encoding)),
                 s(encoding_name(TargetEncoding)), s($grade)]))
    ).

:- pred utf16_to_utf8 `with_type` conv_pred `with_inst` conv_pred.

utf16_to_utf8(Utf16CodeUnits, Utf8CodeUnits) :-
    utf16_to_utf8_2(Utf16CodeUnits, [], Utf8CodeUnits).

:- pred utf16_to_utf8_2(code_unit_list::in)
    `with_type` conv_pred `with_inst` conv_pred.

utf16_to_utf8_2([]) --> [].
utf16_to_utf8_2([Utf16CodeUnit | Utf16CodeUnits]) -->
    { det_from_int(Utf16CodeUnit, Char) },
    (
        { is_leading_surrogate(Char) }
    ->
        (
            { Utf16CodeUnits = [TrailCodeUnit | Utf16CodeUnitsNoTrail],
            from_int(TrailCodeUnit, Trail),
            is_trailing_surrogate(Trail) }
        ->
            { det_from_int((TrailCodeUnit - 0xd800) * 0x400 +
                Utf16CodeUnit - 0xdc00 + 0x10000, Combined) },
            append_utf8_code_units(Combined),
            utf16_to_utf8_2(Utf16CodeUnitsNoTrail)
        ;
            { unexpected($file, $pred, "Lead surrogate without trail") }
        )
    ;
        append_utf8_code_units(Char),
        utf16_to_utf8_2(Utf16CodeUnits)
    ).

:- pred append_utf8_code_units(char::in)
    `with_type` conv_pred `with_inst` conv_pred.

append_utf8_code_units(Char, !CodeUnits) :-
    (
        char.to_utf8(Char, CharUnits)
    ->
        !:CodeUnits =  !.CodeUnits ++ CharUnits
    ;
        unexpected($file, $pred, "Cannot convert char to UTF-8")
    ).

%-----------------------------------------------------------------------------%

native_code_units_from_unicode(SourceEncoding, !CodeUnits) :-
    ( native_encoding = SourceEncoding ->
        true
    ; native_encoding = utf16, SourceEncoding = utf8 ->
        utf8_to_utf16(!CodeUnits)
    ; native_encoding = utf8, SourceEncoding = utf16 ->
        utf16_to_utf8(!CodeUnits)
    ;
        unexpected($file, $pred,
            format("%s -> %s is not supported in grade %s",
                [s(encoding_name(SourceEncoding)),
                 s(encoding_name(native_encoding)), s($grade)]))
    ).

:- pred utf8_to_utf16 `with_type` conv_pred `with_inst` conv_pred.

utf8_to_utf16(Utf8CodeUnits, Utf16CodeUnits) :-
    utf8_to_utf16_2(Utf8CodeUnits, [], Utf16CodeUnits).

:- pred utf8_to_utf16_2(code_unit_list::in)
    `with_type` conv_pred `with_inst` conv_pred.

utf8_to_utf16_2([]) --> [].
utf8_to_utf16_2([Utf8CodeUnit | Utf8CodeUnits]) -->
    (
        { Utf8CodeUnit =< 0x7f }
    ->
        { det_from_int(Utf8CodeUnit, Char) },
        append_utf16_code_units(Char),
        utf16_to_utf8_2(Utf8CodeUnits)
    ;
        { unexpected($file, $pred, "Non-ASCII chars are not supported yet") }
    ).

:- pred append_utf16_code_units(char::in)
    `with_type` conv_pred `with_inst` conv_pred.

append_utf16_code_units(Char, !CodeUnits) :-
    (
        char.to_utf16(Char, CharUnits)
    ->
        !:CodeUnits =  !.CodeUnits ++ CharUnits
    ;
        unexpected($file, $pred, "Cannot convert char to UTF-16")
    ).

%----------------------------------------------------------------------------%
:- end_module string_encoding.
%----------------------------------------------------------------------------%
