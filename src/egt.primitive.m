%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: egt.primitive.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri Nov 14 18:51:23 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Primitive operations to read a binary enhanced grammar table
%----------------------------------------------------------------------------%

:- module shift_reduce.egt.primitive.

:- interface.

:- import_module bitmap. % for types byte_index, bitmap
:- import_module bool.
:- import_module char.
:- import_module string.

%----------------------------------------------------------------------------%

:- type read_pred == pred(byte_index, byte_index, bitmap, bitmap).
:- inst read_pred == (pred(in, out, bitmap_di, bitmap_uo) is det).

:- pred read_byte(byte::out)     : read_pred `with_inst` read_pred.
:- pred read_bool(bool::out)     : read_pred `with_inst` read_pred.
:- pred read_ascii(char::out)    : read_pred `with_inst` read_pred.
:- pred read_word(word::out)     : read_pred `with_inst` read_pred.
:- pred read_string(string::out) : read_pred `with_inst` read_pred.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

%----------------------------------------------------------------------------%

read_byte(Byte, !Index, !Bitmap) :-
    Byte = !.Bitmap ^ byte(!.Index),
    !:Index = !.Index + 1.

read_bool(Bool, !Index, !Bitmap) :-
    read_byte(Byte, !Index, !Bitmap),
    ( Byte = 0 ->
        Bool = no
    ; Byte = 1 ->
        Bool = yes
    ;
        unexpected($file, $pred, format("<%x> %s",
            [i(Byte), s("is not a valid boolean value")]))
    ).

read_ascii(Char, !Index, !Bitmap) :-
    read_byte(Byte, !Index, !Bitmap),
    ( Byte =< 0x7f ->
        Char = det_from_int(Byte)
    ;
        unexpected($file, $pred, "Only ASCII chars are allowed")
    ).

read_word(Word, !Index, !Bitmap) :-
    read_byte(Low, !Index, !Bitmap),
    read_byte(High, !Index, !Bitmap),
    Word = (High << 8) \/ Low.

read_string(String, !Index, !Bitmap) :-
    read_chars_acc([], RevUtf16CodeUnits, !Index, !Bitmap),
    reverse(RevUtf16CodeUnits, Utf16CodeUnits),
    ( if
        from_utf16_code_unit_list(Utf16CodeUnits, String0)
    then
        String = String0
    else
        unexpected($file, $pred, "The string is invalid UTF16-z")
    ).

:- pred read_chars_acc(list(int)::in, list(int)::out) : read_pred
    `with_inst` read_pred.

read_chars_acc(!Chars, !Index, !Bitmap) :-
    read_word(Utf16CodeUnit, !Index, !Bitmap),
    ( Utf16CodeUnit = 0 ->
        true
    ;
        !:Chars = [Utf16CodeUnit | !.Chars],
        read_chars_acc(!Chars, !Index, !Bitmap)
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.egt.primitive.
%----------------------------------------------------------------------------%
