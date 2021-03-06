%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 tw=78 et
%----------------------------------------------------------------------------%
% File: grmc.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Thu Nov 09 10:24:22 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Interface for the command line based grammar compiler.
%----------------------------------------------------------------------------%

:- module shift_reduce.grmc.

:- interface.

:- import_module io.
:- import_module list.

:- type flag
    ---> force_recompile.

:- type flags == list(flag).

    % compile(GrammarFile, EgtFile, !IO):
    %
    % The compiler executable is executed with the
    % `GrammarFile' as the argument, it should be an absolute path.
    %
    % The predicate will succeed if the command line could execute
    % successfully, and will through an exception otherwise.
    %
    % `EgtFile' unifies with the name of the compiled grammar table,
    % currently with the extension: `.egt', meaning enhanced grammar table.
    %
:- pred compile(string::in, string::out, flags::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module maybe.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

compile(GrammarFile, EgtFile, Flags, !IO) :-
    EgtFile = det_remove_suffix(GrammarFile, ".grm") ++ ".egt",
    ( force_recompile `member` Flags ->
        io.remove_file(EgtFile, _, !IO)
    ;
        true
    ),
    CompileCmd = "grmc" ++ " \"" ++ GrammarFile ++ "\"",
    FailMsg = (pred(Fmt::in, Args::in) is erroneous :-
        unexpected($file, $pred, format("compile [%s] failed: " ++ Fmt,
                    [s(CompileCmd)] ++ Args))),
    io.call_system(CompileCmd, CompileResult, !IO),
    (
        CompileResult = ok(ExitCode),
        ( ExitCode \= 0 ->
            FailMsg("error code: %d", [i(ExitCode)])
        ;
            true
        )
    ;
        CompileResult = error(CompileError),
        FailMsg("io: %s", [s(io.error_message(CompileError))])
    ).

%----------------------------------------------------------------------------%
:- end_module shift_reduce.grmc.
%----------------------------------------------------------------------------%
