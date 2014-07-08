:- module main.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string, egt.

main(!IO) :-
    command_line_arguments(Args, !IO),
    (   if list.index0(Args, 0, Arg0) 
        then FileName = Arg0
        else FileName = "ParserTest.egt"
    ),
    io.open_binary_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(FileInput),
        EgtFile = egt.parse(FileInput),
        io.close_binary_input(FileInput, !IO)
    ;
        OpenResult = error(IO_Error),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, io.error_message(IO_Error) ++ "\n", !IO)
    ).

