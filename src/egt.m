:- module egt.

:- interface.

:- use_module io, int.

:- type egt_file == int.

:- func parse(io.binary_input_stream) = egt_file.

:- implementation.

parse(InputStream) = 0.

