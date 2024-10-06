parse_file(FileName) :-
    open(FileName, read, Stream),
    parse_lines(Stream),
    close(Stream).

parse_lines(Stream) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        parse(Line),
        parse_lines(Stream)
    ; true
    ).

parse(String) :-
	split_string(String, " ", ",", Atoms),
    phrase(instruction(X), Atoms),
	write(X).

instruction([X, [Y|Z]]) --> arguement(Y), ["="], command(X), arguement(Z).
instruction([X, Y]) --> command(X), arguement(Y).

command([X]) --> [X], {is_command(X)}.

arguement([]) --> [].
arguement(X) --> [X].
arguement([X|T]) --> [X], arguement(T).

commands(["COPY", "LOAD", "STORE", "CBRANCH", "BRANCH", "BRANCHIND", "CALL", "CALLIND", "RETURN",
"PIECE", "SUBPIECE", "INT2FLOAT", "FLOAT2FLOAT", "TRUNC", "POPCOUNT", "CALLOTHER"]).
is_command(String) :-
    commands(CommandList),
    member(String, CommandList).


