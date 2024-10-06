instruction([X, [Y|Z]]) --> arguement(Y), ["="], command(X), arguement(Z).
instruction([X, Y]) --> command(X), arguement(Y).

command([X]) --> [X].

arguement([]) --> [].
arguement(X) --> [X].
arguement([X|T]) --> [X], arguement(T).

parse_file(FileName, AST) :-
    open(FileName, read, Stream),
    read_lines(Stream, AST),
    close(Stream).

read_lines(Stream, AST) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        parse(Line, ParsedList),
        read_lines(Stream, RestList),
        AST = [ParsedList | RestList]
    ; AST = []
    ).

parse(String, ParsedList) :-
	split_string(String, " ", ",", Atoms),
    phrase(instruction(X), Atoms),
    ParsedList = X.

