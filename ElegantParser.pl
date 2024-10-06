% DCG for ARG = CMD ARGS
instruction([X, [Y|Z]]) --> arguement(Y), ["="], command(X), arguement(Z).

% DCG for CMD ARGS
instruction([X, Y]) --> command(X), arguement(Y).

% DCG for CMD
command([X]) --> [X].

%DCGs for Argument lists
arguement([]) --> [].
arguement(X) --> [X].
arguement([X|T]) --> [X], arguement(T).

% Run this command to parse the file.  Pass it the name of your AST.
parse_file(FileName, AST) :-
    open(FileName, read, Stream),
    read_lines(Stream, AST),
    close(Stream).

% A subcommand that parses lines and adds them to the AST
read_lines(Stream, AST) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        parse(Line, ParsedList),
        read_lines(Stream, RestList),
        AST = [ParsedList | RestList]
    ; AST = []
    ).

% The parse function responsible for parsing lines.
parse(String, ParsedList) :-
	split_string(String, " ", ",", Atoms),
    phrase(instruction(X), Atoms),
    ParsedList = X.

