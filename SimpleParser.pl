expr(tree(copy, Left, Right)) --> destination(Left), ["="], ["COPY"], source(Right).
expr(tree(load, Left, Right)) --> destination(Left), ["="], ["LOAD"], source(Right).
expr(tree(store, Left, Right)) --> ["STORE"], destination(Left), source(Right).
expr(tree(cbranch, Left, Right)) --> ["CBRANCH"], destination(Left), bool(Right).
expr(tree(branch, Left)) --> ["BRANCH"], destination(Left).
expr(tree(branchind, Left)) --> ["BRANCHIND"], offset(Left).
expr(tree(call, Left)) --> ["CALL"], location(Left).
expr(tree(callind, Left)) --> ["CALLIND"], location(Left).
expr(tree(return, Left)) --> ["RETURN"], location(Left).
expr(tree(piece, Left, Right)) --> ["PIECE"], source(Left), source(Right). %This instruction isn't currently used in my example file.  The parse for this may be incorrect
expr(tree(subpiece, Left, Middle, Right)) --> destination(Left), ["="], ["SUBPIECE"], source(Middle), number(Right).
expr(tree(int2float, Left, Right)) --> destination(Left), ["="], ["INT2FLOAT"], source(Right).
expr(tree(float2float, Left, Right)) --> destination(Left), ["="], ["FLOAT2FLOAT"], source(Right).
expr(tree(trunc, Left, Right)) --> destination(Left), ["="], ["TRUNC"], source(Right).
expr(tree(popcount, Left, Right)) --> destination(Left), ["="], ["POPCOUNT"], source(Right).
expr(tree(callother, Left)) -->["CALLOTHER"], instruction(Left).
expr(tree(callother, Left, Middle, Right)) --> destination(Left), ["="], ["CALLOTHER"], instruction(Middle), source(Right).
expr(tree(NUMOP, Left, Right)) --> destination(Left), ["="], numOp(NUMOP), source(Right).
expr(tree(NUMOP, Left, Middle, Right)) --> destination(Left), ["="], numOp(NUMOP), source(Middle), source(Right).
expr(tree(expression_tag, EXPRTAG)) --> tag(EXPRTAG).
% This is actually a destination but I'm cheating
expr(tree(return, Left)) --> ["RETURN"], source(Left).

offset(X) --> address([X]).
instruction(X) --> [String], { X = String }.
bool(X) --> location(X).

destination(X) --> location(X).
destination(X) --> tag(X).

location(X) --> address([X]).
location(X) --> register([X]).
location(X) --> pointer([X]).

source(X) --> location(X).
source(X) --> number(X).

numOp(X) --> [String], { string(String), has_num_prefix(String), string_lower(String, Y), X = Y }.

% An address is currently defined as any string starting with "$U"
address([X]) --> [String], {string(String), has_address_prefix(String), X = String}.
%For unhandled strings
address([X]) --> [String], {string(String), X = String}.

% A register is currently defined as any string of capital letters with 3 or less characters
register([X]) --> [String], {string(String), string_codes(String, Codes),
	forall(member(Code, Codes), (Code >= 65, Code =< 90 ; Code >= 48, Code =< 57 )), length(Codes, Length),
	Length =< 3, X = String}.

register([X]) --> [String], {string(String), has_fpu_prefix(String), X = String}.

% This can process any 'number' P-code gives you, Numbers usually come in the format: Number:Number.
	%Note: It is possible that this is a misunderstanding of number formatting.  I'm 80% sure this is right.
number(X) --> [String], {string(String), string_codes(String, Codes), 
	forall(member(Code, Codes), (Code >= 48, Code =< 58)), X = String}.

% A pointer is defined as a string with the "ram(" prefix.
	%Note: I should come up with a way to confirm that a register is what's being passed into "ram()."
pointer([X]) --> [String], 
	{string(String), has_pointer_prefix(String), X = String }.

% A pointer is defined as a string with the "ram(" prefix.
	%Note: I should come up with a way to confirm that a register is what's being passed into "ram()."
tag([X]) --> [String], 
	{string(String), isTag(String), X = String }.

% Parse Command
parse(String) :-
	split_string(String, " ", ",", Atoms),
	phrase(expr(SyntaxTree), Atoms),
	write(SyntaxTree).

% Define a list of predefined prefixes
address_prefixes(["$U", "*[ram]", "0x", "FS_", "XMM"]).
% Predicate to check if a string has a prefix that matches any predefined prefix
has_address_prefix(String) :-
    address_prefixes(PrefixList),
    member(Prefix, PrefixList),
    atom_concat(Prefix, _, String).

% Define a list of predefined prefixes
pointer_prefixes(["ram("]).
% Predicate to check if a string has a prefix that matches any predefined prefix
has_pointer_prefix(String) :-
    pointer_prefixes(PrefixList),
    member(Prefix, PrefixList),
    atom_concat(Prefix, _, String).

% Define a list of predefined prefixes
numPrefixes(["INT", "FLOAT", "BOOL"]).
% Predicate to check if a string has a prefix that matches any predefined prefix
has_num_prefix(String) :-
    numPrefixes(PrefixList),
    member(Prefix, PrefixList),
    atom_concat(Prefix, _, String).

% Define a list of predefined prefixes
fpuPrefixes(["FPU"]).
% Predicate to check if a string has a prefix that matches any predefined prefix
has_fpu_prefix(String) :-
    fpuPrefixes(PrefixList),
    member(Prefix, PrefixList),
    atom_concat(Prefix, _, String).

% Define a list of predefined prefixes
tagPrefix(["<"]).
% Predicate to check if a string has a prefix that matches any predefined prefix
isTag(String) :-
    tagPrefix(PrefixList),
    member(Prefix, PrefixList),
    atom_concat(Prefix, _, String).

% Command that reads and prints lines from a file
parse_file(FileName) :-
    open(FileName, read, Stream),
    parse_lines(Stream),
    close(Stream).

% Function that reads lines from the stream and prints them
parse_lines(Stream) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        parse(Line),
        parse_lines(Stream)
    ; true
    ).