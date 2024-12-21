:- module day19.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, string, require.

:- pred read_lines(list(string)::out, list(string)::in, io::di, io::uo) is det.
read_lines(LinesOut, LinesIn, !IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(String),
        read_lines(LinesOut, [chomp(String) | LinesIn], !IO)
    ;
        Result = error(_),
        LinesOut = []
    ;
        Result = eof,
        LinesOut = list.reverse(LinesIn)
    ).        

:- pred match(list(string)::in, string::in) is semidet.
match(_Patterns, "").
match(Patterns, Design) :-
    list.member(Pattern, Patterns),
    remove_prefix(Pattern, Design, Design1),
    match(Patterns, Design1).

:- func match2(list(string), string) = int.
:- pragma memo(func(match2/2)).  % the key to part 2
match2(Patterns, Design) =
    ( if Design = "" then
        1
    else
        list.foldl(int.plus,
            list.filter_map(func(Pattern) = match2(Patterns, Design1) is semidet :-
                remove_prefix(Pattern, Design, Design1),
            Patterns),
            0)
    ).

main(!IO) :-
    read_lines(Rows, [], !IO),
    ( if [ PatternLine, _ | Designs ] = Rows then
        Patterns = string.split_at_string(", ", PatternLine),
        io.print_line(list.length(list.filter(match(Patterns), Designs))+0, !IO),  % 242
        io.print_line(list.foldl(int.plus, list.map(match2(Patterns), Designs), 0), !IO)   % 595975512785325
    else
        error("parse error")
    ).
