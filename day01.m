:- module day01.
:- interface.

:- import_module int, io, list.

:- pred main(io::di, io::uo) is det.
:- pred part1(list(int)::in, list(int)::in, int::out) is det.
:- pred part2(list(int)::in, list(int)::in, int::out) is det.

:- implementation.

:- import_module bag, string, require, parsing_utils.

:- pred read_lines(list(string)::out, list(string)::in, io::di, io::uo) is det.

read_lines(LinesOut, LinesIn, !IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(String),
        read_lines(LinesOut, [String | LinesIn], !IO)
    ;
        Result = error(_),
        LinesOut = []
    ;
        Result = eof,
        LinesOut = list.reverse(LinesIn)
    ).        

:- pred parse(string::in, int::out, int::out) is det.

parse(Line, Left, Right) :-
    disable_warning[no_solution_disjunct]
    some[!PS] (
        parsing_utils.new_src_and_ps(Line, Src, !:PS),
        parsing_utils.int_literal(Src, Left, !PS),
        parsing_utils.int_literal(Src, Right, !PS),
        _ = !.PS
    ;
        error("parse error")
    ).

part1(Left, Right, Result) :-
    Diffs = list.map_corresponding(func(L, R) = int.abs(L - R),
        sort(Left), sort(Right)),
    Result = list.foldl(int.plus, Diffs, 0).

part2(Left, Right, Result) :-
    bag.insert_list(Right, bag.init, Bag),
    Prods = list.map(func(L) = (L * bag.count_value(Bag, L)), Left),
    Result = list.foldl(int.plus, Prods, 0).

main(!IO) :-
    read_lines(Lines, [], !IO),
    list.map2(parse, Lines, Left, Right),
    part1(Left, Right, Part1),
    io.print_line(Part1, !IO),   % 1197984
    part2(Left, Right, Part2),
    io.print_line(Part2, !IO).   % 23387399
