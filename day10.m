:- module day10.
:- interface.

:- import_module array2d, int, io.

:- pred main(io::di, io::uo) is cc_multi.
:- pred solve(array2d(int)::in, {{int,int},{int,int}}::out) is nondet.

:- implementation.

:- import_module bool, char, maybe, list, string, solutions, require.

:- pred read_lines(list(list(char))::out, list(list(char))::in, io::di, io::uo) is det.
read_lines(LinesOut, LinesIn, !IO) :-
    io.read_line(Result, !IO),
    (
        Result = ok(StringLn),
        list.delete_all(StringLn, '\n', String),
        read_lines(LinesOut, [String | LinesIn], !IO)
    ;
        Result = error(_),
        LinesOut = []
    ;
        Result = eof,
        LinesOut = list.reverse(LinesIn)
    ).

:- pred walk(array2d(int)::in, int::in, int::in, {int,int}::out) is nondet.
walk(Array, Row, Col, Trailhead) :-
    ( if Array^elem(Row, Col) = 9 then
        Trailhead = {Row, Col},
        true
    else
        ( NRow = Row, NCol = Col + 1
        ; NRow = Row, NCol = Col - 1
        ; NRow = Row + 1, NCol = Col
        ; NRow = Row - 1, NCol = Col
        ),
        array2d.in_bounds(Array, NRow, NCol),
        Array^elem(NRow, NCol) = 1 + Array^elem(Row, Col),
        walk(Array, NRow, NCol, Trailhead)
    ).

solve(Array, Result) :-
    array2d.bounds(Array, MaxRow, MaxCol),
    member(Row, 0..MaxRow-1),
    member(Col, 0..MaxCol-1),
    Array^elem(Row, Col) = 0,

    walk(Array, Row, Col, Trailhead),
    Result = {{Row, Col}, Trailhead}.

main(!IO) :-
    read_lines(CharRows, [], !IO),
    Rows = list.map(func(List) = list.map(char.det_decimal_digit_to_int, List), CharRows),
    Array = array2d.from_lists(Rows),

    solutions(solve(Array), Part1),
    io.print_line(list.length(Part1) `with_type` int, !IO),   % 617

    unsorted_solutions(solve(Array), Part2),
    io.print_line(list.length(Part2) `with_type` int, !IO).   % 1477
