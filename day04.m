:- module day04.
:- interface.

:- import_module array2d, char, int, io.

:- pred main(io::di, io::uo) is det.
:- pred part1(array2d(char)::in, {int, int, int, int}::out) is nondet.
:- pred part2(array2d(char)::in, {int, int}::out) is nondet.

:- implementation.

:- import_module list, string, solutions.

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

:- pred find_chars(array2d(char)::in, list(char)::in, int::in, int::in, int::in, int::in).
find_chars(_, [], _, _, _, _).
find_chars(Array, [Char | Rest], Row, Col, DRow, DCol) :-
    array2d.in_bounds(Array, Row, Col),
    Array^elem(Row, Col) = Char,
    find_chars(Array, Rest, Row+DRow, Col+DCol, DRow, DCol).

part1(Array, {Row, Col, DRow, DCol}) :-
    Chars = string.to_char_list("XMAS"),
    array2d.bounds(Array, MaxRow, MaxCol),
    member(Row, 0..MaxRow),
    member(Col, 0..MaxCol),
    member(DRow, -1..1),
    member(DCol, -1..1),
    not (DRow = 0, DCol = 0),
    find_chars(Array, Chars, Row, Col, DRow, DCol).

part2(Array, {Row, Col}) :-
    array2d.bounds(Array, MaxRow, MaxCol),
    % position of A
    member(Row, 0..MaxRow),
    member(Col, 0..MaxCol),
    
    array2d.in_bounds(Array, Row-1, Col-1),
    array2d.in_bounds(Array, Row+1, Col+1),
    Array^elem(Row, Col) = 'A',
    ( ( Array^elem(Row-1, Col-1) = 'M', Array^elem(Row+1, Col+1) = 'S') ;
      ( Array^elem(Row-1, Col-1) = 'S', Array^elem(Row+1, Col+1) = 'M') ),
    ( ( Array^elem(Row+1, Col-1) = 'M', Array^elem(Row-1, Col+1) = 'S') ;
      ( Array^elem(Row+1, Col-1) = 'S', Array^elem(Row-1, Col+1) = 'M') ).
  
main(!IO) :-
    read_lines(Rows, [], !IO),
    Array = array2d.from_lists(Rows),
    length(solutions(part1(Array)), Part1),
    io.print_line(Part1, !IO),   % 2593
    length(solutions(part2(Array)), Part2),
    io.print_line(Part2, !IO).   % 1950
