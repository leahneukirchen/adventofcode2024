:- module day12.
:- interface.

:- import_module int, io, list, set.

:- pred main(io::di, io::uo) is det.
:- func part1(list(set({int,int}))) = int.
:- func part2(list(set({int,int}))) = int.

:- implementation.

:- import_module array2d, char, eqvclass, string, require.

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

:- pred regions(array2d(T)::in, int::in, int::in, eqvclass({int, int})::in, eqvclass({int, int})::out).
regions(Array, Row, Col, !Eqv) :-
    eqvclass.new_element({Row, Col}, !Eqv),
    Here = Array ^ elem(Row, Col),
    ( if array2d.in_bounds(Array, Row - 1, Col), array2d.lookup(Array, Row - 1, Col, Here) then
        eqvclass.ensure_equivalence({Row, Col}, {Row - 1, Col}, !Eqv)
    else
        true
    ),
    ( if array2d.in_bounds(Array, Row, Col - 1), array2d.lookup(Array, Row, Col - 1, Here) then
        eqvclass.ensure_equivalence({Row, Col}, {Row, Col - 1}, !Eqv)
    else
        true
    ).

:- func area(set({int,int})) = int.
area(Region) = set.count(Region).

:- func perimeter(set({int,int})) = int.
perimeter(Region) =
    list.foldl(int.plus,
        list.map(func({Row,Col}) = 4 - set.count(set.intersect(Region,
            set.from_list([{Row-1, Col}, {Row+1, Col}, {Row, Col-1}, {Row, Col+1}]))),
            set.to_sorted_list(Region)),
        0).

:- func corners_at(set({int,int}), {int,int}) = int.
corners_at(Region, {Row, Col}) =
    ( if not contains(Region, {Row-1, Col}), not contains(Region, {Row, Col-1}) then 1 else 0 ) +
    ( if not contains(Region, {Row-1, Col}), not contains(Region, {Row, Col+1}) then 1 else 0 ) +
    ( if not contains(Region, {Row+1, Col}), not contains(Region, {Row, Col-1}) then 1 else 0 ) +
    ( if not contains(Region, {Row+1, Col}), not contains(Region, {Row, Col+1}) then 1 else 0 ) +

    ( if contains(Region, {Row-1, Col}), contains(Region, {Row, Col-1}), not contains(Region, {Row-1, Col-1}) then 1 else 0 ) +
    ( if contains(Region, {Row-1, Col}), contains(Region, {Row, Col+1}), not contains(Region, {Row-1, Col+1}) then 1 else 0 ) +
    ( if contains(Region, {Row+1, Col}), contains(Region, {Row, Col-1}), not contains(Region, {Row+1, Col-1}) then 1 else 0 ) +
    ( if contains(Region, {Row+1, Col}), contains(Region, {Row, Col+1}), not contains(Region, {Row+1, Col+1}) then 1 else 0 ).

:- func corners(set({int,int})) = int.
corners(Region) =
    list.foldl(int.plus, list.map(corners_at(Region), set.to_sorted_list(Region)), 0).

part1(Regions) =
    list.foldl(int.plus, list.map(func(Region) = area(Region) * perimeter(Region), Regions), 0).
    
part2(Regions) =
    list.foldl(int.plus, list.map(func(Region) = area(Region) * corners(Region), Regions), 0).

main(!IO) :-
    read_lines(Rows, [], !IO),
    Array = array2d.from_lists(Rows),
    array2d.bounds(Array, MaxRow, MaxCol),

    int.fold_up(pred(Row::in, !.Eqv::in, !:Eqv::out) is det :- int.fold_up(
        pred(Col::in, !.Eqv2::in, !:Eqv2::out) is det :-
            regions(Array, Row, Col, !Eqv2),
        0, MaxCol-1, !Eqv),
        0, MaxRow-1, eqvclass.init, RegionsEqv),
    
    eqvclass.partition_list(RegionsEqv, Regions),
    
    io.print_line(part1(Regions), !IO),   % 1375574
    io.print_line(part2(Regions), !IO).   % 830566
