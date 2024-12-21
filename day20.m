:- module day20.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array2d, char, int, list, set, solutions, string, queue, require.

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

:- pred array_find(array2d(T)::in, T::in, int::in, int::in, int::out, int::out) is semidet.
array_find(Array, Item, Row, Col, FoundRow, FoundCol) :-
    if not array2d.in_bounds(Array, Row, Col) then
        false
    else if Array^elem(Row, Col) = Item then
        FoundRow = Row,
        FoundCol = Col
    else if array_find(Array, Item, Row, Col+1, FoundRow0, FoundCol0) then
        FoundRow = FoundRow0,
        FoundCol = FoundCol0
    else
        array_find(Array, Item, Row+1, 0, FoundRow, FoundCol).

:- pred bfs(array2d(char)::in, queue({int,int,int})::in, queue({int,int,int})::out, set({int,int})::in, set({int,int})::out, {int,int}::in, list({int,int})::in, list({int,int})::out).
bfs(Array, !Queue, !Seen, {EndRow, EndCol}, !Path) :-
    queue.get({Row, Col, Dist}, !Queue),
    ( if Row = EndRow, Col = EndCol then
        list.cons({Row, Col}, !Path),
        reverse(!Path)
    else if set.contains(!.Seen, {Row, Col}) then
        bfs(Array, !Queue, !Seen, {EndRow, EndCol}, !Path)
    else
        list.cons({Row, Col}, !Path),
        set.insert({Row, Col}, !Seen),
        ( if not Array^elem(Row-1, Col) = '#' then
            queue.put({Row-1, Col, Dist+1}, !Queue)
        else
            true ),
        ( if not Array^elem(Row, Col-1) = '#' then
            queue.put({Row, Col-1, Dist+1}, !Queue)
        else
            true ),
        ( if not Array^elem(Row+1, Col) = '#' then
            queue.put({Row+1, Col, Dist+1}, !Queue)
        else
            true ),
        ( if not Array^elem(Row, Col+1) = '#' then
            queue.put({Row, Col+1, Dist+1}, !Queue)
        else
            true ),
        bfs(Array, !Queue, !Seen, {EndRow, EndCol}, !Path)
    ).

:- func manhattan({int,int}, {int,int}) = int.
manhattan({R1,C1},{R2,C2}) = abs(R1-R2) + abs(C1-C2).

:- pred drop_upto(int::in, list(T)::in, list(T)::out) is det.
drop_upto(N, Xs, FinalXs) :-
    ( if drop(N, Xs, FinalXs0) then
        FinalXs = FinalXs0
    else
        FinalXs = []
    ).

:- func analyze(list({int,int}), int, int) = {int,int} is det.
analyze(Path, Part1, Part2) = Result :-
    ( if [{Row, Col} | Rest] = Path then
        drop_upto(100, Path, Part),
        list.filter_map_corresponding(
            pred({RowX,ColX}::in, I::in, Distance::out) is semidet :- (
                Diff = 100 + I,
                Distance = manhattan({Row, Col}, {RowX, ColX}),
                Distance =< 20,
                Diff - Distance >= 100),
            Part, 1..length(Part),
            Dists),
        Result = analyze(Rest,
            Part1 + length(filter(>=(2), Dists)),
            Part2 + length(Dists))
    else
        Result = {Part1, Part2}
    ).

main(!IO) :-
    read_lines(Rows, [], !IO),
    Array = array2d.from_lists(Rows),
    ( if array_find(Array, 'S', 0, 0, StartRow, StartCol),
        array_find(Array, 'E', 0, 0, EndRow, EndCol),
        bfs(Array, queue.list_to_queue([{StartRow,StartCol,0}]), _, set.init, _, {EndRow, EndCol}, [], Path) then
            {Part1, Part2} = analyze(Path, 0, 0),
            io.print_line(Part1, !IO),   % 1293
            io.print_line(Part2, !IO)    % 977747
    else
        error("invalid data")
    ).
