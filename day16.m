:- module day16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module array2d, bool, char, int, list, map, maybe, pqueue, set, solutions, string, require.

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

:- pred rotate({int,int}::in, {int,int}::out).
rotate({-1,0}, {0, 1}).
rotate({0, 1}, {1, 0}).
rotate({1, 0}, {0,-1}).
rotate({0,-1}, {-1,0}).

:- pred set_min(map({int,int,int,int}, int)::in, map({int,int,int,int}, int)::out, {int,int,int,int}::in, int::in) is semidet.
set_min(!Map, Pos, Cost) :-
    ( if map.search(!.Map, Pos) = Cost0 then
        ( if Cost =< Cost0 then
            map.set(Pos, Cost, !Map)
        else
            false
        )
    else
        map.set(Pos, Cost, !Map)
    ).

:- pred dijkstra(array2d(char)::in, pqueue(int, list({int,int, int,int, list({int,int})}))::in, {int,int}::in,
    map({int,int,int,int},int)::in, map({int,int,int,int},int)::out, {set({int,int}), int}::out) is nondet.
dijkstra(Array, Queue0, {EndRow, EndCol}, !PathCost, {ResultVisited, ResultCost}) :-
    some [!Queue] (
    !:Queue = Queue0,
    pqueue.remove(Cost, Val, !Queue),
    [{DRow, DCol, Row, Col, Visited} | _] = Val,
    Visited1 = [{Row, Col} | Visited],
    ( if Row = EndRow, Col = EndCol then
        ( ResultVisited = set.from_list(Visited1),
            ResultCost = Cost,
            true
        ;
            dijkstra(Array, !.Queue, {EndRow, EndCol}, !PathCost, {ResultVisited, ResultCost})
        )
    else
        ( if Array^elem(Row + DRow, Col + DCol) \= '#',
            set_min(!PathCost, {DRow, DCol, Row + DRow, Col + DCol}, Cost + 1)
        then
            pqueue.insert(Cost + 1, [{DRow, DCol, Row + DRow, Col + DCol, Visited1} | Val], !Queue)
        else
            true
        ),
        ( if rotate({DRow, DCol}, {RRow, RCol}),
            Array^elem(Row + RRow, Col + RCol) \= '#',
            set_min(!PathCost, {RRow, RCol, Row, Col}, Cost + 1000)
        then
            pqueue.insert(Cost + 1000, [{RRow, RCol, Row, Col, Visited1} | Val], !Queue)
        else
            true
        ),
        ( if rotate({DRow, DCol}, A),
            rotate(A, B),
            rotate(B, {RRRow, RRCol}),
            Array^elem(Row + RRRow, Col + RRCol) \= '#',
            set_min(!PathCost, {RRRow, RRCol, Row, Col}, Cost + 1000)
        then
            pqueue.insert(Cost + 1000, [{RRRow, RRCol, Row, Col, Visited1} | Val], !Queue)
        else
            true
        ),
        dijkstra(Array, !.Queue, {EndRow, EndCol}, !PathCost, {ResultVisited, ResultCost})
    )
).

% gather results as long as Cost is same
:- pred no_longer({set({int,int}), int}::in, bool::out, maybe({set({int,int}), int})::in, maybe({set({int,int}), int})::out) is det.
% no_longer({[], Cost}, bool.no, _, maybe.no).
no_longer({Visited, Cost}, yes, no, yes({Visited, Cost})).
no_longer({Visited, Cost}, More, yes({Visited0, Cost0}), yes({Visited1, Cost1})) :-
    Cost1 = Cost0,
    ( if Cost = Cost0 then
        Visited1 = set.union(Visited, Visited0),
        More = yes
    else
        Visited1 = Visited0,
        More = no
    ).

main(!IO) :-
    read_lines(Rows, [], !IO),
    Array = array2d.from_lists(Rows),
    ( if array_find(Array, 'S', 0, 0, StartRow, StartCol),
        array_find(Array, 'E', 0, 0, EndRow, EndCol) then
            pqueue.insert(0, [{0,1, StartRow,StartCol, []}], pqueue.init, Queue),
            do_while(dijkstra(Array, Queue, {EndRow, EndCol}, map.init, _), no_longer, maybe.no, Result),
            ( if Result = yes({Visited, Cost}) then
                io.print_line(Cost, !IO),                  % 135512
                io.print_line(set.count(Visited)+0, !IO)   % 541
            else
                error("no way")
            )
    else
        error("parse error")
    ).
