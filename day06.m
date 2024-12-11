:- module day06.
:- interface.

:- import_module array2d, char, int, io.

:- pred main(io::di, io::uo) is det.
:- pred part1(array2d(char)::in, int::out) is semidet.
:- pred part2(array2d(char)::in, {int,int}::out) is nondet.

:- implementation.

:- import_module bool, maybe, list, string, solutions, require.

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

% a more efficient list.count(list.filter(...))
:- func count(pred(X)::in(pred(in) is semidet), list(X)::in) = (int::out) is det.
count(_, []) = 0.
count(Pred, [X | Xs]) = (if Pred(X) then 1 + count(Pred, Xs) else count(Pred, Xs)).

:- pred rotate({int,int}::in, {int,int}::out).
rotate({-1,0}, {0, 1}).
rotate({0, 1}, {1, 0}).
rotate({1, 0}, {0,-1}).
rotate({0,-1}, {-1,0}).

:- pred step({int,int}::in, {int,int}::in, {int,int}::out).
step({DRow,DCol}, {Row,Col}, {NRow, NCol}) :-
    NRow = Row + DRow,
    NCol = Col + DCol.

:- pred walk(array2d(char)::in, bool::out,
    {int,int}::in, {int,int}::out,
    {int,int}::in, {int,int}::out,
    array2d(maybe({int,int}))::di, array2d(maybe({int,int}))::uo) is semidet.
walk(Array, Loop, !Position, !Direction, !Visited) :-
    {Row, Col} = !.Position,
    {DRow, DCol} = !.Direction,
    ( if !.Visited^elem(Row, Col) = yes(!.Direction) then
        unsafe_promise_unique(!Visited),
        Loop = yes
    else
        ( if !.Visited^elem(Row, Col) = no then
            array2d.set(Row, Col, yes(!.Direction), !Visited),
            unsafe_promise_unique(!Visited)
        else
            true
        ),
        ( if array2d.in_bounds(Array, Row+DRow, Col+DCol) then
            ( if Array^elem(Row+DRow, Col+DCol) = '#' then
                rotate(!Direction)
            else
                step(!.Direction, !Position)
            ),
            walk(Array, Loop, !Position, !Direction, !Visited)
        else
            Loop = no
        )
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

part1(Array, Result) :-
    array_find(Array, char.det_from_int(94), 0, 0, Row, Col),
    some [!Visited] (
        array2d.bounds(Array, MaxRow, MaxCol),
        !:Visited = array2d.init(MaxRow, MaxCol, maybe.no `with_type` maybe({int,int})),
        unsafe_promise_unique(!Visited),
        walk(Array, _Loop, {Row, Col}, _, {-1,0}, _, !Visited),
        Result = count(pred(X::in) is semidet :- X = yes(_),
            list.condense(array2d.lists(!.Visited)))
    ).

part2(Array0, Result) :-
    array_find(Array0, char.det_from_int(94), 0, 0, Row, Col),
    array2d.bounds(Array0, MaxRow, MaxCol),
    member(BlockRow, 0..MaxRow-1),
    member(BlockCol, 0..MaxCol-1),

    Array = array2d.from_lists(array2d.lists(Array0))^elem(BlockRow, BlockCol) := '#',

    Visited0 = array2d.init(MaxRow, MaxCol, no),
    unsafe_promise_unique(Visited0, Visited1),
    walk(Array, Loop, {Row, Col}, _, {-1,0}, _, Visited1, _),
    Loop = yes,
    Result = {BlockRow,BlockCol}.

main(!IO) :-
    read_lines(Rows, [], !IO),
    Array = array2d.from_lists(Rows),

    ( if part1(Array, Part1) then
        io.print_line(Part1, !IO)   % 4656
    else
        error("fatal")
    ),
    length(solutions(part2(Array)), Part2),
    io.print_line(Part2, !IO).   % 1575
