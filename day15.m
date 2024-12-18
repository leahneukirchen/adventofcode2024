:- module day15.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array2d, char, int, list, parsing_utils, require, set, string.

:- pred move(char::in, {int, int}::in, {int, int}::out, array2d(char)::di, array2d(char)::uo) is det.
move(Step, {!.Row, !.Col}, {!:Row, !:Col}, !Array) :-
%    trace [io(!IO)] io.print_line({"step",Step,Row,Col}, !IO),
    ( if Step = char.det_from_int(62) then
        NRow = !.Row, NCol = !.Col + 1
    else if Step = char.det_from_int(60) then
        NRow = !.Row, NCol = !.Col - 1
    else if Step = char.det_from_int(94) then
        NRow = !.Row - 1, NCol = !.Col
    else if Step = 'v' then
        NRow = !.Row + 1, NCol = !.Col
    else
        error("invalid")
    ),
%    trace [io(!IO)] io.print_line({"step",Step,Row,Col,NRow, NCol,!.Array^elem(NRow, NCol)}, !IO),
%    array2d.in_bounds(!.Array, NRow, NCol),
    ( if !.Array^elem(NRow, NCol) = char.det_from_int(46) then
        Old = !.Array^elem(!.Row, !.Col),
        array2d.set(NRow, NCol, Old, !Array),
        unsafe_promise_unique(!Array),
        array2d.set(!.Row, !.Col, char.det_from_int(46), !Array),
%        trace [io(!IO)] io.print_line(!.Array, !IO),
        unsafe_promise_unique(!Array),
        !:Row = NRow,
        !:Col = NCol
    else if !.Array^elem(NRow, NCol) = 'O' then
        unsafe_promise_unique(!Array),
        move(Step, {NRow, NCol}, _, !Array),
        ( if !.Array^elem(NRow, NCol) = 'O' then
            true
        else
            move(Step, {!.Row, !.Col}, {!:Row, !:Col}, !Array)
        )
    else
        true                    % wall
    ),
    unsafe_promise_unique(!Array).

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

:- pred part1(string::in, array2d(char)::di, array2d(char)::uo) is semidet.
part1(Steps, !Array) :-
    array_find(!.Array, char.det_from_int(64), 0, 0, Row, Col),
    unsafe_promise_unique(!Array),
    string.foldl2(move, Steps, {Row, Col}, _, !Array).

:- func score1(array2d(char)) = int.
score1(Array) = Total :-
    array2d.bounds(Array, MaxRow, MaxCol),
    Total = int.fold_up(func(Row, Sum) =
        Sum + int.fold_up(func(Col, Sum1) =
            Sum1 + ( if Array^elem(Row, Col) = 'O' then
                Col + 100 * Row
            else
                0
            ),
                0, MaxCol-1, 0),
            0, MaxRow - 1, 0).

main(!IO) :-
    io.read_file_as_string(ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok(Input),
        ( if [Map, StepLines] = string.split_at_string("\n\n", Input),
            Array = array2d.from_lists(list.map(string.to_char_list,
                string.split_into_lines(Map))),
            Steps = string.replace_all(StepLines, "\n", "") `with_type` string
        then
            unsafe_promise_unique(Array, Array1),
            ( if part1(Steps, Array1, ArrayOut) then
%               <^^>>>vv<v>>v<<
%                io.print_line(ArrayOut, !IO),
                io.print_line(score1(ArrayOut), !IO)   % 1568399
            else
                true
            )  
        else
            true
        )
    ).
