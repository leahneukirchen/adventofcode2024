:- module day18.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array, char, int, list, set, solutions, string, parsing_utils, queue, require.

:- pred parse_pos(src::in, {int,int}::out, ps::in, ps::out) is semidet.
parse_pos(Src, {Row, Col}) -->
    int_literal(Src, Col),
    punct(",", Src, _),
    int_literal(Src, Row).

:- pred parse(string::in, list({int,int})::out) is semidet.
parse(Input, Result) :-
    parsing_utils.new_src_and_ps(Input, Src, PS),
    parsing_utils.one_or_more(parse_pos, Src, Result, PS, _).

:- pred bfs(set({int,int})::in, queue({int,int,int})::in, queue({int,int,int})::out, set({int,int})::in, set({int,int})::out, {int,int}::in, int::out).
bfs(Corrupted, !Queue, !Seen, {EndRow, EndCol}, Result) :-
    ( if queue.get({Row, Col, Dist}, !Queue) then
        ( if Row = EndRow, Col = EndCol then
            Result = Dist
        else if set.contains(!.Seen, {Row, Col}) then
            bfs(Corrupted, !Queue, !Seen, {EndRow, EndCol}, Result)
        else
            set.insert({Row, Col}, !Seen),
            ( if Row > 0, not set.contains(Corrupted, {Row-1, Col}) then
                queue.put({Row-1, Col, Dist+1}, !Queue)
            else
                true ),
            ( if Col > 0, not set.contains(Corrupted, {Row, Col-1}) then
                queue.put({Row, Col-1, Dist+1}, !Queue)
            else
                true ),
            ( if Row < EndRow, not set.contains(Corrupted, {Row+1, Col}) then
                queue.put({Row+1, Col, Dist+1}, !Queue)
            else
                true ),
            ( if Col < EndCol, not set.contains(Corrupted, {Row, Col+1}) then
                queue.put({Row, Col+1, Dist+1}, !Queue)
            else
                true ),
            bfs(Corrupted, !Queue, !Seen, {EndRow, EndCol}, Result)
        )
    else
        Result = -1
    ).
    
:- func part1(list({int,int})) = int.
part1(Positions) = Result :-
    bfs(set.from_list(take_upto(1024, Positions)), queue.list_to_queue([{0,0,0}]) `with_type` queue(_), _, set.init, _, {70,70}, Result).

:- pred binary_search(pred(int, T)::in(pred(in, out) is semidet), int::in, int::in, T::out).
binary_search(P, Lo, Hi, Out) :-
    Lo =< Hi,
    Mid = Lo + ((Hi - Lo) `unchecked_right_shift` 1),
    ( if P(Mid, Out0) then
        ( if not P(Mid + 1, _) then
            Out = Out0
        else
            binary_search(P, Mid + 1, Hi, Out)
        )            
    else
        binary_search(P, Lo, Mid - 1, Out)
    ).

:- func part2(list({int,int})) = string.
part2(Positions) = Result :-
    ( if binary_search(pred(N::in, Result0::out) is semidet :- (
            bfs(set.from_list(take_upto(N, Positions)), queue.list_to_queue([{0,0,0}]) `with_type` queue(_), _, set.init, _, {70,70}, R),
            R > 0,
            list.index0(Positions, N, Result0)
        ),
        1024, length(Positions), {Row, Col}) then
        string.format("%d,%d", [i(Col), i(Row)], Result)
    else
        Result = "error"
    ).

main(!IO) :-
    io.read_file_as_string(ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok(File),
        (
            parse(File, Positions)
        ->
            io.print_line(part1(Positions), !IO),   % 374
            io.print_line(part2(Positions), !IO)    % 30,12
        ;
            error("parse error")
        )
    ).
