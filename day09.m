:- module day09.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array, char, list, int, pair, require, set, solutions, unit.

:- pred expand_map(list(char)::in, list(int)::out) is det.
expand_map(List, Result) :-
    {_, Final} = list.foldl(func(Digit, {Counter, Blocks}) =
        ( if Counter mod 2 = 0 then
            {Counter+1, [ list.duplicate(det_decimal_digit_to_int(Digit), Counter / 2) | Blocks ]}
        else
            {Counter+1, [ list.duplicate(det_decimal_digit_to_int(Digit), -1) | Blocks ]}
        ),
            List,
            {0, []}),
    Result = list.condense(list.reverse(Final)).

:- pred defrag(int::in, int::in, array(int)::in, array(int)::out) is det.
defrag(Pos, End, !Blocks) :-
    unsafe_promise_unique(!Blocks),
%    trace [io(!IO)] io.print_line(!.Blocks, !IO),
    ( if Pos >= End then
        true
    else if !.Blocks^elem(End) = -1 then
        defrag(Pos, End - 1, !Blocks)
    else if !.Blocks^elem(Pos) = -1 then
        swap(Pos, End, !Blocks),
        defrag(Pos + 1, End - 1, !Blocks)
    else
        defrag(Pos + 1, End, !Blocks)
    ).

:- pred checksum(array(int)::in, int::in, int::in, int::out).
checksum(Blocks, Pos, SumIn, SumOut) :-
    if not in_bounds(Blocks, Pos) then
        SumOut = SumIn
    else if Blocks^elem(Pos) = -1 then
        checksum(Blocks, Pos + 1, SumIn, SumOut)
    else
        checksum(Blocks, Pos + 1, SumIn + Pos * Blocks^elem(Pos), SumOut).

:- type area ---> data(int, int) ; free(int).

:- pred expand_map2(list(char)::in, list(area)::out) is det.
expand_map2(List, Areas) :-
    {_, Areas0} = list.foldl(func(Digit, {Counter, Areas1}) =
        ( if Counter mod 2 = 0 then
            {Counter+1, [ data(det_decimal_digit_to_int(Digit), Counter / 2) | Areas1 ]}
        else
            {Counter+1, [ free(det_decimal_digit_to_int(Digit)) | Areas1 ]}
        ),
        List,
        {0, []}),
    Areas = list.reverse(Areas0).

:- pred split_when(pred(T)::in(pred(in) is semidet), list(T)::in, list(T)::in, list(T)::out, T::out, list(T)::out) is semidet.
split_when(_, [], _, _, _, _) :- fail.
split_when(P, [X | Xs], Front0, Front, Found, Rear) :-
    if P(X) then
        Front = list.reverse(Front0),
        Found = X,
        Rear = Xs
    else
        split_when(P, Xs, [X | Front0], Front, Found, Rear).
        
:- pred defrag2(int::in, list(area)::in, list(area)::out) is semidet.
defrag2(Idx, !Areas) :-
    ( if Idx < 0 then
        true
    else
        split_when(pred(data(_, Idx2)::in) is semidet :- Idx = Idx2, !.Areas, [], A1, Data, A2),
        Data = data(Len, Idx),
        ( if split_when(pred(free(X)::in) is semidet :- X >= Len, A1, [], B1, Free, B2) then
            Free = free(FreeLen),
            !:Areas = B1 ++ [ Data, free(FreeLen - Len) ] ++ B2 ++ [ free(Len) ] ++ A2
        else
            true
        ),    
        defrag2(Idx - 1, !Areas)
    ).

:- func checksum2(list(area)) = {int,int}.
checksum2(Areas) =
    list.foldl(func(Area, {Sum, Index}) =
        ( if Area = data(Len, Idx) then
            {Sum + Idx * list.foldl(int.plus, Index..(Index+Len-1), 0), Index + Len}
        else if Area = free(Len) then
            {Sum, Index + Len}
        else
            {Sum, Index}
        ),
        Areas,
        {0, 0}).

main(!IO) :-
    io.read_line(ReadResult, !IO),
    (
        ReadResult = error(_),
        error("fatal")
    ;
        ReadResult = eof,
        error("fatal")
    ;
        ReadResult = ok(Input),
        ( list.split_last(Input, Line, '\n')
        ->
            expand_map(Line, Blocks),
            defrag(0, length(Blocks)-1, array.from_list(Blocks), Blocks2),
            checksum(Blocks2, 0, 0, Part1),
            io.print_line(Part1, !IO),   % 6259790630969
            
            expand_map2(Line, Areas),
            ( list.last(Areas, data(_, MaxId)),
                defrag2(MaxId, Areas, AreasOut),
                {Part2, _} = checksum2(AreasOut)
            ->
                io.print_line(Part2, !IO)   % 6289564433984
            ;
                true
            )
        ;
            true
        )
    ).
