:- module day08.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module char, list, int, multi_map, require, set, solutions, unit.

:- pred scanner(char::in, {int,int,multi_map(char,{int,int})}::in,
    {int,int,multi_map(char,{int,int})}::out) is cc_multi.
scanner('\n', {Row,_  ,Data}, {Row+1,0,Data}).
scanner('.',  {Row,Col,Data}, {Row,Col+1,Data}).
scanner(X,    {Row,Col,Data}, {Row,Col+1,multi_map.add(Data,X,{Row,Col})}).

:- pred solve(list(int)::in, multi_map(char, {int,int})::in, {int,int}::out) is nondet.
solve(Range, Data, Result) :-
    multi_map.member(Data, Antenna, _),
    multi_map.search(Data, Antenna, Positions),
    member_index0({RowA,ColA}, Positions, IA),
    member_index0({RowB,ColB}, Positions, IB),
    IA < IB,
    list.member(Multiplier, Range),
    R = RowA + Multiplier * (RowB - RowA),
    C = ColA + Multiplier * (ColB - ColA),
    0 =< R, R < 50,             % XXX hardcoded
    0 =< C, C < 50,
    Result = {R, C}.

main(!IO) :-
    io.input_stream_foldl(scanner, {0,0,multi_map.init}, ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok({_,_,Input}),
        (
            solutions_set(solve([-1, 2], Input), Part1),
            io.print_line(set.count(Part1) `with_type` int, !IO),  % 336
            solutions_set(solve(-50..50, Input), Part2),
            io.print_line(set.count(Part2) `with_type` int, !IO)   % 1131
        ;
            error("parse error")
        )
    ).
