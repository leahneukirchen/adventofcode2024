:- module day22.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array2d, char, int, list, set, solutions, string, queue, require, std_util, map.

:- pred read_lines(list(string)::out, list(string)::in, io::di, io::uo) is det.
read_lines(LinesOut, LinesIn, !IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(String),
        read_lines(LinesOut, [chomp(String) | LinesIn], !IO)
    ;
        Result = error(_),
        LinesOut = []
    ;
        Result = eof,
        LinesOut = list.reverse(LinesIn)
    ).        

:- func step(int) = int.
step(!.X) = !:X :-
    !:X = ((!.X * 64) `xor` !.X) `mod` 16777216,
    !:X = ((!.X / 32) `xor` !.X) `mod` 16777216,
    !:X = ((!.X * 2048) `xor` !.X) `mod` 16777216.

:- func part1(list(int)) = int.
part1(Input) =
    list.foldr(int.plus, map(pow(step, 2000), Input), 0).

:- func maximum(list(int)) = int is det.
maximum(L) =
    ( if [X] = L then
        X
    else if [X | Xs] = L then
        X `max` maximum(Xs)
    else
        -1
    ).

:- func part2(list(int)) = int.
part2(Input) = maximum(map.values(Map)) :-
    list.foldl(pred(S::in, !.Map::in, !:Map::out) is det :- (
        list.foldl4(pred(_Count::in,
                         !.S::in, !:S::out, {_,B0,C0,D0}::in, {A,B,C,D}::out,
                         !.Map0::in, !:Map0::out, !.Seen::in, !:Seen::out)
        is det :- (
            Prev = !.S `mod` 10,
            !:S = step(!.S),
%           trace [io(!IO)] io.print_line(!.S, !IO),
            E = (!.S `mod` 10) - Prev,
            {A,B,C,D} = {B0,C0,D0,E},
%           trace [io(!IO)] io.print_line({A,B,C,D}, !IO),
            ( if set.contains(!.Seen, {A,B,C,D}) then
                true
            else
                set.insert({A,B,C,D}, !Seen),
                ( if map.search(!.Map0, {A,B,C,D}, Old) then
                    map.set({A,B,C,D}, Old + (!.S `mod` 10), !Map0)
                else
                    map.set({A,B,C,D}, (!.S `mod` 10), !Map0)
                )
            )),
            1..2000,
            S, _, {10,10,10,10}, _, !Map, set.init, _)
        ), Input, map.init, Map).
    
main(!IO) :-
    read_lines(Lines, [], !IO),
    list.filter_map(string.to_int, Lines, Input),
    io.print_line(part1(Input), !IO),    % 20215960478
    io.print_line(part2(Input), !IO).    % 2221
