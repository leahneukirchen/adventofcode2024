:- module day11.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, string.

:- pred halve(int::in, int::out, int::out) is semidet.
halve(Number, Left, Right) :-
    string.int_to_string(Number, Str),
    string.length(Str) mod 2 = 0,
    string.split(Str, string.length(Str) / 2, LeftStr, RightStr),
    string.to_int(LeftStr, Left),
    string.to_int(RightStr, Right).

:- func blink(int, int) = int.
:- pragma memo(func(blink/2)).  % the key to part 2
blink(Step, Stone) =
    ( if Step = 0 then
        1
    else if Stone = 0 then
        blink(Step - 1, 1)
    else if halve(Stone, Left, Right) then
        blink(Step - 1, Left) + blink(Step - 1, Right)
    else
        blink(Step - 1, Stone * 2024)
    ).

main(!IO) :-
%   Input = [125, 17],
    Input = [0, 37551, 469, 63, 1, 791606, 2065, 9983586],

    Part1 = list.foldl(int.plus, list.map(blink(25), Input), 0),
    io.print_line(Part1, !IO),   % 204022

    Part2 = list.foldl(int.plus, list.map(blink(75), Input), 0),
    io.print_line(Part2, !IO).   % 241651071960597
