:- module day17.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array, char, int, list, solutions, require.

:- pred run(array(int)::in, int::in, int::out, int::in, int::out, int::in, int::out, int::in, int::out) is det.
run(Code, !IP, !A, !B, !C) :-
    ( if not array.in_bounds(Code, !.IP) then
        trace [io(!IO)] io.print_line("", !IO)
    else
        Op = Code^elem(!.IP),
        Literal = Code^elem(!.IP + 1),
        !:IP = !.IP + 2,
        Combo = ( if Literal =< 3 then Literal
        else if Literal = 4 then !.A
        else if Literal = 5 then !.B
        else if Literal = 6 then !.C
        else 0  % error("invalid combo")
        ),
        ( if Op = 0 then
            !:A = !.A / (2 `pow` Combo)
        else if Op = 1 then
            !:B = !.B `xor` Literal
        else if Op = 2 then
            !:B = Combo mod 8
        else if Op = 3 then
            ( if !.A = 0 then
                true
            else
                !:IP = Literal
            )
        else if Op = 4 then
            !:B = !.B `xor` !.C
        else if Op = 5 then
            trace [io(!IO)] io.print(Combo mod 8, !IO),
            trace [io(!IO)] io.print(",", !IO)
        else if Op = 6 then
            !:B = !.A / (2 `pow` Combo)
        else if Op = 7 then
            !:C = !.A / (2 `pow` Combo)
        else
            error("invalid opcode")
        ),
        run(Code, !IP, !A, !B, !C)
    ).

% manually transcribed
:- func step(int) = int.
step(A) = Result :-
    B = (A mod 8) `xor` 1,
    C = (A >> B),
    Result = (B `xor` 5 `xor` C) mod 8.

:- pred solve(list(int)::in, int::in, int::out) is nondet.
solve([], A, A).
solve([Digit | Rest], A, Result) :-
    nondet_int_in_range(0, 8, B),
    step(A*8 + B) = Digit,
    solve(Rest, A*8 + B, Result).

:- func part2(list(int)) = int.
part2(Program) = Result :-
    solutions(solve(reverse(Program), 0), Solutions),
    Result = list.foldl(int.min, Solutions, det_head(Solutions)).

main(!IO) :-
    A = 17323786,
    B = 0,
    C = 0,
    Program = [2,4, 1,1, 7,5, 1,5, 4,1, 5,5, 0,3, 3,0],

%    A = 729,
%    Program = [0,1,5,4,3,0],

    Code = array.from_list(Program),
    IP = 0,

    run(Code, IP, _, A, _, B, _, C, _),   % 7,4,2,5,1,4,6,0,4
    io.write_line(part2(Program), !IO).   % 164278764924605
