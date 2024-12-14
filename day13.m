:- module day13.
:- interface.

:- import_module int, io, list.

:- pred main(io::di, io::uo) is det.
:- func part1(list({{int,int},{int,int},{int,int}})) = int.
:- func part2(list({{int,int},{int,int},{int,int}})) = int.

:- implementation.

:- import_module parsing_utils, require.

:- pred parse_game(src::in, {{int,int},{int,int},{int,int}}::out, ps::in, ps::out) is semidet.
parse_game(Src, {{AX, AY}, {BX, BY}, {PX, PY}}) -->
    punct("Button A: X+", Src, _),
    int_literal(Src, AX),
    punct(", Y+", Src, _),
    int_literal(Src, AY),

    punct("Button B: X+", Src, _),
    int_literal(Src, BX),
    punct(", Y+", Src, _),
    int_literal(Src, BY),

    punct("Prize: X=", Src, _),
    int_literal(Src, PX),
    punct(", Y=", Src, _),
    int_literal(Src, PY).

:- pred parse(string::in, list({{int,int},{int,int},{int,int}})::out) is semidet.
parse(Input, Result) :-
    parsing_utils.new_src_and_ps(Input, Src, PS),
    parsing_utils.one_or_more(parse_game, Src, Result, PS, _).

:- func solve({{int,int},{int,int},{int,int}}) = int is semidet.
solve({{AX, AY}, {BX, BY}, {PX, PY}}) = Result :-
    Det = AX * BY - AY * BX,
    not Det = 0,
    NA = PX * BY - PY * BX,
    NB = PY * AX - PX * AY,
    NA mod Det = 0,
    NB mod Det = 0,
    A = NA / Det,
    B = NB / Det,
    A >= 0,
    B >= 0,
    Result = 3 * A + B.

part1(Input) =
    list.foldr(int.plus, list.filter_map(solve, Input), 0).

part2(Input) =
    list.foldr(int.plus, list.filter_map(func({{AX,AY},{BX,BY},{PX,PY}}) =
        solve({{AX,AY},{BX,BY},{PX + 10000000000000, PY + 10000000000000}}) is semidet, Input), 0).

main(!IO) :-
    io.read_file_as_string(ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok(File),
        (
            parse(File, Input)
        ->
            io.print_line(part1(Input), !IO),    % 26599
            io.print_line(part2(Input), !IO)     % 106228669504887
        ;
            error("parse error")
        )
    ).
