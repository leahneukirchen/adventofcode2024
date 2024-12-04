:- module day03.
:- interface.

:- import_module list, int, io.

:- pred main(io::di, io::uo) is det.
:- func part1(list(instr)) = int.
:- func part2(list(instr)) = int.

:- type instr
    ---> mul(int,int)
    ;    do
    ;    dont.

:- implementation.

:- import_module bool, string, parsing_utils, maybe, require.

:- pred parse_mul(src::in, instr::out, ps::in, ps::out) is semidet.
parse_mul(Src, Result) -->
    punct("mul(", Src, _),
    int_literal(Src, N1),
    punct(",", Src, _),
    int_literal(Src, N2),
    punct(")", Src, _),
    { Result = mul(N1, N2) }.

:- pred parse_mul_then_skip(src::in, maybe(instr)::out, ps::in, ps::out) is semidet.
parse_mul_then_skip(Src, Result) -->
    if parse_mul(Src, Result0) then
        { Result = yes(Result0) }
    else if punct("don't()", Src, _) then
        { Result = yes(dont) }
    else if punct("do()", Src, _) then
        { Result = yes(do) }
    else
        next_char(Src, _),
        { Result = no }.

:- pred parse(string::in, list(instr)::out) is semidet.
parse(Input, Result) :-
    parsing_utils.new_src_and_ps(Input, Src, PS),
    parsing_utils.one_or_more(parse_mul_then_skip, Src, MaybeResult, PS, _),
    list.filter_map(maybe.maybe_is_yes, MaybeResult, Result).

part1(Instructions) = list.foldl(func(Inst, Sum) =
    ( if Inst = mul(N1, N2) then
        Sum + N1*N2
    else
        Sum ),
    Instructions, 0).

:- type state ---> state(enabled::bool, sum::int).

part2(Numbers) = list.foldl(func(Inst, State) =
    ( if (Inst = mul(N1, N2), State^enabled = yes) then
        State^sum := State^sum + N1*N2
    else if Inst = do then
        State^enabled := yes
    else if Inst = dont then
        State^enabled := no
    else
        State),
    Numbers, state(yes, 0)) ^ sum.

main(!IO) :-
    io.read_file_as_string(ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok(File),
        (
            parse(File, Result)
        ->
            io.print_line(part1(Result), !IO),   % 170778545
            io.print_line(part2(Result), !IO)    % 82868252
        ;
            error("parse error")
        )
    ).
