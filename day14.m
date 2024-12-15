:- module day14.
:- interface.

:- import_module int, io, list.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module parsing_utils, require, set.

:- pred parse_line(src::in, {{int,int},{int,int}}::out, ps::in, ps::out) is semidet.
parse_line(Src, {{PX, PY}, {VX, VY}}) -->
    punct("p=", Src, _),
    int_literal(Src, PX),
    punct(",", Src, _),
    int_literal(Src, PY),

    punct("v=", Src, _),
    int_literal(Src, VX),
    punct(",", Src, _),
    int_literal(Src, VY).

:- pred parse(string::in, list({{int,int},{int,int}})::out) is semidet.
parse(Input, Result) :-
    parsing_utils.new_src_and_ps(Input, Src, PS),
    parsing_utils.one_or_more(parse_line, Src, Result, PS, _).

:- func move({{int,int},{int,int}}) = {{int,int},{int,int}}.
move({{PX,PY}, {VX,VY}}) = {{NX,NY}, {VX, VY}} :-
    NX = (PX + VX) mod 101,
    NY = (PY + VY) mod 103.

:- func part1(list({{int,int},{int,int}})) = int.
part1(Robots) =
    list.foldr(int.times,
        [ list.length(list.filter(pred({{PX,PY}, _}::in) is semidet :- (PX < 50, PY < 51), Robots)),
        list.length(list.filter(pred({{PX,PY}, _}::in) is semidet :- (PX < 50, PY > 51), Robots)),
        list.length(list.filter(pred({{PX,PY}, _}::in) is semidet :- (PX > 50, PY < 51), Robots)),
        list.length(list.filter(pred({{PX,PY}, _}::in) is semidet :- (PX > 50, PY > 51), Robots)) ],
        1).

:- pred has_blob_at(set({int,int})::in, int::in, int::in).
has_blob_at(Robots, Row, Col) :-
    all_true(pred(DRow::in) is semidet :-
        all_true(pred(DCol::in) is semidet :-
            set.member({Row + DRow, Col + DCol}, Robots),
            0..2),
        0..2).

:- pred has_blob(set({int,int})::in).
has_blob(Robots) :-
    any_true(pred(Row::in) is semidet :-
        any_true(pred(Col::in) is semidet :-
            has_blob_at(Robots, Row, Col),
            0..100),
        0..100).

:- func position_set(list({{int, int}, {int,int}})) = set({int,int}).
position_set(Robots) =
    set.from_list(list.map(func({{PX,PY}, _}) = {PX,PY}, Robots)).

:- func part2(list({{int, int}, {int,int}})) = int.
part2(Robots) = Result :-
    ( if has_blob(position_set(Robots)) then
        Result = 0
    else
        Result = 1 + part2(list.map(move, Robots))
    ).

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
            io.print_line(Input, !IO),
            io.print_line(move({{2,4},{2,-3}}), !IO),
            After = list.map(int.fold_up(func(_,X) = move(X), 1, 100), Input),
            io.print_line(part1(After), !IO),    % 230686500
            io.print_line(part2(Input), !IO)     % 7672   4min
        ;
            error("parse error")
        )
    ).
