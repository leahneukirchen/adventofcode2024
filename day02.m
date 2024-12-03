:- module day02.
:- interface.

:- import_module int, io, list.

:- pred main(io::di, io::uo) is det.
:- func part1(list(list(int))) = int.
:- func part2(list(list(int))) = int.

:- implementation.

:- import_module string, parsing_utils.

:- pred read_lines(list(string)::out, list(string)::in, io::di, io::uo) is det.

read_lines(LinesOut, LinesIn, !IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(String),
        read_lines(LinesOut, [String | LinesIn], !IO)
    ;
        Result = error(_),
        LinesOut = []
    ;
        Result = eof,
        LinesOut = list.reverse(LinesIn)
    ).        

:- pred parse(string::in, list(int)::out) is semidet.
parse(Line, Numbers) :-
    parsing_utils.new_src_and_ps(Line, Src, PS),
    parsing_utils.one_or_more(parsing_utils.int_literal, Src, Numbers, PS, _).

% a more efficient list.count(list.filter(...))
:- func count(pred(X)::in(pred(in) is semidet), list(X)::in) = (int::out) is det.
count(_, []) = 0.
count(Pred, [X | Xs]) = (if Pred(X) then 1 + count(Pred, Xs) else count(Pred, Xs)).

:- func scan(func(A, A) = A, list(A)) = list(A) is det.
scan(Func, List) =
    (if {_, Result} =
        list.foldl(func(E, {Last, All}) = {E, [ Func(E, Last) | All ]},
            list.tail(List),
            {list.head(List), []})
     then list.reverse(Result)
     else []).

:- pred safe(list(int)::in) is semidet.
safe(Numbers) :-
    Diffs = scan(int.minus, Numbers),
    all_true(>=(3), map(int.abs, Diffs)),
    (all_true(<(0), Diffs) ; all_true(>(0), Diffs)).

part1(Numbers) = count(safe, Numbers).

part2(Numbers) =
    count(pred(Ns::in) is semidet :-
        (list.delete(Ns, _, Ns1),
            safe(Ns1)),
        Numbers).

main(!IO) :-
    read_lines(Lines, [], !IO),
    list.filter_map(parse, Lines, Numbers),
    io.print_line(part1(Numbers), !IO),   % 314
    io.print_line(part2(Numbers), !IO).   % 373
