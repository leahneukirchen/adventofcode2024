:- module day07.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, parsing_utils, require, unit.

:- pragma no_determinism_warning(pred(no_ws/4)).
:- pred no_ws(src::in, unit::out, ps::in, ps::out) is semidet.
no_ws(_Src, unit, PS, PS).

:- pred parse_equation(src::in, {int,list(int)}::out, ps::in, ps::out) is semidet.
parse_equation(Src, {Target, Numbers}) -->
    int_literal(Src, Target),
    punct(": ", Src, _),
    separated_list(" ", int_literal, Src, Numbers),
    punct("\n", Src, _).

:- pred parse(string::in, list({int,list(int)})::out) is semidet.
parse(Input, Equations) :-
    some [!PS] (
        parsing_utils.new_src_and_ps(Input, no_ws, Src, !:PS),
        parsing_utils.one_or_more(parse_equation, Src, Equations, !PS),
        parsing_utils.eof(Src, _, !.PS, _)
    ).

:- func log10(int) = int.
log10(X) = (if X < 10 then 0 else 1 + log10(X / 10)).

% Old forward solution:
%
% :- func concat(int, int, int) = int.
% concat(X, Y, Z) =
%     ( if Z = 0 then
%         X + Y
%     else
%         concat(X * 10, Y, Z div 10)
%     ).
%
% :- pred solve(int::in, list(int)::in, int::in, int::in).
% solve(_,      [      ], Value, Value).
% solve(Part,   [X | Xs], Value, Target) :- solve(Part, Xs, Value + X, Target).
% solve(Part,   [X | Xs], Value, Target) :- solve(Part, Xs, Value * X, Target).
% solve(Part@2, [X | Xs], Value, Target) :- solve(Part, Xs, concat(Value, X, X), Target).

% Backwards is more efficient:
:- pred solve(int::in, list(int)::in, int::in).
%solve(_,      [      ], 0).
solve(_,      [X], X).
solve(Part,   [X | Xs], Target) :- Target >= X, solve(Part, Xs, Target - X).
solve(Part,   [X | Xs], Target) :- Target mod X = 0, solve(Part, Xs, Target / X).
solve(Part@2, [X | Xs], Target) :-
    Zeroes = pow(10, log10(X) + 1),
    Target mod Zeroes = X,
    solve(Part, Xs, Target / Zeroes).

:- func solve(int, {int, list(int)}) = int is semidet.
solve(Part, {Target, Equation}) = Target :- solve(Part, reverse(Equation), Target).

:- func result(int, list.list({int, list(int)})) = int.
result(Part, Equations) =
    list.foldr(int.plus, list.filter_map(solve(Part), Equations), 0).

main(!IO) :-
    io.read_file_as_string(ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok(File),
        (
            parse(File, Equations)
        ->
            io.print_line(result(1, Equations), !IO),   % 6231007345478
            io.print_line(result(2, Equations), !IO)    % 333027885676693
        ;
            error("parse error")
        )
    ).
