:- module day05.
:- interface.

:- import_module list, int, io, pair, set.

:- pred main(io::di, io::uo) is det.
:- func part1(set(pair(int)), list(list(int))) = int.
:- func part2(set(pair(int)), list(list(int))) = int.

:- implementation.

:- import_module bool, string, parsing_utils, maybe, require, unit.

:- pred parse_ordering_rule(src::in, pair(int)::out, ps::in, ps::out) is semidet.
parse_ordering_rule(Src, Result) -->
    int_literal(Src, A),
    punct("|", Src, _),
    int_literal(Src, B),
    punct("\n", Src, _),
    { Result = pair(A,B) }.

:- pred parse_update(src::in, list(int)::out, ps::in, ps::out) is semidet.
parse_update(Src, Result) -->
    comma_separated_list(int_literal, Src, Result),
    punct("\n", Src, _).

:- pragma no_determinism_warning(pred(no_ws/4)).
:- pred no_ws(src::in, unit::out, ps::in, ps::out) is semidet.
no_ws(_Src, unit, PS, PS).

:- pred parse(string::in, set(pair(int))::out, list(list(int))::out) is semidet.
parse(Input, OrderingRules, Updates) :-
    some [!PS] (
        parsing_utils.new_src_and_ps(Input, no_ws, Src, !:PS),
        parsing_utils.one_or_more(parse_ordering_rule, Src, OrderingRulesList, !PS),
        parsing_utils.punct("\n", Src, _, !PS),
        parsing_utils.one_or_more(parse_update, Src, Updates, !PS),
        parsing_utils.eof(Src, _, !.PS, _),
        OrderingRules = set.from_list(OrderingRulesList)
    ).

:- pred violation(set(pair(int))::in, list(int)::in) is semidet.
 violation(OrderingRules, List) :-
    member_index0(A, List, IA),
    member_index0(B, List, IB),
    IA < IB,
    set.member(pair(B, A), OrderingRules).
% violation(OrderingRules, List) :-
%     not List = fix_order(OrderingRules, List).

:- func middle(list(T)) = T is det.
middle(List) = det_index0(List, length(List) / 2).

:- func fix_order(set(pair(int)), list(int)) = list(int) is det.
fix_order(OrderingRules, List) =
    sort(func(A, B) = (
            if A = B then
                (=)
            else if set.member(pair(A,B), OrderingRules) then
                (<)
            else
                (>) ),
         List).

part1(OrderingRules, Updates) =
    list.foldl(int.plus,
        list.map(middle,
            list.negated_filter(violation(OrderingRules), Updates)),
        0).

part2(OrderingRules, Updates) =
    list.foldl(int.plus,
        list.map(middle,
            list.map(fix_order(OrderingRules),
                list.filter(violation(OrderingRules), Updates))),
        0).

%% could be done in one pass, depending on whether fix_order changes anyting.

main(!IO) :-
    io.read_file_as_string(ReadResult, !IO),
    (
        ReadResult = error(_, _),
        error("fatal")
    ;
        ReadResult = ok(File),
        (
            parse(File, OrderingRules `with_type` set(pair(int)), Updates)
        ->
            io.print_line(part1(OrderingRules, Updates), !IO),   % 4996
            io.print_line(part2(OrderingRules, Updates), !IO)    % 6311
        ;
            error("parse error")
        )
    ).
