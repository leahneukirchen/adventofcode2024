:- module day23.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char, int, list, set, solutions, string, require.

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

:- pred grind(list(string)::in, set({string,string})::in, set(string)::out) is nondet.
grind(Computers, Connections, Triple) :-
    set.to_sorted_list(Connections, ConnectionsList),
    list.member({A,B}, ConnectionsList),
    prefix(A, "t"),
    list.member(C, Computers),

    set.contains(Connections, {C,B}),
    set.contains(Connections, {C,A}),

    Triple = set.from_list([A,B,C]).

:- func part1(list(string), set({string,string})) = int.
part1(Computers, Connections) = N :-
    solutions(grind(Computers, Connections), Solutions),
    N = list.length(Solutions).

:- func part2(list(string), set({string,string})) = string.
part2(Computers, Connections) = Result :-
    Networks = list.map(func(N) = (
        list.foldl(func(C, N0) =
            ( if set.all_true(
                pred(D::in) is semidet :- set.contains(Connections, {C,D}),
                N0)
            then
                set.insert(N0, C)
            else
                N0
            ), Computers, set.from_list([N]))
        ), Computers),
    list.sort(pred(X::in, Y::in, R::out) is det :- compare(R, set.count(Y)+0, set.count(X)+0), Networks, NetworksBySize),
    Result = join_list(",", set.to_sorted_list(det_head(NetworksBySize))).

main(!IO) :-
    read_lines(Lines, [], !IO),
    ( if list.foldl2(pred([X, Y]::in, !.Computers::in, !:Computers::out,
            !.Connections::in, !:Connections::out) is semidet :- (
                set.insert(X, !Computers),
                set.insert(Y, !Computers),
                set.insert({X,Y}, !Connections),
                set.insert({Y,X}, !Connections)
            ),
            list.map(split_at_string("-"), Lines),
            set.init, Computers, set.init, Connections) then
        io.print_line(part1(set.to_sorted_list(Computers), Connections), !IO),
        % 1230
        io.print_line(part2(set.to_sorted_list(Computers), Connections), !IO)
        % az,cj,kp,lm,lt,nj,rf,rx,sn,ty,ui,wp,zo
    else
        error("parse error")
    ).
