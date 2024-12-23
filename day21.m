:- module day21.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array2d, char, int, list, set, solutions, string, queue, require.

:- pred numpad(char::in, char::out, char::out) is nondet.
numpad('0', 'R', 'A').
numpad('0', 'U', '2').

numpad('A', 'L', '0').
numpad('A', 'U', '3').

numpad('1', 'U', '4').
numpad('1', 'R', '2').

numpad('2', 'D', '0').
numpad('2', 'L', '1').
numpad('2', 'R', '3').
numpad('2', 'U', '5').

numpad('3', 'D', 'A').
numpad('3', 'L', '2').
numpad('3', 'U', '6').

numpad('4', 'D', '1').
numpad('4', 'R', '5').
numpad('4', 'U', '7').

numpad('5', 'D', '2').
numpad('5', 'L', '4').
numpad('5', 'R', '6').
numpad('5', 'U', '8').

numpad('6', 'D', '3').
numpad('6', 'L', '5').
numpad('6', 'U', '9').

numpad('7', 'D', '4').
numpad('7', 'R', '8').

numpad('8', 'D', '5').
numpad('8', 'L', '7').
numpad('8', 'R', '9').

numpad('9', 'D', '6').
numpad('9', 'L', '8').

:- pred dirpad(char::in, char::out, char::out) is nondet.

dirpad('L', 'R', 'D').

dirpad('D', 'L', 'L').
dirpad('D', 'R', 'R').
dirpad('D', 'U', 'U').

dirpad('R', 'L', 'D').
dirpad('R', 'U', 'A').

dirpad('U', 'D', 'D').
dirpad('U', 'R', 'A').

dirpad('A', 'D', 'R').
dirpad('A', 'L', 'U').

:- pred path(pred(char,char,char)::in(pred(in,out,out) is nondet), char::in, char::out, list(char)::in, list(char)::in, list(char)::out) is nondet.
path(_, To, To, _, Path, reverse(['A' | Path])).
path(Pad, From, To, Seen, !Path) :-
    Pad(From, Dir, To0),
    not member(To0, Seen),
    cons(Dir, !Path),
    path(Pad, To0, To, [To0 | Seen], !Path).

:- pred shortest_paths(pred(char,char,char)::in(pred(in,out,out) is nondet), char::in, char::in, list(list(char))::out).
shortest_paths(Pad, From, To, ShortestPaths) :-
    solutions(path(Pad, From, To, [], []), Paths),
    sort(pred(X::in, Y::in, R::out) is det :-
        compare(R, length(X)+0, length(Y)+0),
        Paths, SortedPaths),
    take_while(pred(X::in) is semidet :-
        length(X)+0 = length(det_head(SortedPaths)),
        SortedPaths, ShortestPaths).

:- func minimum(list(int)) = int is det.
minimum(L) =
    ( if [X] = L then
        X
    else if [X | Xs] = L then
        X `min` minimum(Xs)
    else
        -1
    ).

:- func dfs(int, int, list(char)) = int is det.
:- pragma memo(func(dfs/3)).  % the key to part 2
dfs(Level, I, Seq) = N :-
    Pad = ( if I = 0 then numpad else dirpad ),
    ( if split_last(Seq, Seq0, _) then
        list.map_corresponding(pred(U::in, V::in, L::out) is det :- (
            shortest_paths(Pad, U, V, ShortestPaths),
            L = ( if Level = 0 then
                length(det_head(ShortestPaths))
            else
                minimum(list.map(dfs(Level - 1, 1), ShortestPaths))
            )),
            [ 'A' | Seq0 ],
            Seq,
            Ns),
        N = list.foldl(int.plus, Ns, 0)
    else
        N = 0
    ).

:- func solve(list(string), int) = int.
solve(Codes, X) = list.foldl(int.plus,
    map(func(Code) = dfs(X, 0, string.to_char_list(Code)) *
        string.det_to_int(string.left(Code, 3)),
        Codes),
    0).

main(!IO) :-
    Codes = ["780A", "539A", "341A", "189A", "682A"],
    io.print_line(solve(Codes, 2), !IO),   % 174124
    io.print_line(solve(Codes, 25), !IO).  % 216668579770346
