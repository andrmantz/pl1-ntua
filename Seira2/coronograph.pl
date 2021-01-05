:-dynamic(edge/2).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).


makeGraph(Stream, M) :-
    M > 0 ->
    read_line(Stream, [Node1, Node2]),
    assert(edge(Node1, Node2)),
    assert(edge(Node2, Node1)),
    J is M - 1,
    makeGraph(Stream, J)
    ;!.  

path(A,B,Path) :-  %COPYRIGHTS cpp.edu : https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15.html
       travel(A,B,[A],Q, A), 
       reverse(Q,Path).

travel(A,B,P,P, Parent) :-
        B\== Parent,
        edge(A,B).
travel(A,B,Visited,Path, Parent) :-
       edge(A,C),            
       C \== B, C\== Parent,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path, A).

cycle(X, List) :- 
    once(path(X,X,List)). 

count([], _, Results, Answers) :- msort(Results, Answers).
count([H|T], Cyclelist, Results, Answers) :-
    getpath(H, _,Cyclelist, Counter),
    NewRes = [Counter|Results],
    count(T, Cyclelist, NewRes, Answers).




%path2 Copyrights: https://stackoverflow.com/questions/10797793/how-to-visit-each-point-in-directed-graph
path2(Start, End, Cyclelist) :-
    path2(Start, End, [Start], Cyclelist).

% when target reached, reverse the visited list
path2(End, End, _, _).

% take non deterministically an edge, check if already visited before use
path2(Start, End, Visited,Cyclelist) :-
    edge(Start, Next), 
    \+ memberchk(Next, Visited), \+memberchk(Next, Cyclelist),
    path2(Next, End, [Next|Visited], Cyclelist).        

getpath(Start, End, Cyclelist, Count) :- %Get all nodes a cycle node can reach, without visiting a cycle-node.
    findall(End, path2(Start, End, Cyclelist), List),
    length(List, Count).




runTtimes(Stream, T, Results, Final) :- 
    (T > 0 ->
    retractall(edge(_,_)),
    read_line(Stream, [N,M]),
    makeGraph(Stream, M),
    cycle(_, Cyclelist),
    count(Cyclelist,Cyclelist,[],  Answer),
    makeList(Answer, N , M, Cyclelist, Results, NewResults),
    J is T - 1,
    runTtimes(Stream, J, NewResults, Final)
    ; Final = Results).

    
makeList(AnswersList, N,M, Cyclelist, Results, NewResults) :-
    checkNoCorona(N,M,0) -> append(Results, ['\'NO CORONA\''], NewResults)
    ; length(Cyclelist, X),
    Y = [X, AnswersList],
    append(Results, [Y], NewResults).



coronograph(File, Answer) :-
    open(File, read, Stream),
    read_line(Stream, [T]),
    runTtimes(Stream, T, [], Answer),
    retractall(edge(_,_)),
    close(Stream).    


zzz(C) :- %Check if graph is connected
    findall(T, path(1,T, _), L),
    sort(L,Ans),
    length(Ans, C).

checkNoCorona(N, M, Bool) :-
    N\==M  -> Bool is 0
    ; \+zzz(N) -> Bool is 0
    ; Bool is 1.       