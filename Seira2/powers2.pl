dec2bin(0,[0]).  % https://stackoverflow.com/questions/20437673/prolog-putting-elements-in-a-list-for-a-decimal-to-binary-conversion
dec2bin(1,[1]).
dec2bin(N,L):- 
    N > 1,
    X is N mod 2,
    Y is N // 2,  
    dec2bin(Y,L1),
    L = [X|L1].

%Copyrights https://stackoverflow.com/questions/16174681/how-to-delete-the-last-element-from-a-list-in-prolog
list_butlast([X|Xs], Ys) :-                 % use auxiliary predicate ...
   list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-  
   list_butlast_prev(Xs, Ys, X1).           % lag behind by one

remove0s(List, Answer) :-
    last(List,0) ->
    list_butlast(List, L),
    remove0s(L, Answer)
    ; Answer = List.
    
breakthelist([H|T], H, T).

updateList([H|T], L, L2) :-   %H logiki einai an list_sum < K, 
                              %na meionoume kata 1 to proto stoixeio pou einai > 1 kai na auksanoume to proigoymeno kata 2
    breakthelist([H|T], Y, [H2|T2]),
    H2 > 0 ->
    Z is 2 + Y,
    H3 is H2 - 1,
    add_tail(L2, Z, L3),
    append(L3,[H3|T2], L)
    ;   add_tail(L2, H, L3),
        updateList(T, L, L3).

checkifpossible(N, K, Bool) :- 
    K > N ->  %Epistrefei 0 an den ginetai na graftei os K dunameis tou 2, allios 1
    Bool is 0
    ; dec2bin(N, B),
    sumlist(B,X),
    K < X ->
    Bool is 0
    ; Bool is 1.

add_tail([],X,[X]).  % https://stackoverflow.com/questions/32720673/prolog-insert-the-number-in-the-list-by-the-tail
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).        
    
answer(List, K, Answer) :-
    sumlist(List, Sum),
    Sum < K ->
    updateList(List,L,[]),
    answer(L, K, Answer)
    ; Answer = List.

solve(Answer, N, K) :-
    checkifpossible(N,K,Bool),
    Bool =:= 0 -> Answer = []
    ;dec2bin(N, List),
    answer(List, K, Answer).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).    

runTtimes(T, Answers, Stream, NewList) :- 
    T > 0 ->
    read_line(Stream, [N,K]),
    solve(Answer, N, K),
    remove0s(Answer, NewAnswer),
    append(NewList, [NewAnswer], ToNewList),
    J is T - 1,     
    runTtimes(J, Answers, Stream, ToNewList)
    ; Answers = NewList.

powers2(File, Answers) :- 
    open(File, read, Stream),
    read_line(Stream, T),
    runTtimes(T, Answers, Stream, []),
    close(Stream).    