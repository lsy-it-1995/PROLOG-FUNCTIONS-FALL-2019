my_reverse(List, Rreverse) :-
        my_reverse(List, Rreverse, []).
my_reverse([], L, L).
my_reverse([H|T], L, Rest) :-
        my_reverse(T, L, [H|Rest]).



my_length([],0).
my_length([H|T],R):- my_length(T,X), R is X+1.


my_member(X, [X | _ ]).
my_member(X, [ _ | Z]) :- 
    my_member(X, Z).


my_intersect([],X,[]).
my_intersect([X|R], Y, [X|Z]):-
    my_member(X,Y),!,my_intersect(R,Y,Z).
my_intersect([X|R],Y,Z):-
    my_intersect(R,T,Z).

my_subset(X,[],[]).
my_subset(X,[H|T],[H|Y]):-
    Terms =.. [X,H], Terms,
    my_subset(X,T,Y).
my_subset(X,[H|T], Y):-
    Terms =.. [X,H], not(Terms),
    my_subset(X, T, Y).


compute_change(0,0,0,0,0).
compute_change(M,Q,D,N,P):-
    Q is (M//25),
    (Q == 0 -> D is (M//10), N is ((M - D*10)//5), P is (M - D*10 - N*5);
    D is ((M - (Q*25))//10), N is ((M - Q*25 - D*10)//5), P is (M - Q*25 - D*10 - N*5)).

compose([],[],[]).
compose(T1,[],T1).
compose([],T2,T2).
compose([H1|T1],[H2|T2], [H1,H2|T3]):-
    compose(T1,T2,T3).

palindrome(Pal,R) :-
    my_reverse(Pal, RevList),
    append(Pal, RevList, R).


