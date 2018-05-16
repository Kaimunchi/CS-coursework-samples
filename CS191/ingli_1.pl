/*
  Brandon Ingli
  CS 191.2
  Prolog Program 1
  Solving Section 8.3, problem 16 in the Hein textbook
*/

equalLists([], []) :- !.
equalLists([A|B], [A|D]) :- equalLists(B, D).

my_member(X, [X|_]) :- !.
my_member(X, [_|T]) :- my_member(X, T), !.

all(_, [], []) :- !.
all(X, [X|T], N) :- all(X, T, N), !.
all(X, [H|T], [H|U]) :- all(X, T, U), !.

makeSet([], []).
makeSet([H|T], [H|L]) :- all(H, T, U), makeSet(U, L), !.

subset([], _) :- !.
subset(X, Y) :- makeSet(X, [A|B]), makeSet(Y, C), my_member(A, C), subset(B, C), !.

/*del(X, Y, Z) removes one occurence of X from list Y and "returns" it as list Z */
del(_,[],[]) :- !.
del(X, [X|L], L).
del(X, [Y|L], [Y|M]) :- del(X, L, M), !.

equalSets([], []) :- !.
equalSets(X, Y) :- makeSet(X, [A|B]), makeSet(Y, C), my_member(A, C), del(A, C, D), equalSets(B, D), !.

subBag([], _) :- !.
subBag([X|T], Y) :- my_member(X, Y), del(X, Y, M), subBag(T, M), !.

equalBags([], []) :- !.
equalBags([X|T], Y) :- my_member(X, Y), del(X, Y, M), equalBags(T, M), !.
