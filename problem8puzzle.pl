:- module(problem8puzzle,[move/4, startState/1, goalTest_/1, heuristic/2, writeState/1]).

%heuristicType(wrongPlace).
heuristicType(manhattan).

%startState([1,2,3,4,5,0,7,8,6]). %könnyű
startState([2,3,6,1,0,4,7,5,8]). %nehéz

goalTest_([1,2,3, 4,5,6, 7,8,0]).

% move(State1, State2, Action, Cost).
move([0,B,C, D,E,F, G,H,I], [B,0,C, D,E,F, G,H,I], left, 1).
move([A,0,C, D,E,F, G,H,I], [A,C,0, D,E,F, G,H,I], left, 1).
move([A,B,C, 0,E,F, G,H,I], [A,B,C, E,0,F, G,H,I], left, 1).
move([A,B,C, D,0,F, G,H,I], [A,B,C, D,F,0, G,H,I], left, 1).
move([A,B,C, D,E,F, 0,H,I], [A,B,C, D,E,F, H,0,I], left, 1).
move([A,B,C, D,E,F, G,0,I], [A,B,C, D,E,F, G,I,0], left, 1).

move([A,0,C, D,E,F, G,H,I], [0,A,C, D,E,F, G,H,I], right, 1).
move([A,B,0, D,E,F, G,H,I], [A,0,B, D,E,F, G,H,I], right, 1).
move([A,B,C, D,0,F, G,H,I], [A,B,C, 0,D,F, G,H,I], right, 1).
move([A,B,C, D,E,0, G,H,I], [A,B,C, D,0,E, G,H,I], right, 1).
move([A,B,C, D,E,F, G,0,I], [A,B,C, D,E,F, 0,G,I], right, 1).
move([A,B,C, D,E,F, G,H,0], [A,B,C, D,E,F, G,0,H], right, 1).

move([0,B,C, D,E,F, G,H,I], [D,B,C, 0,E,F, G,H,I], up, 1).
move([A,0,C, D,E,F, G,H,I], [A,E,C, D,0,F, G,H,I], up, 1).
move([A,B,0, D,E,F, G,H,I], [A,B,F, D,E,0, G,H,I], up, 1).
move([A,B,C, 0,E,F, G,H,I], [A,B,C, G,E,F, 0,H,I], up, 1).
move([A,B,C, D,0,F, G,H,I], [A,B,C, D,H,F, G,0,I], up, 1).
move([A,B,C, D,E,0, G,H,I], [A,B,C, D,E,I, G,H,0], up, 1).

move([A,B,C, 0,E,F, G,H,I], [0,B,C, A,E,F, G,H,I], down, 1).
move([A,B,C, D,0,F, G,H,I], [A,0,C, D,B,F, G,H,I], down, 1).
move([A,B,C, D,E,0, G,H,I], [A,B,0, D,E,C, G,H,I], down, 1).
move([A,B,C, D,E,F, 0,H,I], [A,B,C, 0,E,F, D,H,I], down, 1).
move([A,B,C, D,E,F, G,0,I], [A,B,C, D,0,F, G,E,I], down, 1).
move([A,B,C, D,E,F, G,H,0], [A,B,C, D,E,0, G,H,F], down, 1).

/**************************************
  heurisztikák
**************************************/

/**** hány kocka van rossz helyen ****/
heuristic(State, H):-
	heuristicType(wrongPlace),
	goalTest_(Goal),
	h(State,Goal,H).

h([],[],0).
h([I|S],[I|G],H):-
	h(S,G,H),!.
h([_|S],[_|G],H):-
	h(S,G,H1),
	H is H1 + 1.

/**** Manhattan-távolság *************/
heuristic(State, H):-
	heuristicType(manhattan),
	goalTest_(Goal),
	countManDist(State,Goal,H).

countManDist([],[],0).
countManDist([S|R1],[G|R2],H):-
	countManDist(R1,R2,HR),
	pos(S,X1,Y1),
	pos(G,X2,Y2),
	H is HR + abs(X1-X2) + abs(Y1-Y2).

pos(1,1,1).
pos(2,1,2).
pos(3,1,3).
pos(4,2,1).
pos(5,2,2).
pos(6,2,3).
pos(7,3,1).
pos(8,3,2).
pos(0,3,3).


/**************************************
  egy állapot megjelenítése
**************************************/
writeState([A,B,C, D,E,F, G,H,I]):-
	write([A,B,C]), nl,
	write([D,E,F]), nl,
	write([G,H,I]), nl, nl.

