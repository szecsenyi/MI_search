:- module(problem8Queen,[
	writeState/1, 
	goalTest_/1, 

	move/4, 
	startState/1, 
	heuristic/2, 

	neighbor/2,
	randomNeighbor/2,	
	randomState/1,
	evaluate/2,
	betterFittness/2
	]).
:- use_module(library(random)).

/**************************************
  vak és informált keresések 
**************************************/

startState([]).

goalTest_(State):-
	all_diff(State),
	combine([1,2,3,4,5,6,7,8],State,S,D),
	all_diff(S),
	all_diff(D).

% move(State1, State2, Action, Cost).
move(L,[Q|L],Q,1):-
	\+ L=[_,_,_,_,_,_,_,_],
	member(Q,[1,2,3,4,5,6,7,8]),
	\+ member(Q,L).

combine([X1|X],[Y1|Y],[S1|S],[D1|D]) :-
	S1 is X1 +Y1,
	D1 is X1 - Y1,
	combine(X,Y,S,D).
combine([],[],[],[]).

all_diff([X|Y]) :-  \+member(X,Y), all_diff(Y).
all_diff([_]).

/*** nincs heurisztika **/
heuristic(_State, 1).

/**************************************
  hegymászó keresések 
**************************************/

/*** véletlen kiinduló állapot     ***/
randomState([A1,A2,A3,A4,A5,A6,A7,A8]):-
	random(1,9,A1),
	random(1,9,A2),
	random(1,9,A3),
	random(1,9,A4),
	random(1,9,A5),
	random(1,9,A6),
	random(1,9,A7),
	random(1,9,A8),
	!.

neighbor([X|Rest], [Y|Rest]):-
	member(Y,[1,2,3,4,5,6,7,8]),
	X \== Y.

neighbor([X|Rest1], [X|Rest2]):-
	neighbor(Rest1, Rest2).

randomNeighbor(State,Neighbor):-
	random(0,8,Split),
	length(L1,Split),
	append(L1,[Q|L2],State),
	random(1,9,RQ),
	RQ \== Q,!,
	append(L1,[RQ|L2],Neighbor).

evaluate(State,Fitness):-
	beatCount(State,Fitness).

betterFittness(F1,F2):-
	F1 =< F2.	% minél kisebb, annál jobb

beatCount(State,N):-
	setof(X,member(X,State),L1),
	length(L1,N1),
	combine([1,2,3,4,5,6,7,8],State,L21,L31),
	setof(X,member(X,L21),L22),
	length(L22,N2),
	setof(X,member(X,L31),L32),
	length(L32,N3),
	N is 24 - N1 - N2 - N3.
	
/**************************************
  egy állapot megjelenítése
**************************************/
writeState([]):-nl,!.
writeState([1|Rest]):- write('X _ _ _ _ _ _ _'),nl,writeState(Rest).
writeState([2|Rest]):- write('_ X _ _ _ _ _ _'),nl,writeState(Rest).
writeState([3|Rest]):- write('_ _ X _ _ _ _ _'),nl,writeState(Rest).
writeState([4|Rest]):- write('_ _ _ X _ _ _ _'),nl,writeState(Rest).
writeState([5|Rest]):- write('_ _ _ _ X _ _ _'),nl,writeState(Rest).
writeState([6|Rest]):- write('_ _ _ _ _ X _ _'),nl,writeState(Rest).
writeState([7|Rest]):- write('_ _ _ _ _ _ X _'),nl,writeState(Rest).
writeState([8|Rest]):- write('_ _ _ _ _ _ _ X'),nl,writeState(Rest).
