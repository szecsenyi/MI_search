:- module(problemTravelAB,[move/4, startState/1, goalTest_/1, heuristic/2, writeState/1]).

startState(arad).

goalTest_(bukarest).

% move(State1, State2, Action, Cost).
move(A, B, '->', Cost):-
	ut(A,B,Cost).
move(A, B, '->', Cost):-
	ut(B,A,Cost).

ut(arad, szeben, 140).
ut(arad, nagyzerend, 75).
ut(arad, temesvar, 118).
ut(nagyzerend, nagyvarad, 71).
ut(nagyvarad, szeben, 151).
ut(temesvar, lugos, 111).
ut(lugos, mehadia, 70).
ut(mehadia, dobreta, 75).
ut(dobreta, craiova, 120).
ut(szeben, rimnicu_vilcea, 80).
ut(rimnicu_vilcea, craiova, 146).
ut(szeben, fogaras, 99).
ut(rimnicu_vilcea, pitesti, 97).
ut(craiova, pitesti, 138).
ut(pitesti, bukarest, 101).
ut(fogaras, bukarest, 211).
ut(bukarest, csalanos, 85).
ut(bukarest, giurgiu, 90).
ut(csalanos, hirsova, 98).
ut(hirsova, eforie, 86).
ut(csalanos, vaslui, 142).
ut(vaslui, iasi, 92).
ut(iasi, neamt, 87).

heuristic(arad, 366).
heuristic(bukarest, 0).
heuristic(craiova, 160).
heuristic(csalanos, 80).
heuristic(dobreta, 242).
heuristic(eforie, 161).
heuristic(fogaras, 178).
heuristic(giurgiu, 77).
heuristic(hirsova, 151).
heuristic(iasi, 226).
heuristic(lugos, 244).
heuristic(mehadia, 241).
heuristic(szeben, 253).
heuristic(nagyvarad, 380).
heuristic(nagyzerend, 374).
heuristic(neamt, 234).
heuristic(pitesti, 98).
heuristic(temesvar, 329).
heuristic(rimnicu_vilcea, 193).
heuristic(vaslui, 199).


writeState(State):-
	write(State), nl.

