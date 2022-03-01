:- use_module(problemTravelAB,[move/4, startState/1, goalTest_/1, heuristic/2, writeState/1]).
:- dynamic  fringe_/1, searchTree_/1.

% node(ID, State, MotherID, Action, Depth, Cost)
% fringe_([[node1, f1], [node2, f2], ... ])
% searchTree_(node1). searchTree_(node2). ...

%filterMode(nofilter).
filterMode(noBackState). % nincs a saj�t �g�n f�lfel� ugyanilyen �llapot
%filterMode(noSameState). % a keres�si f�ban m�g nincs ilyen �llapot

/********************************************** 
  az els� vagy a k�vetkez� megold�s keres�se 
**********************************************/
firstSolution(Start,Goal,ID):-
	solution(Start,Goal,ID).

firstSolution(Goal,ID):-
	startState(Start),
	solution(Start,Goal,ID).

nextSolution(Goal,ID):-
	search(Goal,ID).

/********************************************** 
   ha l�p�senk�nt akarjuk k�vetni a keres�st  
 **********************************************/
firstStep(Start,Node):-
	retractall(maxID(_)),
	assert(maxID(0)),				% a gy�k�rcsom�pont azonos�t�ja
	retractall(searchTree_(_)), 	% kir�r�tett�k a keres�si f�t
	retractall(fringe_(_)),			% ki�r�tett�k a peremet.
	Root = node(0,Start,0,0,0,0),	% ez a keres�si fa gy�kere
	assert(fringe_([[Root,0]])),	% a gy�keret r�tessz�k a peremre
	select(Node),					% kiv�lasztjuk a perem els� elem�t
	addChildren2Fringe(Node).		% a gyermekeit a peremhez adjuk

firstStep(Node):-
	startState(Start),
	firstStep(Start,Node).

nextStep(Node):-
	select(Node),					% kiv�lasztjuk a perem els� elem�t
	addChildren2Fringe(Node).		% a gyermekeit a peremhez adjuk

/**********************************************
   maga a keres�s innen indul
***********************************************/
solution(Goal,ID):-
	startState(Start),
	solution(Start,Goal,ID).

solution(Start,Goal,ID):-
	retractall(maxID(_)),
	assert(maxID(0)),				% a gy�k�rcsom�pont azonos�t�ja
	retractall(searchTree_(_)), 	% kir�r�tett�k a keres�si f�t
	retractall(fringe_(_)),			% ki�r�tett�k a peremet.
	Root = node(0,Start,0,0,0,0),	% ez a keres�si fa gy�kere
	assert(fringe_([[Root,0]])),	% a gy�keret r�tessz�k a peremre
	search(Goal,ID).				% �s keres�nk - az els� tal�latig

search(State,ID):-
	select(Node),					% kiv�lasztjuk a perem els� elem�t
	addChildren2Fringe(Node),		% a gyermekeit a peremhez adjuk
	doGoalTest(Node, State, ID).	% �s megn�zz�k, hogy a kiv�lasztott csom�pont-e a c�l
									% ha nem, akkor az �jra megh�vja a search(State, ID)-t

doGoalTest(node(ID, State, _, _, _, _), State, ID):-
	goalTest_(State),!.				% ha c�l�llapotot tal�ltunk, akkor kil�p�nk

doGoalTest(_, State, ID):-
	search(State,ID).				% egy�bk�nt folytatjuk a keres�st
	
goalTest(node(_, State, _, _, _, _)):-
	goalTest_(State).				% a l�p�senk�nti futtat�sn�l ezzel ellen�rizhetj�k a c�lt

select(Node):-
	fringe_([[Node,_]|Rest]),!,		% kiv�lasztjuk a perem els� elem�t (ha tudjuk)
	retractall(fringe_(_)),
	assert(fringe_(Rest)),			% �s t�r�lj�k a perem els� elem�t
	assert(searchTree_(Node)).		% �s �ttessz�k a keres�si f�ba

select(_):-							% ha ki�r�lt a perem, akkor
	write('Nincs (t�bb) megold�s'), nl,
	fail.							% le�llunk

addChildren2Fringe(Node):-
	Node = node(ID, State, _, _, Depth, Cost),	% ennek a lesz�rmazottjait keress�k
	findall(									% megkeress�k, hogy mik a lesz�rmazottak
		[Child, ID, Op, Depth, Cost, C], 		% ideiglenes k�dok
		move(State,Child,Op,C), 
		Children),
	filterNewNodes(Children,GoodChildren),		% megn�zz�k, hogy ezek k�z�l melyikek a j�k
	newNodes(GoodChildren,Nodes),				% ezek alapj�n legener�ljuk az �j csom�pontokat
	insertChildren2Fringe(Nodes).						

insertChildren2Fringe([]).
insertChildren2Fringe([[Node,F]|Nodes]):-
	fringe_(Fr),
	insertChild2Fringe([Node,F], Fr, NewFr),
	retractall(fringe_(_)),
	assert(fringe_(NewFr)),
	insertChildren2Fringe(Nodes).

insertChild2Fringe([Node,F],[],[[Node,F]]).
insertChild2Fringe([Node,F], [[N1,F1]|Rest], [[Node,F],[N1,F1]|Rest]):-
	F < F1, !.
insertChild2Fringe([Node,F], [[N1,F1]|Rest], [[N1,F1]|Rest2]):-
	insertChild2Fringe([Node,F], Rest, Rest2).

filterNewNodes(Nodes,Nodes):-					% ha nem akarunk sz�rni
	filterMode(nofilter).

filterNewNodes([],[]):-!.						% �res list�n nincs mit sz�rni

filterNewNodes([Node|Nodes],FdNodes):-			% ha kisz�rj�k...
	filterMode(noBackState),					% sz�r�si m�d
	Node = [State, MotherID, _, _, _, _],
	getAllNodesFromTo(MotherID,0,Mothers),		% megkeress�k az any�t�l a gy�k�rig tart� �gat
	member(node(_,State,_,_,_,_), Mothers),!,	% ellen�rizz�k, hogy nincs-e rajta a k�rd�ses csom�pont
	filterNewNodes(Nodes,FdNodes).				% a t�bbi csom�pontot is ellen�rizz�k

filterNewNodes([Node|Nodes],[Node|FdNodes]):-	% ha nem sz�rj�k ki...
	filterMode(noBackState),
	filterNewNodes(Nodes,FdNodes).

filterNewNodes([Node|Nodes],FdNodes):-
	filterMode(noSameState),
	Node = [State, _, _, _, _, _],
	searchTree_(node(_,State,_,_,_,_)),!,
	filterNewNodes(Nodes,FdNodes).	

filterNewNodes([Node|Nodes],[Node|FdNodes]):-
	filterMode(noSameState),
	filterNewNodes(Nodes,FdNodes).

getAllNodesFromTo(ID,ID,[Node]):-
	Node = node(ID, _, _, _, _, _),
	searchTree_(Node).

getAllNodesFromTo(From,To,[Node|Nodes]):-
	\+ From = To,
	Node = node(From, _, MotherID, _, _, _),
	searchTree_(Node),
	getAllNodesFromTo(MotherID,To,Nodes).

newNodes([],[]).
newNodes([[State, MotherID, Op, Depth, Cost, C]|Children], [[NewNode, F]|Fr2]):-
	NewNode = node(NewID, State, MotherID, Op, NewDepth, NewCost),
	getNewId(NewID),
	NewDepth is Depth + 1,
	NewCost is Cost + C,
	heuristic(State, H),
	F is H + NewCost,
	newNodes(Children, Fr2).

getNewId(NewID):-
	maxID(OldID),
	retract(maxID(OldID)),
	NewID is OldID + 1,
%	write(NewID), nl,
	assert(maxID(NewID)).

writeNode(ID):-
	searchTree_(node(ID,State,_,Action,_,_)),
	write(Action), nl,
	writeState(State).

writeNode(node(_ID, State, _MotherID, Action, _Depth, _Cost)):-
	write(Action), nl,
	writeState(State).

writeNodewCost(node(_ID, State, _MotherID, Action, _Depth, Cost)):-
	write(Action), write(' '), write(Cost), nl,
	writeState(State).

writeSolution(0):-
	selectNodeByID(0,Node),
	writeNode(Node),!.
	
writeSolution(ID):-
	selectNodeByID(ID,Node),
	getMotherID(Node, MotherID),
	writeSolution(MotherID),
	writeNode(Node).

writeSolutionwCost(0):-
	selectNodeByID(0,Node),
	writeNodewCost(Node),!.
	
writeSolutionwCost(ID):-
	selectNodeByID(ID,Node),
	getMotherID(Node, MotherID),
	writeSolutionwCost(MotherID),
	writeNodewCost(Node).

writeCost(ID):-
	selectNodeByID(ID, node(_,_,_,_,_,Cost)),
	write('  cost: '),
	write(Cost),
	nl.

selectNodeByID(ID, Node):-
	Node = node(ID,_,_,_,_,_),
	searchTree_(Node).

getMotherID(node(_,_,MotherID,_,_,_), MotherID).

treeSize(N):-
	findall(ID, searchTree_(node(ID,_,_,_,_,_)),L),
	length(L,N).

info:-
	write('****************************************************'),nl,
	write('* firstSolution(Start?, Goal, ID)                  *'),nl,
	write('* nextSolution(Goal,ID)                            *'),nl,
	write('* firstStep(Start?,Node)                           *'),nl,
	write('* nextStep(Node)                                   *'),nl,
	write('* goalTest(Node)                                   *'),nl,
	write('* writeNode(ID)                                    *'),nl,
	write('* writeNode(Node)                                  *'),nl,
	write('* writeSolution(ID)                                *'),nl,
	write('* writeSolutionwCost(ID)                           *'),nl,
	write('* writeCost(ID)                                    *'),nl,
	write('* treeSize(N)                                      *'),nl,
	write('* info                                             *'),nl,
	write('****************************************************'),nl.
:- info.