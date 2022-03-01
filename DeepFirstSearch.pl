:- use_module(problemTravelAB,[move/4, startState/1, goalTest_/1, heuristic/2, writeState/1]).
:- dynamic  fringe_/1, searchTree_/1.

% node(ID, State, MotherID, Action, Depth, Cost)
% fringe_([[node1, f1], [node2, f2], ... ])
% searchTree_(node1). searchTree_(node2). ...

%filterMode(nofilter).
filterMode(noBackState). % nincs a saját ágán fölfelé ugyanilyen állapot
%filterMode(noSameState). % a keresési fában még nincs ilyen állapot

/********************************************** 
  az elsõ vagy a következõ megoldás keresése 
**********************************************/
firstSolution(Start,Goal,ID):-
	solution(Start,Goal,ID).

firstSolution(Goal,ID):-
	startState(Start),
	solution(Start,Goal,ID).

nextSolution(Goal,ID):-
	search(Goal,ID).

/********************************************** 
   ha lépésenként akarjuk követni a keresést  
 **********************************************/
firstStep(Start,Node):-
	retractall(maxID(_)),
	assert(maxID(0)),				% a gyökércsomópont azonosítója
	retractall(searchTree_(_)), 	% kirürítettük a keresési fát
	retractall(fringe_(_)),			% kiürítettük a peremet.
	Root = node(0,Start,0,0,0,0),	% ez a keresési fa gyökere
	assert(fringe_([[Root,0]])),	% a gyökeret rátesszük a peremre
	select(Node),					% kiválasztjuk a perem elsõ elemét
	addChildren2Fringe(Node).		% a gyermekeit a peremhez adjuk

firstStep(Node):-
	startState(Start),
	firstStep(Start,Node).

nextStep(Node):-
	select(Node),					% kiválasztjuk a perem elsõ elemét
	addChildren2Fringe(Node).		% a gyermekeit a peremhez adjuk

/**********************************************
   maga a keresés innen indul
***********************************************/
solution(Goal,ID):-
	startState(Start),
	solution(Start,Goal,ID).

solution(Start,Goal,ID):-
	retractall(maxID(_)),
	assert(maxID(0)),				% a gyökércsomópont azonosítója
	retractall(searchTree_(_)), 	% kirürítettük a keresési fát
	retractall(fringe_(_)),			% kiürítettük a peremet.
	Root = node(0,Start,0,0,0,0),	% ez a keresési fa gyökere
	assert(fringe_([[Root,0]])),	% a gyökeret rátesszük a peremre
	search(Goal,ID).				% és keresünk - az elsõ találatig

search(State,ID):-
	select(Node),					% kiválasztjuk a perem elsõ elemét
	addChildren2Fringe(Node),		% a gyermekeit a peremhez adjuk
	doGoalTest(Node, State, ID).	% és megnézzük, hogy a kiválasztott csomópont-e a cél
									% ha nem, akkor az újra meghívja a search(State, ID)-t

doGoalTest(node(ID, State, _, _, _, _), State, ID):-
	goalTest_(State),!.				% ha célállapotot találtunk, akkor kilépünk

doGoalTest(_, State, ID):-
	search(State,ID).				% egyébként folytatjuk a keresést
	
goalTest(node(_, State, _, _, _, _)):-
	goalTest_(State).				% a lépésenkénti futtatásnál ezzel ellenõrizhetjük a célt

select(Node):-
	fringe_([[Node,_]|Rest]),!,		% kiválasztjuk a perem elsõ elemét (ha tudjuk)
	retractall(fringe_(_)),
	assert(fringe_(Rest)),			% és töröljük a perem elsõ elemét
	assert(searchTree_(Node)).		% és áttesszük a keresési fába

select(_):-							% ha kiürült a perem, akkor
	write('Nincs (több) megoldás'), nl,
	fail.							% leállunk

addChildren2Fringe(Node):-
	Node = node(ID, State, _, _, Depth, Cost),	% ennek a leszármazottjait keressük
	findall(									% megkeressük, hogy mik a leszármazottak
		[Child, ID, Op, Depth, Cost, C], 		% ideiglenes kódok
		move(State,Child,Op,C), 
		Children),
	filterNewNodes(Children,GoodChildren),		% megnézzük, hogy ezek közül melyikek a jók
	newNodes(GoodChildren,Nodes),				% ezek alapján legeneráljuk az új csomópontokat
	fringe_(FrNodes),
	retractall(fringe_(_)),
	append(Nodes,FrNodes,FrNodes2),				% és a perem végéhez adjuk !!!szélességi keresés!!!
	assert(fringe_(FrNodes2)).

filterNewNodes(Nodes,Nodes):-					% ha nem akarunk szûrni
	filterMode(nofilter).

filterNewNodes([],[]):-!.						% üres listán nincs mit szûrni

filterNewNodes([Node|Nodes],FdNodes):-			% ha kiszûrjük...
	filterMode(noBackState),					% szûrési mód
	Node = [State, MotherID, _, _, _, _],
	getAllNodesFromTo(MotherID,0,Mothers),		% megkeressük az anyától a gyökérig tartó ágat
	member(node(_,State,_,_,_,_), Mothers),!,	% ellenõrizzük, hogy nincs-e rajta a kérdéses csomópont
	filterNewNodes(Nodes,FdNodes).				% a többi csomópontot is ellenõrizzük

filterNewNodes([Node|Nodes],[Node|FdNodes]):-	% ha nem szûrjük ki...
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
newNodes([[State, MotherID, Op, Depth, Cost, C]|Children], [[NewNode,_F]|Fr2]):-
	NewNode = node(NewID, State, MotherID, Op, NewDepth, NewCost),
	getNewId(NewID),
	NewDepth is Depth + 1,
	NewCost is Cost + C,
%	evaluateF(NewNode, F),	% ez akkor kellene, ha informált keresés lenne
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


delBranchIfNoChildren(ID,[]):-
	fringe_(Fringe),
	\+ member([node(_,_,ID,_,_,_),_], Fringe),	% ha a peremen nem várakozik leánya
	findall(X, searchTree_(node(X,_,ID,_,_,_)), Dtrs),
	Dtrs = [],									% és ha nincs a keresési fában sem leánya
	!,
	selectNodeByID(ID, Node),
	Node = node(ID,_,MotherID,_,_,_),
	retract(searchTree_(Node)),					% akkor töröljük
	delBranchIfNoChildren(MotherID,[]).			% esetleg az anyját is

delBranchIfNoChildren(_,_).						% ha van lány, nem töröljük