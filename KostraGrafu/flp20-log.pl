/*
	FLP - Kostra grafu
	Autor: Bc. Ján Folenta
	Datum: 2.5.2021
*/


/* Nasledujuca cast venujuca sa nacitaniu vstupu je prebrana z cvicenia */

/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


/* merge spoji 2 prvky do zoznamu */
merge([],L,L).
merge([H|T], L, [H|M]) :- merge(T,L,M).


/* mergePoints spoji 2 vrcholy zo vstupu do dvojice */
mergePoints([LeftPoint, RightPoint|_], PairOfPoints) :- merge(LeftPoint, RightPoint, PairOfPoints).


/* prepareData transformuje format dat z nacitaneho vstupu do formatu, ktory sa bude pouzivat dalej */
prepareData([], ParsedData, ParsedData).
prepareData([H|T], AlreadyParsed, ParsedData) :- mergePoints(H, PairOfPoints), 
						 append(AlreadyParsed, [PairOfPoints], MergedData), 
						 prepareData(T, MergedData, ParsedData).


/* getVertices ziska zo vstupnych dat vsetky vrcholy */
getVertices(ParsedData, SortedVertices) :- flatten(ParsedData, Vertices), sort(Vertices, SortedVertices).


/* getPathLength ziska z poctu vrcholov spravnu dlzku cesty (stromu) */
getPathLength(Vertices, Length) :- length(Vertices, NumberOfVertices), Length is NumberOfVertices - 1.


/*getNumberOfVertices najde pocet vrcholov */
getNumberOfVertices(Vertices, NumberOfVertices) :- length(Vertices, NumberOfVertices).


/* isEmpty otestuje, ci je zoznam prazdny */
isEmpty([]) :- true.
isEmpty([_|_]) :- false.


/* findAllPaths postupne prechadza jednotlive vrcholy a najde vsetky cesty spravnej dlzky (pocet vrcholov - 1) */
findAllPaths(Path, [], Res, PathLength) :- length(Path, L),
                           (PathLength == L ->
                               sort(Path, Res);
                               Res = []),
                            !.
findAllPaths(Path, [_|T], Res, PathLength) :- findAllPaths(Path, T, Res, PathLength).
findAllPaths(Path, [H|T], Res, PathLength) :- length(H, L), (L \= 1 ->
                                    findAllPaths([H|Path], T, Res, PathLength);
                                    findAllPaths(Path, T, Res, PathLength)).



/* checkLeftPoint skontroluje, ci sa lavy bod nachazda v zozname navstivenych vrcholov a ked nie, prida ho tam */
checkLeftPoint(LeftPoint, AlreadyVisited, NewAlreadyVisited) :- (member(LeftPoint, AlreadyVisited) -> 
								    copy_term(AlreadyVisited, NewAlreadyVisited) ; 
								    append(AlreadyVisited, [LeftPoint], NewAlreadyVisited)).


/* checkRightPoint skontroluje, ci sa pravy bod nachazda v zozname navstivenych vrcholov a ked nie, prida ho tam */
checkRightPoint(RightPoint, AlreadyVisited, NewAlreadyVisited) :- (member(RightPoint, AlreadyVisited) -> 
									copy_term(AlreadyVisited, NewAlreadyVisited) ; 
									append(AlreadyVisited, [RightPoint], NewAlreadyVisited)).


/* findVisitedVertices najde vsetky navstivene vrcholy */
findVisitedVertices([LeftPoint, RightPoint|_], AlreadyVisited, AllVisited) :- checkLeftPoint(LeftPoint, AlreadyVisited, NewAlreadyVisited), 
									      checkRightPoint(RightPoint, NewAlreadyVisited, AllVisited).

/* detectCycles zisti, ci sa vo vstupnej ceste (vstupnom strome) nachadza cyklus a ked ano, tak taku cestu ignoruje */
detectCycles(_, _, _, [], []).
detectCycles(NumberOfVertices, AlreadyVisited, Path, [H], ResultPath) :- findVisitedVertices(H, AlreadyVisited, NewAlreadyVisited), 
									 append(Path, [H], NewPath), length(NewAlreadyVisited, L), 
									 (L == NumberOfVertices -> 
									     copy_term(NewPath, ResultPath) ; 
									     detectCycles(NewAlreadyVisited, NewPath, NumberOfVertices, [], ResultPath)).
detectCycles(NumberOfVertices, AlreadyVisited, Path, [H|T], ResultPath) :- findVisitedVertices(H, AlreadyVisited, NewAlreadyVisited), 
									   append(Path, [H], NewPath), 
									   detectCycles(NumberOfVertices, NewAlreadyVisited, NewPath, T, ResultPath).


/* printPaths a printPath vytlaci na vystup cesty v pozadovanom formate */
printPaths([]).
printPaths([H|T]) :- (isEmpty(H) -> 
			  printPaths([]); 
			  printPath(H)), 
		      printPaths(T).

printPath([]) :- nl.
printPath([H|T]) :- (isEmpty(T) ->
			 format('~w-~w', H);
			 format('~w-~w ', H)),
                    printPath(T).


start :-
	prompt(_, ''),
	read_lines(LL),
	split_lines(LL, Input),
	prepareData(Input, [], ParsedData),
	getVertices(ParsedData, Vertices),
	getPathLength(Vertices, PathLength),
	setof(R, findAllPaths([], ParsedData, R, PathLength), AllPaths),
	getNumberOfVertices(Vertices, NumberOfVertices),
	maplist(detectCycles(NumberOfVertices, [], []), AllPaths, ResultPaths),
	printPaths(ResultPaths),		
	halt.