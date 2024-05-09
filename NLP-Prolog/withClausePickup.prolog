%assert(edge([population], type([city], [tokyo]), type([number], [million3]))).
%assert(edge([friend], type([city], [tokyo]), type([country], [india]))).
%assert(edge([capital], type([city], [delhi]), type([country], [india]))).

:-dynamic
	edge/3.

stop_word(is).
stop_word(are).
stop_word(am).
stop_word(with).
stop_word(of).

stop_word(a).
stop_word(an).
stop_word(the).

qu([what]).

isRelation(relation).


extract_relevant_words([], X, [X]):- !.
extract_relevant_words(['.'], X, [X]):- !.

extract_relevant_words( [W|STail], X, ResultList):- 
	downcase_atom(W,A),
	stop_word(A), !,
	extract_relevant_words( STail, [], RelevantTail),
	(
		(X = []) -> (ResultList = RelevantTail); (ResultList = [ X| RelevantTail] )
	)
	.

extract_relevant_words( [W|STail], X, RelevantTail):-
	downcase_atom(W,A),
	append( X, [A], Y  ),
	extract_relevant_words( STail, Y, RelevantTail).

to_do:-
	readln(S),
	extract_relevant_words(S, [],Words),
	taken_list(Words). % Replace this line with taken

taken_list([X,Y,Z]) :- taken(X,Y,Z).

taken(X,Y,Z):- 
	(
		(	qu(X), !,
			(
				findVal(Y, Z, D);
				findVal(Z, Y, D)
			)
		);
		(
			qu(Y), !,
			(
				findVal(X, Z, D);
				findVal(Z, X, D)
			)
		);
		(
			qu(Z), !,
			(
				findVal(Y, X, D);
				findVal(X, Y, D)
			)
		)
	).

taken(X,Y,Z):- 
	(
		addEdge(X, Y, Z); 
		addEdge(X, Z, Y); 
		addEdge(Y, X, Z); 
		addEdge(Y, Z, X); 
		addEdge(Z, Y, X); 
		addEdge(Z, X, Y)
	), !.

taken(X,Y,Z):-
		addEdgeToGraph(X,Y,Z).
		
addEdge(X, Y, Z):-
	(
		(
			edge(X, type(B, _L), type(C, _M)),
			edge(_J, type(B, Y), type(_Q, _T)),
			assert(edge(X, type(B, Y), type(C, Z)))
		);
		(
			edge(X, type(B, _L), type(C, _M)),
			edge(_J, type(_T, _Q), type(C, Y)),
			assert(edge(X, type(B, Z), type(C, Y)))
		)
	).
	
addEdgeToGraph(X, Y, Z):-
	write(what), tab(1), write(is), tab(1), write(X), nl,
	read(A),
	write(what), tab(1), write(is), tab(1), write(Y), nl,
	read(B),
	write(what), tab(1), write(is), tab(1), write(Z), nl,
	read(C),
	(
		(isRelation(A), assert(edge(X, type([B], Y), type([C], Z))));
		(isRelation(B), assert(edge(Y, type([A], X), type([C], Z))));
		(isRelation(C), assert(edge(Z, type([A], X), type([B], Y))))
	).
	

findVal(X, Y, D):-
	(
		edge(X, type(_A, Y), type(_C, D));
		edge(X, type(_A, D), type(_C, Y));
		edge(D, type(_A, X), type(_C, Y))
	),
	writeln(D).
	
findVal(X, Y, D):-
	(
		edge(X, type(A, B), type(C, D)),
		(
			findRel(Y, B,[]);
			findRel(B, Y,[])
			
		),
		write(B), tab(1), write(X), tab(1), write(D), nl
	);
	(
		edge(X, type(A, D), type(C, B)),
		(
			findRel(B, Y,[]);
			findRel(Y, B,[])
		),
		write(B), tab(1), write(X), tab(1), write(D), nl
	).
	
findRel(B, C, _):-
	edge(A, type(_M, B), type(_J, C)),
	write(B), tab(1), write(A), tab(1), write(C), nl.

findRel(B, C, List):-
	(
		edge(A, type(_M, L), type(_J, C));
		edge(A, type(_M, C), type(_J, L))
	),
	not(member(L,List)),
	(
		findRel(B, L,[C|List]);
		findRel(L, B,[C|List])
	),
	write(L), tab(1), write(A), tab(1), write(C), nl.