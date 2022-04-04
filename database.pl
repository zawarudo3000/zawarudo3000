start(b1).
end(b6).
constraint(b5,2).

/* wie oft kommt X in der Liste vor*/
/*count(X, List, C) :- scount(X, List , 0).*/
/*wenn liste leer einfach zurück gehen*/
scount(_, [ ], 0).
/*X steht ganz vorne an der Liste also C + 1*/
/*C1 erhält im Basisfall dann irgendwann den Wert 0, dieser wird über den Call-Stack zurück gegeben
daraufhin wird der Value im Head also C geupdated*/
scount(X, [X|Rest], C) :- scount(X, Rest, C1), C is C1 + 1.
scount(X, [_|Rest], C) :- scount(X, Rest, C).

/*path(X,Y,V,P) :- ...*/


node(a,[b,c]).
node(b,[d,e]).
node(c,[e]).
node(d,[b]).
node(e,[]).

startnode(a).


edge(X,Y) :- node(X, L), member(Y,L).

countNodes(0).
countNodes(Cnt) :- n