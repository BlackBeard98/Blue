:-dynamic bolsa/2, fabrica/1, pared/4.
bolsa(0,20).
bolsa(1,20).
bolsa(2,20).
bolsa(3,20).
bolsa(4,20).


extender([],[]).

extender([[_,0]|L],R):-
    !,
    extender(L,R).

extender([[C,N]|L],[C|R]):-
    N1 is N-1,
    extender([[C,N1]|L],R).

extaer_bolsa(F):-
    findall([X,Y], bolsa(X,Y),L),
    extender(L,EL),
    random_permutation(EL, PL),
    take(PL,36,Sl),
    quitar_bolsa(Sl),
    hacer_fabricas(Sl,F),
    poner_fabrica(F).
    
take([],_,[]).
take(_,0,[]).
take([X|L],N,[X|R]):-
    N>0,
    N1 is N-1,
    take(L,N1,R).
hacer_fabricas([],[]).
hacer_fabricas([A1,A2,A3,A4|L],[[A1,A2,A3,A4]|R]):-
    !,hacer_fabricas(L,R).
hacer_fabricas([A1,A2,A3|L],[[A1,A2,A3]|R]):-
    !,hacer_fabricas(L,R).
hacer_fabricas([A1,A2|L],[[A1,A2]|R]):-
!,hacer_fabricas(L,R).
hacer_fabricas([A1|L],[[A1]|R]):-
!,hacer_fabricas(L,R).


poner_fabrica([]).
poner_fabrica([X|L]):-
    length(L, Int),
    asserta(fabrica([X,Int])),
    poner_fabrica(L).

quitar_bolsa([]).
quitar_bolsa([X|L]):-
    findall(I,bolsa(X,I),[P|_]),
    retract(bolsa(X,P)),
    P1 is P-1,
    asserta(bolsa(X,P1)),
    quitar_bolsa(L).
    
valida(X,Y):-
    X>= 0,5>X,Y>0,5>Y.

linea(X,Y,MX,MY,_,0):-
    X1 is X+MX,
    Y1 is Y+MY,
    not(valida(X1,Y1)),!.

linea(X,Y,MX,MY,Player,Ans):-
    X1 is X+MX,
    Y1 is Y+MY,
    clause(pared(X1,Y1,Player,_),true),!,
    linea(X1,Y1,MX,MY,Player,Temp),
    Ans is Temp +1.

linea(X,Y,MX,MY,Player,0):-
    X1 is X+MX,
    Y1 is Y+MY,
    not(clause(pared(X1,Y1,Player,_),true)).
