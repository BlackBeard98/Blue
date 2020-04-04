:-dynamic bolsa/2, fabrica/1.
bolsa(azul,20).
bolsa(blanco,20).
bolsa(negro,20).
bolsa(rojo,20).
bolsa(amarillo,20).


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
    asserta(fabrica(X)),
    poner_fabrica(L).

quitar_bolsa([]).
quitar_bolsa([X|L]):-
    findall(I,bolsa(X,I),[P|_]),
    retract(bolsa(X,P)),
    P1 is P-1,
    asserta(bolsa(X,P1)),
    quitar_bolsa(L).
    
        










