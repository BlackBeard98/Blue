:-dynamic bolsa/2, fabrica/2, pared/4, jugador_actual/1,patrones/2,rodapie/2,centro/1,penalizacion/1.

rodapie(0,0).
rodapie(1,-1).
rodapie(2,-2).
rodapie(3,-4).
rodapie(4,-6).
rodapie(5,-8).
rodapie(6,-11).
rodapie(7,-14).

rodapie(X,-14):-
    X>7.

init_game(true):-
    asserta(bolsa(0,20)),asserta(bolsa(1,20)),asserta(bolsa(2,20)),asserta(bolsa(3,20)),asserta(bolsa(4,20)),
    asserta(jugador_actual(0)),
    init_rodapie,
    asserta(patrones(4,[[1,0,0],[2,0,0],[3,0,0],[4,0,0],[5,0,0]])),
    asserta(patrones(1,[[1,0,0],[2,0,0],[3,0,0],[4,0,0],[5,0,0]])),
    asserta(patrones(2,[[1,0,0],[2,0,0],[3,0,0],[4,0,0],[5,0,0]])),
    asserta(patrones(3,[[1,0,0],[2,0,0],[3,0,0],[4,0,0],[5,0,0]])).
   

init_rodapie:-
    asserta(jugador_rodapie(1,0)),asserta(jugador_rodapie(2,0)),asserta(jugador_rodapie(3,0)),asserta(jugador_rodapie(4,0)).

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
hacer_fabricas([A1,A2|L],[[A1,A2]|R]):-!,hacer_fabricas(L,R).
hacer_fabricas([A1|L],[[A1]|R]):-!,hacer_fabricas(L,R).
poner_fabrica([]).
poner_fabrica([X|L]):-
    length(L, Int),
    asserta(fabrica(X,Int)),
    poner_fabrica(L).

quitar_bolsa([]).
quitar_bolsa([X|L]):-
    findall(I,bolsa(X,I),[P|_]),
    retract(bolsa(X,P)),
    P1 is P-1,
    asserta(bolsa(X,P1)),
    quitar_bolsa(L).
    
valida(X,Y):-
    X>= 1,5>=X,Y>=1,5>=Y.

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

rodapie_rango(X,Y,S):-
rodapie(X,Xr),rodapie(Y,Yr),S is Yr-Xr.

combo(Player,X,Y,R):-
    linea(X,Y,1,0,Player,0),
    linea(X,Y,-1,0,Player,0),
    linea(X,Y,0,1,Player,S1),
    linea(X,Y,0,-1,Player,S2),!,
    R is S1+S2+1.
combo(Player,X,Y,R):-
    linea(X,Y,1,0,Player,S1),
    linea(X,Y,-1,0,Player,S2),
    linea(X,Y,0,1,Player,0),
    linea(X,Y,0,-1,Player,0),!,
    R is S1+S2+1.
combo(Player,X,Y,R):-
    linea(X,Y,1,0,Player,S1),
    linea(X,Y,-1,0,Player,S2),
    linea(X,Y,0,1,Player,S3),
    linea(X,Y,0,-1,Player,S4),!,
    R is S1+S2+S3+S4+2.
jugada(X):-
    clause(jugador_actual(X),true).
selecciona_jugada(Jugador,R):-
    clause(patrones(Jugador,X),true),
    filas(X,Jugador,[[-1,-1,-30,-1],0],R).

pon_centro([],_).
pon_centro([X|L],X):-!,
    pon_centro(L,X).
pon_centro([X|L],Y):-
    asserta(centro(X)),
    pon_centro(L,Y).

filas([],_,R,R).
filas([X|L],Jugador,[[_,_,Res,_],P],R):-
    fila_completa(Jugador,X,[[TempX,TempY,Temp_Res,Temp_F],TP]),
    (Temp_Res-TP)>=(Res-P),!,
    filas(L,Jugador,[[TempX,TempY,Temp_Res,Temp_F],TP],R).

filas([X|L],Jugador,[[PX,PY,Res,F],P],R):-
    fila_completa(Jugador,X,[[_,_,Temp_Res,_],TP]),
    (Temp_Res-TP)<(Res-P),
    filas(L,Jugador,[[PX,PY,Res,F],P],R).
libres(Fila,Jugador,R):-
    findall([Fila,X],pared(Fila,X,Jugador,_),L),
    subtract([[Fila,1],[Fila,2],[Fila,3],[Fila,4],[Fila,5]], L, R).



fila_completa(Jugador,[Tamano,Tomados,Color],[[X1,Y1,Res1,F1],P1]):-
    fila_completa_centro(Jugador,[Tamano,Tomados,Color],[[X1,Y1,Res1,F1],P1]),
    fila_completa_fabrica(Jugador,[Tamano,Tomados,Color],[[_,_,Res2,_],P2]),
    Res1-P1>Res2-P2,!
    .

fila_completa(Jugador,[Tamano,Tomados,Color],[[X1,Y1,Res1,F1],P1]):-
    fila_completa_fabrica(Jugador,[Tamano,Tomados,Color],[[X1,Y1,Res1,F1],P1])
    .

fila_completa_centro(Jugador,[Tamano,0,_],[Resp,P]):-
    !,
    libres(Tamano,Jugador,R),
    combos(R,Jugador,C),
    findall(X,centro(X),L),
    recorre_combos(C,Jugador,Tamano,[[L,10]],[-1,-1,-30,-1],Resp),
    clause(penalizacion(P),true).

fila_completa_centro(Jugador,[Tamano,Tomados,Color],[Resp,P]):-
    PosY is (Tamano-Color) mod 5,
    combos([Tamano,PosY],Jugador,C),
    findall(X,centro(X),L),
    Necesita is Tamano-Tomados,
    recorre_combos(C,Jugador,Necesita,[[L,10]],[-1,-1,-30,-1],Resp),
    clause(penalizacion(P),true).

fila_completa_fabrica(Jugador,[Tamano,0,_],[Resp,0]):-
    !,
    libres(Tamano,Jugador,R),
    combos(R,Jugador,C),
    findall([X,Y],fabrica(X,Y),L),
    recorre_combos(C,Jugador,Tamano,L,[-1,-1,-30,-1],Resp).

fila_completa_fabrica(Jugador,[Tamano,Tomados,Color],[Resp,0]):-
    PosY is (Tamano-Color) mod 5,
    combos([[Tamano,PosY]],Jugador,C),
    findall([X,Y],fabrica(X,Y),L),
    Necesita is Tamano-Tomados,
    recorre_combos(C,Jugador,Necesita,L,[-1,-1,-30,-1],Resp).

combos([],_,[]).
combos([[X,Y]|L],Jugador,[[X,Y,S1]|R]):-
    combo(Jugador,X,Y,S1),
    combos(L,Jugador,R).

recorre_combos([],_,_,_,[FPosX,FPosY,FRes,FNo],[FPosX,FPosY,FRes,FNo]).

recorre_combos([[X,Y,Suma]|L],Jugador,Necesita,Fabricas,[_,_,Res,_],[FPosX,FPosY,FRes,FNo]):-
    Color is (X-Y) mod 5,
    %write(Color),write(" color\n"),
    busca_factorias(Fabricas,Necesita,Color,Suma,Jugador,[-30,_],[TRes,TNo])
    ,TRes>=Res,!,
    %write([X,Y,TRes,TNo]),write("\n"),
    recorre_combos(L,Jugador,Necesita,Fabricas,[X,Y,TRes,TNo],[FPosX,FPosY,FRes,FNo]).
recorre_combos([[X,Y,Suma]|L],Jugador,Necesita,Fabricas,[PosX,PosY,Res,No],[FPosX,FPosY,FRes,FNo]):-
    Color is X-Y mod 5,
    busca_factorias(Fabricas,Necesita,Color,Suma,Jugador,[-30,_],[TRes,_])
    ,TRes<Res,!,
    %write([PosX,PosY,Res,No]),write("\n"),
    recorre_combos(L,Jugador,Necesita,Fabricas,[PosX,PosY,Res,No],[FPosX,FPosY,FRes,FNo]).


cantidad(_,[],0).
cantidad(Color,[Color|L],Res):-!,
    cantidad(Color,L,R),
    Res is R+1.
cantidad(Color,[_|L],R):-
    cantidad(Color,L,R).

por_encima([X,_],Necesita,Color,_,_,[-30,-1]):-
    cantidad(Color,X,Ac),
    Ac<Necesita,!.


por_encima([X,No],Necesita,Color,Suma,Jugador,[Res,No]):-
    cantidad(Color,X,Ac),
    clause(jugador_rodapie(Jugador,Act_Rod),true),
    Temp_Rod is Act_Rod+(Ac-Necesita),
    rodapie_rango(Act_Rod,Temp_Rod,Dif),
    Res is Suma+Dif.

busca_factorias([],_,_,_,_,[FRes,FNo],[FRes,FNo]).

busca_factorias([X|L],Necesita,Color,Suma,Jugador,[Res,_],[FRes,FNo]):-
    por_encima(X,Necesita,Color,Suma,Jugador,[Temp_Res,Temp_No]),
    Temp_Res>=Res,!,
    busca_factorias(L,Necesita,Color,Suma,Jugador,[Temp_Res,Temp_No],[FRes,FNo]).

busca_factorias([X|L],Necesita,Color,Suma,Jugador,[Res,No],[FRes,FNo]):-
    por_encima(X,Necesita,Color,Suma,Jugador,[Temp_Res,_]),
    Temp_Res<Res,
    busca_factorias(L,Necesita,Color,Suma,Jugador,[Res,No],[FRes,FNo]).

dibuja_fabricas([]).
dibuja_fabricas([[X,F]|L]):-
    write(X),write(' '),write(F),write("\n"),
    dibuja_fabricas(L).
    
dib_fabricas:-
    findall([X,Y],fabrica(Y,X),L), dibuja_fabricas(L).
    
        