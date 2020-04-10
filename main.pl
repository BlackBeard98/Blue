:-dynamic bolsa/2, fabrica/2, pared/4, jugador_actual/1,proximo_jugador/1,patrones/2,centro/1,penalizacion/1,jugador_rodapie/2,jugador_suma/2,tapa/1,juego_terminado/1, jugador_filas/2.

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
myconcat([],X,X).
myconcat([X|Y],Z,[X|R]):-
    myconcat(Y,Z,R).
init_game:-
    retractall(bolsa(_,_)),retractall(fabrica(_,_)),retractall(pared(_,_,_,_)),retractall(jugador_actual(_)),retractall(proximo_jugador(_)),retractall(patrones(_,_)),retractall(jugador_filas(_,_)),
    retractall(centro(_)),retractall(penalizacion(_)),retractall(jugador_rodapie(_,_)),retractall(jugador_suma(_,_)),retractall(tapa(_)),retractall(juego_terminado(_)),
    asserta(bolsa(0,20)),asserta(bolsa(1,20)),asserta(bolsa(2,20)),asserta(bolsa(3,20)),asserta(bolsa(4,20)),
    asserta(jugador_actual(0)),
    asserta(juego_terminado(0)),
    init_rodapie,
    asserta(penalizacion(1)),
    asserta(jugador_suma(1,0)),
    asserta(jugador_suma(2,0)),
    asserta(jugador_suma(3,0)),
    asserta(jugador_suma(0,0)),
    asserta(jugador_filas(1,0)),
    asserta(jugador_filas(2,0)),
    asserta(jugador_filas(3,0)),
    asserta(jugador_filas(0,0)),
    asserta(patrones(0,[5,0,0])),asserta(patrones(0,[4,0,0])),asserta(patrones(0,[3,0,0])),asserta(patrones(0,[2,0,0])),asserta(patrones(0,[1,0,0])),
    asserta(patrones(1,[5,0,0])),asserta(patrones(1,[4,0,0])),asserta(patrones(1,[3,0,0])),asserta(patrones(1,[2,0,0])),asserta(patrones(1,[1,0,0])),
    asserta(patrones(2,[5,0,0])),asserta(patrones(2,[4,0,0])),asserta(patrones(2,[3,0,0])),asserta(patrones(2,[2,0,0])),asserta(patrones(2,[1,0,0])),
    asserta(patrones(3,[5,0,0])),asserta(patrones(3,[4,0,0])),asserta(patrones(3,[3,0,0])),asserta(patrones(3,[2,0,0])),asserta(patrones(3,[1,0,0])),extaer_bolsa(_).

   

init_rodapie:-
    retractall(jugador_rodapie(_,_)),
    asserta(jugador_rodapie(1,0)),asserta(jugador_rodapie(2,0)),asserta(jugador_rodapie(3,0)),asserta(jugador_rodapie(0,0)).

extender([],[]).

extender([[_,0]|L],R):-
    !,
    extender(L,R).

extender([[C,N]|L],[C|R]):-
    N1 is N-1,
    extender([[C,N1]|L],R).

fichas_en_bolsa([],0).
fichas_en_bolsa([X|L],R):-
    fichas_en_bolsa(L,R1),
    R is R1+X.
sacar_tapa(L):-
length(L,Tam),Tam>=36,!.
sacar_tapa(_):-
    findall([Col,Can],tapa([Col,Can]),L),
    retractall(tapa(_)),
    pon_bolsa(L).

pon_bolsa([]).
pon_bolsa([[Color,Can]|L]):-
    bolsa(Color,Bcan),
    NCan is Can + Bcan,
    retract(bolsa(Color,_)),
    asserta(bolsa(Color,NCan)),
    pon_bolsa(L).

extaer_bolsa(F):-
    findall(Z, bolsa(_,Z),K),
    sacar_tapa(K),
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
    pared(X1,Y1,Player,_),!,
    linea(X1,Y1,MX,MY,Player,Temp),
    Ans is Temp +1.

linea(X,Y,MX,MY,Player,0):-
    X1 is X+MX,
    Y1 is Y+MY,
    not(clause(pared(X1,Y1,Player,_),true)).

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

rodapie_rango(X,Y,S):-
    rodapie(X,Xr),rodapie(Y,Yr),S is Yr-Xr.

color_min([],[ColorViejo,Suma],[_,MejorSuma],[ColorViejo,Suma]):-
    Suma<MejorSuma,!.
color_min([],_,B,B).
color_min([ColorActual|L],[ColorActual,Suma],B,F):-
    !,NewSuma is Suma+1,color_min(L,[ColorActual,NewSuma],B,F).

color_min([ColorActual|L],[ColorViejo,Suma],[_,MejorSuma],F):-
    Suma<MejorSuma,!,
    color_min(L,[ColorActual,1],[ColorViejo,Suma],F).

color_min([ColorActual|L],_,B,F):-
    color_min(L,[ColorActual,1],B,F).
quick_sort2(List,Sorted):-q_sort(List,[],Sorted). 
q_sort([],Acc,Acc). q_sort([H|T],Acc,Sorted):-  pivoting(H,T,L1,L2),  q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
pivoting(_,[],[],[]). 
pivoting(H,[X|T],[X|L],G):-X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X>H,pivoting(H,T,L,G).

fabrica_color_min([],R,R).
fabrica_color_min([[Fab,Fno]|L],[_,_,BCant],R):-
    quick_sort2(Fab,SFab),
    color_min(SFab,[-1,100],[-1,100],[Color,Cant]),
    Cant<BCant,!, fabrica_color_min(L,[Fno,Color,Cant],R).

fabrica_color_min([_|L],B,R):-
    fabrica_color_min(L,B,R).
pon_en_rodapie([10,Color,Cant],Jugador):-!,
    penalizacion(Pen),
    maneja_penalizacion(Jugador,Pen),
    retractall(centro(Color)),
    jugador_rodapie(Jugador,Or),
    Nr is Or +Cant,
    retract(jugador_rodapie(Jugador,Or)),asserta(jugador_rodapie(Jugador,Nr)),
    asserta(tapa([Color,Cant])).

pon_en_rodapie([Fno,Color,Cant],Jugador):-
    fabrica(Fab,Fno),
    retract(fabrica(Fab,Fno)),
    pon_centro(Fab,Color),
    jugador_rodapie(Jugador,Or),
    Nr is Or +Cant,
    retract(jugador_rodapie(Jugador,Or)),asserta(jugador_rodapie(Jugador,Nr)),
    asserta(tapa([Color,Cant])).

juego:-
    juego_terminado(0),jugada,juego.
juego.
 jugada:-
    jugador_actual(JA),
    selecciona_jugada(JA,[[_,_,-30,_],_]),!,
    write("---------------Inicia Turno--------------\n"),
    dibuja_estado,
    findall([F,Fno],fabrica(F,Fno),Fab),
    findall(C,centro(C),Cent),
    myconcat(Fab, [[Cent,10]], Todas),
    fabrica_color_min(Todas,[-1,-1,100],R),
    pon_en_rodapie(R,JA) ,
    write("---Jugada Realizada---\n"),
    dibuja_estado,
    %total_fichas,
    [WF,WC,_] =R,
    write("Jugador: "),write(JA),write(" Todo para el piso desde: "),write(WF),write(" del color "),write(WC),write("\n"),
    retract(jugador_actual(JA)),
    NJA is (JA+1) mod 4,
    asserta(jugador_actual(NJA)),
    write("---------------Finaliza Turno--------------\n"),
    actualizacion. 

jugada:-
    write("---------------Inicia Turno--------------\n"),
    jugador_actual(JA),
    dibuja_estado,
    selecciona_jugada(JA,X), ejecuta_jugada(JA,X),
    write("---Jugada Realizada---\n"),
    dibuja_estado,
    [[PosX,PosY,_,Fab],_] = X,
    write("Jugador: "),write(JA),write(" fila "),write(PosX),write(" columna "),write(PosY),write(" desde Fabrica "),write(Fab),write("\n"),
    retract(jugador_actual(JA)),
    NJA is (JA+1) mod 4,
    asserta(jugador_actual(NJA)),
    write("---------------Finaliza Turno--------------\n"),
    actualizacion.

fin_de_juego:-
    findall(1,( pared(X,1,Jugador,real), linea(X,1,0,1,Jugador,4)  ), []),!.
fin_de_juego:-
    write("Fin de Juego\n"),
    retractall(juego_terminado(_)),
    asserta(juego_terminado(1)),
    findall(JugadorF,( pared(X,1,JugadorF,real), linea(X,1,0,1,JugadorF,4)  ),FilasCompletas),
    premia_filas(FilasCompletas),
    findall(JugadorC,( pared(1,Y,JugadorC,real), linea(1,Y,1,0,JugadorC,4)  ),ColumnasCompletas),
    premia_columnas(ColumnasCompletas),
    todos_colores([0,1,2,3]),
    write("--------------Puntuaciones--------------\n"),
    findall(1,( jugador_suma(X,Y),write("Jugador: "),write(X),write(" obtuvo: "),write(Y),write(" Puntos") , jugador_filas(X,F),
    write(" lleno "),write(F), write(" Filas\n")),_).

todos_colores([]).

todos_colores([X|L]):-
un_color(X,0,A),un_color(X,1,B),un_color(X,2,C),un_color(X,3,D),un_color(X,4,E),
jugador_suma(X,S),
News is S+A+B+C+D+E,
retract(jugador_suma(X,S)),
asserta(jugador_suma(X,News)),
todos_colores(L).

un_color(Jugador,Color,10):-
    findall(1,( pared(PX,PY,Jugador,real),  Color=:= ((PX-PY) mod 5)),Temp),length(Temp,5),!.
un_color(_,_,0).    
premia_filas([]).

premia_filas([X|L]):-
    jugador_suma(X,S),Sn is S +2,retract(jugador_suma(X,_)),asserta(jugador_suma(X,Sn)),
    jugador_filas(X,F),NF is F+1 ,retract(jugador_filas(X,_)),asserta(jugador_filas(X,NF)),
    premia_filas(L).

premia_columnas([]).

premia_columnas([X|L]):-
    jugador_suma(X,S),Sn is S +7,retract(jugador_suma(X,_)),asserta(jugador_suma(X,Sn)),premia_columnas(L).

actualizacion:-
    findall(X , fabrica(_,X),[]),findall(Y , centro(Y),[]),!,
    write("Ronda terminada "),
    retract(jugador_actual(_)),
    proximo_jugador(PJ),
    asserta(jugador_actual(PJ)),   
    actualiza_jugadores([0,1,2,3]),
    fin_de_juego,!,
    init_rodapie,
    asserta(penalizacion(1)),
    extaer_bolsa(_).
actualizacion.
actualiza_jugadores([]).
actualiza_jugadores([X|L]):-
    actualiza_jugador(X),
    actualiza_jugadores(L).

actualiza_jugador(Jugador):-
    findall([X,Y],pared(X,Y,Jugador,pendiente),L),
    sort(L,Sl),
    retractall(pared(_,_,Jugador,pendiente)),
    check_pared(Sl,Jugador,S),
    jugador_rodapie(Jugador,JR),
    rodapie(JR,Rs),
    Ts is S+Rs,
    jugador_suma(Jugador,OS),
    SF is OS + Ts,
    retract(jugador_suma(Jugador,OS)),
    asserta(jugador_suma(Jugador,SF)),
    findall([T,T,K],patrones(Jugador,[T,T,K]),NPatrones),
    limpia_patrones(NPatrones,Jugador).
limpia_patrones([],_).

limpia_patrones([X|L],Jugador):-
    limpia_patron(X,Jugador),
    limpia_patrones(L,Jugador).

limpia_patron([1,1,C],Jugador):-
    !,
    retract(patrones(Jugador,[T,T,C])),
    asserta(patrones(Jugador,[T,0,0])).

limpia_patron([T,T,C],Jugador):-
    Botar is T -1,
    retract(patrones(Jugador,[T,T,C])),
    asserta(patrones(Jugador,[T,0,0])),
    asserta(tapa([C,Botar])).

check_pared([],_,0).
check_pared([[X,Y]|L],Jugador,S):-
    combo(Jugador,X,Y,R),
    asserta(pared(X,Y,Jugador,real)),
    check_pared(L,Jugador,S1),
    S is S1+R.

selecciona_jugada(Jugador,R):-
    findall(Temp,patrones(Jugador,Temp),FPatrones),
    findall([T,T,K],patrones(Jugador,[T,T,K]),NPatrones),
    write("here"),
    subtract(FPatrones,NPatrones,Patrones),
    sort(Patrones,SPatrones),
    filas(SPatrones,Jugador,[[-1,-1,-30,-1],0],R).

ejecuta_jugada(Jugador,[[PX,PY,_,10],P]):-!,
    Color is (PX-PY) mod 5,
    findall(1,centro(Color),TL),
    length(TL,Ac),
    retractall(centro(Color)),
    patrones(Jugador,[PX,Tomados,_]),
    NewTomados is min(PX,Tomados+Ac),
    retract(patrones(Jugador,[PX,Tomados,_])),
    asserta(patrones(Jugador,[PX,NewTomados,Color])),
    pon_pared(Jugador,PX,PY,NewTomados,PX),
    Sobra is max(0,Ac-(PX-Tomados)),
    %write(Sobra),write(" ---------------Sobra\n"),
    pon_tabla(Color,Sobra),
    jugador_rodapie(Jugador,Rod),
    NewRod is Rod+Sobra,
    retract(jugador_rodapie(Jugador,Rod)),
    asserta(jugador_rodapie(Jugador,NewRod)),
    maneja_penalizacion(Jugador, P).

ejecuta_jugada(Jugador,[[PX,PY,_,Fno],P]):-
    Color is (PX-PY) mod 5,
    fabrica(Fab,Fno),
    retract(fabrica(Fab,Fno)),
    pon_centro(Fab,Color),
    patrones(Jugador,[PX,Tomados,_]),
    cantidad(Color,Fab,Ac),
    NewTomados is min(PX,Tomados+Ac),
    retract(patrones(Jugador,[PX,Tomados,_])),
    asserta(patrones(Jugador,[PX,NewTomados,Color])),
    pon_pared(Jugador,PX,PY,NewTomados,PX),
    Sobra is max(0,Ac-(PX-Tomados)),
    %write(Sobra),write(" ---------------Sobra\n"),
    pon_tabla(Color,Sobra),
    jugador_rodapie(Jugador,Rod),
    NewRod is Rod+ Sobra,
    retract(jugador_rodapie(Jugador,Rod)),
    asserta(jugador_rodapie(Jugador,NewRod)),
    maneja_penalizacion(Jugador,P).

pon_tabla(_,0):-
    !.
pon_tabla(Color,Sobra):-
    asserta(tapa([Color,Sobra])).

maneja_penalizacion(Jugador, 1):-
    retract(penalizacion(_)),asserta(penalizacion(0)),
    jugador_rodapie(Jugador,Rod),
    NewRod is Rod+ 1,
    asserta(proximo_jugador(Jugador)),
    retract(jugador_rodapie(Jugador,Rod)),
    asserta(jugador_rodapie(Jugador,NewRod)).
maneja_penalizacion(_,0).
pon_pared(Jugador,X,Y,C,C):-!,
    asserta(pared(X,Y,Jugador,pendiente)).
pon_pared(_,_,_,_,_).
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
    penalizacion(P).

fila_completa_centro(Jugador,[Tamano,Tomados,Color],[Resp,P]):-
    PosY is (Tamano-Color) mod 5,
    combos([Tamano,PosY],Jugador,C),
    findall(X,centro(X),L),
    Necesita is Tamano-Tomados,
    recorre_combos(C,Jugador,Necesita,[[L,10]],[-1,-1,-30,-1],Resp),
    penalizacion(P).

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


por_encima([X,_],_,Color,_,_,[-30,-1]):-
    cantidad(Color,X,0),!.

por_encima([X,No],Necesita,Color,_,_,[0,No]):-
    cantidad(Color,X,Ac),
    Ac<Necesita,!.


por_encima([X,No],Necesita,Color,Suma,Jugador,[Res,No]):-
    cantidad(Color,X,Ac),
    jugador_rodapie(Jugador,Act_Rod),
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

dibuja([]).

dibuja([X|L]):-
    write(X),write("\n"),
    dibuja(L).
    
dibuja_fabricas:-
    write("Fabricas\n"),
    findall([X,Y],fabrica(Y,X),L), dibuja(L).
    
dibuja_pared:-
    write("Pared\n"),
    jugador_actual(Ja),
findall([X,Y,Ja,ES],pared(X,Y,Ja,ES),L),dibuja(L).

dibuja_patrones:-
    write("Patron\n"),
    jugador_actual(Ja),
    findall(Temp,patrones(Ja,Temp),Patrones),
    sort(Patrones, SPatrones),dibuja(SPatrones).
dibuja_centro:-
    write("Centro\n"),
    findall(X,centro(X),L),dibuja(L).
dibuja_rodapie:-
    write("Rodapie\n"),
    jugador_actual(Ja),
    jugador_rodapie(Ja,X),
    write(X),write("\n").
dibuja_estado:-
    jugador_actual(JA),
    write("Jugador Actual "), write(JA),write("\n"),
    dibuja_rodapie,
    dibuja_pared,
    dibuja_patrones,
    dibuja_centro,
    dibuja_fabricas.


real_linea(X,Y,MX,MY,_,0):-
    X1 is X+MX,
    Y1 is Y+MY,
    not(valida(X1,Y1)),!.

suma_ac([],0).
suma_ac([X|L],R):-
    suma_ac(L,TR),
    R is TR+X.

total_fichas:-
    findall(1,pared(_,_,_,real),LPr),length(LPr,SumPr),
    findall(B,bolsa(_,B),LB),suma_ac(LB,SumB),
    findall(T,tapa([_,T]),LT),suma_ac(LT,ST),
    findall(Tp,patrones(_,[_,Tp,_]),LPat),suma_ac(LPat,SumPat),
    findall(TFa,(fabrica(Fab,_),length(Fab,TFa)),LFab),suma_ac(LFab,SumFab),
    findall(1,centro(_) , Cl),suma_ac(Cl,SumC),
    Total is SumPr+SumB+ST+SumPat+SumFab+SumC,
    write("total :"), write(Total),write("\n").