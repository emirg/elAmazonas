%Para ejecutar ?-inicio,juego. (Run!)

% Predicados de GDL
% role(a) a es un rol
% base(p) p es una proposición base
% input(r,a) a es una acción posible para el rol r
% init(p) p es verdadero en el estado inicial.
% true(p) p es verdadero en el estado actual.
% does(r,a) el rol r realiza la acción a en el estado actual.
% next(p) p es verdadero en el próximo estado.
% legal(r,a) es legal para el rol r realizar la acción a en el estado actual.
% goal(r,n) el jugador r ha conseguido n (puntos utilidades) en el estado actual.
% terminal el estado actual es terminal.

:-dynamic t/1,h/2,hs/2,estado/1,does/2,serpiente/4,control/1,score/2.

%%%%%%%%%%%%%%%%%%%%%%%%
% Utilidades de Listas %
%%%%%%%%%%%%%%%%%%%%%%%%

% head/2 - head(List, Head)
% Primer elemento de una lista
head([Head|_],Head).

% tail/2 - tail(List,Tail)
% Cola de una lista
tail([_|[]],[]) :- !.
tail([_|Tail],Tail).

% last/2 - last(List,Elem)
% Ultimo elemento de una lista
last([Head|[]],Head).
last([_|Tail],Head) :-
    last(Tail,Head).

% in/2 - in(Elem, List).
% Retorna si un elemento pertenece a una lista
in(_,[]):- !, fail.
in(X,[X|_]):- !.
in(X,[_|L]):- in(X,L).

% addBegin/3 - addBegin(Elem, List, List).
addBegin(X,L,R):- append([X],L,R).

% addEnd/3 - addEnd(Elem, List, List).
addEnd(X,L,R):- append(L,[X],R).

% removeLast/2 - removeLast(List, List).
removeLast([X|Xs], Ys) :- removeLastAux(Xs, Ys, X).           
removeLastAux([], [], _).
removeLastAux([X1|Xs], [X0|Ys], X0) :- removeLastAux(Xs, Ys, X1). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicados para definir acciones de las serpientes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Direcciones
direccion(left).
direccion(right).
direccion(up).
direccion(down).

% Mapeo direcciones a enteros
mapeoDireccion(1,down).
mapeoDireccion(2,right).
mapeoDireccion(3,left).
mapeoDireccion(4,up).

% Posiciones validas dada una direccion
/*allowed(left,down):- !.
allowed(left,up):- !.
allowed(right,down):- !.
allowed(right,up):- !.
allowed(X,X):- !.
allowed(right,left) :- !,fail.
allowed(up,down) :- fail.
allowed(X,Z) :- allowed(Z,X).*/


allowed(left,down).
allowed(left,up).
allowed(right,down).
allowed(right,up).
allowed(down,left).
allowed(down,right).
allowed(up,left).
allowed(up,right).
allowed(X,X):- !.
allowed(right,left) :- fail.
allowed(up,down) :- fail.
allowed(X,Z) :- allowed(Z,X).

% Posiciones opuestas
oposite(left,right).
oposite(right,left).
oposite(up,down).
oposite(down,up).

% Modificador de posición
decrease(left,0,-1).
decrease(right,0,1).
decrease(up,-1,0).
decrease(down,1,0).

% Predicados para establecer los valores posibles. (No se usa)
agua(a).
barril(b).
persona(p).
isla(i).

%%%%%%%%%
% Roles %
%%%%%%%%%

role(c). %Charlie / Jugador 1
role(s). %Simon / Jugador 2

%%%%%%%%%%%%%%%%%%%%%%%
% Limites del tablero %
%%%%%%%%%%%%%%%%%%%%%%%
limites_tablero(12,12). %(cantidad_filas,cantidad_columnas)
cant_casillas_total(N):- limites_tablero(X,Y), N is X*Y.


%%%%%%%%%%%%%%%%%%
% Estado inicial %
%%%%%%%%%%%%%%%%%%
% Celdas:
%   a -> Agua
%   b -> Barril
%   i -> Isla
%   p -> Persona
%   s -> Serpiente 1
%   c-> Serpiente 2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Asigno mapa %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include('mapaConParedes.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Puntacion inicial %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(score(c,0)).
init(score(s,0)).

init(control(c)). %Empieza jugando Charlie


%Proposiciones base
base(control(X)):- role(X).
base(cell(X,Y,a)):- index(X),index(Y).
base(cell(X,Y,i)):- index(X),index(Y).
base(cell(X,Y,b)):- index(X),index(Y).
base(cell(X,Y,p)):- index(X),index(Y).
base(cell(X,Y,R)):- role(R),index(X),index(Y).
base(serpiente(R,D,L,V)):- role(R), direccion(D),is_list(L),V>0.
base(score(R,Valor)):- role(R), Valor >= 0.

index(0).
index(1).
index(2).
index(3).
index(4).
index(5).
index(6).
index(7).
index(8).
index(9).
index(10).
index(11).
/*
index( 12 ).
index( 13 ).
index( 14 ).
index( 15 ).
index( 16 ).
index( 17 ).
index( 18 ).
index( 19 ).
index( 20 ).
*/

%%%%%%%%%%
% Inputs %
%%%%%%%%%%
%Posibles valores que pueden tener las entradas
input(R,move(X)):- role(R),direccion(X).
input(R,noop):-role(R).

 
%%%%%%%%%%%%%%%%%%%%%%%
% Movimientos legales %
%%%%%%%%%%%%%%%%%%%%%%%
legal(J1,move(M)) :-
  t(serpiente(J1,D,[(X,Y)|_],V)),
  V > 0, 
  allowed(D,M),   
  decrease(M,A,B),
  L1 is X+A,
  L2 is Y+B,
  limites_tablero(LX,LY), L1>0, L1=<LX, L2>0, L2=<LY,
  t(control(J1)).

legal(J2,noop) :-
  role(J1),
  \+control(J2),
  distinct(J1,J2).

% Posiblemente no la necesitemos
can_move(R,(X1,Y1),(X2,Y2)):- 
  orientacion(R,A,B), 
  X2 is X1 + A, 
  Y2 is Y1 + B,
  limites_tablero(LX,LY), X2>0, X2=<LX, Y2>0, Y2=<LY.

% snakeMember / 3 - snakeMember (Snake, XPoint, YPoint)
% Comprueba si el punto (X,Y) pertenece a la serpiente
snakeMember([],_,_) :- !, fail.
snakeMember([(X,Y)|_],X,Y) :- !.
snakeMember([_|Tail],X,Y) :- 
    snakeMember(Tail,X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% OBTENCION DEL PROXIMO ESTADO %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lugar_donde_se_mueve(R,M,N):-
  does(R,move(O)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(O,A,B),
  M is X+A,
  N is Y+B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%%%%%%%%%%%% Mapa %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

% Celda -> Mantiene -> Isla
next(cell(M,N,i)) :-
  t(cell(M,N,i)).

% Celda -> Mantiene -> Agua
next(cell(M,N,a)):-
  t(cell(M,N,a)),
  lugar_donde_se_mueve(_,Q,W),
  (distinct(Q,M);distinct(W,N)).

% Celda -> Mantiene -> Barril
next(cell(M,N,b)) :-
  t(cell(M,N,b)),
  lugar_donde_se_mueve(_,Q,W),
  (distinct(Q,M) ; distinct(W,N)).

% Celda -> Mantiene -> Persona  
next(cell(M,N,p)) :-
  t(cell(M,N,p)),
  lugar_donde_se_mueve(_,Q,W),
  (distinct(Q,M) ; distinct(W,N)).



% Celda -> Mantiene -> Serpiente -> Cuerpo
next(cell(M,N,R)) :-
  t(cell(M,N,R)),
  does(R,move(_)),
  t(serpiente(R,_,S,V)),
  V > 0,
  \+last(S,(M,N)).

% Celda -> Mantiene -> Serpiente -> Cola -> Comio persona
next(cell(M,N,R)) :-
  t(cell(M,N,R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|Sv],V)),
  V > 0,
  last([(X,Y)|Sv],(M,N)),  
  decrease(D,A,B),
  XCabezaNueva is X + A,
  YCabezaNueva is Y + B,
  t(cell(XCabezaNueva,YCabezaNueva,p)).


% Celda -> Cambio -> Serpiente -> Cabeza -> Movio a agua
next(cell(M,N,R)) :- 
  t(cell(M,N,a)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(D,A,B),
  M is X+A,
  N is Y+B.

% Celda -> Cambio -> Serpiente -> Cabeza -> Comio persona
next(cell(M,N,R)) :-
  t(cell(M,N,p)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(D,A,B),
  M is X+A,
  N is Y+B.

% Celda -> Cambio -> Serpiente -> Cabeza -> Comio barril
next(cell(M,N,R)) :-  
  t(cell(M,N,b)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(D,A,B),
  M is X+A,
  N is Y+B.

% Celda -> Cambio -> Serpiente -> Cola-> No comio persona
next(cell(M,N,a)) :-
  t(cell(M,N,R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|Sv],V)),
  V > 0,
  last([(X,Y)|Sv],(M,N)),  
  decrease(D,A,B),
  XCabezaNueva is X+A,
  YCabezaNueva is Y+B,
  t(cell(XCabezaNueva,YCabezaNueva,Z)),
  distinct(Z,p).

% Celda -> Mantiene -> SerpienteEnemiga 
next(cell(M,N,J2)):-
  does(J2,noop),
  t(cell(M,N,J2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%%%%%%%%%%%% Serpiente %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Serpiente -> Movio -> Agua -> Cabeza/Cuerpo/Cola
next(serpiente(R,D,S,V)):-
  t(control(R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|Sv],V)),
  V > 0,
  decrease(D,A,B),
  removeLast([(X,Y)|Sv],L1),
  XCabezaNueva is X + A,
  YCabezaNueva is Y + B,
  t(cell(XCabezaNueva,YCabezaNueva,a)), 
  addBegin((XCabezaNueva,YCabezaNueva),L1,S).

% Serpiente -> Movio -> Isla -> Vida
next(serpiente(R,D,_,0)):-
  t(control(R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(D,A,B),
  XCabezaNueva is X + A,
  YCabezaNueva is Y + B,
  t(cell(XCabezaNueva,YCabezaNueva,i)).

% Serpiente -> Movio -> Barril -> Cabeza/Cuerpo/Cola
next(serpiente(R,D,S,V1)):-
  t(control(R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|Sv],V)),
  V > 0,
  V1 is V-1,
  decrease(D,A,B),
  removeLast([(X,Y)|Sv],L1),
  XCabezaNueva is X + A,
  YCabezaNueva is Y + B,
  t(cell(XCabezaNueva,YCabezaNueva,b)), 
  addBegin((XCabezaNueva,YCabezaNueva),L1,S).

% Serpiente -> Movio -> SerpienteEnemiga -> Vida
next(serpiente(R,D,_,0)):-
  t(control(R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(D,A,B),
  XCabezaNueva is X + A,
  YCabezaNueva is Y + B,
  t(cell(XCabezaNueva,YCabezaNueva,R2)),
  role(R2),
  distinct(R,R2).

% Serpiente -> Movio -> Persona -> Cabeza/Cuerpo/Cola
next(serpiente(R,D,S,V)):-
  t(control(R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|Sv],V)),
  V > 0,
  decrease(D,A,B),
  XCabezaNueva is X + A,
  YCabezaNueva is Y + B,
  t(cell(XCabezaNueva,YCabezaNueva,p)),
  addBegin((XCabezaNueva,YCabezaNueva),[(X,Y)|Sv],S).

next(serpiente(R,D,S,V)):-
  t(control(R2)),
  distinct(R2,R),
  t(serpiente(R,D,S,V)),
  does(R,noop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Puntaje %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Si no se mueve -> No gana puntos
next(score(Rol,Puntos)):-
  role(Rol),
  t(score(Rol,Puntos)),
  does(Rol,noop).

% Si se mueve y no come persona -> No gana puntos
next(score(Rol,P)):-
  role(Rol),
  t(score(Rol,P)),
  lugar_donde_se_mueve(Rol,M,N),
  t(cell(M,N,A)),
  distinct(A,p).

% Si come persona -> 10 puntos
next(score(Rol,P)):-
  role(Rol),
  t(score(Rol,P1)),
  lugar_donde_se_mueve(Rol,M,N),
  t(cell(M,N,p)),
  P is P1+10.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Turnos %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next(control(c)) :-
      t(control(s)).

next(control(s)) :-
      t(control(c)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Correccion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% nunca entra mismo problema de antes no llama al next movimiento invalido
%%ya no sirve
/*
% Serpiente -> Movio -> Pared -> Vida
next(serpiente(R,D,_,0)):-
  t(control(R)),
  does(R,move(D)),
  t(serpiente(R,_,[(X,Y)|_],V)),
  V > 0,
  decrease(D,A,B),
  X2 is X + A,
  Y2 is Y + B,
  limites_tablero(LX,LY), 
  (X2<0; X2>LX; Y2<0; Y2>LY).
*/
%%%%%%%%%
% Goals %
%%%%%%%%%

goal(Rol,Puntaje):-
  role(Rol),
  t(score(Rol,Puntaje)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Finaliza %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           
% terminal :- estado(25). % Para testing

terminal :- t(serpiente(_,_,_,0)). % Si una serpiente muere entonces termina el juego.

terminal :- \+open. % Si no hay mas personas termina el juego

open :- t(cell(X,Y,p)), index(X), index(Y).

distinct(X,Y):- X\==Y.

 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GESTOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test:-
  inicio,
  assert(does(c,move(right))),
  assert(does(s,noop)).


test2:-
  proximo_estado,
  retractall(t(_)),
  crea_estado.


inicio:-
    retractall(t(_)),
    retractall(h(_,_)),
    retractall(estado(_)),
    crea_estado_inicial.

%crea estado inicial
crea_estado_inicial:-
    init(X),
    \+(t(X)),
    assert(t(X)),
    assert(h(0,X)),
    crea_estado_inicial.

crea_estado_inicial:-
    assert(estado(1)).


% gestor del juego
% borra acciones viejas
% busca nuevas acciones
% calcula próximo estado
% crea proximo estado

juego:-
    \+terminal,
    retractall(does(_X,_A)),
    inserta_acciones,
    proximo_estado,
    retractall(t(_Y)),
    crea_estado,nl,
    %  imprimeT([]),
    imprime,
    juego.


juego:-
    terminal,
    nl,
    display('El juego termino'),
    nl,
    goal(s,Po),
    goal(c,Px),
    display('Charlie obtuvo '),
    display(Px),
    display(' puntos y Simon obtuvo '),
    display(Po),
    display(' puntos.').

% busca las nuevas acciones de los jugadores y las inserta
inserta_acciones:-
    t(control(X)),
    jugador(X,A),
    legal(X,A),
    assert(does(X,A)),
    role(O),
    distinct(X,O),
    assert(does(O,noop)).

% buscamos las acciones posibles, elegimos la mejor (en caso de no existir no deberia hacerse nada) y
% la ejecutamos.
proximo_estado:-
    estado(E),
    next(Y),
    \+(h(E,Y)),
    assert(h(E,Y)),
    proximo_estado.

proximo_estado.

%crea el estado actual
crea_estado:-
    estado(E),
    h(E,Y),
    \+(t(Y)),
    assert(t(Y)),
    crea_estado.

crea_estado:-
    retract(estado(N)),
    N2 is N+1,
    assert(estado(N2)).

%imprime estado actual del juego
imprime:-
  estado(E),
  display('Estado: '),display(E),nl,
  t(control(X)),
  display('Control: '),display(X),nl,
  display('Simon '),t(serpiente(s,_,_,V1)),display(V1),display(' vidas') ,nl,
  display('Charlie '),t(serpiente(c,_,_,V2)),display(V2),display(' vidas') ,nl,
  display('Puntaje Simon: '),t(score(s,P1)),display(P1),nl,
  display('Puntaje Charlie: '),t(score(c,P2)),display(P2),nl,
  imprime_fila(0),
  imprime_fila(1),
  imprime_fila(2),
  imprime_fila(3),
  imprime_fila(4),
  imprime_fila(5),
  imprime_fila(6),
  imprime_fila(7),
  imprime_fila(8),
  imprime_fila(9),
  imprime_fila(10),
  imprime_fila(11),/*
  imprime_fila(12),
  imprime_fila(13),
  imprime_fila(14),
  imprime_fila(15),
  imprime_fila(16),
  imprime_fila(17),
  imprime_fila(18),
  imprime_fila(19),
  imprime_fila(20),*/
  display('********').

imprime_fila(N):-

    t(cell(N,0,C0)),t(cell(N,1,C1)),t(cell(N,2,C2)),t(cell(N,3,C3)),

    t(cell(N,4,C4)),t(cell(N,5,C5)),t(cell(N,6,C6)),

    t(cell(N,7,C7)),t(cell(N,8,C8)),t(cell(N,9,C9)),

    t(cell(N,10,C10)),t(cell(N,11,C11)),/*t(cell(N,12,C12)),

    t(cell(N,13,C13)),t(cell(N,14,C14)),t(cell(N,15,C15)),

    t(cell(N,16,C16)),t(cell(N,17,C17)),t(cell(N,18,C18)),

    t(cell(N,19,C19)),t(cell(N,20,C20)),*/

    display(C0),display(C1),display(C2),display(C3),

    display(C4),display(C5),display(C6),

    display(C7),display(C8),display(C9),

    display(C10),display(C11),/*display(C12),

    display(C13),display(C14),display(C15),

    display(C16),display(C17),display(C18),

    display(C19),display(C20),*/nl.

 

 

% Desarrollo jugador j1
% jugador(c,noop). % Se rompe 

jugador(c,move(X)):-
    nl,display('Ingresa proximo movimiento de Charlie:'),
    agente2(c,X),
    display(X),
    nl.
% para probar agente vs agente termina en estado 31 con un empate 
/*
jugador(s,move(X)):-
    nl,display('Ingresa proximo movimiento de Charlie:'),
    agente2(s,X),
    display(X),
    nl.
*/
% Desarrollo jugador j2
jugador(s,X):- display('Ingrese próximo movimiento:'), read(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Agente Propio %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Agente 1 (Especifico de dominio)
% Busca una accion random y si es legal que la haga entonces la hace.
agente1(Rol,A):- 
  random(1,5,Random), 
  mapeoDireccion(Random,M),
  t(control(Rol)),
  legal(Rol,move(M)),
  A = move(M),!.

% Agente 2 (Especifico de dominio)
%
:- include('agente.pl').

% Agente 3 (Generico)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% Agentes Externos %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

