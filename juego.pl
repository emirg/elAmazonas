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

:- dynamic serpiente/2, orientacion/2.

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

addBegin(X,L,R):- append([X],L,R).

addEnd(X,L,R):- append(L,[X],R).

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

% Posiciones validas dada una direccion
allowed(left,down) :- !.
allowed(left,up) :- !.
allowed(right,down) :- !.
allowed(right,up) :- !.
allowed(X,X) :- !,fail.
allowed(right,left) :- !,fail.
allowed(up,down) :- !,fail.
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

%%%%%%%%%
% Roles %
%%%%%%%%%
role(j1). %Charlie / Jugador 1
role(j2). %Simon / Jugador 2

%%%%%%%%%%%%%%%%%%%%%%%
% Limites del tablero %
%%%%%%%%%%%%%%%%%%%%%%%
limites_tablero(20,20). %(cantidad_filas,cantidad_columnas)
cant_casillas_total(N):- limites_tablero(X,Y), N is X*Y.


%%%%%%%%%%%%%%%%%%
% Estado inicial %
%%%%%%%%%%%%%%%%%%
% Celdas:
%   a -> Agua
%   b -> Barril
%   i -> Isla
%   sj1 -> Serpiente 1
%   sj2 -> Serpiente 2

init(cell(1,1,a)).
init(cell(1,2,a)).
init(cell(1,3,a)).
init(cell(1,4,a)).
init(cell(1,5,a)).
init(cell(1,6,a)).
init(cell(1,7,a)).
init(cell(1,8,a)).
init(cell(1,9,a)).
init(cell(1,10,a)).
init(cell(1,11,a)).
init(cell(1,12,a)).
init(cell(1,13,a)).
init(cell(1,14,a)).
init(cell(1,15,a)).
init(cell(1,16,a)).
init(cell(1,17,a)).
init(cell(1,18,a)).
init(cell(1,19,a)).
init(cell(1,20,a)).
init(cell(2,1,a)).
init(cell(2,2,a)).
init(cell(2,3,a)).
init(cell(2,4,a)).
init(cell(2,5,a)).
init(cell(2,6,a)).
init(cell(2,7,a)).
init(cell(2,8,a)).
init(cell(2,9,a)).
init(cell(2,10,a)).
init(cell(2,11,a)).
init(cell(2,12,a)).
init(cell(2,13,a)).
init(cell(2,14,a)).
init(cell(2,15,a)).
init(cell(2,16,a)).
init(cell(2,17,j1)).
init(cell(2,18,j1)).
init(cell(2,19,j1)).
init(cell(2,20,a)).
init(cell(3,1,a)).
init(cell(3,2,a)).
init(cell(3,3,a)).
init(cell(3,4,a)).
init(cell(3,5,a)).
init(cell(3,6,a)).
init(cell(3,7,a)).
init(cell(3,8,a)).
init(cell(3,9,a)).
init(cell(3,10,a)).
init(cell(3,11,a)).
init(cell(3,12,a)).
init(cell(3,13,a)).
init(cell(3,14,a)).
init(cell(3,15,a)).
init(cell(3,16,a)).
init(cell(3,17,p)).
init(cell(3,18,a)).
init(cell(3,19,a)).
init(cell(3,20,a)).
init(cell(4,1,a)).
init(cell(4,2,a)).
init(cell(4,3,a)).
init(cell(4,4,a)).
init(cell(4,5,b)).
init(cell(4,6,a)).
init(cell(4,7,a)).
init(cell(4,8,a)).
init(cell(4,9,a)).
init(cell(4,10,a)).
init(cell(4,11,a)).
init(cell(4,12,a)).
init(cell(4,13,a)).
init(cell(4,14,a)).
init(cell(4,15,a)).
init(cell(4,16,a)).
init(cell(4,17,a)).
init(cell(4,18,a)).
init(cell(4,19,a)).
init(cell(4,20,a)).
init(cell(5,1,a)).
init(cell(5,2,a)).
init(cell(5,3,a)).
init(cell(5,4,a)).
init(cell(5,5,a)).
init(cell(5,6,a)).
init(cell(5,7,a)).
init(cell(5,8,a)).
init(cell(5,9,a)).
init(cell(5,10,a)).
init(cell(5,11,a)).
init(cell(5,12,a)).
init(cell(5,13,a)).
init(cell(5,14,i)).
init(cell(5,15,i)).
init(cell(5,16,i)).
init(cell(5,17,i)).
init(cell(5,18,a)).
init(cell(5,19,a)).
init(cell(5,20,a)).
init(cell(6,1,a)).
init(cell(6,2,a)).
init(cell(6,3,a)).
init(cell(6,4,a)).
init(cell(6,5,a)).
init(cell(6,6,a)).
init(cell(6,7,a)).
init(cell(6,8,a)).
init(cell(6,9,a)).
init(cell(6,10,a)).
init(cell(6,11,a)).
init(cell(6,12,a)).
init(cell(6,13,a)).
init(cell(6,14,i)).
init(cell(6,15,i)).
init(cell(6,16,i)).
init(cell(6,17,i)).
init(cell(6,18,a)).
init(cell(6,19,a)).
init(cell(6,20,a)).
init(cell(7,1,a)).
init(cell(7,2,a)).
init(cell(7,3,a)).
init(cell(7,4,i)).
init(cell(7,5,i)).
init(cell(7,6,i)).
init(cell(7,7,i)).
init(cell(7,8,a)).
init(cell(7,9,a)).
init(cell(7,10,a)).
init(cell(7,11,a)).
init(cell(7,12,a)).
init(cell(7,13,a)).
init(cell(7,14,i)).
init(cell(7,15,i)).
init(cell(7,16,i)).
init(cell(7,17,i)).
init(cell(7,18,a)).
init(cell(7,19,a)).
init(cell(7,20,a)).
init(cell(8,1,a)).
init(cell(8,2,a)).
init(cell(8,3,a)).
init(cell(8,4,i)).
init(cell(8,5,i)).
init(cell(8,6,i)).
init(cell(8,7,i)).
init(cell(8,8,a)).
init(cell(8,9,a)).
init(cell(8,10,a)).
init(cell(8,11,a)).
init(cell(8,12,a)).
init(cell(8,13,a)).
init(cell(8,14,a)).
init(cell(8,15,a)).
init(cell(8,16,a)).
init(cell(8,17,a)).
init(cell(8,18,a)).
init(cell(8,19,a)).
init(cell(8,20,a)).
init(cell(9,1,a)).
init(cell(9,2,a)).
init(cell(9,3,a)).
init(cell(9,4,i)).
init(cell(9,5,i)).
init(cell(9,6,i)).
init(cell(9,7,i)).
init(cell(9,8,a)).
init(cell(9,9,a)).
init(cell(9,10,b)).
init(cell(9,11,a)).
init(cell(9,12,a)).
init(cell(9,13,a)).
init(cell(9,14,a)).
init(cell(9,15,a)).
init(cell(9,16,a)).
init(cell(9,17,a)).
init(cell(9,18,a)).
init(cell(9,19,a)).
init(cell(9,20,a)).
init(cell(10,1,a)).
init(cell(10,2,a)).
init(cell(10,3,a)).
init(cell(10,4,a)).
init(cell(10,5,a)).
init(cell(10,6,a)).
init(cell(10,7,a)).
init(cell(10,8,a)).
init(cell(10,9,a)).
init(cell(10,10,a)).
init(cell(10,11,a)).
init(cell(10,12,a)).
init(cell(10,13,a)).
init(cell(10,14,a)).
init(cell(10,15,a)).
init(cell(10,16,a)).
init(cell(10,17,a)).
init(cell(10,18,a)).
init(cell(10,19,a)).
init(cell(10,20,a)).
init(cell(11,1,a)).
init(cell(11,2,a)).
init(cell(11,3,a)).
init(cell(11,4,a)).
init(cell(11,5,a)).
init(cell(11,6,a)).
init(cell(11,7,a)).
init(cell(11,8,a)).
init(cell(11,9,a)).
init(cell(11,10,a)).
init(cell(11,11,a)).
init(cell(11,12,a)).
init(cell(11,13,a)).
init(cell(11,14,a)).
init(cell(11,15,a)).
init(cell(11,16,a)).
init(cell(11,17,a)).
init(cell(11,18,a)).
init(cell(11,19,a)).
init(cell(11,20,a)).
init(cell(12,1,a)).
init(cell(12,2,a)).
init(cell(12,3,a)).
init(cell(12,4,a)).
init(cell(12,5,a)).
init(cell(12,6,a)).
init(cell(12,7,a)).
init(cell(12,8,a)).
init(cell(12,9,a)).
init(cell(12,10,a)).
init(cell(12,11,a)).
init(cell(12,12,a)).
init(cell(12,13,a)).
init(cell(12,14,a)).
init(cell(12,15,a)).
init(cell(12,16,a)).
init(cell(12,17,a)).
init(cell(12,18,a)).
init(cell(12,19,a)).
init(cell(12,20,a)).
init(cell(13,1,a)).
init(cell(13,2,a)).
init(cell(13,3,a)).
init(cell(13,4,a)).
init(cell(13,5,p)).
init(cell(13,6,a)).
init(cell(13,7,a)).
init(cell(13,8,a)).
init(cell(13,9,a)).
init(cell(13,10,a)).
init(cell(13,11,a)).
init(cell(13,12,a)).
init(cell(13,13,a)).
init(cell(13,14,a)).
init(cell(13,15,a)).
init(cell(13,16,a)).
init(cell(13,17,a)).
init(cell(13,18,a)).
init(cell(13,19,a)).
init(cell(13,20,a)).
init(cell(14,1,a)).
init(cell(14,2,a)).
init(cell(14,3,a)).
init(cell(14,4,a)).
init(cell(14,5,a)).
init(cell(14,6,a)).
init(cell(14,7,a)).
init(cell(14,8,a)).
init(cell(14,9,a)).
init(cell(14,10,a)).
init(cell(14,11,a)).
init(cell(14,12,a)).
init(cell(14,13,a)).
init(cell(14,14,a)).
init(cell(14,15,a)).
init(cell(14,16,b)).
init(cell(14,17,a)).
init(cell(14,18,a)).
init(cell(14,19,a)).
init(cell(14,20,a)).
init(cell(15,1,a)).
init(cell(15,2,a)).
init(cell(15,3,a)).
init(cell(15,4,a)).
init(cell(15,5,a)).
init(cell(15,6,a)).
init(cell(15,7,a)).
init(cell(15,8,a)).
init(cell(15,9,a)).
init(cell(15,10,a)).
init(cell(15,11,a)).
init(cell(15,12,a)).
init(cell(15,13,a)).
init(cell(15,14,a)).
init(cell(15,15,a)).
init(cell(15,16,a)).
init(cell(15,17,a)).
init(cell(15,18,a)).
init(cell(15,19,a)).
init(cell(15,20,a)).
init(cell(16,1,a)).
init(cell(16,2,a)).
init(cell(16,3,a)).
init(cell(16,4,a)).
init(cell(16,5,a)).
init(cell(16,6,a)).
init(cell(16,7,a)).
init(cell(16,8,a)).
init(cell(16,9,a)).
init(cell(16,10,a)).
init(cell(16,11,a)).
init(cell(16,12,a)).
init(cell(16,13,a)).
init(cell(16,14,a)).
init(cell(16,15,a)).
init(cell(16,16,a)).
init(cell(16,17,a)).
init(cell(16,18,a)).
init(cell(16,19,a)).
init(cell(16,20,a)).
init(cell(17,1,a)).
init(cell(17,2,a)).
init(cell(17,3,a)).
init(cell(17,4,a)).
init(cell(17,5,a)).
init(cell(17,6,a)).
init(cell(17,7,a)).
init(cell(17,8,a)).
init(cell(17,9,a)).
init(cell(17,10,i)).
init(cell(17,11,i)).
init(cell(17,12,i)).
init(cell(17,13,i)).
init(cell(17,14,a)).
init(cell(17,15,a)).
init(cell(17,16,a)).
init(cell(17,17,j2)).
init(cell(17,18,j2)).
init(cell(17,19,j2)).
init(cell(17,20,a)).
init(cell(18,1,a)).
init(cell(18,2,a)).
init(cell(18,3,a)).
init(cell(18,4,b)).
init(cell(18,5,a)).
init(cell(18,6,a)).
init(cell(18,7,a)).
init(cell(18,8,a)).
init(cell(18,9,a)).
init(cell(18,10,i)).
init(cell(18,11,i)).
init(cell(18,12,i)).
init(cell(18,13,i)).
init(cell(18,14,a)).
init(cell(18,15,a)).
init(cell(18,16,a)).
init(cell(18,17,p)).
init(cell(18,18,a)).
init(cell(18,19,a)).
init(cell(18,20,a)).
init(cell(19,1,a)).
init(cell(19,2,a)).
init(cell(19,3,a)).
init(cell(19,4,a)).
init(cell(19,5,a)).
init(cell(19,6,a)).
init(cell(19,7,a)).
init(cell(19,8,a)).
init(cell(19,9,a)).
init(cell(19,10,i)).
init(cell(19,11,i)).
init(cell(19,12,i)).
init(cell(19,13,i)).
init(cell(19,14,a)).
init(cell(19,15,a)).
init(cell(19,16,a)).
init(cell(19,17,a)).
init(cell(19,18,a)).
init(cell(19,19,a)).
init(cell(19,20,a)).
init(cell(20,1,a)).
init(cell(20,2,a)).
init(cell(20,3,a)).
init(cell(20,4,a)).
init(cell(20,5,a)).
init(cell(20,6,a)).
init(cell(20,7,a)).
init(cell(20,8,a)).
init(cell(20,9,a)).
init(cell(20,10,a)).
init(cell(20,11,a)).
init(cell(20,12,a)).
init(cell(20,13,a)).
init(cell(20,14,a)).
init(cell(20,15,a)).
init(cell(20,16,a)).
init(cell(20,17,a)).
init(cell(20,18,a)).
init(cell(20,19,a)).
init(cell(20,20,a)).
init(serpiente(j1,left,[(2,17),(2,18),(2,19)])). % Charlie
init(serpiente(j2,left,[(17,17),(17,18),(17,19)])). % Simon
init(control(j1)). %Empieza jugando Charlie

 

%Proposiciones base
base(control(X)):- role(X).
base(cell(X,Y,a)):- index(X),index(Y).
base(cell(X,Y,i)):- index(X),index(Y).
base(cell(X,Y,b)):- index(X),index(Y).
base(cell(X,Y,R)):- role(R),index(X),index(Y).
base(serpiente(R,D,L)):- role(R), direccion(D),is_list(L).

index( 1 ).
index( 2 ).
index( 3 ).
index( 4 ).
index( 5 ).
index( 6 ).
index( 7 ).
index( 8 ).
index( 9 ).
index( 10 ).
index( 11 ).
index( 12 ).
index( 13 ).
index( 14 ).
index( 15 ).
index( 16 ).
index( 17 ).
index( 18 ).
index( 19 ).
index( 20 ).

%%%%%%%%%%
% Inputs %
%%%%%%%%%%
%Posibles valores que pueden tener las entradas
input(R,move(X)):- role(R),direccion(X).
input(R,noop):-role(R).

 
%%%%%%%%%%%%%%%%%%%%%%%
% Movimientos legales %
%%%%%%%%%%%%%%%%%%%%%%%
legal(J1,move(X)) :-
  serpiente(J1,D,L), 
  allowed(D,X), 
  t(control(J1)).

legal(J2,noop) :-
  role(J1),
  distinct(J1,J2).

% Posiblemente no la necesitemos
can_move(R,(X1,Y1),(X2,Y2)):- 
  orientacion(R,A,B), 
  X2 is X1 + A, 
  Y2 is Y1 + B,
  limites_tablero(LX,LY), X2>0, X2=<LX, Y2>0, Y2=<LY.


% snakeMember / 3 - snakeMember (Snake, XPoint, YPoint)
% Comprueba si los puntos X, Y pasados no pertenecen a una Snake
snakeMember([],_,_) :- !, fail.
snakeMember([[X|[Y|_]]|_],X,Y) :- !.
snakeMember([_|Tail],X,Y) :- 
    snakeMember(Tail,X,Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% OBTENCION DEL PROXIMO ESTADO %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%próximo estado
next(cell(M,N,x)) :-

      does(x,mark(M,N)),

      t(cell(M,N,a)).

 

next(cell(M,N,o)) :-

      does(o,mark(M,N)),

      t(cell(M,N,a)).

 

next(cell(M,N,W)) :-

      t(cell(M,N,W)),

      distinct(W,a).

 

next(cell(M,N,a)) :-

      does(_W,mark(J,_K)),

      t(cell(M,N,a)),

      distinct(M,J).

 

next(cell(M,N,a)) :-

      does(_W,mark(_J,K)),

      t(cell(M,N,a)),

      distinct(N,K).

 

next(control(o)) :-

      t(control(x)).

 

next(control(x)) :-

      t(control(o)).

 

%Cambiamos las reglas pierde el que hace TATETI

%goal(x,100) :- line(x),\+line(o).

%goal(x,50) :-  \+line(x), \+line(o).

%goal(x,0) :- \+line(x), line(o).

 

%goal(o,100) :- \+line(x), line(o).

%goal(o,50) :- \+line(x), \+line(o).

%goal(o,0) :- line(x), \+line(o).

 

goal(x,0) :- line(x),\+line(o).

goal(x,50) :-  \+line(x), \+line(o).

goal(x,100) :- \+line(x), line(o).

 

goal(o,0) :- \+line(x), line(o).

goal(o,50) :- \+line(x), \+line(o).

goal(o,100) :- line(x), \+line(o).

 



terminal :- line(x).

terminal :- line(o).

terminal :- \+open.

 

open :- t(cell(X,Y,a)),

index(X),

index(Y).

 

 

 

distinct(X,Y):-

X\==Y.

 

%inicializa estado inicial y borra historial

:-dynamic t/1,h/2,estado/1,does/2.

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

%mark calcula próximo estado

% crea proximo estado

 

juego:- \+terminal,

retractall(does(_X,_A)),

inserta_acciones,

proximo_estado,

retractall(t(_Y)),

crea_estado,

imprime,

juego.

 

juego:- terminal,

                goal(x,Px),goal(o,Po),

                display('x gano '),display(Px),display(' puntos y o gano '),display(Po),display(' puntos.').

 

% busca las nuevas acciones de los jugadores y las inserta

inserta_acciones:- t(control(X)),

   jugador(X,A), legal(X,A),

   assert(does(X,A)),

   role(O), distinct(X,O),

   assert(does(O,noop)).

 

%calcula el próximo estado

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

N2 is N +1,

assert(estado(N2)).

%imprime estado actual del juego
imprime:-
	estado(E),
	display('Estado: '),display(E),nl,
	t(control(X)),
	display('Control: '),display(X),nl,
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
	imprime_fila(11),
	imprime_fila(12),
  imprime_fila(13),
	imprime_fila(14),
	imprime_fila(15),
  imprime_fila(16),
	imprime_fila(17),
	imprime_fila(18),
  imprime_fila(19),
	imprime_fila(20),
	display('********').

imprime_fila(N):-

    t(cell(N,1,C1)),t(cell(N,2,C2)),t(cell(N,3,C3)),

    t(cell(N,4,C4)),t(cell(N,5,C5)),t(cell(N,6,C6)),

    t(cell(N,7,C7)),t(cell(N,8,C8)),t(cell(N,9,C9)),

    t(cell(N,10,C10)),t(cell(N,11,C11)),t(cell(N,12,C12)),

    t(cell(N,13,C13)),t(cell(N,14,C14)),t(cell(N,15,C15)),

    t(cell(N,16,C16)),t(cell(N,17,C17)),t(cell(N,18,C18)),

    t(cell(N,19,C19)),t(cell(N,20,C20)),

    display(C1),display(C2),display(C3),

    display(C4),display(C5),display(C6),

    display(C7),display(C8),display(C9),

    display(C10),display(C11),display(C12),

    display(C13),display(C14),display(C15),

    display(C16),display(C17),display(C18),

    display(C19),display(C20),nl.

 

 

%desarrollo jugador o      

jugador(o,A):-

legal(o,A).

 

 

%desarrollo jugador x

jugador(x,X):-

%legal(x,X).

display('Ingrese próximo movimiento:'),

read(X).

% display('Ingrese Marca en Y:'),

% read_term(Y,[]).

%

/** <examples>

?- inicio,juego.

*/

%
