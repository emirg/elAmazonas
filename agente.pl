agente2(Rol,MejorAccion):- 
  t(control(Rol)), 
  getPersonaCercana((X,Y),_D),
  %getAccionesLegales([],AccionesLegales),
  %write('Acciones validas: '),write(AccionesLegales),nl,
  getVecinosValidos((X,Y),[],VecinosValidos),
  write('Vecinos validos: '),write(VecinosValidos),nl,
  getMejorAccion(VecinosValidos,_,200,MejorAccion,_),
  write('Mejor accion: '),write(MejorAccion),nl.
  

% Obtiene las acciones legales a realizar en el estado
getAccionesLegales(L0,L1) :-
  t(control(J)),
  legal(J,A),
  \+in(A,L0),
  distinct(A,noop),
  getAccionesLegales([A|L0],L1).

getAccionesLegales(L,L).

% Obtiene la distancia de un punto a otro
distancia((X1,Y1),(X2,Y2),D) :-
  D is (abs(X1 - X2) + abs(Y1 -Y2)).


% Obtiene los vecinos validos (es decir, que sean agua o persona y que esten en direccion correcta)
getVecinosValidos((XP,YP),L0,L1):-
  %write('Xpersona  '),write(XP),nl,
  %write('Ypersona  '),write(YP),nl,
  t(control(Rol)),
  t(serpiente(Rol,D,[(X,Y)|_],_)),
  allowed(D,M),
  %write('allowed D '),write(D),nl,
  %write('allowed M '),write(M),nl,
  decrease(M,A,B),
  X1 is X+A,
  Y1 is Y+B,nl,
  %write('XValido  '),write(X1),nl,
  %write('YValido  '),write(Y1),nl,
  t(cell(X1,Y1,C)),
  %write('cell con C  '),write(C),nl,
  (\+distinct(C,a);\+distinct(C,p)),
  %distinct(C,b),
  %distinct(C,i),
  role(Rol1),
  distinct(C,Rol1),
  %write(C),
  \+in((X1,Y1,M,_),L0),
  distancia((X1,Y1),(XP,YP),Dist),
  getVecinosValidos((XP,YP),[(X1,Y1,M,Dist)|L0],L1).

getVecinosValidos((_XP,_YP),L0,L0).


% Obtiene la persona mas cercana de la cabeza de la serpiente
getPersonaCercana((XP,YP),D):-
  t(control(Rol)),
  t(serpiente(Rol,_,[(X,Y)|_],_)),
  getPersonaCercanaAux((X,Y),[],[(XP,YP,D)|_L]).

getPersonaCercanaAux((XCabeza,YCabeza),L,R):-
  t(cell(X,Y,p)),
  %nl,write('XP  '),write(X),nl,write('YP  '),write(Y),nl,
  \+in((X,Y,_),L),
  distancia((XCabeza,YCabeza),(X,Y),D),
  head(L,(_,_,D1)),
  D < D1,
  getPersonaCercanaAux((XCabeza,YCabeza),[(X,Y,D)|L],R).

getPersonaCercanaAux((XCabeza,YCabeza),[],R):-
  t(cell(X,Y,p)),
  distancia((XCabeza,YCabeza),(X,Y),D),
  getPersonaCercanaAux((XCabeza,YCabeza),[(X,Y,D)],R),nl.
  %write('X  '),write(X),nl,write('Y  '),write(Y).

getPersonaCercanaAux((_X,_Y),L,L).




% Obtiene de una lista, la mejor acción.
getMejorAccion([(_X,_Y,Accion,0)|_R],_,_MenorDistanciaParcial,Accion,_MenorDistancia).

getMejorAccion([(_,_,Accion,Distancia)|R],_,MenorDistanciaParcial,MejorAccion,MenorDistancia):-
  Distancia=<MenorDistanciaParcial,
  getMejorAccion(R,Accion,Distancia,MejorAccion,MenorDistancia).

getMejorAccion([(_,_,_,_)|R],MejorAccionParcial,MenorDistanciaParcial,MejorAccion,MenorDistancia):-
  getMejorAccion(R,MejorAccionParcial,MenorDistanciaParcial,MejorAccion,MenorDistancia).

getMejorAccion([],MejorAccion,MenorDistancia,MejorAccion,MenorDistancia).

