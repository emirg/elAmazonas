agente2(Rol,MejorAccion):-  
  getPersonaCercana((X,Y),D),
  getAccionesLegales([],AccionesLegales),
  %write('Acciones validas: '),write(AccionesLegales),nl,
  getVecinosValidos((X,Y),[],VecinosValidos),
  %write('Vecinos validos: '),write(VecinosValidos),nl,
  getMejorAccion(VecinosValidos,_,200,MejorAccion,_).
  %write('Mejor accion: '),write(MejorAccion),nl.
  


getAccionesLegales(L0,L1) :-
  t(control(J)),
  legal(J,A),
  \+in(A,L0),
  distinct(A,noop),
  getAccionesLegales([A|L0],L1).

getAccionesLegales(L,L).


distancia((X1,Y1),(X2,Y2),D) :-
  D is (abs(X1 - X2) + abs(Y1 -Y2)).


getVecinosValidos((XP,YP),L0,L1):-
  t(control(Rol)),
  t(serpiente(Rol,D,[(X,Y)|_],_)),
  allowed(D,M),
  decrease(M,A,B),
  X1 is X+A,
  Y1 is Y+B,
  t(cell(X1,Y1,C)),
  distinct(C,b),
  distinct(C,i),
  \+in((X1,Y1,M,_),L0),
  distancia((X1,Y1),(XP,YP),Dist),
  getVecinosValidos((XP,YP),[(X1,Y1,M,Dist)|L0],L1).

getVecinosValidos((XP,YP),L0,L0).



getPersonaCercana((XP,YP),D):-
  t(control(Rol)),
  t(serpiente(Rol,_,[(X,Y)|_],_)),
  getPersonaCercanaAux((X,Y),[],[(XP,YP,D)|L]).

getPersonaCercanaAux((XCabeza,YCabeza),L,R):-
  t(cell(X,Y,p)),
  \+in((X,Y,_),L),
  distancia((XCabeza,YCabeza),(X,Y),D),
  head(L,(_,_,D1)),
  D < D1,
  getPersonaCercanaAux((XCabeza,YCabeza),[(X,Y,D)|L],R),!.

getPersonaCercanaAux((XCabeza,YCabeza),[],R):-
  t(cell(X,Y,p)),
  distancia((XCabeza,YCabeza),(X,Y),D),
  getPersonaCercanaAux((XCabeza,YCabeza),[(X,Y,D)],R).

getPersonaCercanaAux((X,Y),L,L).




%Obtiene de una lista, la mejor acción.
getMejorAccion([(X,Y,Accion,0)|R],_,MenorDistanciaParcial,Accion,MenorDistancia).

getMejorAccion([(_,_,Accion,Distancia)|R],_,MenorDistanciaParcial,MejorAccion,MenorDistancia):-
  Distancia=<MenorDistanciaParcial,
  getMejorAccion(R,Accion,Distancia,MejorAccion,MenorDistancia).

getMejorAccion([(_,_,_,_)|R],MejorAccionParcial,MenorDistanciaParcial,MejorAccion,MenorDistancia):-
  getMejorAccion(R,MejorAccionParcial,MenorDistanciaParcial,MejorAccion,MenorDistancia).

getMejorAccion([],MejorAccion,MenorDistancia,MejorAccion,MenorDistancia).

