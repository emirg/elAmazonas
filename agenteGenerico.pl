agenteGen(Rol,Accion).


%
getAccionesLegales(M) :-
  t(control(J)),
  legal(J,A),
  \+in(A,L0),
  distinct(A,noop).



