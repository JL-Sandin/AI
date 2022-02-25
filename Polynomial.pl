degree([K|Poly], N) :- 
  	K \= 0, length(Poly, N).
degree([_|Poly], N) :- 
  	degree(Poly, N).

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	length(PA, NA), length(PB, NB), NA > NB,
	plus(PA, [KB|PB], PS), KS is KA.

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	length(PA, NA), length(PB, NB), NA < NB,
	plus([KA|PA], PB, PS), KS is KB.

plus([KA|PA], [KB|PB], [KS|PS]) :- 

  plus(PA, PB, PS), KS is KA + KB.
  plus(PA, PB, PS), KS is KA + KB.
  
%RESTA

%caso primer parametro vacio|
resta_pol([],X,X):- 
    !.
%caso segundo parametro vacio
resta_pol(X,[],X):- 
    !.
% caso de todos los parametros llenos
resta_pol([Uno|A], [Dos|B], [Resp|C]) :-
   Resp is Uno-Dos,
   resta_pol(A, B, C).
  
%?-resta_pol([5,6],[2,3],X).

  	
  
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	length(PA, NA), length(PB, NB), NA > NB,
	minus(PA, [KB|PB], PS), KS is KA.
	
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	length(PA, NA), length(PB, NB), NA < NB,
	minus([KA|PA], PB, PS), KS is -KB.
	
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	minus(PA, PB, PS), KS is KA - KB.
