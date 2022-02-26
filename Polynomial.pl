degree([K|Poly], N) :- 
  	K \= 0, length(Poly, N).
degree([_|Poly], N) :- 
  	degree(Poly, N).

plus([],[],[]).

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	length(PA, NA), length(PB, NB), NA > NB,
	plus(PA, [KB|PB], PS), KS is KA.

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	length(PA, NA), length(PB, NB), NA < NB,
	plus([KA|PA], PB, PS), KS is KB.

plus([KA|PA], [KB|PB], [KS|PS]) :- 
  	plus(PA, PB, PS), KS is KA + KB.
% ?- plus([1,2],[2,3],X).
% ?- plus([4,8],[5,5],[9,13]).


minus([],[],[]).

minus([KA|PA], [KB|PB], [KS|PS]) :- 
	length(PA, NA), length(PB, NB), NA > NB,
	minus(PA, [KB|PB], PS), KS is KA.
	
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	length(PA, NA), length(PB, NB), NA < NB,
	minus([KA|PA], PB, PS), KS is -KB.
	
minus([KA|PA], [KB|PB], [KS|PS]) :- 
	minus(PA, PB, PS), KS is KA - KB.

% ?- minus([5,2],[3],[5,-1]).
% ?- minus([5],[3],[2]).

horner([], _, 0).

horner([K|P], X, R):-
	horner(P,X,RA),
	R is RA * X + K.

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

suma([],[],[]).
suma([],YS,YS).
suma(XS,[],XS).
suma([XH|TX],[YH|TY],[S|TS]):-
        suma(TX,TY,TS),S is XH+YH.

%times: mltiplicacion de dos polinomios

% Predicado auxiliar usado en lpad
repeat(_,0,[]).
repeat(Elem,1,[Elem]).
repeat(Elem,Num,[Elem|RT]):-
        Num>0,
        N1 is Num-1,
        repeat(Elem,N1,RT).

% Predicado que añade 0 por la izquierda en la lista
% para representar la multiplicacion 
lpad(XS,0,XS).
lpad(XS,N,RES):-
        N>0,
        repeat(0,N,S),
        append(S,XS,RES).

% Predicado que multiplica un escalar por un polinomio
% 3(2x+3x^2)=6x+9x^2  | mult(3,[0,2,3]) >> [0,6,9] 
mult(0,_,[]).
mult(_,[],[]).
mult(X,[HY|TY],[HS|TS]):-
        HS is X*HY,
        mult(X,TY,TS).

% Predicado que multiplica un polinomio por una variable
% 2x(3x^2+4x^3)=6x^3+8x^4 | mult(1,2,[0,0,3,4],S) >> [0,0,0,6,8]
mult(_,_,[],[]).
mult(_,0,_,[]).
mult(Grado,Coeficiente,Y,S):-
        mult(Coeficiente,Y,S1),
        lpad(S1,Grado,S).
	
% Predicado que multiplica polinomios
% 

% multPolinomios([1,2],[0,0,3,4],S) >> [0,0,3,10,8]
multPolinomios(_,[],[]).
multPolinomios([],_,[]).
multPolinomios(X,Y,S):-
        multPolinomios(0,X,Y,S).
multPolinomios(_,[],_,[]).
multPolinomios(_,_,[],[]).
multPolinomios(N,[XH|XT],Y,S):-
        N1 is N+1,
        multPolinomios(N1,XT,Y,S1),
        mult(N,XH,Y,S2),
        suma(S1,S2,S).
	
%diferentiate: derivada del polinomio

derivada(X,S):-
        derivada(0,X,S).

derivada(_,[],[]).

derivada(0,[_|T],S):-
        derivada(1,T,S).

derivada(I,[XN|T],[SN|TS]):-
        I1 is I+1,
        derivada(I1,T,TS),
        SN is XN * I.

derivada(plus,X,Y,S):-
        derivada(X,D1),derivada(Y,D2),
        plus(D1,D2,S). 

derivada(times,X,Y,S):-
        derivada(X,D1),derivada(Y,D2),
        times(Y,D1,M1),
        times(X,D2,M2),
        plus(M1,M2,S).
