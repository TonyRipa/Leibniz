
%	Author:		Anthony John Ripa
%	Date:		2023.02.20
%	Leibniz:	A Rule System for Expressions

:- op(0500,fy,/).
:- op(0600,fy,*).

map(_,[],[]). map(F,[H|T],[H1|T1]) :- call(F,H,H1) , map(F,T,T1) .

array_sub([],[],[]) :- ! .
array_sub([H1|T1],[H2|T2],[H3|T3]) :- H3 is H1-H2 , array_sub(T1,T2,T3) , ! .

array_zero([]) :- ! .
array_zero([0|T]) :- array_zero(T) , ! .

pre(X,poly(X,[0,1])) :- atom(X) , ! .
pre(+X,Ans) :- pre(X,Ans) , ! .
pre(*X,Ans) :- pre(X,Ans) , ! .
pre(/X,Z) :- pre(1/X,Z) , ! .
pre(A,Ans) :- ( A =.. [Op|Args] ) , map(pre,Args,L) , Ans =.. [Op|L] , ! .
pre(X,X) :- ! .

go(X+Y,Z) :- integer(X) , integer(Y) , Z is X+Y , ! .
go(X-Y,Z) :- integer(X) , integer(Y) , Z is X-Y , ! .
go(X*Y,Z) :- integer(X) , integer(Y) , Z is X*Y , ! .
go(X/0,X/0) :- integer(X) , ! .
go(X/Y,Z) :- integer(X) , integer(Y) , Z is X/Y , ! .
go(poly(V,X)-poly(V,Y),poly(V,Z)) :- array_sub(X,Y,Z) , ! .
go(X,X) .

post(poly(X,[0,1]),X) :- ! .
post(poly(_,Z),0) :- array_zero(Z) , ! .
post(A,Ans) :- ( A =.. [Op|Args] ) , map(post,Args,L) , Ans =.. [Op|L] , ! .
post(X,X) .

simp(X,Ans) :- pre(X,Pre) , go(Pre,Go) , post(Go,Ans) .
