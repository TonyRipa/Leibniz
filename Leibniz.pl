
%	Author:		Anthony John Ripa
%	Date:		2023.04.20
%	Leibniz:	A Rule System for Expressions

:- op(0500,fy,/).
:- op(0600,fy,*).

map(_,[],[]). map(F,[H|T],[H1|T1]) :- call(F,H,H1) , map(F,T,T1) .

array_sum([],X,X) :- ! .
array_sum(X,[],X) :- ! .
array_sum([H1|T1],[H2|T2],[H3|T3]) :- H3 is H1+H2 , array_sum(T1,T2,T3) , ! .

array_sub([],[],[]) :- ! .
array_sub(List,[],List) :- ! .
array_sub([],[H|T],[H2|T2]) :- H2 is 0-H , array_sub([],T,T2) , ! .
array_sub([H1|T1],[H2|T2],[H3|T3]) :- H3 is H1-H2 , array_sub(T1,T2,T3) , ! .

array_mul([],_,[]) :- ! .
array_mul([E],List,Ans) :- map({E}/[In,Out]>>(Out is E*In),List,Ans) , ! .
array_mul([H|T],List,Ans) :- array_mul([H],List,First) , array_mul(T,List,Rest) , array_sum(First,[0|Rest],Ans) .

get_degree(List,Ans) :- reverse(List,[0|Tr]) , reverse(Tr,T) , get_degree(T,Ans) , ! .
get_degree(List,Ans) :- reverse(List,[H|Tr]) , length(Tr,Deg) , Ans=[Deg,H] , ! .

div10s(List,0,Ans) :- Ans = List , ! .
div10s(List,N,Ans) :- List = [_|T] , N2 is N-1 , div10s(T,N2,Ans) , ! .

unscale(List,C,Ans) :- map({C}/[In,Out]>>(C\=0,Out is In/C),List,Ans) , ! .

array_div(Num,Den,Ans) :- length(Num,Iter), array_div_h(Num,Den,Iter,Ans).

array_div_h(_Num,_Den,0,[0]) :- ! .
array_div_h(Num,Den,C,Ans) :-
	get_degree(Den,[Deg,Val]),
	div10s(Num,Deg,Temp1),
	unscale(Temp1,Val,Quotient),
	(
		array_zero(Quotient) -> Ans = Quotient;
		array_mul(Quotient,Den,Temp2),
		array_sub(Num,Temp2,Remainder),
		C1 is C-1,
		array_div_h(Remainder,Den,C1,Q2),
		array_sum(Quotient,Q2,Ans)
	).

array_zero([]) :- ! .
array_zero([0|T]) :- array_zero(T) , ! .

pre(X,poly(X,[0,1])) :- atom(X) , ! .
pre(I,poly(_,[I])) :- integer(I) , ! .
pre(+X,Ans) :- pre(X,Ans) , ! .
pre(-X,Ans) :- pre(0-X,Ans) , ! .
pre(*X,Ans) :- pre(X,Ans) , ! .
pre(/X,Z) :- pre(1/X,Z) , ! .
pre(A,Ans) :- ( A =.. [Op|Args] ) , map(pre,Args,L) , Ans =.. [Op|L] , ! .
pre(X,X) :- ! .

int(X) :- X=poly(_,[_]) .

go(X/0,X/0) :- integer(X) , ! .
go(X/Y,Z) :- integer(X) , integer(Y) , Z is X/Y , ! .
go(poly(V,X)+poly(V,Y),poly(V,Z)) :- array_sum(X,Y,Z) , ! .
go(poly(V,X)-Y1,poly(V,Z)) :- go(Y1,poly(V,Y)) , array_sub(X,Y,Z) , ! .
go(poly(V,X)*poly(V,Y),poly(V,Z)) :- array_mul(X,Y,Z) , ! .
go(poly(V,X)/Y1,Ans) :- Y1=poly(V,Y) -> array_div(X,Y,Z) , Ans = poly(V,Z) ; go(Y1,poly(V,Y)) , go(poly(V,X)/poly(V,Y),Ans) , ! .
go(N-D,Ans) :- go(N,N1) , N\=N1 , go(N1-D,Ans) , ! .
go(N*D,Ans) :- go(N,N1) , N\=N1 , go(N1*D,Ans) , ! .
go(N*D,Ans) :- go(D,D1) , D\=D1 , go(N*D1,Ans) , ! .
go(N/D,Ans) :- go(N,N1) , N\=N1 , go(N1/D,Ans) , ! .
go(X,X) .

post(poly(_,Z),0) :- array_zero(Z) , ! .
post(poly(_,[I]),I) :- ! .
post(poly(X,[0,1]),X) :- ! .
post(poly(X,[0,-1]),-X) :- ! .
post(poly(X,[0,I]),I*X) :- ! .
post(poly(X,[0,0,1]),X^2) :- ! .
post(poly(X,[0,0,I]),I*X^2) :- ! .
post(poly(X,[H1,H2|T]),First-Rest) :- post(poly(X,[H1]),First) , H2<0 , Neg_H2 is -H2 , post(poly(X,[0,Neg_H2|T]),Rest) , ! .
post(poly(X,List),Rest) :- reverse(List,[0|T]) , reverse(T,T2) , post(poly(X,T2),Rest) , ! .
post(poly(X,List),Rest+Last) :-
	reverse(List,[H|T]) ,
	length(T,Pow) ,
	(H=1,Pow=1 -> Last=X ; (H=1 -> Last=X^Pow ; (Pow=1 -> Last=H*X ; Last=H*X^Pow))),
	reverse(T,T2) ,
	post(poly(X,T2),Rest) , ! .
post(A,Ans) :- ( A =.. [Op|Args] ) , map(post,Args,L) , Ans =.. [Op|L] , ! .
post(X,X) .

simp(X,Ans) :- pre(X,Pre) , go(Pre,Go) , post(Go,Ans) .
