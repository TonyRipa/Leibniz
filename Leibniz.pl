
%	Author:		Anthony John Ripa
%	Date:		2023.05.20
%	Leibniz:	A Rule System for Expressions

:- op(0600,fy,*).

pre(X,poly(X,[[1,1]])) :- atom(X) , ! .
pre(I,poly(_,[[I,0]])) :- integer(I) , ! .
pre(+X,Ans) :- pre(X,Ans) , ! .
pre(-X,Ans) :- pre(0-X,Ans) , ! .
pre(*X,Ans) :- pre(X,Ans) , ! .
pre(A,Ans) :- A =.. [Op|Args] , map(pre,Args,L) , Ans =.. [Op|L] , ! .
pre(X,X) :- ! .

go(poly(V,X)+poly(V,Y),poly(V,Z)) :- sparse_sum(X,Y,Z) , ! .
go(poly(V,X)-poly(V,Y),poly(V,Z)) :- sparse_sub(X,Y,Z) , ! .
go(poly(V,X)*poly(V,Y),poly(V,Z)) :- sparse_mul(X,Y,Z) , ! .
go(A,Ans) :- A =.. [Op|Args] , Op\=poly , map(go,Args,Args2) , Args\=Args2 , A2 =.. [Op|Args2] , go(A2,Ans) , ! .
go(X,X) .

post(poly(_,Sparse),0) :- normalize(Sparse,[]) , ! .
post(poly(_,[[I,0]]),I) :- ! .
post(poly(X,[[V,Pow]  ]),Ans) :- ( Pow=1 -> XPow=X ; XPow=X^Pow ) , ( V=1 -> Ans=XPow ; V= -1 -> Ans= -XPow ; Ans=V*XPow ) , ! .
post(poly(X,[[V,Pow]|T]),Ans) :- abs(V,V2) , post(poly(X,[[V2,Pow]]),H) , post(poly(X,T),T2) , (V<0 -> Ans = T2-H ; Ans = T2+H) , ! .
post(X,X) .

simp(X,Ans) :- pre(X,Pre) , go(Pre,Go) , post(Go,Ans) .

%%%%%%%%%%%%%%%%%%%%			Array Operations		%%%%%%%%%%%%%%%%%%%%%%

map(_,[],[]). map(F,[H|T],[H1|T1]) :- call(F,H,H1) , map(F,T,T1) .

%%%%%%%%%%%%%%%%%%%%	Sparse	Array Operations		%%%%%%%%%%%%%%%%%%%%%%

sparse_sub(A,B,Ans) :- negate(B,Neg_B) , sparse_sum(A,Neg_B,Ans) , ! .

sparse_mul([],_,[]) :- ! .
sparse_mul([[V,K]],List,Ans) :- map({V,K}/[[Vi,Ki],[Vo,Ko]]>>(Vo is Vi*V,Ko is Ki+K),List,Ans) , ! .
sparse_mul([H|T],List,Ans) :- sparse_mul([H],List,First) , sparse_mul(T,List,Rest) , sparse_sum(First,Rest,Ans) , ! .

sparse_sum(A,B,Ans) :- append(A,B,C), normalize(C,Ans) .

negate([],[]) :- ! .
negate([[V,K]|T],[[Neg_V,K]|T2]) :- Neg_V is -V , negate(T,T2) , ! .

normalize(Sparse,Ans) :-
	map([[A,B],[B,A]]>>true, Sparse, Rev_Sparse),
	sort(1,@=<,Rev_Sparse,Rev_Sorted_Sparse) ,
	map([[A,B],[B,A]]>>true, Rev_Sorted_Sparse, Sorted_Sparse),
	combine_like_terms(Sorted_Sparse,Combined) ,
	trim(Combined,Ans) .

combine_like_terms([],[]) :- ! .
combine_like_terms([X],[X]) :- ! .
combine_like_terms([[V1,K],[V2,K]|T], [[V,K]|T2]) :- V is V1+V2 , combine_like_terms(T,T2) , ! .
combine_like_terms([H|T], [H|T2]) :- combine_like_terms(T,T2) , ! .

trim([],[]) :- ! .
trim([[0,_]|T],T2) :- trim(T,T2) , ! .
trim([H|T],[H|T2]) :- trim(T,T2) , ! .
