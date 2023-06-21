
%	Author:		Anthony John Ripa
%	Date:		2023.06.20
%	Leibniz:	A Rule System for Expressions

:- op(0500,fy,/).
:- op(0600,fy,*).

pre(X,poly(X,[[1,1]])) :- atom(X) , ! .
pre(I,poly(_,[[I,0]])) :- integer(I) , ! .
pre(+X,Ans) :- pre(X,Ans) , ! .
pre(-X,Ans) :- pre(0-X,Ans) , ! .
pre(*X,Ans) :- pre(X,Ans) , ! .
pre(/X,Ans) :- pre(1/X,Ans) , ! .
pre(A,Ans) :- A =.. [Op|Args] , map(pre,Args,L) , Ans =.. [Op|L] , ! .
pre(X,X) :- ! .

go(poly(V,X)+poly(V,Y),poly(V,Z)) :- sparse_sum(X,Y,Z) , ! .
go(poly(V,X)-poly(V,Y),poly(V,Z)) :- sparse_sub(X,Y,Z) , ! .
go(poly(V,X)*poly(V,Y),poly(V,Z)) :- sparse_mul(X,Y,Z) , ! .
go(poly(V,X)/poly(V,Y),poly(V,Z)) :- sparse_div(X,Y,Z) , ! .
go(A,Ans) :- A =.. [Op|Args] , Op\=poly , map(go,Args,Args2) , Args\=Args2 , A2 =.. [Op|Args2] , go(A2,Ans) , ! .
go(X,X) .

post(poly(_,Sparse),0) :- normalize(Sparse,[]) , ! .
post(poly(_,[[I,0]]),I) :- ! .
post(poly(X,[[V,Pow]  ]),Ans) :- ( Pow=1 -> XPow=X ; XPow=X^Pow ) , ( V=1 -> Ans=XPow ; V= -1 -> Ans= -XPow ; Ans=V*XPow ) , ! .
post(poly(X,[[V,Pow]|T]),Ans) :- num_abs(V,V2) , post(poly(X,[[V2,Pow]]),H) , post(poly(X,T),T2) , (num_pos(V) -> Ans = T2+H ; Ans = T2-H) , ! .
post(X,X) .

simp(X,Ans) :- pre(X,Pre) , go(Pre,Go) , post(Go,Ans) .

%%%%%%%%%%%%%%%%%%%%			Number Operations		%%%%%%%%%%%%%%%%%%%%%%

num_div(0,0,'%') :- ! .
num_div(_,0,∞) :- ! .
num_div(N,D,Q) :- Q is N/D , ! .

num_abs(∞,∞) :- ! .
num_abs('%','%') :- ! .
num_abs(X,Ans) :- abs(X,Ans) , ! .

num_pos(X) :- num_abs(X,X) , ! .

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

sparse_div(Num,Den,Ans) :- length(Num,Len) , succ(Len,Iter) , sparse_div_h(Num,Den,Iter,Ans).

sparse_div_h(_Num,_Den,0,[]) :- ! .
sparse_div_h([],Den,C,Ans) :- sparse_div_h([[0,0]],Den,C,Ans) , ! .
sparse_div_h(Num,Den,C,Ans) :-
	last(Num,[N_Coef,N_Pow]),
	last(Den,[D_Coef,D_Pow]),
	num_div(N_Coef,D_Coef,Q_Coef),
	Q_Pow is N_Pow - D_Pow,
	Quotient = [[Q_Coef,Q_Pow]],
	(
		D_Coef = 0 -> Ans = Quotient;
		sparse_mul(Quotient,Den,Temp2),
		sparse_sub(Num,Temp2,Remainder),
		C1 is C-1,
		sparse_div_h(Remainder,Den,C1,Q2),
		sparse_sum(Quotient,Q2,Ans)
	).
