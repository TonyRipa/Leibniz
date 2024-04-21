
%	Author:		Anthony John Ripa
%	Date:		2024.04.20
%	Leibniz:	A Rule System for Expressions

:- op(0500,fy,/).
:- op(0600,fy,*).

pre(X,poly([X],[[1,[1]]])) :- atom(X) , ! .
pre(I,poly([],[[I,[0]]])) :- integer(I) , ! .
pre(+X,Ans) :- pre(X,Ans) , ! .
pre(-X,Ans) :- pre(0-X,Ans) , ! .
pre(*X,Ans) :- pre(X,Ans) , ! .
pre(/X,Ans) :- pre(1/X,Ans) , ! .
pre(A,Ans) :- A =.. [Op|Args] , map(pre,Args,L) , Ans =.. [Op|L] , ! .
pre(X,X) :- ! .

go(poly(V,X)+poly(V,Y),poly(V,Z)) :- sparse_sum(X,Y,Z) , ! .
go(poly(V1,X)+poly(V2,Y),Ans) :- poly_sum(poly(V1,X),poly(V2,Y),Ans) , ! .
go(Poly1+Poly2,Ans) :- go(Poly1,poly(V1,X)) , go(Poly2,poly(V2,Y)) , poly_sum(poly(V1,X),poly(V2,Y),Ans) , ! .
go(poly(V,X)-poly(V,Y),poly(V,Z)) :- sparse_sub(X,Y,Z) , ! .
go(poly(V1,X)-poly(V2,Y),Ans) :- poly_sub(poly(V1,X),poly(V2,Y),Ans) , ! .
go(Poly1-Poly2,Ans) :- go(Poly1,poly(V1,X)) , go(Poly2,poly(V2,Y)) , poly_sub(poly(V1,X),poly(V2,Y),Ans) , ! .
go(poly(V,X)*poly(V,Y),poly(V,Z)) :- sparse_mul(X,Y,Z) , ! .
go(poly(V1,X)*poly(V2,Y),Ans) :- poly_mul(poly(V1,X),poly(V2,Y),Ans) , ! .
go(Poly1*Poly2,Ans) :- go(Poly1,poly(V1,X)) , go(Poly2,poly(V2,Y)) , poly_mul(poly(V1,X),poly(V2,Y),Ans) , ! .
go(poly(V,X)/poly(V,Y),poly(V,Z)) :- sparse_div(X,Y,Z) , ! .
go(poly(V1,X)/poly(V2,Y),Ans) :- poly_div(poly(V1,X),poly(V2,Y),Ans) , ! .
go(Poly1/Poly2,Ans) :- go(Poly1,poly(V1,X)) , go(Poly2,poly(V2,Y)) , poly_div(poly(V1,X),poly(V2,Y),Ans) , ! .
go(X,X) .

post(poly(_,Sparse),0) :- normalize(Sparse,[]) , ! .
post(poly(_,[[I,[]]]),I) :- ! .
post(poly(_,[[I,[0]]]),I) :- ! .
post(poly([X|_],[[V,[Pow]]  ]),Ans) :- ( Pow=1 -> XPow=X ; XPow=X^Pow ) , ( V=1 -> Ans=XPow ; V= -1 -> Ans= -XPow ; Ans=V*XPow ) , ! .
post(poly([X,_],[[V,[Pow,0]]]),Ans) :- ( Pow=1 -> XPow=X ; XPow=X^Pow ) , ( V=1 -> Ans=XPow ; V= -1 -> Ans= -XPow ; Ans=V*XPow ) , ! .
post(poly([_,X],[[V,[0,Pow]]]),Ans) :- ( Pow=1 -> XPow=X ; XPow=X^Pow ) , ( V=1 -> Ans=XPow ; V= -1 -> Ans= -XPow ; Ans=V*XPow ) , ! .
post(poly([X,Y],[[V,[P1,P2]]]),Ans) :- ( P1 =1 -> XPow=X ; XPow=X^P1  ) , ( P2=1 -> YPow=Y ; YPow=Y^P2 ) , ( V=1 -> Ans=XPow*YPow ; V= -1 -> Ans= -XPow*YPow ; Ans=V*XPow*YPow ) , ! .
post(poly(B,[[V,PV]|T]),Ans) :- num_abs(V,V2) , post(poly(B,[[V2,PV]]),H) , post(poly(B,T),T2) , (num_pos(V) -> Ans = T2+H ; Ans = T2-H) , ! .
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

array_add([],X,X) :- ! .
array_add(X,[],X) :- ! .
array_add([H1|T1],[H2|T2],[H3|T3]) :- H3 is H1+H2 , array_add(T1,T2,T3) .

array_sub([],X,X) :- ! .
array_sub(X,[],X) :- ! .
array_sub([H1|T1],[H2|T2],Ans) :- H3 is H1-H2 , array_sub(T1,T2,T3) , array_trim([H3|T3],Ans) , ! .

array_trim(X,Ans) :- reverse(X,XR) , array_trim_front(XR,AnsR) , reverse(AnsR,Ans) , ! .

array_trim_front([0|T],Ans) :- array_trim_front(T,Ans) , ! .
array_trim_front(X,X) :- ! .

%%%%%%%%%%%%%%%%%%%%	Sparse	Array Operations 1		%%%%%%%%%%%%%%%%%%%%%%

sparse1_mul([],_,[]) :- ! .
sparse1_mul([[V,K]],List,Ans) :- map({V,K}/[[Vi,Ki],[Vo,Ko]]>>(Vo is Vi*V,Ko is Ki+K),List,Ans) , ! .
sparse1_mul([H|T],List,Ans) :- sparse_mul([H],List,First) , sparse_mul(T,List,Rest) , sparse_sum(First,Rest,Ans) , ! .

sparse1_sum(A,B,Ans) :- append(A,B,C), normalize1(C,Ans) .

negate([],[]) :- ! .
negate([[V,K]|T],[[Neg_V,K]|T2]) :- Neg_V is -V , negate(T,T2) , ! .

normalize1(Sparse,Ans) :-
	map([[A,B],[B,A]]>>true, Sparse, Rev_Sparse),
	sort(1,@=<,Rev_Sparse,Rev_Sorted_Sparse) ,
	map([[A,B],[B,A]]>>true, Rev_Sorted_Sparse, Sorted_Sparse),
	combine_like_terms1(Sorted_Sparse,Combined) ,
	trim1(Combined,Ans) .

combine_like_terms1([],[]) :- ! .
combine_like_terms1([X],[X]) :- ! .
combine_like_terms1([[V1,K],[V2,K]|T], [[V,K]|T2]) :- V is V1+V2 , combine_like_terms1(T,T2) , ! .
combine_like_terms1([H|T], [H|T2]) :- combine_like_terms1(T,T2) , ! .

trim1([],[]) :- ! .
trim1([[0,_]|T],T2) :- trim1(T,T2) , ! .
trim1([H|T],[H|T2]) :- trim1(T,T2) , ! .

sparse1_div(Num,Den,Ans) :- length(Num,Len) , succ(Len,Iter) , sparse1_div_h(Num,Den,Iter,Ans).

sparse1_div_h(_Num,_Den,0,[]) :- ! .
sparse1_div_h([],Den,C,Ans) :- sparse1_div_h([[0,0]],Den,C,Ans) , ! .
sparse1_div_h(Num,Den,C,Ans) :-
	last(Num,[N_Coef,N_Pow]),
	last(Den,[D_Coef,D_Pow]),
	num_div(N_Coef,D_Coef,Q_Coef),
	Q_Pow is N_Pow - D_Pow,
	Quotient = [[Q_Coef,Q_Pow]],
	(
		D_Coef = 0 -> Ans = Quotient;
		sparse1_mul(Quotient,Den,Temp2),
		sparse1_sub(Num,Temp2,Remainder),
		C1 is C-1,
		sparse1_div_h(Remainder,Den,C1,Q2),
		sparse1_sum(Quotient,Q2,Ans)
	).

%%%%%%%%%%%%%%%%%%%%	Sparse	Array Operations		%%%%%%%%%%%%%%%%%%%%%%

sparse_sum(A,B,Ans) :- append(A,B,C), normalize(C,Ans) .

sparse_sub(A,B,Ans) :- negate(B,Neg_B) , sparse_sum(A,Neg_B,Ans) , ! .

sparse_negate(Sparse,New_Sparse) :- map([[V,K],[-V,K]]>>true, Sparse, New_Sparse) , ! .

sparse_mul([],_,[]) :- ! .
sparse_mul([[V,K]],List,Ans) :- map({V,K}/[[Vi,Ki],[Vo,Ko]]>>(Vo is Vi*V,array_add(Ki,K,Ko)),List,Ans) , ! .
sparse_mul([H|T],List,Ans) :- sparse_mul([H],List,First) , sparse_mul(T,List,Rest) , sparse_sum(First,Rest,Ans) , ! .

sparse_div(Num,Den,Ans) :- length(Num,Len) , succ(Len,Iter) , sparse_div_h(Num,Den,Iter,Ans).

sparse_div_h(_Num,_Den,0,[]) :- ! .
sparse_div_h([],Den,C,Ans) :- sparse_div_h([[0,[0]]],Den,C,Ans) , ! .
sparse_div_h(Num,Den,C,Ans) :-
	last(Num,[N_Coef,N_Pow]),
	last(Den,[D_Coef,D_Pow]),
	num_div(N_Coef,D_Coef,Q_Coef),
	array_sub(N_Pow, D_Pow, Q_Pow),
	Quotient = [[Q_Coef,Q_Pow]],
	(
		D_Coef = 0 -> Ans = Quotient;
		sparse_mul(Quotient,Den,Temp2),
		sparse_sub(Num,Temp2,Remainder),
		C1 is C-1,
		sparse_div_h(Remainder,Den,C1,Q2),
		sparse_sum(Quotient,Q2,Ans)
	).

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

pad0([],[]) :- ! .
pad0([[Coef,Pow]],[[Coef,[0|Pow]]]) :- ! .
pad0([H|T],Ans) :-
	pad0([H],PH) ,
	pad0(T,PT) ,
	append(PH,PT,Ans) , ! .

pad1([],[]) :- ! .
pad1([[Coef,Pow]],[[Coef,Pow2]]) :- reverse([0|Pow],Pow2) , ! .
pad1([H|T],Ans) :-
	pad1([H],PH) ,
	pad1(T,PT) ,
	append(PH,PT,Ans) , ! .

swap([[Coef,[Pow1,Pow2]]],[[Coef,[Pow2,Pow1]]]) :- ! .
swap([H|T],Ans) :-
	swap([H],SH) ,
	swap(T,ST) ,
	append(SH,ST,Ans) , ! .

%%%%%%%%%%%%%%%%%%%%	Set			  Operations		%%%%%%%%%%%%%%%%%%%%%%

union(List1,List2,Ans) :-
	append(List1,List2,List) ,
	list_to_set(List,Set) ,
	set_clean(Set,Ans) .

set_clean([],[]) :- ! .
set_clean([H|T],T2) :- var(H) , set_clean(T,T2) , ! .
set_clean([H|T],[H|T2]) :- not(var(H)) , set_clean(T,T2) , ! .

%%%%%%%%%%%%%%%%%%%%	Polynomial	  Operations		%%%%%%%%%%%%%%%%%%%%%%

poly_negate(Poly,NewPoly) :-
	Poly = poly(Base,Sparse) ,
	sparse_negate(Sparse,Sparse2) ,
	NewPoly = poly(Base,Sparse2) , ! .

poly_sum(P1,P2,Ans) :-
	align(P1,P2,poly(B,S1),poly(B,S2)) ,
	sparse_sum(S1,S2,S) ,
	Ans = poly(B,S) , ! .

poly_sub(P1,P2,Ans) :-
	align(P1,P2,poly(B,S1),poly(B,S2)) ,
	sparse_sub(S1,S2,S) ,
	Ans = poly(B,S) , ! .

poly_mul(P1,P2,Ans) :-
	align(P1,P2,poly(B,S1),poly(B,S2)) ,
	sparse_mul(S1,S2,S) ,
	Ans = poly(B,S) , ! .

poly_div(P1,P2,Ans) :-
	align(P1,P2,poly(B,S1),poly(B,S2)) ,
	sparse_div(S1,S2,S) ,
	Ans = poly(B,S) , ! .

align(Poly1,Poly2,poly(Base,NewSparse1),poly(Base,NewSparse2)) :-
	Poly1 = poly(Base1,_) ,
	Poly2 = poly(Base2,_) ,
	union(Base1,Base2,Base) ,
	alignpoly2base(Poly1,Base,poly(Base,Sparse1)) ,
	alignpoly2base(Poly2,Base,poly(Base,Sparse2)) ,
	normalize(Sparse1,NewSparse1) ,
	=(Sparse2,NewSparse2) .

alignpoly2base(Poly,X,NewPoly) :-
	Poly = poly([], Sparse) ,
	NewPoly = poly(X, Sparse) , ! .

alignpoly2base(Poly,X,NewPoly) :-
	Poly = poly(X,_) ,
	NewPoly = Poly , ! .

alignpoly2base(Poly,[X,Y],NewPoly) :-
	Poly = poly([X],OldSparse) ,
	pad1(OldSparse,NewSparse) ,
	NewPoly = poly([X,Y], NewSparse) , ! .

alignpoly2base(Poly,[X,Y],NewPoly) :-
	Poly = poly([Y],OldSparse) ,
	pad0(OldSparse,NewSparse) ,
	NewPoly = poly([X,Y], NewSparse) , ! .

alignpoly2base(Poly,[X,Y],NewPoly) :-
	Poly = poly([Y,X],OldSparse) ,
	swap(OldSparse,NewSparse) ,
	NewPoly = poly([X,Y], NewSparse) , ! .
