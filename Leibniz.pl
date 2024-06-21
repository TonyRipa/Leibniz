
%	Author:		Anthony John Ripa
%	Date:		2024.06.20
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
post(poly(B,[[V,P]]),Ans) :- term(B,P,T) , ( V=1 -> Ans=T ; V= -1 -> Ans= -T ; Ans=V*T ) , ! .
post(poly(B,[[V,PV]|T]),Ans) :- num_abs(V,V2) , post(poly(B,[[V2,PV]]),H) , post(poly(B,T),T2) , (num_pos(V) -> Ans = T2+H ; Ans = T2-H) , ! .
post(X,X) .

term(B,P,Ans) :- B=[] , P=[] , Ans = 1 , ! .
term(B,P,Ans) :- B=[BH|BT] , P=[PH|PT] , term(BT,PT,1) , ( PH=0 -> Ans = 1 ; PH=1 -> Ans = BH ; Ans = BH^PH ) , ! .
term(B,P,Ans) :- B=[BH|BT] , P=[PH|PT] , term(BT,PT,TT) , ( PH=0 -> Ans = TT ; PH=1 -> Ans = BH*TT ; Ans = BH^PH*TT ) , ! .

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

sparse_sub(A,B,Ans) :- sparse_negate(B,Neg_B) , sparse_sum(A,Neg_B,Ans) , ! .

sparse_negate(Sparse,New_Sparse) :- map([[V,K],[NV,K]]>>is(NV,-V), Sparse, New_Sparse) , ! .

sparse_mul([],_,[]) :- ! .
sparse_mul([[V,K]],List,Ans) :- map({V,K}/[[Vi,Ki],[Vo,Ko]]>>(Vo is Vi*V,array_add(Ki,K,Ko)),List,Ans) , ! .
sparse_mul([H|T],List,Ans) :- sparse_mul([H],List,First) , sparse_mul(T,List,Rest) , sparse_sum(First,Rest,Ans) , ! .

sparse_div(Num,Den,Ans) :- length(Num,Len) , succ(Len,Iter) , sparse_div_h(Num,Den,Iter,Ans).

sparse_div_h(_Num,_Den,0,[]) :- ! .
sparse_div_h([],Den,C,Ans) :- sparse_div_h([[0,[0]]],Den,C,Ans) , ! .
sparse_div_h(Num,[],C,Ans) :- sparse_div_h(Num,[[0,[0]]],C,Ans) , ! .
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

%%%%%%%%%%%%%%%%%%%%	Polynomial	Align	Operations	%%%%%%%%%%%%%%%%%%%%%%

align(poly(Base1,OldSparse1),poly(Base2,OldSparse2),poly(Base,NewSparse1),poly(Base,NewSparse2)) :-
	union(Base1,Base2,Base) ,
	alignsparse2base(OldSparse1,Base1,Base,Sparse1) ,
	alignsparse2base(OldSparse2,Base2,Base,Sparse2) ,
	normalize(Sparse1,NewSparse1) ,
	normalize(Sparse2,NewSparse2) ,
	=(NewSparse2,NewSparse2) .

alignsparse2base(Sparse,[],[_],Sparse) :-	! .
alignsparse2base(Sparse,[Y],[_,Y],NewSparse) :- pad0(Sparse,NewSparse) , ! .
alignsparse2base(Sparse,Type1,Type2,NewSparse) :- castmap(Sparse,Type1,Type2,NewSparse) , ! .

castmap([],_,_,[]) :- ! .
castmap([H|T],Type1,Type2,[H2|T2]) :-
	H = [Coef,Indices] ,
	cast(Indices,Type1,Type2,Indices2) ,	
	H2 = [Coef,Indices2] ,
	castmap(T,Type1,Type2,T2) , ! .

cast(Data,Type1,Type2,DataPart) :-
	Type1 = [Elem1] ,
	length(Type2,N) ,
	length(DataPart,N) ,
	cast1(Data,Elem1,Type1,Type2,DataPart) ,
	varto0(DataPart) , ! .
cast(Data,Type1,Type2,DataPart) :-
	Type1 = [Elem1,Elem2] ,
	length(Type2,N) ,
	length(DataPart,N) ,
	cast1(Data,Elem1,Type1,Type2,DataPart) ,
	cast1(Data,Elem2,Type1,Type2,DataPart) ,
	varto0(DataPart) , ! .

cast1(Data,Elem1,Type1,Type2,DataPart) :-
	nth0(I1,Type1   ,Elem1) ,		%	Find Index1
	nth0(I1,Data    ,Datum) ,		%	Find Datum
	nth0(I2,Type2   ,Elem1) ,		%	Find Index2
	nth0(I2,DataPart,Datum) , ! .	%	Set  Datum

varto0(L) :- map([E,E]>>(E=0;true), L, _) , ! .
