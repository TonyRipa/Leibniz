
%	Author:		Anthony John Ripa
%	Date:		2021.1.20
%	Leibniz:	A Rule System for Math

:- op(0600,fy,/).
:- op(0700,fy,*).
:- op(0800,xfx,@).
:- op(0900,xfx,<-).
:- op(1000,xfx,<--).
:- op(1100,xfx,<---).
:- op(1200,xfx,<----).

:- dynamic see/0.

if(Condition,Statement) :- Condition->Statement ; true .
if(Condition,Statement1,Statement2) :- Condition->Statement1 ; Statement2 .

shop(Text) :- if(see, (write('□ '),writeln(Text)) ) .
show(Text) :- if(see, (write('. '),writeln(Text)) ) .
succ(Text) :- if(see, (write('+ '),writeln(Text)) ) .

prepro(0,traction([],[])) :- shop(('01',0)) , ! .
prepro(1,fraction([],[])) :- shop(('02',1)) , ! .
prepro(f(X),Ans) :- shop(('03',f(X))) , prepro(X*X, Ans) , ! .
prepro(A,Ans) :- A =.. [+|Args] , shop(('04',A)) , map(prepro,Args,L) , prepro(traction(L,[]),Ans) , ! .
prepro(A,Ans) :- A =.. [*|Args] , shop(('05',A)) , map(prepro,Args,L) , prepro(fraction(L,[]),Ans) , ! .
prepro(A,Ans) :- A =.. [-|Args] , shop(('06',A)) , map(prepro,Args,L) , rev(L,[D|N]) , prepro(traction(N,[D]),Ans) , ! .
prepro(A,Ans) :- A =.. [/|Args] , shop(('07',A)) , map(prepro,Args,L) , rev(L,[D|N]) , prepro(fraction(N,[D]),Ans) , ! .
prepro(A@B=C,AP@B=CP) :- shop(('08',A@B)) , prepro(A,AP) , prepro(C,CP) , ! .
prepro(exp(X+Y),Ans) :- shop(('09',exp(X+Y))) , prepro(exp(X),X1) , prepro(fraction([X1,exp(Y)],[]),Ans) , ! .
prepro(X,Ans) :- shop(('10',X)) , flat(X,Ans) , ! .

map(_,[],[]). map(F,[H|T],[H1|T1]) :- call(F,H,H1) , map(F,T,T1) .
mem(E,L) :- basics:member(E,L) .
sel(E,L,A) :- basics:select(E,L,A) .
app(F,B,L) :- basics:append(F,B,L) .
rev(L1,L2) :- basics:reverse(L1,L2) .

flat(fraction([H],[]),Ans) :- flat(H,Ans) , ! .
flat(traction(L,[]),Ans) :- sel(traction(A,B),L,L2) , app(A,L2,L3) , flat(traction(L3,B),Ans) , ! .
flat(traction(L,[]),traction(L2,[])) :- map(flat,L,L2) , ! .
flat(fraction(N,D),Ans) :- sel(fraction(A,B),N,N2) , app(A,N2,N3) , app(B,D,D2) , flat(fraction(N3,D2),Ans) , ! .
flat(fraction(N,D),Ans) :- sel(fraction(A,B),D,D2) , app(A,D2,D3) , app(B,N,N2) , flat(fraction(N2,D3),Ans) , ! .
flat(traction(N,D),Ans) :- sel(fraction(A,[]),D,D2) , flat(fraction(A,[]),A2) , fraction(A,[]) \= A2 , app([A2],D2,D3) , flat(traction(N,D3),Ans) , ! .
flat(traction(N,D),Ans) :- sel(traction(A,B),N,N2) , app(A,N2,N3) , app(B,D,D2) , flat(traction(N3,D2),Ans) , ! .
flat(traction(N,D),Ans) :- sel(traction(A,B),D,D2) , app(A,D2,D3) , app(B,N,N2) , flat(traction(N2,D3),Ans) , ! .
flat(X,X) :- ! .

empty(fraction(A,[]),traction([],[])) :- mem(traction([],[]),A) , ! .
empty(traction([A],[]),Ans) :- empty(A,Ans) , ! .
empty(traction(A,[]),traction(Ans,[])) :- sel(traction([],[]),A,B) , empty(B,Ans) , ! .
empty(fraction(A,B),Ans) :- map(empty,A,A1) , map(empty,B,B1) , Ans=fraction(A1,B1) , ! .
empty(traction(A,B),Ans) :- map(empty,A,A1) , map(empty,B,B1) , Ans=traction(A1,B1) , ! .
empty(X,X) :- ! .

haso(o) :- ! .
haso(fraction(A,_)) :- mem(X,A) , haso(X) , ! .

equal(X,X) :- ! .
equal(X,Y) :- go(X,Z) , ( Y=Z , ! ; go(Y,Z) ) , ! .

is0(X) :- equal(X, traction([],[]) ) , ! .
is1(X) :- equal(X, fraction([],[]) ) , ! .
nan(X) :- is0(Z) , X=fraction([Z],[Z]) , ! .
nan(X) :- haso(X) , ! .
 an(X) :- not(nan(X)) , ! .

take(E,[H|T],T) :- equal(E,H) , ! .
take(E,[H|T],[H|T0]) :- take(E,T,T0) , ! .

sub(N1,D1,N0,D0) :- D1=[H|T] , not(is0(H)) , take(H,N1,Ni) , sub(Ni,T,N0,D0) , ! .
sub(N1,D1,N0,D0) :- D1=[H|T] , sub(N1,T,N0,Di) , D0 = [H|Di] , ! .
sub(N,D,N,D) :- ! .

subd(N1,D1,N0,D0) :- sub(N1,D1,N0,D0) , N1\=N0 , ! .

expand(fraction(L,[]),Ans) :- app(Front,[traction(S,[])|Back],L) , app(Front,Back,L2) , expand1(traction(S,[]),fraction(L2,[]),E1) , expand(E1,Ans) , ! .
expand(traction(A,B),traction(EA,EB)) :- map(expand,A,EA) , map(expand,B,EB) , ! .
expand(X,X) :- ! .
expand1(traction([],[]),fraction(_,[]),traction([],[])) :- ! .
expand1(traction([SH|ST],[]),fraction(P,[]),traction(E,[])) :- app([SH],P,EH) , expand1(traction(ST,[]),fraction(P,[]),traction(ET,[])) , app([fraction(EH,[])],ET,E) , ! .

flatterms(E,A) :- expand(E,F) , flat(F,traction(A,[])) , ! .

reduce(_,[H],H) .
reduce(F,[H|T],R) :- reduce(F,T,RL) , call(F,H,RL,R) .

multi_intersect([],_,[]) :- ! .
multi_intersect([H|T],L,[H|A]) :- app(F,[H|B],L) , app(F,B,L2) , multi_intersect(T,L2,A) , ! .
multi_intersect([_|T],L,A) :- multi_intersect(T,L,A) , ! .

multi_diff([],N,N) :- ! .
multi_diff([H|T],N,A) :- app(F,[H|B],N) , app(F,B,N2) , multi_diff(T,N2,A) , ! .
multi_diff([_|T],N,A) :- multi_diff(T,N,A) , ! .

factors(N,[N]) :- number(N) , ! .
factors(N,[N]) :- atom(N) , ! .
factors(fraction(F,[]),F) :- ! .
factors(traction([F],[]),A) :- factors(F,A) , ! .
factors(traction(S,[]),[fraction(D1,[]),traction(N3,[])]) :- map(factors,S,N1) , reduce(multi_intersect,N1,D1) , map(multi_diff(D1),N1,N2) , bagof(fraction(X,[]),mem(X,N2),N3) , ! .
factors(traction(X,Y),[fraction(I,[]),traction([fraction(X2,[])],[fraction(Y2,[])])]) :- factors(traction(X,[]),X1),factors(traction(Y,[]),Y1) , multi_intersect(X1,Y1,I) , multi_diff(I,X1,X2) , multi_diff(I,Y1,Y2) , ! .

flatfactors(E,A) :- factors(E,F) , flat(fraction(F,[]),fraction(A,[])) , ! .

go(A,Ans) :- show(('01',A)) , empty(A,B) , A\=B , go(B,Ans) , succ(('01',Ans)) , ! .
go(A,Ans) :- show(('02',A)) ,  flat(A,B) , A\=B , go(B,Ans) , succ(('02',Ans)) , ! .
go(traction(A,B),Ans) :- show(('03',traction(A,B))) , flatterms(traction(A,[]),A1) , flatterms(traction(B,[]),B1) , subd(A1,B1,A2,B2) , Ans=traction(A2,B2) , succ(('03',Ans)) , ! .
go(fraction(L,[]),Ans) :- show(('04',fraction(L,[]))) , is0(Zero) , mem(Zero,L) , Ans=traction([],[]) , succ(('04',Ans)) , ! .
go(fraction(Zero,D),Ans) :- show(('05',fraction(Zero,D))) , mem(Z,Zero) , is0(Z) , ( mem(Z2,D) , is0(Z2) -> nan(Ans) ; Ans=traction([],[]) ) , succ(('05',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('06',fraction(N,D))) , subd(N,D,N2,D2) , Ans=fraction(N2,D2) , succ(('06',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('07',fraction(N,D))) , D\=[] , expand(fraction(N,[]),E) , go(E,G) , flatfactors(G,N1) , subd(N1,D,N2,D2) , Ans=fraction(N2,D2) , succ(('07',Ans)) , ! .
go(A@X,Ans) :- show(('08',A@X)) , Max=1 , mem(N,[0,Max]) , eval(A@X,N,Ans) , ( N=Max ; an(Ans) ) , succ(('08',Ans)) , ! .
go(X,X) :- show(('09',X)) , succ(('09',X)) , ! .

eval(A@X=Y,N,Ans):- show(('ev',A@X=Y)) , if(is0(Y),norder(exp(X),N,R),R=exp(Y)) , replace(exp(X),R,A,B), go(B,C) , replace(X,Y,C,Sub) , go(Sub,Ans) , succ(('ev',Ans)) , ! .

norder(exp(X),0,traction([fraction([],[]),fraction([o,X],[])],[])) :- ! .
norder(exp(X),1,traction([fraction([],[]),X,fraction([o,X,X],[])],[])) :- ! .

isleaf(X) :- ( atomic(X) ; X=exp(_) ) , ! .
replace(OldLeaf,NewLeaf,OldTree,NewTree) :- isleaf(OldTree) , (OldTree = OldLeaf -> NewTree = NewLeaf ; NewTree = OldTree) , ! .
replace(OldLeaf,NewLeaf,OldTree,NewTree) :- =..(OldTree,[Name|Args]) , map(replace(OldLeaf,NewLeaf),Args,Args2) , =..(NewTree,[Name|Args2]) , ! .

postpro(traction([],[]),0) :- shop(('01',0-0)) , ! .
postpro(traction([A],[]),AP) :- shop(('02',A-0)) , postpro(A,AP) , ! .
postpro(traction(L,[]),AP+BP) :- shop(('03',L-0)) , app(L1,[E],L) , postpro(traction(L1,[]),AP) , postpro(E,BP) , ! .
postpro(fraction([],[]),1) :- shop(('04',1/1)) , ! .
postpro(fraction([A],[]),AP) :- shop(('05',A/1)) , postpro(A,AP) , ! .
postpro(fraction(L,[]),AP*BP) :- shop(('06',L/1)) , app(L1,[E],L) , postpro(fraction(L1,[]),AP) , postpro(E,BP) , ! .
postpro(traction([],A),Ans) :- shop(('07',0-A)) , postpro(traction(A,[]),AP) , number(AP) , is(Ans,-AP) , ! .
postpro(traction([],A),-(AP)) :- shop(('08',0-A)) , postpro(traction(A,[]),AP) , ! .
postpro(traction(A,B),AP-BP) :- shop(('09',A-B)) , postpro(traction(A,[]),AP) , postpro(traction(B,[]),BP) , ! .
postpro(fraction(A,B),AP/BP) :- shop(('10',A/B)) , postpro(fraction(A,[]),AP) , postpro(fraction(B,[]),BP) , ! .
postpro(exp(A),exp(AP)) :- shop(('11',exp(A))) , postpro(A,AP) , ! .
postpro(X,X) :- shop(('12',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show('PreProcessing') , prepro(X,Answer) .	%	PreProcess
<---(Answer,X) :- <----(P,X) , show('Simplifying') , go(P,Answer) .							%	Simplify
<--(Answer,X) :- <---(Simp,X) , show('PostProcessing') , postpro(Simp,Answer) .				%	PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , go(P,Simp) , postpro(Simp,Answer) .	%	Quiet
