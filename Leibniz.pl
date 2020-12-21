
%	Author:		Anthony John Ripa
%	Date:		2020.12.20
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

shop(Text) :- if(see, (write('□ '),writeln(Text))) .
show(Text) :- if(see, (write('. '),writeln(Text))) .
succ(Text) :- if(see, (write('+ '),writeln(Text))) .

prepro(0,traction([],[])) :- shop(('01',0)) , ! .
prepro(1,fraction([],[])) :- shop(('02',1)) , ! .
prepro(f(X),Ans) :- shop(('03',f(X))) , prepro(X*X, Ans) , ! .
prepro(A+B,Ans) :- shop(('04',A+B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , append(AL,BL,LR) , prepro(traction(LR,[]),Ans) , ! .
prepro(A*B,Ans) :- shop(('05',A*B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , append(AL,BL,LR) , prepro(fraction(LR,[]),Ans) , ! .
prepro(A-B,Ans) :- shop(('06',A-B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , prepro(traction(AL,BL),Ans) , ! .
prepro(A/B,Ans) :- shop(('07',A/B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , prepro(fraction(AL,BL),Ans) , ! .
prepro(A@B=C, AP@B=CP) :- shop(('08',A@B)) , prepro(A,AP) , prepro(C,CP) , ! .
prepro(+A,Ans) :- shop(('09',+A)) , prepro(A,Ans) , ! .
prepro(*A,Ans) :- shop(('10',*A)) , prepro(A,Ans) , ! .
prepro(-A,Ans) :- shop(('11',-A)) , prepro(A,AP) , getsum(AP,AL) , prepro(traction([],AL),Ans) , ! .
prepro(/A,Ans) :- shop(('12',/A)) , prepro(A,AP) , getprod(AP,AL) , prepro(fraction([],AL),Ans) , ! .
prepro(exp(X+Y+Z),fraction([exp(X),exp(Y),exp(Z)],[])) :- shop(('13',f(X))) , ! .
prepro(exp(X+Y),fraction([exp(X),exp(Y)],[])) :- shop(('14',f(X))) , ! .
prepro(X,Ans) :- shop(('15',X)) , flat(X,Ans) , ! .

flat(fraction([H],[]),Ans) :- flat(H,Ans) , ! .
flat(traction(L,[]),Ans) :- select(traction(A,B),L,L2) , append(A,L2,L3) , flat(traction(L3,B),Ans) , ! .
flat(traction(L,[]),traction(L2,[])) :- maplist(flat,L,L2) , ! .
flat(fraction(N,D),Ans) :- select(fraction(A,B),N,N2) , append(A,N2,N3) , append(B,D,D2) , flat(fraction(N3,D2),Ans) , ! .
flat(fraction(N,D),Ans) :- select(fraction(A,B),D,D2) , append(A,D2,D3) , append(B,N,N2) , flat(fraction(N2,D3),Ans) , ! .
flat(traction(N,D),Ans) :- select(fraction(A,[]),D,D2) , flat(fraction(A,[]),A2) , fraction(A,[]) \= A2 , append([A2],D2,D3) , flat(traction(N,D3),Ans) , ! .
flat(traction(N,D),Ans) :- select(traction(A,B),N,N2) , append(A,N2,N3) , append(B,D,D2) , flat(traction(N3,D2),Ans) , ! .
flat(traction(N,D),Ans) :- select(traction(A,B),D,D2) , append(A,D2,D3) , append(B,N,N2) , flat(traction(N2,D3),Ans) , ! .
flat(X,X) :- ! .

empty(fraction(A,[]),traction([],[])) :- member(traction([],[]),A) , ! .
empty(traction([A],[]),Ans) :- empty(A,Ans) , ! .
empty(traction(A,[]),traction(Ans,[])) :- select(traction([],[]),A,B) , empty(B,Ans) , ! .
empty(fraction(A,B),Ans) :- maplist(empty,A,A1) , maplist(empty,B,B1) , Ans=fraction(A1,B1) , ! .
empty(traction(A,B),Ans) :- maplist(empty,A,A1) , maplist(empty,B,B1) , Ans=traction(A1,B1) , ! .
empty(X,X) :- ! .

haso(o) :- ! .
haso(fraction(A,_)) :- member(X,A) , haso(X) , ! .

getprod(L,Ans) :- maplist(getprod,L,Ans) , ! .
getprod(A,Ans) :- flat(A,fraction(Ans,[])) , ! .
getprod(A,[A1]) :- flat(A,A1) , ! .

getsum(L,Ans) :- maplist(getsum,L,Ans) , ! .
getsum(A,Ans) :- flat(A,traction(Ans,[])) , ! .
getsum(A,[A1]) :- flat(A,A1) , ! .	

equal(X,X) :- ! .
equal(X,Y) :- go(X,Z) , ( Y=Z , ! ; go(Y,Z) ) , ! .

is0(X) :- equal(X, traction([],[]) ) ; equal(X, traction([],[]) ) , ! .
is1(X) :- equal(X, fraction([],[]) ) , ! .
nan(X) :- =(X, fraction([traction([],[])],[traction([],[])])) , ! .
nan(X) :- haso(X) , ! .
 an(X) :- not(nan(X)) , ! .

take(E,[H|T],T) :- equal(E,H) , ! .
take(E,[H|T],[H|T0]) :- take(E,T,T0) , ! .

sub(N1,D1,N0,D0) :- D1=[H|T] , not(equal(H,traction([],[]))) , take(H,N1,Ni) , sub(Ni,T,N0,D0) , ! .
sub(N1,D1,N0,D0) :- D1=[H|T] , sub(N1,T,N0,Di) , D0 = [H|Di] , ! .
sub(N,D,N,D) :- ! .

subd(N1,D1,N0,D0) :- sub(N1,D1,N0,D0) , N1\=N0 , ! .

expand(fraction(L,[]),Ans) :- append(Front,[traction(S,[])|Back],L) , append(Front,Back,L2) , expand1(traction(S,[]),fraction(L2,[]),E1) , expand(E1,Ans) , ! .
expand(traction(A,B),traction(EA,EB)) :- maplist(expand,A,EA) , maplist(expand,B,EB) , ! .
expand(X,X) :- ! .
expand1(traction([],[]),fraction(_,[]),traction([],[])) :- ! .
expand1(traction([SH|ST],[]),fraction(P,[]),traction(E,[])) :- append([SH],P,EH) , expand1(traction(ST,[]),fraction(P,[]),traction(ET,[])) , append([fraction(EH,[])],ET,E) , ! .

reduce(_,[H],H) .
reduce(F,[H|T],R) :- reduce(F,T,RL) , call(F,H,RL,R) .

multi_intersect([],_,[]) :- ! .
multi_intersect([H|T],L,[H|A]) :- append(F,[H|B],L) , append(F,B,L2) , multi_intersect(T,L2,A) , ! .
multi_intersect([_|T],L,A) :- multi_intersect(T,L,A) , ! .

multi_diff([],N,N) :- ! .
multi_diff([H|T],N,A) :- append(F,[H|B],N) , append(F,B,N2) , multi_diff(T,N2,A) , ! .
multi_diff([_|T],N,A) :- multi_diff(T,N,A) , ! .

factors(N,[N]) :- number(N) , ! .
factors(N,[N]) :- atom(N) , ! .
factors(fraction(F,[]),F) :- ! .
factors(traction([F],[]),A) :- factors(F,A) , ! .
factors(traction(S,[]),[fraction(D1,[]),traction(N3,[])]) :- maplist(factors,S,N1) , reduce(multi_intersect,N1,D1) , maplist(multi_diff(D1),N1,N2) , bagof(fraction(X,[]),member(X,N2),N3) , ! .
factors(traction(X,Y),[fraction(I,[]),traction([fraction(X2,[])],[fraction(Y2,[])])]) :- factors(traction(X,[]),X1),factors(traction(Y,[]),Y1) , multi_intersect(X1,Y1,I) , multi_diff(I,X1,X2) , multi_diff(I,Y1,Y2) , ! .

factor(E,A) :- factors(E,F) , flat(fraction(F,[]),A) , ! .

go(A,Ans) :- show(('01',A)) , empty(A,B) , A\=B , go(B,Ans) , succ(('01',Ans)) , ! .
go(A,Ans) :- show(('02',A)) ,  flat(A,B) , A\=B , go(B,Ans) , succ(('02',Ans)) , ! .
go(traction(A,B),Ans) :- show(('03',traction(A,B))) , expand(traction(A,[]),A0) , getsum(A0,A1) , expand(traction(B,[]),B0) , getsum(B0,B1) , subd(A1,B1,A2,B2) , go(traction(A2,B2),Ans) , succ(('03',Ans)) , ! .
go(fraction(L,[]),traction([],[])) :- show(('04',fraction(L,[]))) , is0(Zero) , member(Zero,L) , succ(('04',traction([],[]))) , ! .
go(fraction(Zero,D),Ans) :- show(('05',fraction(Zero,D))) , member(Z,Zero) , is0(Z) , ( member(Z2,D) , is0(Z2) -> nan(Ans) ; Ans=traction([],[]) ) , succ(('05',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('06',fraction(N,D))) , factor(fraction(N,[]),F) , getprod(F,N1) , subd(N1,D,N2,D2) , go(fraction(N2,D2),Ans) , succ(('06',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('07',fraction(N,D))) , D\=[] , expand(fraction(N,[]),E) , go(E,G) , factor(G,F) , getprod(F,N1) , subd(N1,D,N2,D2) , go(fraction(N2,D2),Ans) , succ(('07',Ans)) , ! .
go(A@X,Ans) :- show(('08',A@X)) , Max=1 , member(N,[0,Max]) , eval(A@X,N,Ans) , ( N=Max ; an(Ans) ) , succ(('08',Ans)) , ! .
go(X,X) :- show(('09',X)) , succ(('09',X)) , ! .

eval(A@X=Y,N,Ans):- show(('ev',A@X=Y)) , if(is0(Y),norder(exp(X),N,R),R=exp(Y)) , replace(exp(X),R,A,B), go(B,C) , replace(X,Y,C,Sub) , go(Sub,Ans) , succ(('ev',Ans)) , ! .

norder(exp(X),0,traction([fraction([],[]),fraction([o,X],[])],[])) :- ! .
norder(exp(X),1,traction([fraction([],[]),X,fraction([o,X,X],[])],[])) :- ! .

isleaf(X) :- ( atomic(X) ; X=exp(_) ) , ! .
cna(C,N,A) :- compound_name_arguments(C,N,A) .
replace(OldLeaf,NewLeaf,OldTree,NewTree) :- isleaf(OldTree) , (OldTree = OldLeaf -> NewTree = NewLeaf ; NewTree = OldTree) , ! .
replace(OldLeaf,NewLeaf,OldTree,NewTree) :- cna(OldTree, Name, Args) , maplist(replace(OldLeaf,NewLeaf),Args,Args2) , cna(NewTree, Name, Args2) , ! .

postpro(traction([],[]),0) :- shop(('01',sum([]))) , ! .
postpro(traction([A],[]),AP) :- shop(('02',sum([A]))) , postpro(A,AP) , ! .
postpro(traction(L,[]),AP+BP) :- shop(('03',sum(L))) , append(L1,[E],L) , postpro(traction(L1,[]),AP) , postpro(E,BP) , ! .
postpro(fraction([],[]),1) :- shop(('04',prod([]))) , ! .
postpro(fraction([A],[]),AP) :- shop(('05',prod([A]))) , postpro(A,AP) , ! .
postpro(fraction(L,[]),AP*BP) :- shop(('06',prod(L))) , append(L1,[E],L) , postpro(fraction(L1,[]),AP) , postpro(E,BP) , ! .
postpro(traction([],A),Ans) :- shop(('07',0-A)) , postpro(traction(A,[]),AP) , number(AP) , is(Ans,-AP) , ! .
postpro(traction([],A),-(AP)) :- shop(('08',0-A)) , postpro(traction(A,[]),AP) , ! .
postpro(traction(A,B),AP-BP) :- shop(('09',A-B)) , postpro(traction(A,[]),AP) , postpro(traction(B,[]),BP) , ! .
postpro(fraction(A,B),AP/BP) :- shop(('10',A/B)) , postpro(fraction(A,[]),AP) , postpro(fraction(B,[]),BP) , ! .
postpro(exp(A),exp(AP)) :- shop(('11',exp(A))) , postpro(A,AP) , ! .
postpro(X,X) :- shop(('12',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show("PreProcessing") , prepro(X,Answer) .	%	PreProcess
<---(Answer,X) :- <----(P,X) , show("Simplifying") , go(P,Answer) .							%	Simplify
<--(Answer,X) :- <---(Simp,X) , show("PostProcessing") , postpro(Simp,Answer) .				%	PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , go(P,Simp) , postpro(Simp,Answer) .	%	Quiet
