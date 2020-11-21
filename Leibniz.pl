
%	Author:		Anthony John Ripa
%	Date:		2020.11.20
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

prepro(0,sum([])) :- shop(('01',0)) , ! .
prepro(1,prod([])) :- shop(('02',1)) , ! .
prepro(f(X),Ans) :- shop(('03',f(X))) , prepro(X*X, Ans) , ! .
prepro(A+B,Ans) :- shop(('04',A+B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , append(AL,BL,LR) , prepro(sum(LR),Ans) , ! .
prepro(A*B,Ans) :- shop(('05',A*B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , append(AL,BL,LR) , prepro(prod(LR),Ans) , ! .
prepro(A-B,Ans) :- shop(('06',A-B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , prepro(traction(AL,BL),Ans) , ! .
prepro(A/B,Ans) :- shop(('07',A/B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , prepro(fraction(AL,BL),Ans) , ! .
prepro(A@B=C, AP@B=CP) :- shop(('08',A@B)) , prepro(A,AP) , prepro(C,CP) , ! .
prepro(+A,Ans) :- shop(('09',+A)) , prepro(A,Ans) , ! .
prepro(*A,Ans) :- shop(('10',*A)) , prepro(A,Ans) , ! .
prepro(-A,Ans) :- shop(('11',-A)) , prepro(A,AP) , getsum(AP,AL) , prepro(traction([],AL),Ans) , ! .
prepro(/A,Ans) :- shop(('12',/A)) , prepro(A,AP) , getprod(AP,AL) , prepro(fraction([],AL),Ans) , ! .
prepro(exp(X+Y+Z),prod([exp(X),exp(Y),exp(Z)])) :- shop(('13',f(X))) , ! .
prepro(exp(X+Y),prod([exp(X),exp(Y)])) :- shop(('14',f(X))) , ! .
prepro(X,Ans) :- shop(('15',X)) , flat(X,Ans) , ! .

flat(prod([H]),Ans) :- flat(H,Ans) , ! .
flat(sum([H]),Ans) :- flat(H,Ans) , ! .
flat(prod(L),Ans) :- select(prod(A),L,L2) , append(A,L2,L3) , flat(prod(L3),Ans) , ! .
flat(prod(L),Ans) :- select(fraction(A,B),L,L2) , append(A,L2,L3) , flat(fraction(L3,B),Ans) , ! .
flat(sum(L),Ans) :- select(sum(A),L,L2) , append(A,L2,L3) , flat(sum(L3),Ans) , ! .
flat(sum(L),Ans) :- select(traction(A,B),L,L2) , append(A,L2,L3) , flat(traction(L3,B),Ans) , ! .
flat(prod(L),prod(L2)) :- maplist(flat,L,L2) , ! .
flat(sum(L),sum(L2)) :- maplist(flat,L,L2) , ! .
flat(fraction([N],[]),Ans) :- flat(N,Ans) , ! .
flat(traction([N],[]),Ans) :- flat(N,Ans) , ! .
flat(fraction(N,D),Ans) :- select(prod(A),N,N2) , append(A,N2,N3) , flat(fraction(N3,D),Ans) , ! .
flat(traction(N,D),Ans) :- select(sum(A),N,N2) , append(A,N2,N3) , flat(traction(N3,D),Ans) , ! .
flat(traction(N,D),Ans) :- select(sum(A),D,D2) , append(A,D2,D3) , flat(traction(N,D3),Ans) , ! .
flat(fraction(N,D),Ans) :- select(fraction(A,B),N,N2) , append(A,N2,N3) , append(B,D,D2) , flat(fraction(N3,D2),Ans) , ! .
flat(fraction(N,D),Ans) :- select(fraction(A,B),D,D2) , append(A,D2,D3) , append(B,N,N2) , flat(fraction(N2,D3),Ans) , ! .
flat(fraction(N,D),Ans) :- select(traction(A,B),N,N2) , flat(traction(A,B),T) , traction(A,B) \= T , append([T],N2,N3) , flat(fraction(N3,D),Ans) , ! .
flat(traction(N,D),Ans) :- select(prod(A),N,N2) , flat(prod(A),A2) , prod(A) \= A2 , append([A2],N2,N3) , flat(traction(N3,D),Ans) , ! .
flat(traction(N,D),Ans) :- select(prod(A),D,D2) , flat(prod(A),A2) , prod(A) \= A2 , append([A2],D2,D3) , flat(traction(N,D3),Ans) , ! .
flat(traction(N,D),Ans) :- select(traction(A,B),N,N2) , append(A,N2,N3) , append(B,D,D2) , flat(traction(N3,D2),Ans) , ! .
flat(traction(N,D),Ans) :- select(traction(A,B),D,D2) , append(A,D2,D3) , append(B,N,N2) , flat(traction(N2,D3),Ans) , ! .
flat(L,Ans) :- maplist(flat,L,Ans) , ! .
flat(X,X) :- ! .

getprod(L,Ans) :- maplist(getprod,L,Ans) , ! .
getprod(A,Ans) :- flat(A,prod(Ans)) , ! .
getprod(A,[A1]) :- flat(A,A1) , ! .

getsum(L,Ans) :- maplist(getsum,L,Ans) , ! .
getsum(A,Ans) :- flat(A,sum(Ans)) , ! .
getsum(A,[A1]) :- flat(A,A1) , ! .	

equal(X,X) :- ! .
equal(X,Y) :- go(X,Z) , ( Y=Z , ! ; go(Y,Z) ) , ! .

is0(X) :- equal(X, sum([]) ) ; equal(X, traction([],[]) ) .
is1(X) :- equal(X, prod([])		   ) , ! .
nan(X) :- =(X, sum([])/sum([]) ) , ! .
 an(X) :- not(nan(X))				 , ! .

take(E,[H|T],T) :- equal(E,H) , ! .
take(E,[H|T],[H|T0]) :- take(E,T,T0) , ! .

sub(N1,D1,N0,D0) :- D1=[H|T] , not(equal(H,sum([]))) , take(H,N1,Ni) , sub(Ni,T,N0,D0) , ! .
sub(N1,D1,N0,D0) :- D1=[H|T] , sub(N1,T,N0,Di) , D0 = [H|Di] , ! .
sub(N,D,N,D) :- ! .

subd(N1,D1,N0,D0) :- sub(N1,D1,N0,D0) , N1\=N0 , ! .

expandf(X,Ans) :- expand(X,E) , flat(E,Ans) , ! .
expand(sum(L),sum(EL)) :- maplist(expand,L,EL) , ! .
expand(prod(L),Ans) :- append(Front,[sum(S)|Back],L) , append(Front,Back,L2) , expand1(sum(S),prod(L2),E1) , expand(E1,Ans) , ! .
expand(traction(A,B),traction(EA,EB)) :- maplist(expand,A,EA) , maplist(expand,B,EB) , ! .
expand(X,X) :- ! .
expand1(sum([]),prod(_),sum([])) :- ! .
expand1(sum([SH|ST]),prod(P),sum(E)) :- append([SH],P,EH) , expand1(sum(ST),prod(P),sum(ET)) , append([prod(EH)],ET,E) , ! .

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
factors(prod([F]),A) :- factors(F,A) , ! .
factors(prod(F),F) :- ! .
factors(sum([F]),A) :- factors(F,A) , ! .
factors(sum(S),[prod(D1),sum(N3)]) :- maplist(factors,S,N1) , reduce(multi_intersect,N1,D1) , maplist(multi_diff(D1),N1,N2) , bagof(prod(X),member(X,N2),N3) , ! .
factors(traction(X,Y),[prod(I),traction([prod(X2)],[prod(Y2)])]) :- factors(sum(X),X1),factors(sum(Y),Y1) , multi_intersect(X1,Y1,I) , multi_diff(I,X1,X2) , multi_diff(I,Y1,Y2) , ! .
factors(exp(X),[exp(X)]) :- ! .

factor(E,A) :- factors(E,F) , flat(prod(F),A) , ! .

go(traction(A,B),Ans) :- show(('01',traction(A,B))) , flat(traction(A,B),T) , traction(A,B)\=T , go(T,Ans) , succ(('01',Ans)) , ! .
go(fraction(A,B),Ans) :- show(('02',fraction(A,B))) , flat(fraction(A,B),F) , fraction(A,B)\=F , go(F,Ans) , succ(('02',Ans)) , ! .
go(prod(L),Ans) :- show(('03',prod(L))) , maplist(go,L,S) , flat(prod(S),F) , prod(L)\=F , go(F,Ans) , succ(('03',Ans)) , ! .
go(prod(L),sum([])) :- show(('04',prod([sum([])]))) , is0(Zero) , member(Zero,L) , succ(('04',sum([]))) , ! .
go(sum([H|T]),Ans) :- show(('05',sum([H|T]))) , go(H,HS) , go(sum(T),TS) , getsum(HS,HL) , getsum(TS,TL) , append(HL,TL,LS) , Ans=sum(LS) , succ(('05',Ans)) , ! .
go(traction(A,B),Ans) :- show(('06',traction(A,B))) , expandf(sum(A),A0) , getsum(A0,A1) , expandf(sum(B),B0) , getsum(B0,[B1]) , select(B1,A1,L) , go(sum(L),Ans) , succ(('06',Ans)) , ! .
go(fraction(N,One),Ans) :- show(('07',fraction(N,One))) , is1(prod(One)) , go(prod(N),Ans) , succ(('07',Ans)) , ! .
go(fraction(Zero,D),Ans) :- show(('08',fraction(Zero,D))) , is0(prod(Zero)) , ( is0(prod(D)) -> nan(Ans) ; Ans=sum([]) ) , succ(('08',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('09',fraction(N,D))) , factor(prod(N),F) , getprod([F,prod(D)],[N1,D1])    , subd(N1,D1,N2,D2) , flat([N2,D2],[N3,D3]) , go(fraction(N3,D3),Ans) , succ(('09',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('10',fraction(N,D))) , go(prod(N),S) , expandf(S,E) , go(E,G) , factor(G,F) , getprod([F,prod(D)],[N1,D1])    , subd(N1,D1,N2,D2) , flat([N2,D2],[N3,D3]) , go(fraction(N3,D3),Ans) , succ(('10',Ans)) , ! .
go(traction(N,D),Ans) :- show(('11',traction(N,D))) ,                     getsum([sum(N),sum(D)],[N1,D1]) , subd(N1,D1,N2,D2) , flat([N2,D2],[N3,D3]) , go(traction(N3,D3),Ans) , succ(('11',Ans)) , ! .
go(fraction([N],[D]),Ans) :- show(('12',fraction(N,D))) , go(N,sum([H|T])) , go(D,D1) , go(fraction([H],[D1]),H1) , go(fraction([sum(T)],[D1]),T0) , getsum(T0,T1) , go(sum([H1|T1]),Ans) , succ(('12',Ans)) , ! .
go(A@X,Ans) :- show(('13',A@X)) , member(N,[0,1]) , eval(A@X,N,Ans) , an(Ans) , succ(('13',Ans)) , ! .
go(X,X) :- show(('14',X)) , succ(('14',X)) , ! .

eval(A@X=Y,N,Ans):- show(('ev',A@X=Y)) , if(is0(Y),norder(exp(X),N,R),R=exp(Y)) , replace(exp(X),R,A,B), if(N=0,B=C,go(B,C)) , replace(X,Y,C,Sub) , go(Sub,Ans) , succ(('ev',Ans)) , ! .

norder(exp(_),0,prod([])) :- ! .
norder(exp(X),1,sum([prod([]),X])) :- ! .

isleaf(X) :- ( atomic(X) ; X=exp(_) ) , ! .
cna(C,N,A) :- compound_name_arguments(C,N,A) .
replace(OldLeaf,NewLeaf,OldTree,NewTree) :- isleaf(OldTree) , (OldTree = OldLeaf -> NewTree = NewLeaf ; NewTree = OldTree) , ! .
replace(OldLeaf,NewLeaf,OldTree,NewTree) :- cna(OldTree, Name, Args) , maplist(replace(OldLeaf,NewLeaf),Args,Args2) , cna(NewTree, Name, Args2) , ! .

postpro(sum([]),0) :- shop(('01',sum([]))) , ! .
postpro(sum([A]),AP) :- shop(('02',sum([A]))) , postpro(A,AP) , ! .
postpro(sum(L),AP+BP) :- shop(('03',sum(L))) , append(L1,[E],L) , postpro(sum(L1),AP) , postpro(E,BP) , ! .
postpro(prod([]),1) :- shop(('04',prod([]))) , ! .
postpro(prod([A]),AP) :- shop(('05',prod([A]))) , postpro(A,AP) , ! .
postpro(prod(L),AP*BP) :- shop(('06',prod(L))) , append(L1,[E],L) , postpro(prod(L1),AP) , postpro(E,BP) , ! .
postpro(-A,Ans) :- shop(('07',-A)) , postpro(A,AP) , number(AP) , is(Ans,-AP) , ! .
postpro(A-B,Ans) :- shop(('08',A-B)) , postpro(A,0) , postpro(B,BP) , postpro(-BP,Ans) , ! .
postpro(A-B,AP-BP) :- shop(('09',A-B)) , postpro(A,AP) , postpro(B,BP) , ! .
postpro(A/B,AP/BP) :- shop(('10',A/B)) , postpro(A,AP) , postpro(B,BP) , ! .
postpro(fraction(A,B),AP/BP) :- shop(('11',A/B)) , postpro(prod(A),AP) , postpro(prod(B),BP) , ! .
postpro(traction(A,[]),AP) :- shop(('12',A-0)) , postpro( sum(A),AP) , ! .
postpro(traction([],A),Ans) :- shop(('13',0-A)) , postpro(sum(A),AP) , number(AP) , is(Ans,-AP) , ! .
postpro(traction([],A),-(AP)) :- shop(('14',0-A)) , postpro(sum(A),AP) , ! .
postpro(traction(A,B),AP-BP) :- shop(('15',A-B)) , postpro(sum(A),AP) , postpro(sum(B),BP) , ! .
postpro(exp(A),exp(AP)) :- shop(('16',exp(A))) , postpro(A,AP) , ! .
postpro(X,X) :- shop(('17',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show("PreProcessing") , prepro(X,Answer) .	%	PreProcess
<---(Answer,X) :- <----(P,X) , show("Simplifying") , go(P,Answer) .							%	Simplify
<--(Answer,X) :- <---(Simp,X) , show("PostProcessing") , postpro(Simp,Answer) .				%	PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , go(P,Simp) , postpro(Simp,Answer) .	%	Quiet
