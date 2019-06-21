
%	Author:		Anthony John Ripa
%	Date:		2019.06.20
%	Leibniz:	A Rule System for Math

:- op(0800,xfx,@).
:- op(0900,xfx,<-).
:- op(1000,xfx,<--).
:- op(1100,xfx,<---).
:- op(1200,xfx,<----).

:- dynamic see/0.

if(Condition,Statement) :- Condition->Statement ; true .

shop(Text) :- if(see, (write('□ '),writeln(Text))) .
show(Text) :- if(see, (write('. '),writeln(Text))) .
succ(Text) :- if(see, (write('+ '),writeln(Text))) .

prepro(0,sum([])) :- shop(('1',0)) , ! .
prepro(1,prod([])) :- shop(('2',1)) , ! .
prepro(f(X),Ans) :- shop(('3',f(X))) , prepro(X*X, Ans) , ! .
prepro(A+B,Ans) :- shop(('4',A+B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , append(AL,BL,LR) , prepro(sum(LR),Ans) , ! .
prepro(A*B,Ans) :- shop(('5',A*B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , append(AL,BL,LR) , prepro(prod(LR),Ans) , ! .
prepro(A-B,Ans) :- shop(('6',A-B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , prepro(traction(AL,BL),Ans) , ! .
prepro(A/B,Ans) :- shop(('7',A/B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , prepro(fraction(AL,BL),Ans) , ! .
prepro(A@B=C, AP@B=CP) :- shop(('8',A@B)) , prepro(A,AP) , prepro(C,CP) , ! .
prepro(X,Ans) :- shop(('9',X)) , flat(X,Ans) , ! .

flat(prod([H]),Ans) :- flat(H,Ans) , ! .
flat(sum([H]),Ans) :- flat(H,Ans) , ! .
flat(prod(L),Ans) :- select(prod(A),L,L2) , append(A,L2,L3) , flat(prod(L3),Ans) , ! .
flat(prod(L),Ans) :- select(fraction(A,B),L,L2) , append(A,L2,L3) , flat(fraction(L3,B),Ans) , ! .
flat(sum(L),Ans) :- select(sum(A),L,L2) , append(A,L2,L3) , flat(sum(L3),Ans) , ! .
flat(sum(L),Ans) :- select(traction(A,B),L,L2) , append(A,L2,L3) , flat(traction(L3,B),Ans) , ! .
flat(prod(L),prod(L2)) :- maplist(flat,L,L2) , ! .
flat(sum(L),sum(L2)) :- maplist(flat,L,L2) , ! .
flat(fraction(N,D),Ans) :- select(fraction(A,B),N,N2) , append(A,N2,N3) , append(B,D,D2) , flat(fraction(N3,D2),Ans) , ! .
flat(fraction(N,D),Ans) :- select(fraction(A,B),D,D2) , append(A,D2,D3) , append(B,N,N2) , flat(fraction(N2,D3),Ans) , ! .
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

take(E,[H|T],T) :- equal(E,H) , ! .
take(E,[H|T],[H|T0]) :- take(E,T,T0) , ! .

sub(N1,D1,N0,D0) :- D1=[H|T] , not(equal(H,sum([]))) , take(H,N1,Ni) , sub(Ni,T,N0,D0) , ! .
sub(N1,D1,N0,D0) :- D1=[H|T] , sub(N1,T,N0,Di) , D0 = [H|Di] , ! .
sub(N,D,N,D) :- ! .

subd(N1,D1,N0,D0) :- sub(N1,D1,N0,D0) , N1\=N0 , ! .

expandf(X,Ans) :- expand(X,E) , flat(E,Ans) , ! .
expand(sum(L),sum(EL)) :- maplist(expand,L,EL) , ! .
expand(prod(L),Ans) :- append(Front,[sum(S)|Back],L) , append(Front,Back,L2) , expand1(sum(S),prod(L2),E1) , expand(E1,Ans) , !.
expand(X,X) :- ! .
expand1(sum([]),prod(_),sum([])) :- ! .
expand1(sum([SH|ST]),prod(P),sum(E)) :- append([SH],P,EH) , expand1(sum(ST),prod(P),sum(ET)) , append([prod(EH)],ET,E) , ! .

go(prod(L),Ans) :- show(('01',prod(L))) , flat(prod(L),F) , prod(L)\=F , go(F,Ans) , succ(('01',Ans)) , ! .
go(prod(L),sum([])) :- show(('02',prod([sum([])]))) , member(sum([]),L) , succ(('02',sum([]))) , ! .
go(sum([H|T]),Ans) :- show(('03',sum([H|T]))) , go(H,HS) , go(sum(T),TS) , getsum(HS,HL) , getsum(TS,TL) , append(HL,TL,LS) , Ans=sum(LS) , succ(('03',Ans)) , ! .
go(A-B,Ans) :- show(('04',A-B)) , expandf(A,A0) , getsum(A0,A1) , expandf(B,B0) , getsum(B0,[B1]) , select(B1,A1,L) , go(sum(L),Ans) , succ(('04',Ans)) , ! .
go(N-Zero,Ans) :- show(('05',N-Zero)) , equal(Zero,sum([])) , go(N,Ans) , succ(('05',Ans)) , ! .
go(N/One,Ans) :- show(('06',N/One)) , equal(One,prod([])) , go(N,Ans) , succ(('06',Ans)) , ! .
go(Zero/D,sum([])) :- show(('07',Zero/D)) , equal(Zero,sum([])) , not(equal(D,sum([]))) , succ(('07',sum([]))) , ! .
go(N/D,Ans) :- show(('08',N/D)) , getprod([N,D],[N1,D1]) , subd(N1,D1,N0,D0) , flat([prod(N0),prod(D0)],[N2,D2]) , go(N2/D2,Ans) , succ(('08',Ans)) , ! .
go(N-D,Ans) :- show(('09',N-D)) , getsum([N,D],[N1,D1]) , subd(N1,D1,N0,D0) , flat([sum(N0),sum(D0)],[N2,D2]) , go(N2-D2,Ans) , succ(('09',Ans)) , ! .
go(N/D,Ans) :- show(('10',N/D)) , go(N,sum([H|T])) , go(D,D1) , go(H/D1,H1) , go(sum(T)/D1,T0) , getsum(T0,T1) , go(sum([H1|T1]),Ans) , succ(('10',Ans)) , ! .
go(fraction(N,D),Ans) :- show(('11',f(N/D))) , go(prod(N)/prod(D),Ans) , succ(('11',Ans)) , ! .
go(traction(N,D),Ans) :- show(('12',f(N-D))) , go(sum(N)-sum(D),Ans) , succ(('12',Ans)) , ! .
go(A@A=B,B) :- show(('13',A@A=B)) , succ(('13',B)) , ! .
go(A@B=C,Ans) :- show(('14',A@B=C)) , go(A,sum([H|T])) , go(H@B=C,H1) , go(sum(T)@B=C,sum(T1)) , go(sum([H1|T1]),Ans) , succ(('14',Ans)) , ! .
go(A@B=C,Ans) :- show(('15',A@B=C)) , go(A,prod([H|T])) , go(H@B=C,H1) , go(prod(T)@B=C,prod(T1)) , go(prod([H1|T1]),Ans) , succ(('15',Ans)) , ! .
go(A@B,Ans) :- show(('16',A@B)) , go(A,Ans) , succ(('16',Ans)) , ! .
go(X,X) :- show(('17',X)) , succ(('17',X)) , ! .

postpro(sum([]),0) :- shop(('01',sum([]))) , ! .
postpro(sum([A]),AP) :- shop(('02',sum([A]))) , postpro(A,AP) , ! .
postpro(sum(L),AP+BP) :- shop(('03',sum(L))) , append(L1,[E],L) , postpro(sum(L1),AP) , postpro(E,BP) , ! .
postpro(prod([]),1) :- shop(('04',prod([]))) , ! .
postpro(prod([A]),AP) :- shop(('05',prod([A]))) , postpro(A,AP) , ! .
postpro(prod(L),AP*BP) :- shop(('06',prod(L))) , append(L1,[E],L) , postpro(prod(L1),AP) , postpro(E,BP) , ! .
postpro(A-B,AP-BP) :- shop(('07',A-B)) , postpro(A,AP) , postpro(B,BP) , ! .
postpro(A/B,AP/BP) :- shop(('08',A/B)) , postpro(A,AP) , postpro(B,BP) , ! .
postpro(traction(A,B),AP-BP) :- shop(('09',A-B)) , postpro(sum(A),AP) , postpro(sum(B),BP) , ! .
postpro(X,X) :- shop(('10',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show("PreProcessing") , prepro(X,Answer) .	%	PreProcess
<---(Answer,X) :- <----(P,X) , show("Simplifying") , go(P,Answer) .							%	Simplify
<--(Answer,X) :- <---(Simp,X) , show("PostProcessing") , postpro(Simp,Answer) .				%	PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , go(P,Simp) , postpro(Simp,Answer) .	%	Quiet
