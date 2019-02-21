
%	Author:		Anthony John Ripa
%	Date:		2019.02.20
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
prepro(A+B,Ans) :- shop(('4',A+B)) , prepro(A,AP) , prepro(B,BP) , getsum(AP,AL) , getsum(BP,BL) , conc(AL,BL,LR) , prepro(sum(LR),Ans) , ! .
prepro(A*B,Ans) :- shop(('5',A*B)) , prepro(A,AP) , prepro(B,BP) , getprod(AP,AL) , getprod(BP,BL) , conc(AL,BL,LR) , prepro(prod(LR),Ans) , ! .
prepro(A-B,AP-BP) :- shop(('6',A-B)) , prepro(A,AP) , prepro(B,BP) , ! .
prepro(A/B,AP/BP) :- shop(('7',A/B)) , prepro(A,AP) , prepro(B,BP) , ! .
prepro(A@B=C, AP@B=CP) :- shop(('8',A/B)) , prepro(A,AP) , prepro(C,CP) , ! .
prepro(X,X) :- shop(('9',X)) , ! .

flat(prod([H]),Ans) :- flat(H,Ans) , ! .
flat(sum([H]),Ans) :- flat(H,Ans) , ! .
flat(prod(L),Ans) :- select(prod(A),L,L2) , append(A,L2,L3) , flat(prod(L3),Ans) , ! .
flat(sum(L),Ans) :- select(sum(A),L,L2) , append(A,L2,L3) , flat(sum(L3),Ans) , ! .
flat(prod(L),prod(L2)) :- maplist(flat,L,L2) , ! .
flat(sum(L),sum(L2)) :- maplist(flat,L,L2) , ! .
flat(X,X) :- ! .

getprod(A,Ans) :- flat(A,prod(Ans)) , ! .
getprod(A,[A1]) :- flat(A,A1) , ! .

getsum(A,Ans) :- flat(A,sum(Ans)) , ! .
getsum(A,[A1]) :- flat(A,A1) , ! .	

conc(A,B,Ans) :- append(A,B,C) , msort(C,S) , reverse(S,Ans) .

go(prod(L),Ans) :- show(('01',prod(L))) , conc(L,[],LR) , L\=LR , go(prod(LR),Ans) , succ(('01',Ans)) , ! .
go(prod([sum([]),B]),sum([])) :- show(('02',prod([sum([]),B]))) , succ(('02',sum([]))) , ! .
go(prod([sum([H|T]),B]),Ans) :- show(('03',prod([sum([H|T]),B]))) , SH=prod([H,B]) , go(prod([sum(T),B]),ST) , go(sum([SH|[ST]]),Ans) , succ(('03',Ans)) , ! .
go(prod([A,B,C|T]),Ans) :- show(('04',prod([A,B,C]))) , go(prod([A,B]),AB) , go(prod([AB,C|T]),Ans) , succ(('04',Ans)) , ! .
go(sum([H|T]),Ans) :- show(('05',sum([H|T]))) , go(H,HS) , go(sum(T),TS) , getsum(HS,HL) , getsum(TS,TL) , conc(HL,TL,LS) , Ans=sum(LS) , succ(('05',Ans)) , ! .
go(A-B,Ans) :- show(('06',A-B)) , go(A,A0) , getsum(A0,A1) , go(B,B0) , getsum(B0,[B1]) , select(B1,A1,L) , go(sum(L),Ans) , succ(('06',Ans)) , ! .
go(N/D,Ans) :- show(('07',N/D)) , go(N,N0) , getprod(N0,N1) , go(D,D1) , D1\=sum([]) , select(D1,N1,L) , go(prod(L),Ans) , succ(('07',Ans)) , ! .
go(N/D,Ans) :- show(('08',N/D)) , go(N,sum([H|T])) , go(D,D1) , go(H/D1,H1) , go(sum(T)/D1,T0) , getsum(T0,T1) , go(sum([H1|T1]),Ans) , succ(('08',Ans)) , ! .
go(A@A=B,B) :- show(('09',A@A=B)) , succ(('09',B)) , ! .
go(A@B=C,Ans) :- show(('10',A@B=C)) , go(A,sum([H|T])) , go(H@B=C,H1) , go(sum(T)@B=C,sum(T1)) , go(sum([H1|T1]),Ans) , succ(('10',Ans)) , ! .
go(A@B=C,Ans) :- show(('11',A@B=C)) , go(A,prod([H|T])) , go(H@B=C,H1) , go(prod(T)@B=C,prod(T1)) , go(prod([H1|T1]),Ans) , succ(('11',Ans)) , ! .
go(A@B,Ans) :- show(('12',A@B)) , go(A,Ans) , succ(('12',Ans)) , ! .
go(X,X) :- show(('13',X)) , succ(('13',X)) , ! .

postpro(prod([]),1) :- shop(('1',prod([]))) , ! .
postpro(sum([]),0) :- shop(('2',sum([]))) , ! .
postpro(X,X) :- shop(('3',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show("PreProcessing") , prepro(X,Answer) .	%	PreProcess
<---(Answer,X) :- <----(P,X) , show("Simplifying") , go(P,Answer) .							%	Simplify
<--(Answer,X) :- <---(Simp,X) , show("PostProcessing") , postpro(Simp,Answer) .				%	PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , go(P,Simp) , postpro(Simp,Answer) .	%	Quiet
