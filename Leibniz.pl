
%	Author:		Anthony John Ripa
%	Date:		2019.1.20
%	Leibniz:	A Rule System for Math

:- op(0800,xfx,@).
:- op(0900,xfx,<-).
:- op(1000,xfx,<--).
:- op(1100,xfx,<---).
:- op(1200,xfx,<----).

:- dynamic see/0.

if(Condition,Statement) :- Condition->Statement ; true .

show(Text) :- if(see, writeln(Text)) .

prepro(0,sum([])) :- show(('PrePro1',0)) , ! .
prepro(1,prod([])) :- show(('PrePro2',1)) , ! .
prepro(f(X),Ans) :- show(('PrePro3',f(X))) , prepro(X*X, Ans) , ! .
prepro(A+B,Ans) :- show(('PrePro4',A+B)) , prepro(A,AP) , prepro(B,BP) , makesum(AP,sum(AL)) , makesum(BP,sum(BL)) , conc(AL,BL,LR) , prepro(sum(LR),Ans) , ! .
prepro(A*B,Ans) :- show(('PrePro5',A*B)) , prepro(A,AP) , prepro(B,BP) , makeprod(AP,prod(AL)) , makeprod(BP,prod(BL)) , conc(AL,BL,LR) , prepro(prod(LR),Ans) , ! .
prepro(A-B,AS-BS) :- show(('PrePro6',A-B)) , prepro(A,AP) , prepro(B,BP) , makesum(AP,AS) , makesum(BP,BS) , ! .
prepro(A/B,AM/BM) :- show(('PrePro7',A/B)) , prepro(A,AP) , prepro(B,BP) , makeprod(AP,AM) , makeprod(BP,BM) , ! .
prepro(A@B=C, AP@B=CP) :- show(('PrePro8',A/B)) , prepro(A,AP) , prepro(C,CP) , ! .
prepro(X,X) :- show(('PrePro9',X)) , ! .

makesum(sum(A),sum(A)) :- ! .
makesum(A,sum([A])) :- ! .

makeprod(prod(A),prod(A)) :- ! .
makeprod(A,prod([A])) :- ! .

conc(A,B,Ans) :- append(A,B,C) , msort(C,S) , reverse(S,Ans) .

go(prod(L),Ans) :- show(('Go1',prod(L))) , conc(L,[],LR) , L\=LR , go(prod(LR),Ans) , ! .
go(prod([A]),Ans) :- show(('Go2',prod([A]))) , go(A,Ans) , ! .
go(prod([sum([]),B]),sum([])) :- show(('Go3',prod([sum([]),B]))) , ! .
go(prod([sum([H|T]),B]),Ans) :- show(('Go4',prod([sum([H|T]),B]))) , SH=prod([H,B]) , go(prod([sum(T),B]),ST) , go(sum([SH|[ST]]),Ans) , ! .
go(prod([A,B,C]),Ans) :- show(('Go5',prod([A,B,C]))) , go(prod([A,B]),AB) , go(prod([AB,C]),Ans) , ! .
go(sum([H]),Ans) :- show(('Go6',su([H]))) , go(H,Ans) , ! .
go(sum([H|T]),Ans) :- show(('Go7',sum([H|T]))) , go(H,HS) , go(sum(T),TS) , makesum(HS,sum(HL)) , makesum(TS,sum(TL)) , conc(HL,TL,LS) , Ans=sum(LS) , ! .
go(A-B,Ans) :- show(('Go8',A-B)) , go(A,sum(A1)) , go(B,B1) , select(B1,A1,L) , go(sum(L),Ans) , ! .
go(A/B,Ans) :- show(('Go9',A/B)) , go(A,prod(A1)) , go(B,B1) , B1\=sum([]) , select(B1,A1,L) , go(prod(L),Ans) , ! .
go(A/B,Ans) :- show(('Go10',A/B)) , go(A,sum([H])) , go(B,B1) , go(H/B1,H1) , go(sum([H1]),Ans) , ! .
go(A/B,Ans) :- show(('Go11',A/B)) , go(A,sum([H|T])) , go(B,B1) , go(H/B1,H1) , go(sum(T)/B1,sum(T1)) , go(sum([H1|T1]),Ans) , ! .
go(A@A=B,B) :- show(('Go12',A@A=B)) , ! .
go(A@B=C,Ans) :- show(('Go13',A@B=C)) , go(A,sum([H|T])) , go(H@B=C,H1) , go(sum(T)@B=C,sum(T1)) , go(sum([H1|T1]),Ans) , ! .
go(A@B,Ans) :- show(('Go14',A@B)) , go(A,Ans) , ! .
go(X,X) :- show(('Go15',X)) , ! .

simp(A,Ans) :- go(A,Ans) , ! .

postpro(prod([]),1) :- show(('PostPro1',prod([]))) , ! .
postpro(sum([]),0) :- show(('PostPro2',sum([]))) , ! .
postpro(X,X) :- show(('PostPro3',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show("PreProcessing") , prepro(X,Answer) .	%	PreProcess
<---(Answer,X) :- <----(P,X) , show("Simplifying") , simp(P,Answer) .						%	Simplify
<--(Answer,X) :- <---(Simp,X) , show("PostProcessing") , postpro(Simp,Answer) .				%	PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , simp(P,Simp) , postpro(Simp,Answer) .	%	Quiet
