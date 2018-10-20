
%	Author:		Anthony John Ripa
%	Date:		2018.10.20
%	Leibniz:	A Rule System for Math

:- op(0800,xfx,@).
:- op(0900,xfx,<-).
:- op(1000,xfx,<--).
:- op(1100,xfx,<---).
:- op(1200,xfx,<----).

preprocess(f(X),Ans) :- preprocess(X^2, Ans) , ! .
preprocess( _ ^ 0 , 1) :- ! .
preprocess(X^N, Ans) :- N1 is N-1 , preprocess(X^N1, XN1) , preprocess(XN1*X, Ans) , ! .
preprocess(X+Y,X1+Y1) :- preprocess(X,X1) , preprocess(Y,Y1) , ! .
preprocess(X-Y,X1-Y1) :- preprocess(X,X1) , preprocess(Y,Y1) , ! .
preprocess(X*Y,X1*Y1) :- preprocess(X,X1) , preprocess(Y,Y1) , ! .
preprocess(X/Y,X1/Y1) :- preprocess(X,X1) , preprocess(Y,Y1) , ! .
preprocess(X@Y,X1@Y1) :- preprocess(X,X1) , preprocess(Y,Y1) , ! .
preprocess(X,X) :- ! .

simp(X/X, 1) :- show([9,X/X=1,trying]) , X\=0 , ! , show([9,X/X=1,succeed]).

simp(X*1, Ans) :- show([11,X*1=Ans,trying]) , simp(X, Ans) , ! , show([11,X,succeed]).
simp(1*X, Ans) :- show([12,X]) , simp(X, Ans) , ! , show([12,X,succeed]).
simp(X*0, 0) :- show([13,X]) , ! , show([13,X,succeed]).
simp(0*X, 0) :- show([14,X]) , ! , show([14,X,succeed]).
simp(A*B, Ans) :- show([15,A,B]) , simp(A, A1) , A\=A1 , simp(A1*B, Ans) , ! , show([15,A,B,succeed]).
simp(A*B, Ans) :- show([16,A,B]) , simp(B, B1) , B\=B1 , simp(A*B1, Ans) , ! , show([16,A,B,succeed]).

simp(X+0, Ans) :- show([18,X]) , simp(X, Ans) , ! .
simp(0+X, Ans) :- show([19,X]) , simp(X, Ans) , ! .
simp(A+B, Ans) :- show([20,A,B]) , ground(A) , simp(A, A1) , A\=A1 , simp(A1+B, Ans) , ! .
simp(A+B, Ans) :- show([21,A,B]) , ground(B) , simp(B, B1) , B\=B1 , simp(A+B1, Ans) , ! .
simp(C*(A+B), Ans) :- show([22,A,B,C]) , simp(C*A+C*B, Ans) , ! .
simp((A+B)*C, Ans) :- show([23,A,B,C]) , simp(A*C+B*C, Ans) , ! .

simp(A+B-C, Ans) :- show([25,A,B,C]) , simp(B,B1) , simp(C,C1) , B1=C1 , simp(A, Ans) , ! .
simp(A+B-C, Ans) :- show([26,A,B,C]) , simp(A-C,Z) , simp(Z+B, Ans) , ! .
simp(A-B, Ans) :- show([28,A,B]) , simp(A, A1+A2) , simp(A1+A2-B,Ans) , ! .
simp(A-B, Ans) :- show([29,A,B]) , simp(A, A1) , simp(B,B1) , simp(B1+Ans, A1) , ! .
simp(A-B, Ans) :- show([30,A,B]) , simp(A, A1) , simp(B,B1) , simp(Ans+B1, A1) , ! .

simp(A*B/C, Ans) :- show([34,A]) , simp(B,B1) , simp(C,C1) , B1=C1 , simp(A, Ans) , ! .
simp(A*B/C, Ans) :- show([35,A]) , simp(A/C,Z) , simp(Z*B, Ans) , ! .
simp(A/B, Ans) :- show([37,A,B]) , simp(A,A1) , A\=A1 , simp(A1/B, Ans) , ! , show([37,A/B=Ans,succeed]) .
simp((A+B)/C, Ans) :- show([38,A,B,C]) , simp(A/C+B/C, Ans) , ! .
simp((A-B)/C, Ans) :- show([39,A,B,C]) , simp(A/C-B/C, Ans) , ! .

simp(@(Exp,Var=Con), Ans) :- show([48]) , simp(Exp,Exp2) , (
	Exp2 = Var , Ans = Con ;
	atomic(Exp2) , Ans = Exp2 ;
	Exp2 = X+Y , simp(@(X,Var=Con),X1) , simp(@(Y,Var=Con),Y1) , simp(X1+Y1,Ans) ;
	Exp2 = X*Y , simp(@(X,Var=Con),X1) , simp(@(Y,Var=Con),Y1) , simp(X1*Y1,Ans) ;
	Exp2 = X/Y , simp(@(X,Var=Con),X1) , simp(@(Y,Var=Con),Y1) , simp(X1/Y1,Ans)
) , ! .

simp(X, X):- show([52,X=X,succeed]).

<----(Answer,X) :- nb_setval(see,true) , show("Preprocessing") , preprocess(X,Answer) .	%	Preprocess

<---(Answer,X) :- nb_setval(see,true) , <----(P,X) , show("Simplifying") , simp(P,Answer) .	%	Simplify

<--(Answer,X) :- nb_setval(see,true) , <---(Simp,X) , show("Factoring") , factor(Simp,Answer) .	%	Factor

<-(Answer,X) :- nb_setval(see,false) , preprocess(X,P) , simp(P,Simp) , factor(Simp,Answer) .	%	Quiet

show(Text) :- nb_getval(see,See) , See -> writeln(Text) ; ! .

factor(X+X, 2*X1) :- factor(X, X1) , ! .
factor(N*X+X, N2*X) :- number(N) , N2 is N+1 , ! .
factor(A+B, Ans) :- factor(A, A1) , A\=A1 , factor(A1+B, Ans) , ! .
factor(A+B, Ans) :- factor(B, B1) , B\=B1 , factor(A+B1, Ans) , ! .
factor(X*X, X^2) :- ! .
factor(X^N*X, X^N2) :- number(N) , N2 is N+1 , ! .
factor(A*B, Ans) :- factor(A, A1) , A\=A1 , factor(A1*B, Ans) , ! .
factor(A*B, Ans) :- factor(B, B1) , B\=B1 , factor(A*B1, Ans) , ! .
factor(X, X).
