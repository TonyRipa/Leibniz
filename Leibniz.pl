
%	Author:		Anthony John Ripa
%	Date:		2018.09.20
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

simp(X/X, 1, S) :- writeln([S,9,X/X=1,trying]) , S>0 , X\=0 , ! , writeln([S,9,X/X=1,succeed]).

simp(X*1, Ans, S) :- writeln([S,11,X*1=Ans,trying]) , S>0 , S2 is S-1 , simp(X, Ans, S2) , ! , writeln([S,11,X,succeed]).
simp(1*X, Ans, S) :- writeln([S,12,X]) , S>0 , S2 is S-1 , simp(X, Ans, S2) , ! , writeln([S,12,X,succeed]).
simp(X*0, 0, S) :- writeln([S,13,X]) , S>0 , ! , writeln([S,13,X,succeed]).
simp(0*X, 0, S) :- writeln([S,14,X]) , S>0 , ! , writeln([S,14,X,succeed]).
simp(A*B, Ans, S) :- writeln([S,15,A,B]) , S>0, S2 is S-1 , simp(A, A1, S2) , A\=A1 , simp(A1*B, Ans, S2) , ! , writeln([S,15,A,B,succeed]).
simp(A*B, Ans, S) :- writeln([S,16,A,B]) , S>0 , S2 is S-1 , simp(B, B1, S2) , B\=B1 , simp(A*B1, Ans, S2) , ! , writeln([S,16,A,B,succeed]).

simp(X+0, Ans, S) :- writeln([S,18,X]) , S>0 , S2 is S-1 , simp(X, Ans, S2) , ! .
simp(0+X, Ans, S) :- writeln([S,19,X]) , S>0 , S2 is S-1 , simp(X, Ans, S2) , ! .
simp(A+B, Ans, S) :- writeln([S,20,A,B]) , S>0 , S2 is S-1 , simp(A, A1, S2) , A\=A1 , simp(A1+B, Ans, S2) , ! .
simp(A+B, Ans, S) :- writeln([S,21,A,B]) , S>0 , S2 is S-1 , simp(B, B1, S2) , B\=B1 , simp(A+B1, Ans, S2) , ! .
simp(C*(A+B), Ans, S) :- writeln([S,22,A,B,C]) , S>0 , S2 is S-1 , simp(C*A+C*B, Ans, S2) , ! .
simp((A+B)*C, Ans, S) :- writeln([S,23,A,B,C]) , S>0 , S2 is S-1 , simp(A*C+B*C, Ans, S2) , ! .

simp(A+B-C, Ans, S) :- writeln([S,25,A,B,C]) , S>0 , S2 is S-1 , simp(B,B1,S2) , simp(C,C1,S2) , B1=C1 , simp(A, Ans, S2) , ! .
simp(A+B-C, Ans, S) :- writeln([S,26,A,B,C]) , S>0 , S2 is S-1 , simp(A-C,Z,S2) , simp(Z+B, Ans, S2) , ! .
simp(A-B, Ans, S) :- writeln([S,27,A,B]) , S>0 , S2 is S-1 , simp(A, A1+A2,S2) , simp(A1+A2-B,Ans,S2) , ! .
simp(A-B, Ans, S) :- writeln([S,28,A,B]), S>0 , S2 is S-1 , simp(A, A1, S2) , simp(B,B1,S2) , simp(B1+Ans, A1,S2) , ! .
simp(A-B, Ans, S) :- writeln([S,29,A,B]) , S>0 , S2 is S-1 , simp(A, A1, S2) , simp(B,B1,S2) , simp(Ans+B1, A1,S2) , ! .

simp(A*B/C, Ans, S) :- writeln([S,34,A]) , S>0 , S2 is S-1 , simp(B,B1,S2) , simp(C,C1,S2) , B1=C1 , simp(A, Ans, S2) , ! .
simp(A*B/C, Ans, S) :- writeln([S,35,A]) , S>0 , S2 is S-1 , simp(A/C,Z,S2) , simp(Z*B, Ans, S2) , ! .
simp(A/B, Ans, S) :- writeln([S,37,A,B]) , S>0 , S2 is S-1 , simp(A,A1,S2) , A\=A1 , simp(A1/B, Ans, S2) , ! , writeln([S,37,A/B=Ans,succeed]) .
simp((A+B)/C, Ans, S) :- writeln([S,38,A,B,C]) , S>0 , S2 is S-1 , simp(A/C+B/C, Ans, S2) , ! .
simp((A-B)/C, Ans, S) :- writeln([S,39,A,B,C]) , S>0 , S2 is S-1 , simp(A/C-B/C, Ans, S2) , ! .

simp(@(Exp,Var=Con), Ans, S) :- writeln([S,48]) , S>0 , S2 is S-1 , simp(Exp,Exp2,S2) , (
	Exp2 = Var , Ans = Con ;
	atomic(Exp2) , Ans = Exp2 ;
	Exp2 = X+Y , simp(@(X,Var=Con),X1,S2) , simp(@(Y,Var=Con),Y1,S2) , simp(X1+Y1,Ans,S2) ;
	Exp2 = X*Y , simp(@(X,Var=Con),X1,S2) , simp(@(Y,Var=Con),Y1,S2) , simp(X1*Y1,Ans,S2) ;
	Exp2 = X/Y , simp(@(X,Var=Con),X1,S2) , simp(@(Y,Var=Con),Y1,S2) , simp(X1/Y1,Ans,S2)
) , ! .

simp(X, X, S):- writeln([S,52,X=X,succeed]).

<----(Answers,X) :- writeln("Depth Limiting") , bagof(Answer,simp(X,Answer,25),Answers).	%	Depth Limited

<---(Answers,X) :- writeln("Preprocessing") , preprocess(X,X1) , <----(Answers,X1).	%	Preprocess

<--(Answer,X) :- writeln("Pick Best") , <---(Answers,X) , last(Answers,Answer).	%	Pick Best

<-(Answer,X) :- writeln("Factoring") , <--(Best,X) , factor(Best,Answer).	%	Factor Best

factor(X+X, 2*X1) :- factor(X, X1) , ! .
factor(N*X+X, N2*X) :- number(N) , N2 is N+1 , ! .
factor(A+B, Ans) :- factor(A, A1) , A\=A1 , factor(A1+B, Ans) , ! .
factor(A+B, Ans) :- factor(B, B1) , B\=B1 , factor(A+B1, Ans) , ! .
factor(X*X, X^2) :- ! .
factor(X^N*X, X^N2) :- number(N) , N2 is N+1 , ! .
factor(A*B, Ans) :- factor(A, A1) , A\=A1 , factor(A1*B, Ans) , ! .
factor(A*B, Ans) :- factor(B, B1) , B\=B1 , factor(A*B1, Ans) , ! .
factor(X, X).
