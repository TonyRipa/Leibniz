
%	Author:		Anthony John Ripa
%	Date:		2018.08.20
%	Leibniz:	A Rule System for Math

:- op(0900,xfx,@).
:- op(1000,xfx,<-).
:- op(1100,xfx,<--).
:- op(1200,xfx,<---).

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

simp( X ^ 0 , 1, S) :- writeln([S,31,X]) , S>0 , ! .
simp(X^N, Ans, S) :- writeln([S,32,X,N]) , S>0 , S2 is S-1 , N1 is N-1 , simp(X^N1, XN1, S2) , simp(XN1*X, Ans, S2) , ! .

simp(A*B/C, Ans, S) :- writeln([S,34,A]) , S>0 , S2 is S-1 , simp(B,B1,S2) , simp(C,C1,S2) , B1=C1 , simp(A, Ans, S2) , ! .
simp(A*B/C, Ans, S) :- writeln([S,35,A]) , S>0 , S2 is S-1 , simp(A/C,Z,S2) , simp(Z*B, Ans, S2) , ! .
simp(N/D, Ans, S) :- writeln([S,36,N,D]) , S>0 , S2 is S-1 , D\=0 , simp(N,N1,S2) , simp(Ans*D, N1, S2) , writeln([S,36,N/D=Ans,succeed]).
simp(A/B, Ans, S) :- writeln([S,37,A,B]) , S>0 , S2 is S-1 , simp(A,A1,S2) , A\=A1 , simp(A1/B, Ans, S2) , ! , writeln([S,37,A/B=Ans,succeed]) .
simp((A+B)/C, Ans, S) :- writeln([S,38,A,B,C]) , S>0 , S2 is S-1 , simp(A/C+B/C, Ans, S2) , ! .
simp((A-B)/C, Ans, S) :- writeln([S,39,A,B,C]) , S>0 , S2 is S-1 , simp(A/C-B/C, Ans, S2) , ! .


simp(@(Num, _ = _ ), Num, S) :- writeln([S,42]) , S>0 , number(Num) , ! .
simp(@(Var,Var=Con), Con, S) :- writeln([S,43]) , S>0 , !.
simp(@(Ato, _ = _ ), Ato, S) :- writeln([S,44]) , S>0 , atom(Ato) , ! .
simp(@(X+Y,Var=Con), Ans, S) :- writeln([S,45]) , S>0 , S2 is S-1 , simp(@(X,Var=Con),X1,S2) , simp(@(Y,Var=Con),Y1,S2) , simp(X1+Y1,Ans,S2) , ! .
simp(@(X*Y,Var=Con), Ans, S) :- writeln([S,46]) , S>0 , S2 is S-1 , simp(@(X,Var=Con),X1,S2) , simp(@(Y,Var=Con),Y1,S2) , simp(X1*Y1,Ans,S2) , ! .
simp(@(X/Y,Var=Con), Ans, S) :- writeln([S,47]) , S>0 , S2 is S-1 , simp(@(X,Var=Con),X1,S2) , simp(@(Y,Var=Con),Y1,S2) , simp(X1/Y1,Ans,S2) , writeln([S,47,(X/Y@Var=Con)=Ans,succeed]).
simp(@(Exp,Var=Con), Ans, S) :- writeln([S,48]) , S>0 , S2 is S-1 , simp(Exp,Exp2,S2) , Exp\=Exp2 , simp(@(Exp2,Var=Con), Ans,S2) , ! .

simp(f(X),Ans,S) :- writeln([S,50,X]) , S>0 , S2 is S-1 , simp(X^2, Ans, S2) , ! .

simp(X, X, S):- writeln([S,52,X=X,succeed]).

<---(Answers,X) :- writeln("Depth Limiting") , bagof(Answer,simp(X,Answer,25),Answers).	%	Depth Limited

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
