
%	Author:		Anthony John Ripa
%	Date:		2018.06.20
%	Leibniz:	A Rule System for Math


simp(0/0, _) :- ! , fail.	%	0/0 is a non-terminal
%simp(0/0, 0/0) :- ! .		%	0/0 is a terminal
simp(X/X, 1) :- X\=0 , ! .

simp(X*1, Ans) :- simp(X, Ans) , ! .
simp(1*X, Ans) :- simp(X, Ans) , ! .
simp( _ * 0 , 0) :- ! .
simp( 0 * _ , 0) :- ! .
simp(A*B, Ans) :- simp(A, A1) , A\=A1 , simp(A1*B, Ans) , ! .
simp(A*B, Ans) :- simp(B, B1) , B\=B1 , simp(A*B1, Ans) , ! .

simp(X+0, Ans) :- simp(X, Ans) , ! .
simp(0+X, Ans) :- simp(X, Ans) , ! .
simp(A+B, Ans) :- simp(A, A1) , A\=A1 , simp(A1+B, Ans) , ! .
simp(A+B, Ans) :- simp(B, B1) , B\=B1 , simp(A+B1, Ans) , ! .
simp(C*(A+B), Ans) :- simp(C*A+C*B, Ans) , ! .
simp((A+B)*C, Ans) :- simp(A*C+B*C, Ans) , ! .

simp(A+B-C, Ans) :- simp(B,B1) , simp(C,C1) , B1=C1 , simp(A, Ans) , ! .
simp(A+B-C, Ans) :- simp(A-C,Z) , simp(Z+B, Ans) , ! .
simp(A-B, Ans) :- simp(A, A1+A2) , simp(A1+A2-B,Ans) , ! .
simp(A-B, Ans) :- simp(A, A1) , simp(B,B1) , simp(B1+Ans, A1) , ! .
simp(A-B, Ans) :- simp(A, A1) , simp(B,B1) , simp(Ans+B1, A1) , ! .

simp( _ ^ 0 , 1) :- ! .
simp(X^N, Ans) :- N1 is N-1 , simp(X^N1, XN1) , simp(XN1*X, Ans) , ! .

simp(A*B/C, Ans) :- simp(B,B1) , simp(C,C1) , B1=C1 , simp(A, Ans) , ! .
simp(A*B/C, Ans) :- simp(A/C,Z) , simp(Z*B, Ans) , ! .
simp(N/D, Ans) :- simp(N,N1) , simp(Ans*D, N1) , ! .
simp(A/B, Ans) :- simp(A,A1) , A\=A1 , simp(A1/B, Ans) , ! .
simp((A+B)/C, Ans) :- simp(A/C+B/C, Ans) , ! .
simp((A-B)/C, Ans) :- simp(A/C-B/C, Ans) , ! .


simp(eval(Num, _ = _ ), Num) :- number(Num) , ! .
simp(eval(Var,Var=Con), Con) :- !.
simp(eval(Ato, _ = _ ), Ato) :- atom(Ato) , ! .
simp(eval(X+Y,Var=Con), Ans) :- simp(eval(X,Var=Con),X1) , simp(eval(Y,Var=Con),Y1) , simp(X1+Y1,Ans) , ! .
simp(eval(X*Y,Var=Con), Ans) :- simp(eval(X,Var=Con),X1) , simp(eval(Y,Var=Con),Y1) , simp(X1*Y1,Ans) , ! .
simp(eval(X/Y,Var=Con), Ans) :- simp(eval(X,Var=Con),X1) , simp(eval(Y,Var=Con),Y1) , simp(X1/Y1,Ans) , ! .
simp(eval(Exp,Var=Con), Ans) :- simp(Exp,Exp2) , Exp\=Exp2 , simp(eval(Exp2,Var=Con), Ans) , ! .

simp(f(X),Ans) :- simp(X^2, Ans) , ! .

simp(X, X).
