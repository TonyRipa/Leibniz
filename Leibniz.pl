
%	Author:		Anthony John Ripa
%	Date:		2022.09.20
%	Leibniz:	A Rule System for Expressions

:- op(0200,xfx,:).
:- op(0300,fy,s).
:- op(0400,fy,p).
:- op(0500,fy,/).
:- op(0600,fy,*).
:- op(0700,yfx,@).
:- op(0800,yfx,=).
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

prepro(0,s []:[]) :- shop(('01',0)) , ! .
prepro(1,p []:[]) :- shop(('02',1)) , ! .
prepro(s A:B, s AP:B) :- shop(('03',s A:B)) , map(prepro,A,AP) , A\=AP , ! .
prepro(s A:B, s A:BP) :- shop(('04',s A:B)) , map(prepro,B,BP) , B\=BP , ! .
prepro(p A:B, p AP:B) :- shop(('05',p A:B)) , map(prepro,A,AP) , A\=AP , ! .
prepro(p A:B, p A:BP) :- shop(('06',p A:B)) , map(prepro,B,BP) , B\=BP , ! .
prepro(X,Ans) :- integer(X) , X>1 , shop(('07','Int')) , Y is X-1 , prepro(1+Y,Ans) , ! .
prepro(f(X),Ans) :- shop(('08',f(X))) , prepro(X*X, Ans) , ! .
prepro(A,Ans) :- A =.. [+|Args] , shop(('09',A)) , map(prepro,Args,L) , prepro(s L:[],Ans) , ! .
prepro(A,Ans) :- A =.. [*|Args] , shop(('10',A)) , map(prepro,Args,L) , prepro(p L:[],Ans) , ! .
prepro(A,Ans) :- A =.. [-|Args] , shop(('11',A)) , map(prepro,Args,L) , rev(L,[D|N]) , prepro(s N:[D],Ans) , ! .
prepro(A,Ans) :- A =.. [/|Args] , shop(('12',A)) , map(prepro,Args,L) , rev(L,[D|N]) , prepro(p N:[D],Ans) , ! .
prepro(A^0,Ans) :- shop(('13',A^0)) , prepro(1,Ans) , ! .
prepro(A^N,Ans) :- integer(N) , N>0 , shop(('14',A^N)) , N2 is N-1 , prepro(A*A^N2,Ans) , ! .
prepro(A=B,AP=BP) :- shop(('15',A=B)) , prepro(A,AP) , prepro(B,BP) , ! .
prepro(A@B,AP@BP) :- shop(('16',A@B)) , prepro(A,AP) , prepro(B,BP) , ! .
prepro(exp(X+Y),Ans) :- shop(('17',exp(X+Y))) , prepro(exp(X),X1) , prepro(p [X1,exp(Y)]:[],Ans) , ! .
prepro(X,Ans) :- shop(('18',X)) , flat(X,Ans) , ! .

map(_,[],[]). map(F,[H|T],[H1|T1]) :- call(F,H,H1) , map(F,T,T1) .
mem(H,[H|_]). mem(H,[_|T]) :- mem(H,T).
app([],L,L). app([H|L1],L2,[H|L3]) :- app(L1,L2,L3).
sel(E,Big,Lit) :- app(F,[E|B],Big) , app(F,B,Lit)  .
rev([A],[A]). rev([A,B],[B,A]).
len([],0) . len([_|L],C1) :- len(L,C2) , C1 is C2+1 .

in(X,X) . in(P,W) :- W =.. [_|Args] , mem(A,Args) , in(P,A) .
ins(P,W,N) :- findall(_,in(P,W),B) , len(B,N) .

flat(p [H]:[],Ans) :- flat(H,Ans) , ! .
flat(s L:[],Ans) :- sel(s A:B,L,L2) , app(A,L2,L3) , flat(s L3:B,Ans) , ! .
flat(s L:[],s L2:[]) :- map(flat,L,L2) , ! .
flat(s N:D,Ans) :- sel(p A:[],D,D2) , flat(p A:[],A2) , p A:[] \= A2 , app([A2],D2,D3) , flat(s N:D3,Ans) , ! .
flat(X,Ans) :- X=..[O,N:D] , Y=..[O,A:B] , sel(Y,N,N2) , app(A,N2,N3) , app(B,D,D2) , Z=..[O,N3:D2] , flat(Z,Ans) , ! .
flat(X,Ans) :- X=..[O,N:D] , Y=..[O,A:B] , sel(Y,D,D2) , app(A,D2,D3) , app(B,N,N2) , Z=..[O,N2:D3] , flat(Z,Ans) , ! .
flat(X,X) :- ! .

empty(p A:[],s []:[]) :- mem(s []:[],A) , ! .
empty(s [A]:[],Ans) :- empty(A,Ans) , ! .
empty(s A:[],s Ans:[]) :- sel(s []:[],A,B) , empty(B,Ans) , ! .
empty(X,Ans) :- X=..[O,A:B] , map(empty,A,A1) , map(empty,B,B1) , Ans=..[O,A1:B1] , ! .
empty(X,X) :- ! .

haso(o) :- ! .
haso(p A:_) :- mem(X,A) , haso(X) , ! .
haso(s A:_) :- mem(X,A) , haso(X) , ! .

equal(X,X) :- ! .
equal(X,Y) :- go(X,Z) , ( Y=Z , ! ; go(Y,Z) ) , ! .

is0(X) :- equal(X, s []:[] ) , ! .
is1(X) :- equal(X, p []:[] ) , ! .
nan(X) :- is0(Z) , X=p [Z]:[Z] , ! .
nan(X) :- haso(X) , ! .
 an(X) :- not(nan(X)) , ! .

const(s []:[]) :- ! .
const(p []:[]) :- ! .
const(s N:D) :- forall(member(N1,N),const(N1)) , forall(member(D1,D),const(D1)) , ! .
const(p N:D) :- forall(member(N1,N),const(N1)) , forall(member(D1,D),const(D1)) , ! .

take(s E:[],[s H:[]|T],T) :- normalize(E:H,[]:[]) , ! .
take(E,[H|T],T) :- equal(E,H) , ! .
take(E,[H|T],[H|T0]) :- take(E,T,T0) , ! .

normalize(N1:[H|T],S) :- not(is0(H)) , take(H,N1,Ni) , normalize(Ni:T,S) , ! .
normalize(N1:[H|T],N0:[H|Di]) :- normalize(N1:T,N0:Di) , ! .
normalize(S,S) :- ! .

normalized(S1,S0) :- normalize(S1,S0) , S1\=S0 , ! .

expand(p L:[],Ans) :- app(Front,[s S:[]|Back],L) , app(Front,Back,L2) , expand1(s S:[],p L2:[],E1) , expand(E1,Ans) , ! .
expand(s A:B,s EA:EB) :- map(expand,A,EA) , map(expand,B,EB) , ! .
expand(X,X) :- ! .
expand1(s []:[],p _:[],s []:[]) :- ! .
expand1(s [SH|ST]:[],p P:[],s E:[]) :- app([SH],P,EH) , expand1(s ST:[],p P:[],s ET:[]) , app([p EH:[]],ET,E) , ! .

flatterms(E,A) :- expand(E,F) , flat(F,s A:[]) , ! .

reduce(_,[H],H) .
reduce(F,[H|T],R) :- reduce(F,T,RL) , call(F,H,RL,R) .

multi_intersect([],_,[]) :- ! .
multi_intersect([H|T],L,[H|A]) :- app(F,[H|B],L) , app(F,B,L2) , multi_intersect(T,L2,A) , ! .
multi_intersect([_|T],L,A) :- multi_intersect(T,L,A) , ! .

multi_diff([],N,N) :- ! .
multi_diff([H|T],N,A) :- app(F,[H|B],N) , app(F,B,N2) , multi_diff(T,N2,A) , ! .
multi_diff([_|T],N,A) :- multi_diff(T,N,A) , ! .

factors(p F:[],F) :- ! .
factors(p []:D,Ans) :- factors(p D:[],D2) , bagof(p []:[X],mem(X,D2),Ans) , ! .
factors(p N:D,Ans) :- factors(p N:[], N1) , factors(p D:[], D1) , multi_intersect(N1,D1,I) , multi_diff(I,N1,N2) , multi_diff(I,D1,D2) , factors(p N2:[],N3) , factors(p []:D2,D3) , app(N3,D3,Ans) , ! .
factors(s [F]:[],A) :- factors(F,A) , ! .
factors(s N:[],[s N4:[]]) :- map(factors,N,N1) , reduce(multi_intersect,N1,D1) , map(multi_diff(D1),N1,N2) , D1=[] , bagof(p X:[],mem(X,N2),N3) , map(flat,N3,N4) , ! .
factors(s N:[],[D2,s N4:[]]) :- map(factors,N,N1) , reduce(multi_intersect,N1,D1) , map(multi_diff(D1),N1,N2) , flat(p D1:[],D2) , bagof(p X:[],mem(X,N2),N3) , map(flat,N3,N4) , ! .
factors(s X:Y,[p I:[],s [p X2:[]]:[p Y2:[]]]) :- factors(s X:[],X1),factors(s Y:[],Y1) , multi_intersect(X1,Y1,I) , multi_diff(I,X1,X2) , multi_diff(I,Y1,Y2) , ! .
factors(X,[X]) :- ! .

divide(N,D,Q) :- equal(N,D) , is1(Q) , ! .
divide(s N:[],s D:[],Q) :- go(s N:D,s N2:[]) , len(N,Nn) , len(N2,N2n) , N2n < Nn , divide(s N2:[],s D:[], Rem) , go(s [p []:[], Rem]:[] , Q) .

flatfactors(E,A) :- factors(E,F) , flat(p F:[],p A:[]) , ! .

go(A,Ans) :- show(('01',A)) , empty(A,B) , A\=B , go(B,Ans) , succ(('01',Ans)) , ! .
go(A,Ans) :- show(('02',A)) ,  flat(A,B) , A\=B , go(B,Ans) , succ(('02',Ans)) , ! .
go(s A:B,Ans) :- show(('03',s A:B)) , map(go,A,A2) , A\=A2 , go(s A2:B,Ans) , succ(('03',Ans)) , ! .
go(p A:B,Ans) :- show(('04',p A:B)) , map(go,A,A2) , A\=A2 , go(p A2:B,Ans) , succ(('04',Ans)) , ! .
go(s A:B,Ans) :- show(('05',s A:B)) , flatterms(s A:[],A1) , flatterms(s B:[],B1) , normalized(A1:B1,A2:B2) , Ans=s A2:B2 , succ(('05',Ans)) , ! .
go(p Zero:D,Ans) :- show(('06',p Zero:D)) , mem(Z,Zero) , is0(Z) , ( mem(Z2,D) , is0(Z2) -> nan(Ans) ; Ans=s []:[] ) , succ(('06',Ans)) , ! .
go(p(S),Ans) :- show(('07',p(S))) , normalized(S,S2) , go(p S2,Ans) , succ(('07',Ans)) , ! .
go(p [s A:B]:[s C:D],Ans) :- show(('08',p A:B)) , divide(s A:B,s C:D,S) , S\=p [s A:B]:[s C:D] , go(S,Ans) , succ(('08',Ans)) , ! .
go(p N:D,Ans) :- show(('09',p N:D)) , D\=[] , expand(p N:[],E) , go(E,G) , flatfactors(G,N1) , normalized(N1:D,S) , Ans=p S , succ(('09',Ans)) , ! .
go(A@X,Ans) :- show(('10',A@X)) , go(A,A1) , Ans=A1@X , succ(('10',Ans)) , ! .
go(A=X,Ans) :- show(('11',A=X)) , Max=2 , between(0,Max,N) , eval(A=X,N,Ans) , ( N=Max ; an(Ans) ) , succ(('11',Ans)) , ! .
go(p N:D,Ans) :- show(('12',p N:D)) , const(p N:D) , expand(p N:D,E) , p N:D\=E , go(E,Ans) , succ(('12',Ans)) , ! .
go(X,X) :- show(('13',X)) , succ(('13',X)) , ! .

eval(A@X=Y, _ , _) :- show(('ev',A@X=Y)) , ins(A,X,M) , M > 1 , ! , fail .
eval(X@X=Y, _ , X) :- show(('ev',X@X=Y)) , 0/0 <- Y , ! .
eval(X@p P:[]=Y, _ , Ans) :- show(('ev',X@X*C=Y)) , sel(X,P,C) , go(p [Y]:C,R) , eval(X@X=R, _ , Ans) , ! .
eval(A@X=Y,N,E):- show(('ev',A@X=Y)) , if(is0(Y),norder(exp(X),N,R),R=exp(Y)) , replace(exp(X),R,A,B), go(B,C) , replace(X,Y,C,D) , go(D,E) , succ(('ev',E)) , ! .
eval(A=Y,N,E):- show(('ev',A=Y)) , go(A,B) , A\=B, eval(B=Y,N,E) .

norder(exp(X),0,s [p []:[] ,   	 										p [o,X]	   :[]]	:[]) :- ! .
norder(exp(X),1,s [p []:[] , X , 										p [o,X,X]  :[]]	:[]) :- ! .
norder(exp(X),2,s [p []:[] , X , p [X,X]:[ s [p []:[],p []:[]] : [] ],	p [o,X,X,X]:[]]	:[]) :- ! .

replace(Find,Repl,OldTree,NewTree) :- OldTree = Find , NewTree = Repl , ! .
replace(Find,Repl,OldTree,NewTree) :- =..(OldTree,[Name|Args]) , map(replace(Find,Repl),Args,Args2) , =..(NewTree,[Name|Args2]) , ! .

postpro(X,Z) :- shop(('01',1)) , replace(p []:[],1,X,Y) , X\=Y , postpro(Y,Z) , ! .
postpro(s []:[],0) :- shop(('02',0-0)) , ! .
postpro(s [A]:[],AP) :- shop(('03',A-0)) , postpro(A,AP) , ! .
postpro(s L:[],AP) :- shop(('04',L-0)) , sel(N1,L,L1) , number(N1) , sel(N2,L1,L2) , number(N2) , N3 is N1+N2 , postpro(s [N3|L2]:[],AP) , ! .
postpro(s L:[],AP+BP) :- shop(('05',L-0)) , app(L1,[E],L) , postpro(s L1:[],AP) , postpro(E,BP) , ! .
postpro(p [A]:[],AP) :- shop(('06',A/1)) , postpro(A,AP) , ! .
postpro(p L:[],EP^N) :- shop(('07',L/1)) , not((mem(E1,L),mem(E2,L),E1\=E2)) , mem(E,L) , len(L,N) , postpro(E,EP) , ! .
postpro(p L:[],AP*BP) :- shop(('08',L/1)) , app(L1,[E],L) , postpro(p L1:[],AP) , postpro(E,BP) , ! .
postpro(s []:A,Ans) :- shop(('09',0-A)) , postpro(s A:[],AP) , number(AP) , is(Ans,-AP) , ! .
postpro(s []:A,-(AP)) :- shop(('10',0-A)) , postpro(s A:[],AP) , ! .
postpro(s A:B,AP-BP) :- shop(('11',A-B)) , postpro(s A:[],AP) , postpro(s B:[],BP) , ! .
postpro(p A:B,AP/BP) :- shop(('12',A/B)) , postpro(p A:[],AP) , postpro(p B:[],BP) , ! .
postpro(exp(A),exp(AP)) :- shop(('13',exp(A))) , postpro(A,AP) , ! .
postpro(A@B,AP@BP) :- shop(('14',A@B)) , postpro(A,AP) , postpro(B,BP) , ! .
postpro(A=B,AP=BP) :- shop(('15',A=B)) , postpro(A,AP) , postpro(B,BP) , ! .
postpro(X,X) :- shop(('16',X)) , ! .

<----(Answer,X) :- if(not(see),assert(see)) , show('PreProcessing') , prepro(X,Answer) .    %   PreProcess
<---(Answer,X) :- <----(P,X) , show('Simplifying') , go(P,Answer) .                         %   Simplify
<--(Answer,X) :- <---(Simp,X) , show('PostProcessing') , postpro(Simp,Answer) .             %   PostProcess
<-(Answer,X) :- if(see,retract(see)) , prepro(X,P) , go(P,Simp) , postpro(Simp,Answer) .    %   Quiet
