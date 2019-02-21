
Leibniz: A Rule System for Math
========================

Author:	Anthony John Ripa

Date:	2019.02.20

Live Demo of Version 1 at <a href='https://swish.swi-prolog.org/p/hVEWFHXN.pl'>https://swish.swi-prolog.org/p/hVEWFHXN.pl</a>

Live Demo of Version 2 at <a href='https://swish.swi-prolog.org/p/JWdsRkzO.pl'>https://swish.swi-prolog.org/p/JWdsRkzO.pl</a>

Live Demo of Version 3 at <a href='https://swish.swi-prolog.org/p/fMJuWNsH.pl'>https://swish.swi-prolog.org/p/fMJuWNsH.pl</a>

Live Demo of Version 4 at <a href='https://swish.swi-prolog.org/p/oeuNtQbk.pl'>https://swish.swi-prolog.org/p/oeuNtQbk.pl</a>

Live Demo of Version 6 at <a href='https://swish.swi-prolog.org/p/zhOrBjYr.pl'>https://swish.swi-prolog.org/p/zhOrBjYr.pl</a>

Live Demo of Version 7 at <a href='https://swish.swi-prolog.org/p/nTiEvFQq.pl'>https://swish.swi-prolog.org/p/nTiEvFQq.pl</a>

Live Demo of Version 8 at <a href='https://swish.swi-prolog.org/p/XLCwOknT.pl'>https://swish.swi-prolog.org/p/XLCwOknT.pl</a>

Live Demo of Version 9 at <a href='https://swish.swi-prolog.org/p/bDaiBkLV.pl'>https://swish.swi-prolog.org/p/bDaiBkLV.pl</a>

Leibniz
--------
<code>Leibniz</code> is a Rule System for expression simplification written in Prolog. <code>Leibniz</code> is named after Gottfried Wilhelm Leibniz (one of the inventors of Calculus) whose notation for his calculus was algebraic.

Historically, applying the rules of algebra strictly to problems in Calculus has led to contradictions.

Consider a calculation of the slope of the tangent line of the function f(x)=x^2. We may try to find that slope by simplifying the expression (f(x+h)-f(x))/h @h=0. The part @h=0 means evaluate at h=0. Proceeding with our calculation, [(f(x+h)-f(x))/h @ h=0 ] = [((x+h)^2-x^2)/h @ h=0 ] = [(x^2+2xh+h^2-x^2)/h @ h=0 ] = [(2xh+h^2)/h @ h=0 ] = [2x+h @ h=0 ] = 2x+0 = 2x. We correctly found that the slope of the tangent line is 2x. Let's try applying the same rules again but in a different order. [(f(x+h)-f(x))/h @ h=0 ] = (f(x+0)-f(x))/0 = (f(x)-f(x))/0 = 0/0. Since two different answers were gotten by the same system of rules, the system was considered to be bad (not well defined). This apparent problem was dispensed with by disallowing division by zero (in any and all forms). So, (f(x+h)-f(x))/h @ h=0 was said to be undefined. The above short calculation of the slope of the tangent line yielding 2x was labeled an invalid proof. The baby was thrown away with the bath water.

Consider the relatively simple (h/h)@h=0. We may proceed [h/h@h=0] = [1@h=0] = 1. Alternately, we may proceed [h/h@h=0] = [h@h=0]/[h@h=0] = 0/0. Again, the same rules applied in different order yields different results.

This bares a striking resemblance to parse trees for Context Free Grammars. You start with an expression. You apply some transformation rules to the expression. You get a new expression. However, in parse trees each node can be either a terminal or a non-terminal node. If a node is a terminal node then you can terminate and return that node as the parsed value of the original. If a node is a non-terminal node then you cannot terminate and return that node as the parsed value of the original, but you can transform that node using the transformation rules.

We may use a parse tree approach to parse algebraic expressions. Instead of returning 0/0 as a terminal node, we return it as a non-terminal node. This has the same effect as not returning it at all. This results in backtracking that branch of the tree to continue to search for the answer in other parts of the tree. In simplifying h/h@h=0, we avoid returning 0/0, and end up returning 1. Similarly, in simplifying (f(x+h)-f(x))/h @ h=0, we do not return 0/0, and end up returning 2x (literally 2*x).

<pre>
  			(h@h=0)/(h@h=0)	---	0/0	
  		  /
  h/h@h=0
  		  \
  			1@h=0	---		1
</pre>

This is reminiscent of L'Hopital's rule. Yet with L'Hopital's rule, you still employ all the rigmarole of Calculus.

To be clear, it should be stated that <code>Leibniz</code> is not a Calculus system. It is an algebra system. It simply does the algebra so well that it can answer questions which would otherwise require a needlessly complex workaround like Calculus.

One way to think about where this advantage is coming from is in terms of the properties of the transformation rules. The transformation rules of algebra as they are ordinarily cast, as well as our transformation rules, are transitive ( A transformsto B and B transformsto C implies A transformsto C ). However, in addition, the transformation rules of algebra as they are ordinarily cast, are symmetric transformation rules ( A transformsto B implies B transformsto A ) and reflexive ( A transformsto A ). So the transformsto relation for the rules of algebra as they are ordinarily cast, satisfies all the requirements of an equivalence relation. So, if A transforms to B we can just write A = B. This creates the problem. 1 = 0/0 = 0 implies 0 = 1. By not having symmetry, we avoid that problem 1 → 0/0 and 0 → 0/0 but there's nothing that 0/0 transformto. So, we never get 0 → 1 or 1 → 0 or 0=1.

To expand on this, we might consider the problem of simplifying 0/0. If 0/0 is a non-terminal, and the replacement rules can never transform it into a terminal node, then we would have an expression that cannot be parsed into anything (including itself). One solution is to allow 0/0 as a terminal. This might obscure other solutions if there are any so instead of returning the first terminal we could return the list of all terminals. In the case of parsing 0/0 we get the list [0/0]. In case of parsing h/h@h=0 we get the list [0/0,1]. As the parse order always tries to evaluate @ first the 0/0 will always be the earlier solution. We can return the last solution as the canonical solution. So, 0/0 parses to 0/0, and h/h@h=0 parses to 1.

Furthermore, if we are always going to return the canonical solution, we might as well save time and calculate a smaller parse tree. When calculating h/h@h=0, we might as well always simplify the expression to the left of the @ symbol first (in this case h/h simplifies to 1) then apply the @h=0 (in this case 1@h=0) to get the answer (in this case 1). This way we only return 0/0 when required (for example, if the input were literally 0/0, or if the input were something like 0/0@h=0).
