
Leibniz: A Rule System for Math
========================

Author:	Anthony John Ripa
Date:	2018.06.20

Leibniz
--------
<code>Leibniz</code> is a Rule System for expression simplification written in Prolog. <code>Leibniz</code> is named after Gottfried Wilhelm Leibniz (one of the inventors of Calculus) whose notation for his calculus was algebraic.

Historically, applying the rules of algebra strictly to problems in Calculus has led to contradictions.

Consider a calculation of the slope of the tangent line of the function f(x)=x^2. We may try to find that slope by simplifying the expression (f(x+h)-f(x))/h |h=0. The part |h=0 means evaluate at h=0. Proceeding with our calculation, [(f(x+h)-f(x))/h | h=0 ] = [((x+h)^2-x^2)/h | h=0 ] = [(x^2+2xh+h^2-x^2)/h | h=0 ] = [(2xh+h^2)/h | h=0 ] = [2x+h | h=0 ] = 2x+0 = 2x. We correctly found that the slope of the tangent line is 2x. Let's try applying the same rules again but in a different order. [(f(x+h)-f(x))/h | h=0 ] = (f(x+0)-f(x))/0 = (f(x)-f(x))/0 = 0/0. Since two different answers were gotten by the same system of rules, the system was considered to be bad (not well defined). This apparent problem was dispensed with by disallowing division by zero (in any and all forms). So, (f(x+h)-f(x))/h | h=0 was said to be undefined. The above short calculation of the slope of the tangent line yielding 2x was labeled an invalid proof. The baby was thrown away with the bath water.

Consider the relatively simple (h/h)|h=0. We may proceed [h/h|h=0] = [1|h=0] = 1. Alternately, we may proceed [h/h|h=0] = [h|h=0]/[h|h=0] = 0/0. Again, the same rules applied in different order yields different results.

This bares a striking resemblance to parse trees for Context Free Grammars. You start with an expression. You apply some transformation rules to the expression. You get a new expression. However, in parse trees each node can be either a terminal or a non-terminal node. If a node is a terminal node then you can terminate and return that node as the parsed value of the original. If a node is a non-terminal node then you cannot terminate and return that node as the parsed value of the original, but you can transform that node using the transformation rules.

We may use a parse tree approach to parse algebraic expressions. Instead of returning 0/0 as a terminal node, we return it as a non-terminal node. This has the same effect as not returning it at all. This results in backtracking that branch of the tree to continue to search for the answer in other parts of the tree. In simplifying h/h|h=0, we avoid returning 0/0, and end up returning 1. Similarly, in simplifying (f(x+h)-f(x))/h | h=0, we do not return 0/0, and end up returning 2x (literally x+x).


  			(h|h=0)/(h|h=0)	---	0/0	
  		  /
  h/h|h=0
  		  \
  			1|h=0	---		1


This is reminiscent of L'Hopital's rule. Yet with L'Hopital's rule, you still employ all the rigmarole of Calculus.

To be clear, it should be stated that <code>Leibniz</code> is not a Calculus system. It is an algebra system. It simply does the algebra so well that it can answer questions which would otherwise require a needlessly complex workaround like Calculus.
