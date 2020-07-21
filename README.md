
# Leibniz: A Rule System for Math

Author:	Anthony John Ripa

Date:	2020.07.20

Live Demo of Version  1 at <a href='https://swish.swi-prolog.org/p/hVEWFHXN.pl'>https://swish.swi-prolog.org/p/hVEWFHXN.pl</a>

Live Demo of Version  2 at <a href='https://swish.swi-prolog.org/p/JWdsRkzO.pl'>https://swish.swi-prolog.org/p/JWdsRkzO.pl</a>

Live Demo of Version  3 at <a href='https://swish.swi-prolog.org/p/fMJuWNsH.pl'>https://swish.swi-prolog.org/p/fMJuWNsH.pl</a>

Live Demo of Version  4 at <a href='https://swish.swi-prolog.org/p/oeuNtQbk.pl'>https://swish.swi-prolog.org/p/oeuNtQbk.pl</a>

Live Demo of Version  5 at <a href='https://swish.swi-prolog.org/p/txPnJtvm.pl'>https://swish.swi-prolog.org/p/txPnJtvm.pl</a>

Live Demo of Version  6 at <a href='https://swish.swi-prolog.org/p/zhOrBjYr.pl'>https://swish.swi-prolog.org/p/zhOrBjYr.pl</a>

Live Demo of Version  7 at <a href='https://swish.swi-prolog.org/p/XLCwOknT.pl'>https://swish.swi-prolog.org/p/XLCwOknT.pl</a>

Live Demo of Version  8 at <a href='https://swish.swi-prolog.org/p/nTiEvFQq.pl'>https://swish.swi-prolog.org/p/nTiEvFQq.pl</a>

Live Demo of Version  9 at <a href='https://swish.swi-prolog.org/p/bDaiBkLV.pl'>https://swish.swi-prolog.org/p/bDaiBkLV.pl</a>

Live Demo of Version 10 at <a href='https://swish.swi-prolog.org/p/EuEYjSIE.pl'>https://swish.swi-prolog.org/p/EuEYjSIE.pl</a>

Live Demo of Version 11 at <a href='https://swish.swi-prolog.org/p/BmSRzUaV.pl'>https://swish.swi-prolog.org/p/BmSRzUaV.pl</a>

Live Demo of Version 12 at <a href='https://swish.swi-prolog.org/p/jBWdZyZS.pl'>https://swish.swi-prolog.org/p/jBWdZyZS.pl</a>

Live Demo of Version 13 at <a href='https://swish.swi-prolog.org/p/XKwSQIHP.pl'>https://swish.swi-prolog.org/p/XKwSQIHP.pl</a>

Live Demo of Version 14 at <a href='https://swish.swi-prolog.org/p/FrxKkJXa.pl'>https://swish.swi-prolog.org/p/FrxKkJXa.pl</a>

Live Demo of Version 15 at <a href='https://swish.swi-prolog.org/p/lbjsQXIh.pl'>https://swish.swi-prolog.org/p/lbjsQXIh.pl</a>

Live Demo of Version 16 at <a href='https://swish.swi-prolog.org/p/GSfUPNft.pl'>https://swish.swi-prolog.org/p/GSfUPNft.pl</a>

Live Demo of Version 17 at <a href='https://swish.swi-prolog.org/p/dRcCftbI.pl'>https://swish.swi-prolog.org/p/dRcCftbI.pl</a>

Live Demo of Version 18 at <a href='https://swish.swi-prolog.org/p/oJwKjMry.pl'>https://swish.swi-prolog.org/p/oJwKjMry.pl</a>

Live Demo of Version 19 at <a href='https://swish.swi-prolog.org/p/lltCdZLR.pl'>https://swish.swi-prolog.org/p/lltCdZLR.pl</a>

Live Demo of Version 20 at <a href='https://swish.swi-prolog.org/p/DEKeboal.pl'>https://swish.swi-prolog.org/p/DEKeboal.pl</a>

Live Demo of Version 21 at <a href='https://swish.swi-prolog.org/p/KAPHrSTb.pl'>https://swish.swi-prolog.org/p/KAPHrSTb.pl</a>

Live Demo of Version 22 at <a href='https://swish.swi-prolog.org/p/KyEsVtPj.pl'>https://swish.swi-prolog.org/p/KyEsVtPj.pl</a>

Live Demo of Version 23 at <a href='https://swish.swi-prolog.org/p/ywXZGtSr.pl'>https://swish.swi-prolog.org/p/ywXZGtSr.pl</a>

Live Demo of Version 24 at <a href='https://swish.swi-prolog.org/p/nLRXyBmQ.pl'>https://swish.swi-prolog.org/p/nLRXyBmQ.pl</a>

Live Demo of Version 25 at <a href='https://swish.swi-prolog.org/p/vERRvwXC.pl'>https://swish.swi-prolog.org/p/vERRvwXC.pl</a>

Live Demo of Version 26 at <a href='https://swish.swi-prolog.org/p/YnEhbMPB.pl'>https://swish.swi-prolog.org/p/YnEhbMPB.pl</a>

## Leibniz

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

## Semantics

One way to think about the comparison between <code>Leibniz</code> and other approaches, is in terms of semantics. The term semantics is popular in the field of logic. To logicians, the term semantics is intended to mean something like the everyday word meaning. However, logicians' definition seems somewhat forced, and alternative interpretations seem needed. Nevertheless, the word can be used to refer to a practical distinction in logic. Sentences have two things about them that can be studied. One is their syntax. This is like the way the symbols are arranged in the sentence, and relates to the rules of arranging such symbols. The second is their semantics. This is the so-called meaning of the sentence. As an example, one can imagine the same sequence of symbols having one meaning in one language, and a different meaning in another language. This is the practical distinction that I want now.

Consider x+x=2x. What this means depends on the semantics. The normal semantics is that it means that no matter what you substitute in for x, the equation is still true. This semantics defers algebraic truths to arithmetic truths.

Consider x/x=1. What this means depends on the semantics. The normal semantics is that it means that no matter what you substitute in for x, the equation is still true. Under that semantics you try many numbers and it works. However, if you try 0 then you get 0/0=1. This is not unambiguously true in normal arithmetic. Therefore, x/x=1 is not unambiguously true in normal algebra.

Let us now consider an alternate semantics. This semantics will not defer to arithmetic for its meaning. This semantics will ground its meaning directly in generic quantities. Consider x+x=2x. This is true because whenever I add a thing to itself, then I have 2 of that thing. So, x+x=2x is true. That's the argument. It is not deferred to arithmetic to check, because that is not what is meant with this new semantics. Consider x/x=1. How many quantities per quantity do we have? Well, 1. So x/x=1. This statement is judged true.

This semantics seems to have a corresponding visual interpretation:

<pre>
           ▄█
         ▄▀ █ x
       ▄▀   █ 
     ▄█▄▄▄▄▄█
         x
</pre>

If we consider how many steps we rise, for each step we go to the right, then the answer is 1. So x/x=1. You can think the slope is 1.

Similarly:

<pre>
            █
           ██
          █ █
         █  █ 
        █   █ 2x
       █    █
      █     █ 
     ████████
         x
</pre>

If we consider how many steps up we go up, for each step we go to the right, then the answer is 2. So 2x/x=2. You can think the slope is 2.

Normally, x stands in for a particular number, or rather any particular number. We check that an algebraic equation is true by checking that the equation holds after substituting particular numbers in for x. This kind of x is usually referred to as a variable. The name variable is not that great but it is intended to signify that it is not one particular number.

Our x has a meaning that does not seem to be identical to its normal usage. We do not mean varying over different particular numbers that may be substituted for x. We mean that x is a quantity. It is not a particular quantity. It is a general quantity. It is a generic quantity. We merely think about what would be true for a generic quantity.

This suggests that we may escape the degeneracy that happens in normal algebra where a square, and a right triangle of same length are the same thing if the length is zero. For example, ordinarily [½x²/x² @ x=0] = 0/0 undefined. For us [½x²/x² @ x=0] = ½. Triangles are triangles, no matter their size. Same for squares. Their ratio is always ½. For them, some triangles are so small they lose their identity and become a point.

The semantics of arithmetic is something of a foundation. The semantics of normal algebra is grounded in that foundation. As such, normal algebra struggles in the same places where arithmetic struggles. For example, they both struggle with ratios of small quantities. Fortunately, algebra need not be grounded in arithmetic. We have provided an alternate semantics for algebra. It is robust to ratios of small quantities. Other systems may use this semantics as their foundation. We may turn the tables and ground arithmetic in algebra. We may also ground calculus in algebra.

One reason to think that we may have gotten it backwards is by thinking about number and quantity. Numbers are things like 5 or 7. Numbers are particular quantities. In normal algebra, we have variables. We think of them as varying over different numbers. Variables vary over particular quantities. The notion of not being a particular quantity is constructed as varying over particular quantities. This does not quite capture the notion of not being particular. This construction leads to confusions about things like x/x. Instead, we may think of a generic quantity. This generic quantity is not standing in for particular quantities. Generic quantities are not generic particular quantities. The particularity fails to help, and helps to fail. The generic quantity is just that: a quantity. It may have a name, like x. However, this does not make it particular. Furthermore, the rules of how to manipulate quantities that are not particular, is not constrained by the rules of how to manipulate quantities that are particular.

In the alternative, rather than changing the semantics of algebra, we can change the semantics of arithmetic. Earlier, we spoke about parsing x/x@x=0. We can parse it  as (x@x=0)/(x@x=0) then 0/0. We can parse it as 1@x=0 then 1. In this 2 branch tree we avoid the 0/0 branch to get 1. We formalized this at one point with directional parses → instead of bidirectional =. We could try fixing arithmetic this way then basing algebra on it. 1 → 0/0 and 0 → 0/0 but not 0/0 → something. Perhaps, it is not entirely important if we fix algebra, and then base arithmetic on the fixed algebra, or if we fix arithmetic and base algebra on the new arithmetic. It is important that we fix. We could also fix both and merely have them compatible, and perhaps interderivable without the need of declaring one as the foundation.