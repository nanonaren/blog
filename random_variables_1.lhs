    [BLOpts]
    profile    = nanonaren
    postid     = 1030
    title      = "Random Variables - Problem (50/365)"
    tags       = daily, probability
    categories =

Moving on to the next chapter "Random Variables - I", take a look at
the following problem. Show that the random variable $\theta$ is
continuous if and only if $P(\theta = x) = 0$ for all $x \in
\mathbb{R}$.

(Forward direction) Suppose $\theta$ is a continuous random variable,
then its distribution function $F$ is also continuous by
definition. Hence, $P(\theta = x) = F(x) - F(x-) = F(x) - \lim_{y
\rightarrow x} F(y) = 0$ by definition of continuity.

(Reverse direction) Suppose $P(\theta = x) = 0$ for all $x \in
\mathbb{R}$ and let $F$ be the corresponding distribution
function. Let $\{A_n\}$ be a sequence of sets such that $A_n \supseteq
A_{n+1}$ such that $\cap_{n=1}^\infty A_n = \{x\}$. Then,
$P(\cap_{n=1}^\infty A_n) = \lim_n P(A_n)$ because $P$ is countably
additive. Thus, $\lim_n P(A_n) = P(x)$ and since $P(a,b] = F(b) -
F(a)$ we see that $\lim_{x \rightarrow c} F(x) = F(c)$ for any $c$
which is the definition of continuity.
