    [BLOpts]
    profile    = nanonaren
    postid     = 1014
    title      = "Measurable Spaces - Problem (46/365)"
    tags       = daily, probability
    categories =

The rest of the chapter on $\sigma$-algebras goes through the
construction of various other measurable spaces such as those on the
space of 1) continuous functions, 2) functions continuous on the
right, and 3) direct products of measurable spaces. The next chapter
introduces methods of introducing probability measures on measurable
spaces.

The way we do this is to start with a distribution function $F(x)$
from which we derive a unique probability measure where $P(a,b]) =
F(b) - F(a)$. Here is a problem.

Let $F(x) = P(-\infty, x)$, then verify that $P(a,b] = F(b) - F(a)$.

$$
P(a,b] = P\left( (-\infty,b] \setminus (-\infty, a] \right) \\
= P(-\infty, b] - P(-\infty, a] \text{ by additivity} \\
= F(b) - F(a)
$$

Verify that $P(a,b) = F(b-) - F(a)$ where $F(x-) = \lim_{y \uparrow x} F(y)$.

$$
P(a,b) = P \left( \cup_{n=1}^\infty P(a,b-\frac{1}{n}] \right) \\
= \lim_n P(a,b - \frac{1}{n}] \text{ because } P \text{ is countably additive over } \mathcal{B}(R) \\
= \lim_n F(b - \frac{1}{n}) - F(a) \\
= F(b-) - F(a)
$$

The proof for the following are similar: $P[a,b] = F(b) - F(a-)$,
$P[a,b) = F(b-) - F(a-)$, and $P\{x\} = F(x) - F(x-)$.
