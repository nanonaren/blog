    [BLOpts]
    profile    = nanonaren
    postid     = 854
    title      = "Sequence of Decompositions (22/365)"
    tags       = daily, probability
    categories =

I am now at the end of Chapter 1 in with four sections to go on random
walks, martingales, and markov chains. I am going to skip the section
on random walks for now and come back to it later. I haven't seen
martingales before so I'll start with that. Since martingales makes
use of expectations with respect to decompositions heavily I want to
get a little more comfortable with it.

Suppose that $\mathcal{D}_1 \le \mathcal{D}_2$ are two decompositions
of the sample space where $\mathcal{D}_2$ is *finer* than
$\mathcal{D}_1$. Finer means that $\forall D \in \mathcal{D}_2,
\exists E \in \mathcal{D}_1 \text{ s.t. } D \subseteq E$.

Let $\theta$ be a random variable. First, recall the expectation of a
random variable with respect to a decomposition $\mathcal{D}_1$.

$$
E(\theta | \mathcal{D}_1) = \sum_j x_j P(A_j | \mathcal{D}_1) \\
\text{where } A_j = \theta^{-1}(x_j)
$$

Note the special case when $\mathcal{A} \le \mathcal{D}_1$ (i.e., when
$\theta$ is $\mathcal{D}_1$-measurable).

$$
E(\theta | D \in \mathcal{D}_1) = \sum_j x_j P(A_j | D) \\
= \sum_j x_j \frac{P(A_j \cap D)}{P(D)} \\
= \sum_j x_j I(D \subseteq A_j) \\
= x_j \\
\text{hence } E(\theta | \mathcal{D}_1) = \theta
$$

Next, recall the generalized total probability formula

$$
E \left[ E(\theta | \mathcal{D}) \right] = E \theta
$$

Suppose we took a conditional expection instead

$$
E \left[ E(\theta | \mathcal{D}) | \mathcal{D}' \right] \\
\text{let } D' \in \mathcal{D}' \\
E \left[ E(\theta | \mathcal{D}) | D' \right] \\
= \sum_i \left[ \sum_j x_j P(A_j | D_i) \right] P(D_i | D')
$$

This gets simplified if $\mathcal{D}$ is a finer decomposition than
$\mathcal{D}'$ because $D'$ is now decomposed by $\mathcal{D}$

$$
= \sum_i \sum_j x_j P(A_j | D_i) P(D_i | D') \\
= \sum_j x_j \sum_i P(A_j | D_i) P(D_i | D') \\
= \sum_j x_j P(A_j | D')
$$

Therefore if $\mathcal{D}_1 \le \mathcal{D}_2$

$$
E \left[ E(\theta | \mathcal{D}_2) | \mathcal{D}_1 \right] = E(\theta | \mathcal{D}_1)
$$

And in general if $\mathcal{D}_1 \le \mathcal{D}_2 \le \dots \le \mathcal{D}_n$

$$
E \left[ E(\theta | \mathcal{D}_j) | D_1 \right] = E(\theta | \mathcal{D}_1) \text{ for } j > 1
$$
