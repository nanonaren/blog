    [BLOpts]
    profile    = nanonaren
    title      = "Probability Foundations - Problem (37/365)"
    tags       = daily, probability
    categories =

A problem similar to the previous post. Let $\Omega$ be a countable
set and $\mathcal{A}$ a collection of all its subsets. Put $\mu(A) =
0$ if $A$ is finite and $\mu(A) = \infty$ if $A$ is inifinite. Show
that the set function $\mu$ is finitely additive but not countable
additive.

To see that it is finitely additive, let $A,B \in \mathcal{A}$ be disjoint.

$$
\mu(A \cup B) = \mu(A) + \mu(B) = 0 + 0 = 0 \text{ if both finite} \\
\mu(A \cup B) = \mu(A) + \mu(B) = \infty \text{ if either one infinite}
$$

To show that it is not countably additive, consider the case where
$\Omega = \mathbb{N}$ is the set of natural numbers. Then

$$
\mu(\{ 1, 2, 3, \dots \}) = \infty \\
\text{But,}
\sum_{i=1}^\infty \mu(i) = 0
$$
