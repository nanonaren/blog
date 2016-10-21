    [BLOpts]
    profile    = nanonaren
    postid     = 946
    title      = "Measurable Spaces - Problem (40/365)"
    tags       = daily, probability
    categories =

The past few problems looked at how probability works when we have an
infinite sample space. It didn't cover how one can actually assign
probabilities to such spaces. That will be the next task. Before that,
the book covers the topic of $\sigma$-algebras which form the algebra
of events on top of which we can assign a measure.

Given a set $\Omega$, and a set of subsets $\mathcal{A}$, we say that
$\mathcal{A}$ is an algebra if $\Omega \in \mathcal{A}$ and is closed
under unions and complementation. A $\sigma$-algebra adds to that the
requirement that it also be closed under *countable* unions. The pair
$(\Omega, \mathcal{A})$ is called a measureable space.

Let $\mathcal{A}_1, \mathcal{A}_2$ be $\sigma$-algebras of
$\Omega$. Are the following systems of sets $\sigma$-algebras?

$$
\mathcal{A}_1 \cap \mathcal{A}_2 = \{ A : A \in \mathcal{A}_1 \text{ and } A \in \mathcal{A}_2 \} \\
\mathcal{A}_1 \cup \mathcal{A}_2 = \{ A : A \in \mathcal{A}_1 \text{ or } A \in \mathcal{A}_2 \}
$$

The intersection of $\sigma$-algebras is also a $\sigma$-algebra
because $\Omega \in \mathcal{A}_1 \cap \mathcal{A}_2$, and $A_1 \cup A_2
\cup \dots \in$ in the intersection is contained in both
$\mathcal{A}_1$ and $\mathcal{A}_2$.

However, the union of $\sigma$-algebras is not always a
$\sigma$-algebra. For instance, let $\mathcal{A} = \{ A, \bar{A},
\emptyset, \Omega_1 \}$, $\mathcal{B} = \{ B, \bar{B}, \emptyset,
\Omega_2 \}$, then their union does not contain $A \cup B$.
