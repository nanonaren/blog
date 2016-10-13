    [BLOpts]
    profile    = nanonaren
    postid     = 909
    title      = "Markov Chains - Problem (32/365)"
    tags       = daily, probability
    categories =

Let $M$ be a stochastic matrix (i.e. $M_{ij} > 0$ and $\sum_j M_{ij} =
0$ for all $i$). Show that $1$ is an eigenvalue of this matrix and all
its eigenvalues $\lambda$ satisfy $| \lambda | \le 1$.

Since, $M$ is a stochastic matrix we can apply the Ergodic theorem
which tells us that there exists $\pi = \langle \pi_1, \dots, \pi_n \rangle$
with $\sum_i p_i = 1$ such that $M^n v \rightarrow p$ as $n
\rightarrow \infty$. Thus, we see that $Mp = (1)p$.

To show that all eigenvalues of $M$ have a magnitude less than $1$, note that

$$
\text{for all vectors } v \\
\min \{v_i \} \le \sum_j M_{ij} v_j \le \max \{ v_i \} \\
\text{because } M_i \text{ is a convex combination}
$$

As a result, if an eigenvalue $\lambda > 1$ for an eigenvector $v$,
then $\lambda \max \{ v_i \} > \max \{ v_i \}$. Similarly, if $\lambda
< -1$, then $\lambda \min \{ v_i \} < \min \{ v_i \}$. Therefore, all
eigenvalues must satisfy $| \lambda | \le 1$.
