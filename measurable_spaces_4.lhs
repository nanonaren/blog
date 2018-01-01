    [BLOpts]
    profile    = nanonaren
    postid     = 1004
    title      = "Measurable Spaces - Problem (43/365)"
    tags       = daily, probability
    categories =

Continuing the last post on showing that certain sets are Borel sets,
today we ask if the following is a Borel set.

$$
\{ x \in R^\infty : x_n \rightarrow \}
$$

This is the set of all sequences converging to a finite limit. My
initial thought was to use the result from last time where we showed
that the set of sequences bounded from above or below by $a$

$$
\{ x \in R^\infty : x_n \rightarrow a \} = \{ x \in R^\infty : \sup \inf x_n \ge a \} \cap \{ x \in R^\infty : \inf \sup x_n \le a \}
$$

But we can't then union all the sets of the above form for each
possible limit because there are uncountably many choices. It would
seem that we need a way to characterize limits without picking the
value of the limit. Luckily, there is such a characterization for
converging sequences of real numbers; namely, Cauchy sequences. A
sequence $x_1, x_2, \dots$ is a Cauchy sequence, if for all $\epsilon > 0$,
there is a positive integer $N$ such that for all $m,n > N$, $|
x_m - x_n | < \epsilon$. All sequences of real numbers converging to a
finite limit are also Cauchy sequences.

The Cauchy condition is true if and only if for all $m \ge 1$, $\lim_n
| x_n - x_{n+m} | = 0$. We can write the set of all converging
sequences as

$$
\cap_{m=1}^\infty \{ x \in R^\infty : | x_n - x_{n+m} | \rightarrow 0 \} \\
\cap_{m=1}^\infty \{ x \in R^\infty : \sup_n \inf_{k \ge n} | x_k - x_{k+m} | \ge 0 \} \cap \{ x \in R^\infty : \inf_n \sup_{k \ge n} | x_k - x_{k+m} | \le 0 \}
$$

As a result, the set of all sequences converging to a finite limit is
measurable.
