    [BLOpts]
    postid     = 1161
    profile    = nanonaren
    title      = "Formal Power Series and Determinants"
    tags       = math, combinatorics
    categories = Combinatorics

It seems that wherever you go in mathematics linear algebra comes chasing
you. Here is one such example where you are simply dealing with
subsets and nothing more. You are probably aware that subsets of $\{1,
\dots, n\}$ can be represented formally using the power series

$$
\sum_{1 \le i_1 < i_2 < \dots < i_k \le n} x_{i_1}x_{i_2}\dots x_{i_k}
$$

The above expression can be more succintly written as the following product.

$$
(1 + x_1)(1 + x_2)\dots (1 + x_n)
$$

The sign alternating version of this is given below where subsets of even size have $+1$ as their coefficient and subsets of odd size have $-1$ as their coefficient.

$$
(1 - x_1)(1 - x_2) \dots (1 - x_n)
$$

Surprisingly, this can be computed as a determinant! Recall, that a determinant of a square matrix is

$$
\text{det}(A)_{1 \le i, j \le n} = \sum_{w \in S_n} \text{sgn}(w) A_{1w(1)}\dots A_{nw(n)}
$$

Now, construct a matrix $A$ as follows 1) $A_{ij} = x_j$ if $i \le j$,
2) $A_{ij} = 1$ if $i = j+1$, and 3) $A_{ij} = 0$ otherwise.

Convince yourself that the determinant of $A$ is $(1 - x_1)(1 - x_2)
\dots (1 - x_n)$.
