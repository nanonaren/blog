    [BLOpts]
    profile    = nanonaren
    postid     = 939
    title      = "Probability Foundations - Problem (38/365)"
    tags       = daily, probability
    categories =

Let $\mu$ be a finitely additive measure on an algebra $\mathcal{A}$,
and let $A_1, A_2, \dots \in \mathcal{A}$ be pairwise disjoint and
satisfy $A = \cup_{i=1}^\infty A_i \in \mathcal{A}$. Then show that
$\mu(A) \ge \sum_{i=1}^\infty \mu(A_i)$.

$$
\text{Since } A \in \mathcal{A}, A - A_1 \in \mathcal{A} \text{, and } A - \cup_{i=1}^k A_i \in \mathcal{A} \\
\text{Let } B_i = \cup_{k=i}^\infty A_k \\
\sum_{i=1}^\infty \mu(A_i) \\
= \sum_{i=1}^\infty \mu(B_i - B_{i+1}) \\
= \sum_{i=1}^\infty \mu(B_i) - \mu(B_{i+1}) \\
= \mu(B_1) - \mu(B_2) + \mu(B_2) - \mu(B_3) + \mu(B_4) - \mu(B_5) + \dots \\
= \mu(B_1) - \lim_n B_n \\
= \mu(A) - \lim_n B_n \\
\le \mu(A)
$$
