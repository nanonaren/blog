    [BLOpts]
    profile    = nanonaren
    postid     = 860
    title      = "Martingales - Problem (23/365)"
    tags       = daily, probability
    categories =

Having looked at sequence of decompositions in the last post I can now
get some motivation for the definition of a martingale: A sequence of random variables $\theta_1, \dots, \theta_n$ is called a martingale with respect to the decompositions $\mathcal{D}_1 \le \dots \le \mathcal{D}_n$ if

$$
\text{(1) } \theta_k \text{ is } \mathcal{D}_k\text{-measurable} \\
\text{(2) } E(\theta_{k+1} | \mathcal{D}_k) = \theta_k
$$

Examples
--------

Let $\omega_1, \dots, \omega_n$ be independent Bernoulli random
variables with $P(\omega_i = 1) = p$ and $P(\omega_i = -1) = q$ and
$S_k(\omega) = \sum_i \omega_i$. If $p \ne q$ show that the sequence
$\theta_k = \left( \frac{q}{p} \right)^{S_k}$ is a martingale.

$$
\theta_{k+1}(\omega) = \left( \frac{q}{p} \right)^{S_{k+1}(\omega)} = \left( \frac{q}{p} \right)^{S_k(\omega) + \omega_{k+1}} = \theta_k(\omega) \left( \frac{q}{p} \right)^{\omega_{k+1}} \\
\text{therefore } \left\{ \omega : \theta_{k+1}(\omega) = \left( \frac{q}{p} \right)^{S_{k+1}} \right\} \subseteq \left\{ \omega : \theta_k(\omega) = \left( \frac{q}{p} \right)^{S_k} \right\}
$$

Hence, the sequence of $(\theta_k)$ is a martingale. Show that the sequence $\theta_k = S_k - k(p - q)$ is also a martingale.

$$
\theta_{k+1}(\omega) = S_{k+1} - (k+1)(p-q) \\
= S_k + \omega_{k+1} - k(p-q) - (p-q) \\
= \theta_k(\omega) + \omega_{k+1} - (p-q) \\
\text{The same argument as above therefore applies.} 
$$
