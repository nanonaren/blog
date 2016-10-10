    [BLOpts]
    profile    = nanonaren
    postid     = 896
    title      = "Martingales - Problem (28/365)"
    tags       = daily, probability
    categories =

Let the random variables $\eta_1, \dots, \eta_k$ satisfy $E(\eta_k |
\eta_1, \dots, \eta_{k-1})=0$. Show that the sequence $\theta =
(\theta_k)_{1\le k \le n}$ with $\theta_1 = \eta_1$ and

$$
\theta_{k+1} = \sum_{i=1}^k \eta_{i+1}f_i(\eta_1, \dots, \eta_i)
$$

where $f_i$ are given functions, is a martingale.

Once again, we let the sequence of decompositions be $\mathcal{D}_k =
\mathcal{D}_{\eta_1, \dots, \eta_k}$. Then $\eta_k$ is
$\mathcal{D}_k$-measurable because (1) $f_i$ is $D_{k \ge
i}$-measurable, (2) $f_{i+1} f_i$ is $\mathcal{D}_{i+1}$-measurable,
and (3) $f_i + f_j$ is $\mathcal{D}_i$-measurable if $i \ge j$.

Next, we show that $E(\theta_{k+1} | \mathcal{D}_k) = \theta_k$

$$
E(\theta_{k+1}|\mathcal{D}_{k})	\\
= E\left[\sum_{i=1}^{k}\eta_{i+1}f_{i}(\eta_{1},\dots,\eta_{i})|\mathcal{D}_{k}\right] \\
= \sum_{i=1}^{k}E\left[\eta_{i+1}f_{i}(\eta_{1},\dots,\eta_{i})|\mathcal{D}_{k}\right] \\
= \sum_{i=1}^{k}E\left[\eta_{i+1}|\mathcal{D}_{k}\right]f_{i}(\eta_{1},\dots,\eta_{i})\mbox{ since }f_{i}(\eta_{1},\dots,\eta_{i})\mbox{ is }\mathcal{D}_{k\ge i}\mbox{-measurable} \\
= E\left[\eta_{k+1}|\mathcal{D}_{k}\right]f_{i}(\eta_{1},\dots,\eta_{k})+\sum_{i=1}^{k-1}E\left[\eta_{i}|\mathcal{D}_{k}\right]f_{i}(\eta_{1},\dots,\eta_{i}) \\
= \sum_{i=1}^{k-1}E\left[\eta_{i}|\mathcal{D}_{k}\right]f_{i}(\eta_{1},\dots,\eta_{i})\mbox{ since we are given }E\left[n_{k}|\eta_{1},\dots,\eta_{k-1}\right]=0 \\
= \sum_{i=1}^{k-1}\eta_{i+1}f_{i}(\eta_{1},\dots,\eta_{i})\mbox{ since }\eta_{i}\mbox{ is }\mathcal{D}_{k\ge i}\mbox{-measurable} \\
= \theta_{k}
$$
