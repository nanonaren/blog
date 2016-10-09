    [BLOpts]
    profile    = nanonaren
    postid     = 890
    title      = "Martingales - Problem (27/365)"
    tags       = daily, probability
    categories =

I am still unable to follow the martingales based proof for the Ballot
Theorem, so I'll work out a few more problems first. A problem asks
the following. Let $\mathcal{D}_0 \le \dots \le \mathcal{D}_n$ be a
sequence of decompositions with $\mathcal{D}_0 = \{ \Omega \}$, and
let $\eta_k$ be $\mathcal{D}_k$-measurable variables. Show that the
sequence $\theta = (\theta_k, \mathcal{D}_k)$ with

$$
\theta_k = \sum_{l=1}^{k+1}\left(\eta_{l}-E(\eta_{l}|\mathcal{D}_{l-1})\right)
$$

is a martingale.

To show this we need to show that (1) $\theta_k$ is
$\mathcal{D}_k$-measurable. This is clear because $\theta_k$ takes on
a single value (the expectation) conditioned on each $D \in \mathcal{D}_k$.
Next, we need to show that (2) $E(\theta_{k+1} | \mathcal{D}_k) = \theta_k$.

$$
E(\theta_{k+1}|\mathcal{D}_{k})	\\
= E\left[\sum_{l=1}^{k+1}\left(\eta_{l}-E(\eta_{l}|\mathcal{D}_{l-1})\right)|\mathcal{D}_{k}\right] \\
= \sum_{l=1}^{k+1}E\left[\eta_{l}-E(\eta_{l}|\mathcal{D}_{l-1})|\mathcal{D}_{k}\right] \\
= E(\eta_{k+1}|\mathcal{D}_{k})-E\left[E(\eta_{k+1}|\mathcal{D}_{k})|\mathcal{D}_{k}\right]+\sum_{l=1}^{k}E(\eta_{l}|\mathcal{D}_{k})-E\left[E(\eta_{l}|\mathcal{D}_{l-1})|\mathcal{D}_{k}\right] \\
= E(\eta_{k+1}|\mathcal{D}_{k})-E\left[E(\eta_{k+1}|\mathcal{D}_{k})|\mathcal{D}_{k}\right]+\sum_{l=1}^{k}\eta_{l}-E\left[E(\eta_{l}|\mathcal{D}_{l-1})|\mathcal{D}_{k}\right] \\
\mbox{ since }\eta_{l}\mbox{ is }\mathcal{D}_{i\ge l}\mbox{-measurable} \\
= E(\eta_{k+1}|\mathcal{D}_{k})-E\left[E(\eta_{k+1}|\mathcal{D}_{k})|\mathcal{D}_{k}\right]+\sum_{l=1}^{k}\eta_{l}-E(\eta_{l}|\mathcal{D}_{l-1}) \\
\mbox{ since }\mathcal{D}_{l-1}\le\mathcal{D}_{k}\mbox{ and }E(\eta_{l}|\mathcal{D}_{l-1})\mbox{ is }\mathcal{D}_{l-1}\mbox{-measurable} \\
= E(\eta_{k+1}|\mathcal{D}_{k})-E(\eta_{k+1}|\mathcal{D}_{k})+\sum_{l=1}^{k}\eta_{l}-E(\eta_{l}|\mathcal{D}_{l-1}) \\
\mbox{ since }E(\eta_{k+1}|\mathcal{D}_{k})\mbox{ is }\mathcal{D}_{k}\mbox{-measurable} \\
= \theta_{k}
$$
