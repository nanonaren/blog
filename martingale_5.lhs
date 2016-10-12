    [BLOpts]
    profile    = nanonaren
    postid     = 902
    title      = "Martingales - Problem (30/365)"
    tags       = daily, probability
    categories =

Let $(\theta_k, \mathcal{D}_k)$ and $(\phi_k, \mathcal{D}_k)$ be two
martingales, $\theta_1 = \phi_1 = 0$. Show that

$$
E\theta\phi = \sum_{k=2}^{n}E(\theta_{k}-\theta_{k-1})(\phi_{k}-\phi_{k-1})
$$

Proceed as follows.

$$
\sum_{k=2}^{n}E(\theta_{k}-\theta_{k-1})(\phi_{k}-\phi_{k-1}) \\
= E\sum_{k=2}^{n}E\left[(\theta_{k}-\theta_{k-1})(\phi_{k}-\phi_{k-1})|\mathcal{D}_{k-1}\right]\mbox{ by total probability formula} \\
= E\sum_{k=2}^{n}E(\theta_{k}\phi_{k}|\mathcal{D}_{k-1})-E(\theta_{k}\phi_{k-1}|\mathcal{D}_{k-1})-E(\theta_{k-1}\phi_{k}|\mathcal{D}_{k-1})+E(\theta_{k-1}\phi_{k-1}|\mathcal{D}_{k-1}) \\
= E\sum_{k=2}^{n}E(\theta_{k}\phi_{k}|\mathcal{D}_{k-1})-\phi_{k-1}E(\theta_{k}|\mathcal{D}_{k-1})-\theta_{k-1}E(\phi_{k}|\mathcal{D}_{k-1})+E(\theta_{k-1}\phi_{k-1}|\mathcal{D}_{k-1}) \\
\mbox{ because }\phi_{k-1},\theta_{k-1}\mbox{ are }\mathcal{D}_{k-1}\mbox{-measurable} \\
= E\sum_{k=2}^{n}E(\theta_{k}\phi_{k}|\mathcal{D}_{k-1})-2\phi_{k-1}\theta_{k-1}+E(\theta_{k-1}\phi_{k-1}|\mathcal{D}_{k-1})\mbox{ by definition of martingale} \\
= E\sum_{k=2}^{n}E(\theta_{k}\phi_{k}|\mathcal{D}_{k-1})-\phi_{k-1}\theta_{k-1}\mbox{ because }\theta_{k-1}\phi_{k-1}\mbox{ is }\mathcal{D}_{k-1}\mbox{-measurable} \\
= E\sum_{k=2}^{n}E(\theta_{k}\phi_{k}|\mathcal{D}_{k-1})-E(\phi_{k-1}\theta_{k-1}|\mathcal{D}_{k-1}) \\
= E\theta_{n}\phi_{n}-E\phi_{1}\theta_{1} \\
= E\theta_{n}\phi_{n}\mbox{ since }\theta_{1}=\phi_{1}=0
$$
