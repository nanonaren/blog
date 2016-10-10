    [BLOpts]
    profile    = nanonaren
    postid     = 898
    title      = "Martingales - Problem (29/365)"
    tags       = daily, probability
    categories =

Show that every martingale $\theta = (\theta_i, \mathcal{D}_k)$ has uncorrelated increments: if $a < b < c < d$ then

$$
\text{cov}(\theta_d - \theta_c, \theta_b - \theta_a)
$$

Proceed as follows.

$$
\mbox{cov}(\theta_{d}-\theta_{c},\theta_{b}-\theta_{a})	\\
= E\left[(\theta_{d}-\theta_{c}-E(\theta_{d}-\theta_{c}))(\theta_{b}-\theta_{a}-E(\theta_{b}-\theta_{a}))\right] \\
= E\left[(\theta_{d}-\theta_{c}-E\theta_{d}+E\theta_{c})(\theta_{b}-\theta_{a}-E\theta_{b}+E\theta_{a})\right] \\
= E\left[(\theta_{d}-\theta_{c})(\theta_{b}-\theta_{a})\right]\mbox{ since }E\theta_{k}=E\theta_{1}\mbox{ for all }k=0 \\
= E\left[(\theta_{d}-E(\theta_{d}|\mathcal{D}_{c}))(\theta_{b}-E(\theta_{b}|\mathcal{D}_{a}))\right]\mbox{ by martingale definition} \\
= E\left[(E(\theta_{n}|\mathcal{D}_{d})-E(\theta_{n}|\mathcal{D}_{c}))(E(\theta_{n}|\mathcal{D}_{b})-E(\theta_{n}|\mathcal{D}_{a}))\right] \\
\mbox{ by martingale definition} \\
= E\left[E(\theta_{n}|\mathcal{D}_{d})E(\theta_{n}|\mathcal{D}_{b})\right]-E\left[E(\theta_{n}|\mathcal{D}_{b})E(\theta_{n}|\mathcal{D}_{c})\right]-E\left[E(\theta_{n}|\mathcal{D}_{d})E(\theta_{n}|\mathcal{D}_{a})\right]+E\left[E(\theta_{n}|\mathcal{D}_{c})E(\theta_{n}|\mathcal{D}_{a})\right] \\
= EE(\theta_{n}^{2}|\mathcal{D}_{d})-EE(\theta_{n}^{2}|\mathcal{D}_{c})-EE(\theta_{n}^{2}|\mathcal{D}_{d})+EE(\theta_{n}^{2}|\mathcal{D}_{c}) \\
\mbox{ because the finer decomposition subsumes the coarser one} \\
= 2\theta_{n}^{2}-2\theta_{n}^{2}\mbox{ by total probability formula} \\
= 0
$$

As a quick note as to why $E\theta_k = E\theta_1$,

$$
\theta_{1} \\
= E(\theta_{2}|\mathcal{D}_{1}) \\
= E(E(\theta_{3}|\mathcal{D}_{2})|\mathcal{D}_{1})\mbox{ by definition of martingales} \\
= E(\theta_{3}|\mathcal{D}_{1})\mbox{ since }\mathcal{D}_{1}\le\mathcal{D}_{2} \\
= E(\theta_{k}|\mathcal{D}_{1})\mbox{ continuing the same way} \\
E\theta_{1} = EE(\theta_{k}|\mathcal{D}_{1})=E\theta_{k}\mbox{ by total probability formula}
$$
