    [BLOpts]
    profile    = nanonaren
    postid     = 831
    title      = "Conditional Probability - Problem (18/365)"
    tags       = daily, probability
    categories =

A problem asks the following. The conditional variance of $\theta$
with respect to $\mathcal{D}$ is the random variable

$$
V(\theta | \mathcal{D}) = E[(\theta - E(\theta | \mathcal{D}))^2 | \mathcal{D}]
$$

where $\mathcal{D}$ is a decomposition of the sample space. Show that

$$
V\theta = EV(\theta | \mathcal{D}) + VE(\theta | \mathcal{D})
$$

We can read this as the follows. The variance of $\theta$ is the sum
of the expectation of its conditional variances and the variance of
the conditional expectations. For example, if $\mathcal{D} = \Omega$,
then we ought to see a $0$ variance in the conditional expectations
(since there is only one condition)

$$
EV(\theta | \Omega) + VE(\theta | \Omega) \\
= EV\theta + VE\theta \\
= EV\theta + E(E\theta - EE\theta)^2 \\
= V\theta
$$

In general, the variance of conditional expectations expands to

$$
VE(\theta | \mathcal{D}) \\
= E \left[ E(\theta | \mathcal{D}) - EE(\theta | \mathcal{D}) \right]^2 \\
= E \left[ E(\theta | \mathcal{D}) - E \theta \right]^2 \\
= E \left[ E(\theta | \mathcal{D}) \right]^2 - 2E \left[ E(\theta | \mathcal{D}) E \theta \right] + E(E\theta)^2 \\
= E \left[ E(\theta | \mathcal{D}) \right]^2 - (E\theta)^2
$$

and the expectation of conditional variances expands to

$$
EV(\theta | \mathcal{D}) \\
= \sum_i E \left[ \left( \theta - E(\theta | D_i) \right)^2 | D_i \right] P(D_i) \\
= EE(\theta^2 | \mathcal{D}) - 2 E \left[ E(\theta | \mathcal{D}) E(\theta | \mathcal{D}) \right] + E \left[ E(\theta | \mathcal{D}) \right]^2 \\
= E\theta^2 - E \left[ E(\theta | \mathcal{D}) \right]^2
$$

Adding the two

$$
EV(\theta | \mathcal{D}) + VE(\theta | \mathcal{D}) \\
= E\theta^2 - E \left[ E(\theta | \mathcal{D}) \right]^2 + E \left[ E(\theta | \mathcal{D}) \right]^2 - (E\theta)^2 \\
= E\theta^2 - (E\theta)^2 \\
= V\theta
$$
