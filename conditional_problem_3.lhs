    [BLOpts]
    profile    = nanonaren
    postid     = 838
    title      = "Conditional Probability - Problem (19/365)"
    tags       = daily, probability
    categories =

A problem asks the following. Let $\phi, \theta_1, \dots, \theta_n$ be independent random variables where $\{ \theta_i \}$ are identically distributed and $\phi$ takes values $1, \dots, n$. Show that if $S_\phi = \theta_1 + \dots + \theta_\phi$ then

$$
E(S_\phi | \phi) = \phi E\theta_1 \\
V(S_\phi | \phi) = \phi V\theta_1 \\
E S_\phi = E \phi E \theta_1 \\
V S_\phi = E \phi V \theta_1 + V \phi (E \theta_1)^2
$$

The first two follow due to $\{ \theta_i \}$ being idependenty and identically distributed

$$
E(S_\phi | \phi) = E \theta_1 + E \theta_2 + \dots + E \theta_\phi = \phi E \theta_1 \\
V(S_\phi | \phi) = V S_1 + V S_2 + \dots + V S_\phi = \phi V \theta_1 
$$

The last two we compute by using 1) the generalized total probability
formula as seen
[here](https://nanonaren.wordpress.com/2016/09/28/expectations-16365/)
and 2) the conditional variance formula as seen
[here](https://nanonaren.wordpress.com/2016/09/29/conditional-probability-problem-18365/).

$$
E S_\phi = E E(S_\phi | \phi) = E \phi E \theta_1 \\
V S_\phi = EV(S_\phi | \phi) + VE(S_\phi | \phi) \\
= E \phi V \theta_1 + V(\phi E \theta_1) \\
= E \phi V \theta_1 + \phi (E \theta_1)^2 \text{ because } V(a \theta + b) = a^2 V \theta
$$
