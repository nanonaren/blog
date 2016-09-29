    [BLOpts]
    profile    = nanonaren
    postid     = 828
    title      = "Conditional Probability - Problem (17/365)"
    tags       = daily, probability
    categories =

Going with the theme of trying to transplant probability laws to
expectations consider the following. if $A,B$ are independent events
we know that $P(A|B) = P(A)$. Now, if $\theta,\phi$ are independent
random variables then

$$
\text{for all x } E(\theta | \phi = x) \\
= \sum_\omega \theta(\omega) P(\omega | \phi = x) \\
= \sum_\omega \theta(\omega) P(\omega) \\
= E\theta
$$

A question now asks to give an example of random variables
$\theta,\phi$ which are *not independent* but for which $E(\theta |
\phi) = E\theta$.

$$
\text{Let } P(\omega) = \frac{1}{6} \\
\text{Let } \phi(1)=\phi(2)=\phi(3)=1 \text{ and } \phi(4)=\phi(5)=\phi(6)=0 \\
\text{Let } \theta(1)=1, \theta(4)=2, \theta(5)=-1 \text{ and rest is } 0 \\
\text{Note } P(\theta=0 | \phi=1) = \frac{2}{3} \ne P(\theta=0) = \frac{3}{6} \text{ so not independent} \\
\text{But } E(\theta | \phi=0) = E\theta = \frac{1}{3} = E(\theta | \phi=1)
$$
