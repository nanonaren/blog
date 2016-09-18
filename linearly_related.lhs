    [BLOpts]
    profile    = nanonaren
    postid     = 790
    title      = "Correlation - III, Linear Dependence (8/365)"
    tags       = daily, probability
    categories =

In a previous
[post](https://nanonaren.wordpress.com/2016/09/16/correlation-i-6365/),
I showed one way to arrive at the correlation coefficient but it
doesn't really convince me of its need. What follows is a derivation
that will show you a more direct meaning for the correlation
coefficent and how it emerges as a quantity of interest when we look
at the following optimization problem.

Given two random variables $\theta, \phi$ a very natural question is
to ask how similar these two are. This is a loaded question because we
need to 1) state in what way they can be similar and 2) state how
certain we are about their similarity.

Let's say we will 1) only look for a linear relationship and 2) say
they are close based on the square error. That is, we want to find coefficients $a,b$ such that

$$
\underset{a,b}{\arg \min} E(\theta - (a\phi + b))^2
$$

I won't go through the derivation as it only uses the techniques
[here](https://nanonaren.wordpress.com/2016/09/13/how-are-mean-and-variance-related-2365/). When
optimized you will find the following.

$$
a\phi + b = E\phi + \frac{\text{cov}(\theta,\phi)}{V\phi}(\phi - E\phi)
$$

Let's see the error it generates

$$
E(\theta - (a\phi + b))^2 \\
= E(\theta - E\theta)^2 - 2\frac{\text{cov}^2(\theta,\phi)}{V\phi} + \frac{\text{cov}^2(\theta,\phi)}{V\phi} \\
= V\theta - \frac{\text{cov}^2(\theta,\phi)}{V\phi} \\
= V\theta \left( 1 - \frac{\text{cov}^2(\theta,\phi)}{V\theta V\phi} \right) \\
= V\theta \left( 1 - \rho^2(\theta,\phi) \right)
$$

And there is your correlation coefficient. We now see that $\theta,
\phi$ are linearly dependent when the $| \rho(\theta,\phi) | = 1$ and,
in general, the correlation coefficient implies the strength of linear
dependence between random variables.
