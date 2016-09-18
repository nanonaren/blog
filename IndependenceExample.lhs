    [BLOpts]
    profile    = nanonaren
    postid     = 787
    title      = "Correlation - II (7/365)"
    tags       = daily, probability
    categories =

In the last
[post](https://nanonaren.wordpress.com/2016/09/16/correlation-i-6365/]),
I said I'd give an example of two random variables $\theta, \phi$
whose expectations are independent $E\theta \phi = E\theta E\phi$ but
are not themselves independent.

For a first attempt, this turned out not to be an easy task because
when coming up with an example I had to control two things: 1) making
sure I can get $P(\theta = x | \phi = y) \ne p(\theta = x)$ for some $x,
y$ and 2) making sure I can get $E\theta \phi = E\theta E\phi$.

I realized that to make like easier I should try to get $E\theta \phi
= 0$ which means I only have to make sure one of the expectations
$E\theta = 0$ and I can use $\phi$ to freely adjust to try and make
$P(\theta = x | \phi = y) \ne p(\theta = x)$.

Consider a fair six-sided die. Let

$$
\theta(2) = \theta(4) = \theta(6) = 1 \text{ i.e. when even} \\
\theta(1) = \theta(3) = \theta(5) = -1 \text{ i.e. when odd} \\
\text{therefore } E\theta = 0
$$

Now, after playing around with $\phi$, I came up with this

$$
\phi(1) = 1 \\
\phi(2) = \phi(4) = \phi(6) = 0 \\
\phi(3) = \phi(5) = -0.5 \\
\text{therefore } E\theta\phi = \frac{1}{6} ((-1)(1) + (1)(0) + (-1)(-0.5) + (1)(0) + (-1)(-0.5) + (1)(0) ) = 0
$$

Yay! Now, check for non-independence

$$
p(\phi = 1 | \theta=1) = \frac{0}{3} \ne p(\phi = 1) = \frac{1}{6}
$$
