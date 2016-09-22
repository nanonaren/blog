    [BLOpts]
    profile    = nanonaren
    postid     = 807
    title      = "Chebyshev's Inequality For Bounding From Below? (12/365)"
    tags       = daily, probability
    categories =

Problem 6.2 asks the following. Let $f = f(x)$ be a nonnegative even
function that is nondecreasing for positive $x$. Then for a random
variable $\theta$ with $|\theta(\omega)| \le C$,

$$
\frac{Ef(\theta) - f(\epsilon)}{f(C)} \le P(|\theta - E\theta | \ge \epsilon) \le \frac{Ef(\theta - E\theta)}{f(\epsilon)}
$$

From the looks of it, the upper bound seems simple enough because it
looks like a direct application of [Chebyshev's
inequality](https://nanonaren.wordpress.com/2016/09/21/writing-probability-as-an-expectation-10365/). Whereas,
the lower bound doesn't look familiar. If we go back to the proof of
Chebyshev's inequality we can try to see if we can arrive at a lower
bound instead of an upper bound. It happens that we can once we know
that the random variable is bounded $|\theta(\omega)| \le C$.

$$
P(\theta \ge \epsilon) = EI(\theta \ge \epsilon) \\
\ge E\frac{\theta}{C} I(\theta \ge \epsilon) \\
= E\frac{\theta}{C} - E\frac{\theta}{C}I(\theta < \epsilon) \\
\ge E\frac{\theta}{C} - E\frac{\epsilon}{C}I(\theta < \epsilon) \\
\ge \frac{E(\theta - \epsilon)}{C}
$$

Applying this quickly leads to the required solution. The problem points out the case where $f(x) = x^2$, which leads to

$$
\frac{E\theta^2 - \epsilon^2}{C^2} \le P( | \theta - E\theta | \ge \epsilon) \le \frac{V\theta}{\epsilon^2}
$$

We are in essence bounding the probability that the random variable
$\theta$ takes on a value close to its mean $E\theta$, which I should
imagine is pretty useful to have not so much for computing the
probabilities but for asymptotic analysis like we used for the [law of
large
numbers](https://nanonaren.wordpress.com/2016/09/22/whats-real-about-probability-11365/).
