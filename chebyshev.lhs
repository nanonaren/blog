    [BLOpts]
    profile    = nanonaren
    postid     = 800
    title      = "Writing Probability as an Expectation (10/365)"
    tags       = daily, probability
    categories =

Expectation $E(\cdot)$ is such an ubiquitous construct in probability
that it's worth looking at the various situations in which it 
appears. It's one of those constructs that was so cheap to invent but
pays off time and again.

Consider a random variable $\theta$ and asking the following question.

$$
P(\theta \ge \epsilon) = ?
$$

We can already write this as an expectation

$$
P(\theta \ge \epsilon) = EI(\theta \ge \epsilon)
$$

where $I(\theta \ge \epsilon)$ is the indicator function.
If $\epsilon > 0$ we can make the following substitution

$$
EI(\theta \ge \epsilon) \le E\frac{\theta}{\epsilon} I(\theta \ge \epsilon)
$$

By dividing $\theta$ by $\epsilon$, the random variable takes on
values $\ge 1$ whenever $I(\theta \ge \epsilon)$ and thus we end up
with the above inequality. Further, if $\theta \ge 0$ is a non-negative
random variable we can continue

$$
P(\theta \ge \epsilon) \\
= EI(\theta \ge \epsilon) \\
\le E\frac{\theta}{\epsilon} I(\theta \ge \epsilon) \text{ if } \epsilon > 0\\
= \frac{1}{\epsilon} E\theta - \frac{1}{\epsilon} E\theta I(\theta < \epsilon) \\
\le \frac{1}{\epsilon} E\theta \text{ if } \theta \ge 0
$$

This gives us one version of the Chebyshev's inequality

$$
P(\theta \ge \epsilon) \ge E\frac{\theta}{\epsilon} \text{ when } \theta \ge 0 \text{ and } \epsilon > 0
$$

### Example

As an example, I have computed the chebyshev approximations of the
probabilities of a die taking on value greater than $x$ for two
different dies.

    [ghci]
    :l Stats.hs
    let fair_die = replicate 6 (1/6 :: Rational)
    let die2 = map (/21) [6,5..1 :: Rational]
    let var = [1..6]
    let error dist xs ys = e dist (zipWith (\x y -> (x-y)^2) xs ys)

    print $ map (1-) (cumul fair_die)
    print $ map (e fair_die var /) var

    let error_fair = error fair_die (map (1-) (cumul fair_die)) $ map (e fair_die var /) var
    let error_die2 = error die2 (map (1-) (cumul die2)) $ map (e die2 var /) var
    error_fair > error_die2

As you can see, the upper bound is pretty poor. But we see that the
bound is closer when we consider probabilities closer to the
tail. I'll come back to this and a couple of other versions of the
Chebyshev's inequality at a later time.
