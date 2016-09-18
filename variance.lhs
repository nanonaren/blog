    [BLOpts]
    profile    = nanonaren
    postid     = 774
    title      = "Correlation - I (6/365)"
    tags       = daily, probability
    categories =

Let me follow up on the theme -- talked about [here](https://nanonaren.wordpress.com/2016/09/15/product-of-expectations-4365/) -- of
distributing an operator over another. To recap, we saw two ways of
distributing an expectation

$$
E(\theta + \phi) = E(\theta) + E(\phi) \\
(E \theta \phi)^2 = E \theta^2 E \phi^2 \\
E\theta \phi = E\theta E\phi \text{ if } \theta, \phi \text{ are independent}
$$

Can we say anything like this about variance? Consider the variance of
the sum of random variables (recall that $V\theta = E(\theta -
E\theta)^2$).

$$
V(\theta + \phi) = V\theta + V\phi + 2E(\theta - E\theta)(\phi - E\phi)
$$

Looks like we have an extra term. Before trying to see what this is,
can we think of a situation when this term could be $0$ and thus give
us $V(\theta + \phi) = V\theta + V\phi$? The answer is yes because if
$\theta$ and $\phi$ are independent, then $2E(\theta - E\theta)(\phi -
E\phi) = 2E\theta\phi - 2E\theta E\phi = 2E\theta \phi - 2E\theta \phi
= 0$.

In general, though, this extra term will be non-zero. How can we make
sense of this extra term? If you go back to the Cauchy-Bunyakovskii
inequality we looked at, we can write this term as

$$
(E(\theta - E\theta)(\phi - E\phi))^2 \le E(\theta - E\theta)^2 E(\phi - E\phi)^2 \\
(E(\theta - E\theta)(\phi - E\phi))^2 \le V\theta V\phi
$$

So it looks as if we can relate the extra term to the variance of the
random variables. More specifically, we see that

$$
| \rho(\theta,\phi) | = | \frac{E(\theta - E\theta)(\phi - E\phi)}{\sqrt{V\theta V\phi}} | \le 1
$$

And this gets to be called the *correlation coefficient*. Why is this
interesting you ask? First, note that

$$
\rho(\theta,\phi) = 0 \implies V(\theta + \phi) = V\theta + V\phi
$$

You've often heard how a lack of correlation does not imply
independence of random variables. Well, the above identity is the
conclusive proof. Because

$$
\text{if } E(\phi - E\phi)(\theta - E\theta) = E\theta\phi - E\theta E\phi = 0 \\
\text{then } E\theta\phi = E\theta E\phi \text{ does not imply } \theta,\phi \text{ independent}
$$

Take the simple example of a fair coin and two random variables
$f(H)=2,f(T)=0$ and $g(H)=0.5,g(T)=0$, then

$$
Ef Eg = (2+0)(0.5+0) = 1 \\
Efg = (2 \times 0.5) + 0 = 1 \\
\text{But } p(f=2,g=0.5) = p(f=2 | g=0.5)p(g=0.5) = (1)(0.5) \\
\text{which is } \ne p(f=2)p(g=0.5)
$$
