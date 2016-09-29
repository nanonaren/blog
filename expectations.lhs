    [BLOpts]
    profile    = nanonaren
    postid     = 820
    title      = "Expectations (16/365)"
    tags       = daily, probability
    categories =

Sometimes expectations and random variables appear in so many ways
that I find it a little confusing at times. The book I am following
has a neat diagram that helps quite a bit.

![Expectations](expectations.png)

Basic Expectation
-----------------

Start with a distribution $P(\cdot)$ for sample space $\Omega$ and a
random variable $\theta$. The basic expectation tells us the value
$\theta$ is likely to take *on average*.

$$
E\theta = \sum_\omega \theta(\omega) P(\omega) \\
\text{Notice that } E\theta = E[\theta I_\Omega]
$$

Note that $\theta$ induces a decomposition $D_1, \dots, D_n$ of $\Omega$ as follows

$$
E\theta = \sum_{i=1}^n x_i P(D_i) \text{ where } D_i = \{ \omega | \theta(\omega) = x_i \}
$$

Conditional Expectation
-----------------------

Instead of $P(\cdot)$ we could be given a distribution $P(\cdot | D)$
with respect to an event $D$. The expectation over this is the value
$\theta$ is likely to take *on average* conditioned on the event $D$
(i.e. restricted to).

$$
E(\theta | D) = \sum_\omega \theta(\omega) P(\omega | D) \\
\text{Notice that } E(\theta | D) = \frac{E[\theta I_D]}{P(D)}
$$

Let's suppose that we have an event $A$ and its conditional
probabilities $P(A | D_1), \dots, P(A | D_n)$ where $\{ D_i \}$ is a
decomposition of $\Omega$. We can write this as a random variable that
takes on the value $P(A | D_i)$ whenever $\omega \in D_i$.

$$
P(A | \mathcal{D})(\omega) = \sum_{i=1}^n P(A | D_i) I_{D_i}(\omega)
$$

Now that we have this random variable, we can once again take its
expectation which in this case gives the probability of $A$
*on average* conditioned on an event from $\mathcal{D}$. This has a
special name and is called the *total probability*.

$$
E P(A | \mathcal{D}) = \sum_{i=1}^n P(A | D_i) P(D_i) = P(A)
$$

One More Generalization
-----------------------

Suppose now that we have a random variable $\theta$ inducing the
decomposition $A_1, \dots, A_m$ where $A_j = \{ \omega |
\theta(\omega) = x_j \}$. We also have conditional probabilities
$P(A_j | D_i)$. We can certainly take the following expectation from
before

$$
E(\theta | D_i) = \sum_j x_j P(A_j | D_i)
$$

which is the expectation of $\theta$ conditioned on $D_i$. We now do
this for the entire decomposition $\mathcal{D}$ to arrive at this
random variable

$$
E(\theta | \mathcal{D})(\omega) = \sum_i E(\theta | D_i) I_{D_i}(\omega)
$$

which takes on a conditional expectation of $\theta$ at each
$\omega$. We can now generalize the total probability formula such
that it gives the the expectation of $\theta$ *on average* conditioned
on an event from $\mathcal{D}$.

$$
E(A | \mathcal{D}) = P(A) \\
\text{generalizes to} \\
EE(\theta | \mathcal{D}) = E\theta
$$

The lesson here is to always translate the things we do with
probabilities to expectations.
