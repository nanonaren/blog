    [BLOpts]
    profile    = nanonaren
    postid     = 817
    title      = "Unbiased Estimator (15/365)"
    tags       = daily, probability
    categories =

In the coin tossing setup, we saw that the fraction $T_n(\omega) =
\frac{S_n(\omega)}{n}$ of observed heads in $n$ trails $\omega$ approaches $p$
as $n \rightarrow \infty$. The function $T_n$ is called an estimator that takes on a value in $[0,1]$. It's a special kind of estimator called an unbiased estimator because it satisfies

$$
E_\theta T_n = \theta \text{ for all } \theta \in [0,1]
$$

It says that on average, this estimator deduces the correct answer. Consider a different estimator $T_n(\omega) = \frac{S_n(\omega)}{n+1}$ which essentially starts with an assumed 'tails'. Then

$$
E_\theta T_n = \frac{n}{n+1} E_\theta \frac{S_n}{n} = \frac{n\theta}{n+1}
$$

which, on average, slightly underestimates the success probability.

A problem asks the following. Let it be known *a priori* that $\theta$
has a value in the set $\Omega_0 \subseteq [0,1]$. Construct an
unbiased estimator for $\theta$, taking values only in $\Omega_0$.

Consider the case where $\Omega_0 = \{ r \}$ for $r \in [0,1]$. Then
$T_n = r$ is an unbiased estimator because

$$
E_\theta T_n = r\theta + (1-\theta)r \text{ for } \theta \in \Omega_0
$$

which is unbiased because $\theta$ can only be $r$. Now, I can't seem
to proceed further than this. For example, what is the estimator when
$\Omega_0 = \{ r_1, r_2 \}$? I'll have to return with an answer
another day.
