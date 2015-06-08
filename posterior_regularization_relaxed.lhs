    [Blopts]
    profile    = nanonaren
    postid     = 231
    title      = "Adding (more Relaxed) Constraints during Model Inference"
    tags       = modeling, statistics, regularization, constraints
    categories = modeling, statistics

In the [previous
post](https://nanonaren.wordpress.com/2015/06/05/adding-constraints-during-model-inference/)
on posterior regularization we saw how to specify constraints during
the $E$-step of expectation maximization that would otherwise be
difficult to incorporate into the model itself. The constraints took the following form

$$
\mathbf{E}_q[\mathbf{f}(\mathbf{x},\mathbf{z})] \le \mathbf{b}
$$

where we specified our constraints as expectations over the
distribution of hidden variables. As an example, we took GMM
clustering and used this framework to specify the constraint that at
most two of the points in $\{x_1,x_2,x_3\}$ should be in the same
cluster: $\sum_{i=1}^N \sum_{z=1}^K p_{\mu}(z | x_i) (\delta_{i1} +
\delta_{i2} + \delta_{i3}) \le 2$.

What if we wanted to specify instead that those points should be
separated into as many clusters as possible -- if possible, each of
those three points in three different clusters? This poses a problem
because we do not know *a priori* if the data will allow (in a
sensible way) for the three points to be separated and all we may know
is that they are very likely well-separated.

A solution is presented in "Posterior vs. Parameter Sparsity in Latent
Variable Models" [1] where the posterior regularization framework is
relaxed by allowing $\mathbf{b}$ to be variable while being penalized
by a function $R(\cdot)$. Specifically, the $E$-step now looks like

$$
\underset{q,b}{\arg\min} \text{ KL}(q||p) + R(\mathbf{b}) \text{ s.t. } \mathbf{E}_q[\mathbf{f}(\mathbf{x},\mathbf{z})] \le \mathbf{b}
$$

With this setup, a simple way to ensure our points to live in separate
clusters is to let $R(b) = \sigma b$ where $\sigma$ is the strength of
the regularization whose value must be ascertained
experimentally. From the test section we find values of $\sigma$
between $0.6$ and $2.1$.

## Experiments

The experiments in this paper are conducted with respect to
Part-of-speech (POS) tagging which warrants its own look later on. For
now, let's look at a useful number computed to compare models. The
experiments in the paper want to show that a HMM augmented with this
framework manages to assign fewer POS tags to a word than
otherwise. To show this they consider words occurring more than $10$
times in the data and compute

$$
(\ell_1 / \ell_{\infty})(w) = \frac{n_w}{ \max_t \{ n_{wt} \}}
$$

where $n_w$ is the number of times $w$ occurred and $n_{wt}$ is the
number of times $w$ is assigned tag $t$. The closer this value is to
$1$ the sparser the tag assignments.


[1] Joao Graca and Kuzman Ganchev and others. 2009. "Posterior vs
Parameter Sparsity in Latent Variable Models". *Advances in Neural
Information Processing Systems 22*
