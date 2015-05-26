    [BLOpts]
    profile    = nanonaren
    title      = "The Entropic Prior"
    tags       = modeling, statistics
    categories = modeling

> -- started : Sun May 24 09:08:09 IST 2015
> -- finished: Tue May 26 13:18:23 IST 2015

Dirichlet (either by itself, or as a mixture of, or as a hierarchy of) priors
are by no means the only option of controlling sparsity of topic
mixtures. *Entropic priors* stand out as an interesting
alternative. Given a probability distribution $\theta$, the entropic
prior is defined as

$$
P_e(\theta) \propto e^{-\alpha \mathcal{H}(\theta)} \\
\mathcal{H}(\theta) = - \sum_i \theta_i \log \theta_i
$$

Positive and negative values of $\alpha$ control the favorability
towards higher or lower entropies.

The paper "Sparse Overcomplete Latent Variable Decomposition of Counts
Data" [1] explores the use of entropic priors to counts data such as
words in documents or colors in images. Their main claim is that
entropic priors provide increasingly finer, yet non-trival, refinements
of data as the number of components are increased. This is in contrast
to the diminishing returns one sees when the number of topics are
increased in LDA or clustering models.

The paper on "Pattern Discovery via Entropy Minimization" [2] offers
an in-depth analysis of the consequences of using entropic priors. Two points are claimed:

1. An entropic prior $P_e(\cdot)$ "is a bias for compact models
having less ambiguity, more determinism, and therefore more
structure."

2. $P_e(\cdot)$ "is invariant to reparametrizations of the model,
because the entropy is defined in terms of the model's joint and/or
factored distributions."

The first point is illustrated with experiments in [1] and [2]. The second
point is an interesting property. A reparametrization of a model is
a change of variables given by a bijective function $f : \theta
\mapsto \phi$. Take for example the reparametrization $f(\theta = \langle \theta_1, \dots,
\theta_N \rangle) = \langle \theta_N,\dots,\theta_1 \rangle$ that
creates a bijection by reversing the co-ordinates. We can clearly see
that $P_e(\theta) = P_e(f(\theta))$ but $\text{Dirichlet}(\theta |
\alpha_1,\dots,\alpha_N) \ne \text{Dirichlet}(f(\theta) |
\alpha_1,\dots,\alpha_N)$.

## Inference

I mainly want to look at the inference of models with entropic priors
and [1] provides the setup. Consider that we are working with $M$
documents, $K$ topics with $\theta_{i}$ as the document-level mixture
distributions, and $\omega_k$ as the word distributions. The paper
performs an EM-derivation -- so we'll start with the complete
log-likelihood equation (where $V_{ij}$ is the count of word $j$ in document $i$)

$$
\Lambda \propto \prod_{k=1}^K P_e(\omega_k;\beta)
        \times \prod_{i=1}^M P_e(\theta_i;\alpha)
        \prod_{j=1}^{W} \left( \theta_{ik} \omega_{k \mathbf{w}_{ij}} \right)^{V_{ij} \gamma_{ijk}} \\
\log \Lambda \propto \alpha \sum_i \sum_k \theta_{ik} \log \theta_{ik}
             + \beta \sum_k \sum_j \omega_{kj} \log \omega_{kj}
             + \sum_i \sum_j V_{ij} \gamma_{ijk} \left( \log \theta_{ik} + \log \omega_{kj} \right)
$$

The E-step is given by

$$
\gamma_{ijk} \propto \frac{ \theta_{ik} \omega_{kj} }{ \sum_k \theta_{ik} \omega_{kj} }
$$

For the M-step we take the derivative of $\Lambda$
w.r.t. $\theta_{ik}$ and $\omega_{kj}$ (only this is shown below as
the other is very similar) under the constraints $\sum_k \theta_{ik} =
1$ and $\sum_j \omega_{kj} = 1$ (lagrange multipliers $\lambda_k$
below).

$$
\frac{\partial \Lambda}{\partial \omega_{kj}} = \beta + \log \omega_{kj} + \sum_i V_{ij} \frac{\gamma_{ijk}}{\omega_{kj}} + \lambda_k = 0
$$

Normally, at this point we're able to solve these set of equations by
writing $\omega_{kj}$ in terms of $\lambda_k$ but the $\log
\omega_{kj}$ is problematic. The involvement of $\log$ in this
fashion makes these into a set of *transcendental equations*.

### Fixed point equations

The solution, as pointed to in the paper, is to employ the Lambert
$\mathcal{W}$ function and solve a set of fixed-point equations. The
derivation is explained in [2] and i'll follow it here with a bit of
background. Why do want this? Well, in situations such as this where
we need to find an $x$ such that $f(x)=0$ one way to solve these is to
use fixed-point iterations as follows:

1. Convert the equation to the form $x = g(x)$
2. Start with an initial guess $x_0$
3. Iterate, using $x_{n+1} := g(x_n)$ for $n = 0,1,2 \dots$

and when $f$ is continuous and the limit $(x_n)_0^\infty$ exists, then,
this limit is a root of $f$.

In our set of equations we have a function of two
variables $\omega_{kj}$ and $\lambda_k$. To apply the fixed point
method here we need to come up with two functions $\omega_{kj} =
g(\lambda_k)$ and $\lambda_k = h(\omega_{ki})$ so that we may iterate
$\lambda_k^{(0)} \mapsto \{ \omega_{k1}^{(0)},\dots,\omega_{kW}^{(0)} \}
\mapsto \lambda_k^{(1)} \mapsto \dots \mapsto
\{ \omega_{k1}^{(n)},\dots,\omega_{kW}^{(n)} \}$.

Writing $\lambda_k$ as a function of $\omega_{ki}$ is obtained by
rearranging $\frac{\partial \Lambda}{\partial \omega_{kj}} = 0$. To
write $\omega_{kj}$ as a function of $\lambda_k$ requires help. Consider the
Lambert $\mathcal{W}$ function known to satisfy the following

$$
\mathcal{W}(y) e^{\mathcal{W}(y)} = y \\
\log \mathcal{W}(y) + \mathcal{W}(y) = \log y
$$

The structure of this identity is very similar to what we have in that
if we could coax this identity to look exactly like our equation with
$\mathcal{W}(y)$ taking the place of $\omega_{kj}$ and $\lambda_k$
involved in $y$ then we'll end up writing $\omega_{kj}$ in terms of
$\lambda_k$. Let $ \xi = \sum_i V_{ij} \gamma_{ijk} $. Starting with the identity above and setting $y = e^x$
to get rid of the $\log y$ term we have

$$
0 = -\mathcal{W}(e^x) - \log \mathcal{W}(e^x) + x \\
= \frac{-1}{1 / \mathcal{W}(e^x)} - \log \mathcal{W}(e^x) + x + \log q - \log q \\
= \frac{-q}{q / \mathcal{W}(e^x)} + \log q / \mathcal(W)(e^x) + x - \log q
$$

Setting $x = 1 + \lambda_k / \beta + \log q$ and $q = - \xi / \beta$ we arrive at

$$
\omega_{kj} = \frac {- \xi / \beta}{\mathcal{W}(-\xi e^{1 + \lambda_k / \beta} / \beta}
$$

Thus, we have arrived at the fixed-point equations for
$\omega_{k\cdot}$ and $\lambda$. I leave it to [2] to show that these
fixed-point equations do indeed converge.

[1] Madhusudana Shashanka and Bhiksha Raj and Smaragdis. 2008. "Sparse
Overcomplete Latent Variable Decomposition of Counts Data." *Advances
in Neural Information Processing Systems 20*.

[2] Matthew Brand. 1999. "Pattern Discovery via Entropy Minimization."
*Uncertainty 1999*.
