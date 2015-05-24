    [BLOpts]
    profile    = nanonaren
    title      = "Learning the Dirichlet hyperparameters"
    tags       = modeling, statistics
    categories = modeling

> -- started : Thu May 23 10:35:45 IST 2015
> -- finished: Thu May 23 14:35:14 IST 2015

One of the things you'll notice in papers describing generative models
of documents using a Dirichlet prior is to simply fix the Dirichlet
hyperparameter $\alpha$ that controls the distributions of topic
mixtures for each document. This isn't ideal when you wish to then
compute the probability of an unseen document $p(\mathbf{w}_d |
\alpha, \beta)$ because a fixed $\alpha$ encodes no knowledge of
the distribution of topic-mixtures over documents in the training corpus.

In the appendix of the journal paper by Blei et. al [1], we find a
procedure to learn the $\alpha$ that maximizes the following
log-likelihood where $\theta_j$ are the document specific topic
mixtures and $\alpha$ the dirichlet hyperparameter.

$$
\Lambda := \log \prod_j p(\theta_j | \alpha) = \\
  N \log \Gamma(\sum \alpha_i) - N\sum \log \Gamma(\alpha_i) + \sum_{i,j} (\alpha_i -1) \log \theta_{ij}
$$

The expression is maximized using the <A HREF="http://en.wikipedia.org/wiki/Newton%27s_method">Newton-Raphson
method</A>, which
requires computing the derivative of $\Lambda$ and the Hessian (the
matrix of second-order derivatives) -- with respect to
$\alpha$.

## Derivative of $\Lambda$

Making use of the digamma function $\psi(x) = \frac{\partial \log
\Gamma(x)}{\partial x}$ the partial derivatives with respect of each
component of $\alpha$ gives

$$
\frac{\partial \Lambda}{\partial \alpha_r} = N \psi(\sum \alpha_i) - N \psi(\alpha_r) + \sum_j \log \theta_{jr}
$$

## The Hessian

The Hessian component-wise with respect of $\alpha_r$ and $\alpha_s$ is

$$
\frac{\partial \log \Lambda}{\partial \alpha_r \alpha_s} = N \psi'(\sum \alpha_i) - \delta_{rs} N \psi'(\alpha_r)
$$

where $\delta_{rs}$ is the Kronecker delta. Note that this Hessian
matrix can be written in the following form

$$
H = \text{diag}(h) + \mathbf{1}z\mathbf{1}^T
$$

where $\text{diag}(h)$ is the diagonal matrix containing the
second-order derivatives with respect to $\alpha_i$s across the
diagonal and zero elsewhere; $z = \psi'(\sum \alpha_i)$; and
$\mathbf{1}$ is vector of $1$s. The inverse of a matrix of his form is
given by the *Matrix Inversion Lemma* which states

$$
(A - BD^{-1}C)^{-1} = \frac{A^{-1} + A^{-1}BCA^{-1}}{D - CA^{-1}B}
$$

In our case, $H^{-1}$ is given by

$$
H^{-1} = \text{diag}(h)^{-1} - \frac{\text{diag}(h)^{-1} \mathbf{1}\mathbf{1}^T \text{diag}(h)^{-1}}{ z^-1 + \sum_{j=1}^k h_j^{-1}} \\
H^{-1}_{ij} = \text{diag}(h)^{-1}_{ij} - \frac{h^{-1}(h^{-1})^T}{ z^{-1} + \sum_j h^{-1}_j}
$$

## The upside

We are now ready to compute the new guess for the next iteration in
the Newton-Raphson method: $\alpha_{new} = \alpha_{old} -
H^{-1}(\alpha_{old}) g(\alpha_{old})$. The offsets component-wise are
given

$$
\alpha_{old} - (H^{-1} g)_i = \alpha_{old} - \sum_j H^{-1}_{ij} g_j \\
= \alpha_{old} - h^{-1}g_i - \frac{\sum_j  (h^{-1} (h^{-1})^T)_{ij} g_j}{ z^{-1} + \sum_j h^{-1}_j} \\
= \alpha_{old} - \frac{1}{h^{-1}} \left( g_i - \frac{ \sum_j \frac{g_j}{h_j} }{z^{-1} + \sum_j h^{-1}_j} \right)
$$

The reason for the special attention given to the form of the Hessian
in this problem si that it requires the computation of only the values
$h_i$ and $g_i$ which amount to $2k$ values which is only *linear* in
$k$ (the dimension of $\alpha$) to the otherwise $O(k^3)$ required for a
full-blown matrix inversion of $H^{-1}$.

----
[1] David M. Blei and Andrew Y. Ng and Michael I. Jordan and John
Lafferty. 2003. "Latent Dirichlet Allocation." *Journal of Machine
Learning Research*.
