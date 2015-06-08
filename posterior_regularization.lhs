    [Blopts]
    postid     = 220
    profile    = nanonaren
    title      = "Adding Constraints during Model Inference"
    tags       = modeling, statistics, regularization, constraints
    categories = modeling, statistics

Coming up with a probabilistic model and its inference procedure is
only half the work because it's well known that just a single run of
the inference procedure is hardly likely to give you a satisfactory
answer. Out of the many reasons this could be -- local minima is not
satisfactory, there's not enough sparsity constraints, the model makes
too many independence assumptions, and so on -- the one that is
significant in all cases, from the point of view of a consumer of the model, is that
viable solutions clearly have to come from a restricted subspace
within the complete parameter space of the model.

Take for instance the inference of an HMM where we would like the
probability of transition from state $s_1 \mapsto s_2$ to be same as
from $s_2 \mapsto s_1$ thereby forcing the transition matrix to be
symmetric. This is easily solved by adding lagrange multipliers
$\lambda_{ij}(M_{ij} - M_{ji})$ in the HMM's EM derivation. Similarly,
other equality constraints can be added in this manner.

But most times the constraints are a lot softer such as the data point
$X_1$ and $X_2$ must not belong to the same cluster or that there must
be at least $10$ points in each cluster. Baking these constraints in
the probabilistic model itself is (pretty much) impossible and so is
converting these into equality constraints because the number of such
constraints is combinatorial.

A solution is proposed in "Expectation Maximization and Posterior
Constraints" [1] (referred to in literature as *posterior
regularization*) that allows one to specify a vector of constraints of
the form

$$
\mathbf{E}_q[f_1(\mathbf{x},\mathbf{z})] \le b_1] \\
\dots \\
\mathbf{E}_q[f_n(\mathbf{x},\mathbf{z})] \le b_n] \\
\text{ i.e. } \mathbf{E}_q[\mathbf{f}(\mathbf{x},\mathbf{z})] \le \mathbf{b}]
$$

This modification is not without consequences. In particular, the
paper shows that this modification trades off the maximization of the
log-likelihood against the distance to the desired posterior subspace
(created by the constraints). This trade-off follows from the
observation that maximizing log-likelihood does not always yield
desired solutions due to a model's generality.

## Example

Let's work out an example using this framework. Consider a GMM
clustering of data points $\{ x_1,\dots,x_N \}$ using a fixed number of
clusters $K$, fixed variance $\sigma^2$, and unknown means
$\mathbf{\mu}$.

In addition, suppose that we would like to enforce the constraint that
at most two of the points in $\{x_1, x_2, x_3\}$ should be in the same
cluster. The paper allows us to specify this constraint as an
inequality of an expectation over the cluster membership probabilities
$p_{\mu}(z | x)$

$$
\sum_{i=1}^N \sum_{z=1}^K p_{\mu}(z | x_i) (\delta_{i1} + \delta_{i2} + \delta_{i3}) \le 2 \\
p_{\mu}(z | x_i) = \frac{p(x_i | \mu_z)}{\sum_z p(x_i | \mu_z)}
$$

The paper shows that we may bake in this constraint to the
$E$-step by solving the following dual optimization problem

$$
\underset{\lambda \ge 0}{\arg \max}
\left(
  \lambda 2 -
  \sum_i \log \sum_z p_{\mu}(z | x_i) e^{\lambda (\delta_{i1} + \delta_{i2} + \delta_{i3})}
\right) \\
p_{\lambda}(z | x_i) = \frac{p_{\mu}(z | x_i )e^{\lambda (\delta_{i1} + \delta_{i2} + \delta_{i3})}}
  {\sum_z p_{\mu}(z | x_i) e^{\lambda (\delta_{i1} + \delta_{i2} + \delta_{i3})}}
$$

Notice how that this only matters when $i=1,2,3$. Otherwise, the
$e^{\lambda (\delta_{i1} + \delta_{i2}
+ \delta_{i3})}$ term evaluates to $1$ and recovers the traditional
$E$-step where $p_{\lambda}(z | x_i) = p_{\mu}(z | x_i)$. The
derivative with respect to $\lambda$

$$
2 - \sum_i \sum_z \frac{ (\delta_{i1} + \delta_{i2} + \delta_{i3}) p_{\mu}(z | x_i) e^{\lambda (\delta_{i1} + \delta_{i2} + \delta_{i3})}}
  {\sum_{z} p_{\mu}(z | x_i) e^{\lambda (\delta_{i1} + \delta_{i2} + \delta_{i3})}} \\
2 - \sum_i \sum_z p_{\lambda}(z | x_i) (\delta_{i1} + \delta_{i2} + \delta_{i3})
$$

With that, we get our gradient update (for use in gradient ascent) for
$\lambda_i$ as the following and after convergence (or a few
iterations) arrive at $\lambda^*$ and set the $E$-step to
$p_{\lambda^*}(z|x_i)$.

$$
\lambda \gets \lambda + \eta (2 - \sum_i \sum_z p_{\lambda_i}(z | x_i) (\delta_{i1} + \delta_{i2} + \delta_{i3}))
$$

Though this framework is already quite accommodating people have
relaxed the framework even more and we'll look at one next time.

[1] Joao Graca and Kuzman Ganchev and Ben
Taskar. 2007. "Expectation Maximization and Posterior
Constraints". *Advances in Nerual Information Processing Systems 20*
