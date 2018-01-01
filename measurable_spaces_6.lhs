    [BLOpts]
    profile    = nanonaren
    postid     = 1009
    title      = "Measurable Spaces - Problem (45/365)"
    tags       = daily, probability
    categories =

Show that the following is not a Borel set in $\mathcal{B}(R^{[0,1]})$
(this is the $\sigma$-algebra of functions over the domain $[0,1]$
unlike the previous onces we looked at where the domain was over the
natural numbers).

$$
A = \{ x : x_t = 0 \text{ for at least one } t \in [0,1] \}
$$

If $A$ is a Borel set, then so is its complement $\bar{A}$. We know
that all sets in $\mathcal{B}(R^{[0,1]})$ must have this form $\{ x :
(x_{t_1}, x_{t_2}, \dots) \in B \}$ for some $B \in
\mathcal{B}(R^\infty)$ and $t_1, t_2, \dots \in [0,1]$ (proved in book
and is simple). The function $y_t = 1$ belongs to $\bar{A}$ and
therefore $(y_{t_1}, \dots) \in B$. Consider

$$
z_t = y_t \text{ if } t \in (t_1, t_2, \dots) \\
z_t = 0 \text{ otherwise}
$$

Then, since $(z_{t_1}, \dots) = (y_{t_1}, \dots)$ the function $z_t$
belongs to $\{ x : (x_{t_1}, x_{t_2}, \dots) \in B \}$. But $z_t$
clearly does not belong to $\bar{A}$. Hence $\bar{A}$ is not a Borel
set and neither is $A$.
