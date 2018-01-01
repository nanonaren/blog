    [BLOpts]
    profile    = nanonaren
    postid     = 1024
    title      = "Measurable Spaces - Problem (48/365)"
    tags       = daily, probability
    categories =

Another question on distribution functions. Show that each of the functions

$$
G(x,y) = 1 \text{ if } x+y \ge 0 \text{ and } 0 \text{ otherwise} \\
G(x,y) = [x+y] \text{, the integral part of } x+y
$$

is continuous on the right but is not a distribution function in
$\mathbb{R}^2$.

Take the first function. To show that it is continuous on the right,
let $\epsilon > 0$ and let $(x,y) \in \mathbb{R}^2$. We need to show
that there exists $\delta > 0$ such that for all $(a,b) > (x,y)$ and
within a distance of $\delta$ the following holds: $| G(x,y) - G(a,b)
| < \epsilon$. If $G(x,y) = 0$, then let $\delta$ be the distance to
the nearest point $(p,q)$ where $p+q = 0$. We see that in this case,
picking a point $(a,b) > (x,y)$ within $\delta$ of $(x,y)$ will take
on a value of $0$ and the difference will be less that $\epsilon$. If
$G(x,y) = 1$, then $x \ge y$ and we can easily pick $(a,b) > (x,y)$
such that $a > b$, meaning it will also take on a value of $1$ and
satisfy $\epsilon$. Thus, the first function is continuous on the
right.

However, it is not a distribution function because it does not satisfy
the requirement [described in the last post](https://nanonaren.wordpress.com/2016/11/02/measurable-spaces-problem-47365/) that
$\Delta_{a_1b_1}\Delta_{a_2b_2} \ge 0$ because if $(a_1,a_2) = (1,2)$
and $(b_1,b_2) = (-4,3)$ the difference function evaluates to $-1$.

The second function $G(x,y) = [x+y]$ is not a distribution function
because $G(\infty, \infty) \ne 1$. But it is continuous on the right
because if we pick a point $(x,y)$ the function is constant on the
interval $[[x+y],[x+y+1])$ and it is open on the right meaning we can
always find a delta on the right to satify any $\epsilon > 0$.
