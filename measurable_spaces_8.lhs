    [BLOpts]
    profile    = nanonaren
    postid     = 1017
    title      = "Measurable Spaces - Problem (47/365)"
    tags       = daily, probability
    categories =

The previous post involved distribution functions over the real
numbers but it's also possible to have distribution functions over
$R^n$. A problem asks to show that if we have the distribution function

$$
F_n(x_1, \dots, x_n) = P((-\infty, x_1] \times \dots \times (\infty, x_n])
$$

and a difference function

$$
\Delta_{a_i,b_i} = F_n(x_1, \dots, x_{i-1}, b_i, x_{i+1}, \dots) - F_n(x_1, \dots, x_{i-1}, a_i, x_{i+1}, \dots) \\
\text{for } a_i \le b_i
$$

then show that

$$
\Delta_{a_1,b_1} \dots \Delta_{a_n,b_n} = P(a,b] \\
\text{where } a = (a_1, \dots, a_n), b = (b_1, \dots, b_n)
$$

We can derive this as follows

$$
P(a,b] = P((a_1,b_1] \times \dots \times (a_n,b_n]) \\
= \int_{a_1 < x_1 \le b_1} P(\{x_1\} \times (a_2,b_2] \times \dots \times (a_n,b_n])
$$
