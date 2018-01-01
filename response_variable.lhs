    [BLOpts]
    profile    = nanonaren
    postid     = 1054
    title      = "Response Variable (55/365)"
    tags       = daily, probability
    categories =

A quick aside. I was thinking about how response variables are
attached to generative models. For instance, if we want to say have
binary classification on documents we would normally 1) take the dot
product the topic vector with a global vector of coefficients and then
2) pass it through a logistic function. This is fine, but the problem
is that if we were sampling the topic model using a Gibbs sampler we
have to use some form of gradient descent to compute the dot product
coefficients and to compute the coefficients of the logistic function.

This is painful. I'd rather have everything be probabilistic. I was
pondering on ways to do this. Let's say that the topic vector has
dimension $n$. Define two multinomial distributions $\{t_i\}$ and
$\{f_i\}$. Now define the response through this procedure

0. Given topic distribution $z$
1. $i \gets t$
2. $j \gets f$
3. response $\gets \text{Bernoulli}\left( \frac{z_i}{z_i + z_j} \right)$

Essentially, the idea is $t$ and $f$ end up finding mutually exclusive
dimensions such that either one or the other has a high value in order
to produce the correct class label with high probability. I'd like to
try it after I get done with the Gibbs code.
