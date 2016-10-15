    [BLOpts]
    profile    = nanonaren
    postid     = 916
    title      = "Random Walk (33/365)"
    tags       = daily, probability
    categories =

One of the annoying things, for me, when studying probability are the
examples furnished in textbooks because they almost always have to do
with physics or gambling or finance. There is nothing wrong with
physics examples except for the fact that they too easily fit into the
probabilistic framework. So, I can't generalize them to more abstract
situtations. Gambling and financial examples hold no interest for me
whatsoever. What I really want are suprising examples.

Let me give an example. As I mentioned, I am currently working out
problems on random walks, martingales, and markov chains. Take the
basic random walk which we construct as follows. Let the sample space
be $\Omega = \{ \omega : \omega = (\omega_1, \dots, \omega_n), \omega_i = \pm 1
\}$ and $p(\omega) = p^{v(\omega)} (1-p)^{n- v(\omega)}$ where
$v(\omega) = (\sum \omega_i + n) / 2$.

Now, we consider the sequence of these random variables

$$
S_0(\omega) = 0 \\
S_k(\omega) = \omega_1 + \dots + \omega_k = S_{k-1}(\omega) + \omega_k
$$

Here are some standard ways of interpreting this sequence of random variables

1. Given a 2-dimensional grid, $S_k$ represents the position after $k$
steps (starting from $(0,0)$) taken by either going up one or going to
the right one.

2. Consider a gambling game with two players. If $\omega_i = 1$ lets
say player A gains one dollar from player B and when $\omega_i = -1$
player B gains one dollar from player A. Suppose player A and B start
with $d_A$ dollars and $d_B$ dollars. $S_k$ therefore represents the
amount of money won by player $A$ after $k$ turns. If $S_k = d_B$ then
player B has lost all his money. So we here can ask questions like
what is the probability that player A or player B will be ruined
(i.e. loses all his money).

This is all well and good and the examples generalize to more general
random walks but I want to consider some completely left-field
examples. I can't guarantee they will lead anywhere and may be utterly
rubbish but I think it will be an interesting exercise. See you next
time!
