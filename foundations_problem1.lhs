    [BLOpts]
    profile    = nanonaren
    postid     = 931
    title      = "Probability Foundations - Problem (36/365)"
    tags       = daily, probability
    categories =

I did say it would be one post a day and it already looks like i'll
only achieve it as an expectation of posts every day. So, let me catch
up first by solving more problems. This time from chapter 2 of the
book: Mathematical Foundations of Probability Theory.

This chapter introduces us to how we can extend the probability
framework we had for finite sample spaces. The key problem we face is
that in the finite case we were simply able to assign a probability to
each $\omega \in \Omega$ and therefore get $P(X \subseteq \Omega) =
\sum_{x \in X} p(x)$. But we can no longer follow this approach for an
infinite sample space.

Anyway, the problem asks the following. Let $\Omega$ be the set of
rational numbers in $[0,1]$. Let $\mathcal{A}$ be the algebra of sets
where each set takes on one of these forms: $\{r : a < r < b \}$, $\{r
: a \le r < b \}$, $\{r : a < r \le b \}$, $\{r : a \le r \le b \}$
and $P(A) = b - a$. Show that $P(A)$ is a finitely additive set
function but not countably additive.

Let $A < B \in \mathcal{A}$ be disjoint sets. Then, we see that
$P(\cdot)$ is finitely additive.

$$
\text{We can write } P(A) = b - a = \sup A - \inf A \\
P(A \cup B) = \sup (A \cup B) - \inf (A \cup B) \\
= \sup B - \inf A \\
= (\sup A + P(B)) - \inf A \\
= P(A) + P(B)
$$

To show that $P(\cdot)$ is not countably additve we need to show that
we can come up with an infinite sequence of disjoint sets whose sum of
probabilitites is not equal to the probability of its union. This
should bring back memories of converging sequences. Consider the sets
$(\frac{1}{2},1], (\frac{1}{3}, \frac{1}{2}], \dots, (\frac{1}{n+1},
\frac{1}{n}], \dots$. It is clear that the union of these sets is $[0,1]$. But

$$
\sum_{i=1}^\infty P(( \frac{1}{n+1}, \frac{1}{n} ]) \\
= \sum_{i=1}^\infty \frac{1}{n} - \frac{1}{n+1} \\
= \sum_{i=1}^\infty \frac{1}{n(n+1)} \\
= \frac{1}{1(2)} + \frac{1}{2(3)} + \frac{1}{3(4)} + \frac{1}{4(5)} + \frac{1}{5(6)} + \frac{1}{6(7)} + \frac{1}{7(8)} + \dots \\
 > \frac{1}{2(2)} + \frac{1}{4(4)} + \frac{1}{4(4)} + \frac{1}{8(8)} + \frac{1}{8(8)} + \frac{1}{8(8)} + \frac{1}{8(8)} + \dots \\
= \frac{1}{4} + 2 \frac{1}{4(4)} + 4 \frac{1}{8(8)} + \dots \\
= \frac{1}{4} + \frac{1}{4} + \frac{1}{4} + \dots \\
= \infty
$$
