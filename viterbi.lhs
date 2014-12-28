    [BLOpts]
    postid     = 129
    profile    = nanonaren
    title      = "Viterbi Algorithm to discover the best observation order"
    categories = Algorithm

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/viterbi.lhs)

I have a backlog of posts to get through. Here's number one. I
recently had to consider a problem with a setup very similar to the
one required by the Viterbi Algorithm. The well-worn Viterbi algorithm
allows one to find the sequence of latent states $s_1,\dots,s_n$ that
maximizes the probability of observing a sequence of observables
$o_1,\dots,o_n$ with respect to a Hidden Markov Model. The algorithm
is fully defined by the following recurrence (explained thoroughly in
[Wikipedia](http://en.wikipedia.org/wiki/Viterbi_algorithm)).

$$ V_{1,k} = P(o_1 | k) \cdot \pi_k \\
 V_{t,k} = P(o_k | k) \cdot \max_{x \in S}(a_{x,k} \cdot V_{t-1,x}) $$

where $\pi$ is the initial state distribution, $a_{i,j}$ the
transition distribution, and $V_{t,k}$ gives the probability of the
most probable sequence of states upto to time $t$.

My problem has the same setup except that the sequence of states
$s_1,\dots,s_n$ is known and I have to find the best permutation of a
set of $n$ observations $\{ o_1,\dots,o_n \}$. I instinctively surmised
that this problem must have a similar solution to the above recurrence
and I proceeded to derive one by letting $V_{t,k}$ stand for the
probability of the most probable permutation $\sigma \in S_n$ giving
the sequence of observations $o_{\sigma(1)},\dots,o_{\sigma(t)}$ upto
time $t$ where $\sigma(t)=k$. I'll refer to $o_{\sigma(i)}$ simply as
$\sigma(i)$. Here is the base case

$$
V_{1,k} \propto P(\sigma(1)=k | s_1)
$$

Note how I don't have to deal with the probabilities of states since
they are constant over all possible $\sigma$. I try to derive the
recursive case next

$$
V_{t,k} \propto \underset{\sigma \in S_n, \sigma(t)=k}{\max} \prod_{i=1}^{t} P(\sigma(i) | s_i) \\
\propto P(\sigma(t)=k | t) \underset{\sigma \in S_n, \sigma(t)=k}{\max} \prod_{i=1}^{t-1} P(\sigma(i) | s_i) \\
\propto P(\sigma(t)=k | t) \underset{r \ne k}{\max} \left[ \underset{\sigma \in S_n, \sigma(t)=k,\sigma(t-1)=r}{\max} \prod_{i=1}^{t-1} P(\sigma(i) | s_i) \right]
$$

But, then you hit this roadblock. The requirement of $\sigma(t)=k$
inside the brackets stops you from making it into the recursive call
$V_{t-1,r}$. After a couple of minutes of doodling, I decided to give
a name to this stumbling block. The base case

$$
V_{1,-k} \propto \underset{r \ne k}{\max} P(\sigma(1)=r|s_1)
$$

which gives the likelihood of the most probable observation at time
$t=1$ when the $k$-th observation is not considered. I tried to derive
$V_{t,-k}$ and hoped I to find the recursive step.

$$
V_{t,-k} \propto \underset{\sigma \in S_n,\sigma(\le t) \ne k}{\max} \prod_{i=1}^{t} P(\sigma(i) | s_i) \\
\propto \underset{\sigma \in S_n,\sigma(\le t) \ne k}{\max} P(\sigma(t)|s_t) \prod_{i=1}^{t-1} P(\sigma(i) | s_i) \\
\propto \underset{\sigma(t)=r \ne k}{\max} \left[ \underset{\sigma \in S_n,\sigma(\le t-1) \ne k}{\max} P(\sigma(t)=r|s_t) \prod_{i=1}^{t-1} P(\sigma(i) | s_i) \right] \\
\propto \underset{r \ne k}{\max} P(\sigma(t)=r | s_t) \cdot \underset{\sigma \in S_n,\sigma(\le t-1) \ne k}{\max} \prod_{i=1}^{t-1} P(\sigma(i) | s_i) \\
\propto \underset{r \ne k}{\max} P(\sigma(t)=r | s_t) V_{t-1,-k}
$$

There you have it; the recurrence for finding the best permutation of
observables when the state sequence is known (you can plug in the
state probabilites to make it an equality)

$$
V_{1,-k} \propto \underset{r \ne k}{\max} P(\sigma(1)=r | s_1) \\
V_{t,-k} \propto \underset{r \ne k}{\max} P(\sigma(t)=r | s_t) \cdot V_{t-1,-k}
$$

I was very satisfied that the symmetry of two problems gave rise to a
symmetry between the two solutions. If you find this problem/solution
in literature please post it in the comments.
