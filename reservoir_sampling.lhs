    [BLOpts]
    profile    = nanonaren
    postid     = 258
    title      = "Reservoir Sampling"
    tags       = statistics,sampling,reservoir
    categories = statistics

If you want to uniformly sample a handful of elements from a very
large stream of data you probably don't want to read it all into
memory first. It would be ideal if you could sample while streaming
the data. For this, *reservoir sampling* provides one answer. The algorithm is summarized thusly

1. Let $k$ be the number of samples required (without replacement), $R$ the array to hold the $k$
samples, and $S$ the stream of elements.
2. Fill $R$ with the first $k$ elements from $S$
3. for each $k+1 <= i <= |S|$, randomly sample $j \in [1,i]$, and set $R[j] = S[i]$ if $j \le k$.
4. The sample set is the final state of $R$

Can we show that this algorithm does indeed uniformly sample the
elements (without replacement), that is, the probability of sampling
any subset of $k$ elements is
$\frac{k!}{(n)_k} = \frac{k}{n}\frac{k-1}{n-1}\dots\frac{1}{n-k+1}$?

Suppose $k=1$, $|S|=n$, and $1 \le r \le n$. The probability of
picking $r$ is the probability of picking it at step $3$ when $i=r$
and then making sure in the later steps it doesn't get replaced by
another element

$$
\frac{1}{r} \times \prod_{r < s \le n} \frac{s-1}{s} \\
= \frac{1}{r} \times \frac{r}{n} \\
= \frac{1}{n}
$$

Thus, the sampling is uniform when $k=1$. In general, for sampling $k$
elements $r_1 < r_2 < \dots < r_k$ the probability due to step $3$ is

$$
\frac{k}{r_1}\frac{k-1}{r_2}\dots\frac{1}{r_k}
$$

That is the probability that each element is put into its own
slot. The probability that $r_1$ doesn't get overwritten by the
upcoming elements (except for $r_2,\dots,r_k$ which have already been
picked by step $3$) is

$$
\prod_{r_1 < s \le n-k+1} \frac{s-1}{s} = \frac{r_1}{n-k+1}
$$

Thus, the probability of selecting $r_1 < \dots < r_k$ according to
the algorithm is

$$
\frac{k}{r_1}\frac{k-1}{r_2}\dots\frac{1}{r_k} \times \frac{r_1}{n-k+1}\dots \frac{r_k}{n} = \frac{k!}{(n)_k}
$$
