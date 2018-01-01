    [BLOpts]
    profile    = nanonaren
    postid     = 1007
    title      = "Measurable Spaces - Problem (44/365)"
    tags       = daily, probability
    categories =

Following on from the previous post, is the following a Borel set?

$$
\{ x \in R^\infty : \lim_n x_n > a \}
$$

This is a Borel set because we can intersect the set of [converging
sequences](https://nanonaren.wordpress.com/2016/10/26/measurable-spaces-problem-43365/)
and the set of sequences [bounded from
below](https://nanonaren.wordpress.com/2016/10/22/measurable-spaces-problem-42365/).

$$
\{ x \in R^\infty : \lim_n x_n > a \} = \{ x \in R^\infty : x_n \rightarrow \} \cap \{ x \in R^\infty : \sup_n \inf_{k \ge n} x_k > a \}
$$
