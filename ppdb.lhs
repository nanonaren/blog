    [BLOpts]
    profile    = nanonaren
    postid     = 1143
    title      = "Paper: PPDB: The Paraphrase Database (66/365)"
    tags       = daily, language, nlp
    categories = [Papers]

In the [last
post](https://nanonaren.wordpress.com/2018/01/06/paper-building-a-semantic-parser-overnight-65-365/),
the authors made use of paraphrases. It turns out that there is in
fact a [paraphrase database](paraphrase.org) and it's quite
interesting how it was created. It starts with the basic observation
that given translated texts from english to some foreign language, if
two english phrases $e_1$ and $e_2$ translate to the same foreign
phrase $f$ then we may assume that $e_1$ and $e_2$ have similar
meaning; i.e., that they paraphrase eachother.

The paper goes a little further than this. The goal is to extract a paraphrase rule as follows

$$
\mathbf{r}_p : = C \rightarrow \langle e_1, e_2, \sim_p, \phi_p \rangle
$$

where $C$ is a non-terminal, $e_1$ and $e_2$ are mix of terminal and
non-terminal symbols where both share the same set of non-terminal
symbols (given by the correspondence $~_p$, and a feature vector
$\phi_p$.

Such a rule is construction from a syntactic machine translator, where
two applied translation rules having the same syntactic construct and
foreign phrase are matched

$$
\mathbf{r}_ 1 : = C \rightarrow \langle f, e_1, \sim_1, \phi_ 1 \rangle \\
\mathbf{r}_ 2 : = C \rightarrow \langle f, e_2, \sim_2, \phi_ 2 \rangle
$$

where once again $f$ and $e$ share the same non-terminals so that the
above rule for pairing $e_1$ and $e_2$ can be constructed. The paper
also defines a way to combine the feature vectors but I will skip that
here.
