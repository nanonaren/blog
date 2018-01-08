    [BLOpts]
    profile    = nanonaren
    postid     = 1151
    title      = "Paper: A Generative Model for Parsing Natural Language to Meaning Representations (67/365)"
    tags       = daily, language, nlp
    categories = [Papers]

Today's paper once again constructs a semantic parser. The parser is
trained on sentences paired with their meaning representations. But
there is no finer labeling of the correspondence between words and
meaning tokens.

The meaning representation in this paper takes the form of a tree
whose nodes have both natural language words and meaning
representation tokens. They say that the meaning representation is
variable-free but I have to trace another reference to see what that
precisely means (for later). An example meaning representation is shown below

![Meaning representation as a tree](./meaning_rep.png)

Every node in the tree takes on the following form where $X_1$ to
$X_k$ are also semantic cateogries. Some examples are 'River:
largest(River)' and 'Num: count(State)'.

$$
\text{semantic category} \rightarrow \text{function}(X_1, \dots, X_k)
$$

A hybrid tree is an extension of this tree that captures both the
sentence and the meaning representation. The only difference is that
every node can also emit NL tokens. The leaves of an MR are always NL
tokens. Generation of a tree is viewed as a Markov process where 1) we
start with a root production, 2) and we recursively expand its
parameters, 3) at each node we can emit NL tokens.

Unlike in a PCFG parsing task where the correspondence between NL
words and syntactic structures is available, the current model does
not have access to this data. Thus, we need to compute the expected
parameters from all possible tree derivations. The authors adapt the
inside-outside algorithm for this purpose. I won't go into the details
at this point. Again, I'll wait to link it with other papers.

I kind of lost interest towards the end because there is an extra
re-ranking phase after finding the most likely hybrid-tree for a given
sentence. Anyway, as this is a paper is pretty old (2008), let's see
what more recent papers do.

