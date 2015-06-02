    [Blopts]
    postid     = 214
    profile    = nanonaren
    title      = "Modeling and Indexing"
    tags       = modeling, statistics
    categories = modeling

It has been well-tested in the real-world and is generally accepted
that simple models of indexing perform really well. They have no
problems scaling or dealing with gigantic vocabularies. The biggest
downside to them is that they can only match queries to documents if
the words in the query are also present in the document.

Many alternatives -- in the form of probabilistic indexing -- exist
but are not quite as mature or as predictable in its behavior as the
classical vector-space model. The paper "Polynomial Semantic
Indexing" [1] proposes a discriminative model to link queries and
documents trained in a supervised manner with inputs
$(q,d_{+},d_{-})$ where $q$ is a query, $d_{+}$ is a relevant document, and
$d_{-}$ is an irrelevant document.

The model in this paper combines the concept of the traditional
inner product between normalized word-count vectors to measure
similarity and the factorization method to provide an embedding of the
words in a smaller latent space. Ignoring the "polynomial" in the
title of the paper the basic model looks like

$$
f^2(q,d) = \sum_{i,j} W_{ij} q_i d_j
$$

where $W_{ij}$ is a matrix of weights specified over each pair of
words. Note how when $W = I$ (the identity matrix) the equation is
nothing but the standard inner product. To avoid the $W^2$ parameters
in the matrix $W$ the authors choose to represent $W$ as the product
of smaller matrices $U$ and $V$

$$
W_{ij} = (U^TV)_{ij} + I_{ij} = \sum_z U^T_{iz}V_{zj} + I_{ij} \\
f^2(q,d) = q^T (U^T V + I) d = \sum_{i=1}^N (Uq)_i (Vd)_i + q^T d
$$

The expression above shows clearly how the relevance of a query to a
document is a combination of the standard inner product and the
product of the latent space embedding of $q$ and $d$.

The authors then show that the model generalizes to more than just a
pairwise weighting. As in

$$
f^3(q,d) = \sum_{i,j,k} W_{ijk} q_i d_j d_k \\
= \sum_{i=1}^N (Uq)_i (Vd)_i (Yd)_i + f^2(q,d)
$$

## Inference

The training data consists of tuples of the form $(q,d+,d-)$ where
$d+$ and $d-$ are a relevant and a non-relevant document with respect
to the query $q$. The learning of $W$ should satisfy $f(q,d+) >
f(q,d-)$. The paper minimizes the margin ranking loss function through
stochastic gradient descent

$$
\Lambda = \sum_{(q,d_{+},d_{-})} \max(0, 1 - f(q,d_{+}) + f(q,d_{-}))
$$

The gradient with respect to matrix $V$ is derived below when $1 - f^2(q,d_{+}) + f^2(q,d_{-}) > 0$

$$
\frac{\partial \Lambda_{(q,d_{+},d_{-})}}{\partial V} = \frac{\partial}{\partial V} \left( 1 - f^2(q,d_{+}) + f^2(q,d_{-}) \right) \\
= \frac{\partial}{\partial V} \left( 1 - q^T U^T V d_{+} + q^T d_{+} + q^T U^T V d_{-} + q^T d_{-} \right) \\
= -q^T U^T d_{+} + q^T U^T d_{-} \\
= q^T U^T (d_{+} - d_{-}) \\
$$

And so, $V$ is updated (and similarly $U$) using a learning-rate (fixed in this paper) $\lambda$

$$
V \gets V + \lambda \left( q^T U^T (d_{+} - d_{-}) \right)
$$

## How is it tested?

The authors make use of Wikipedia articles and the links contained
therein to create test data that considers a document $d$ as relevant
to a document $q$ if there is a link to $d$ from $q$. Thus a test set
of documents and links is evaluated by considering a random document
$q$ and ranking the rest of the documents and seeing if linked
documents are ranked higher than others. A modified version of this
experiment is also undertaken where only a small set of random words
is selected from a query document to mimic a keyword search.

Another interesting application of this model is to cross-language
document retrieval. That is, query documents are taken from one
language and relevant documents are taken from another. In this case,
the authors pair a Japanese Wikipedia article (acting as the query
document) with its English counterpart or a document linked to this
one (acting as the relevant document) during training. Then, given a
Japanese query document $q_j$ that is the pair of the english document
$e_j$ we can evaluate if the model ranks documents linked to $e_j$
higher than those that are not linked to it.

[1] Bing Bai and Weston and others. 2009. "Polynomial Semantic
Indexing". *Advances in Neural Information Processing Systems 22*
