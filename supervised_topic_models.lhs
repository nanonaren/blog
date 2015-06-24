    [BLOpts]
    profile    = nanonaren
    postid     = 247
    title      = "Regression-guided Generative Models"
    tags       = modeling,statistics,lda,regression
    categories = modeling,statistics

A generative model is pretty pointless on its own unless the
generative structure itself holds intrinsic interest. Hence, papers
justify their generative models either by comparing its predictive
performance against another model or by extending the model to
accommodate for standard machine learning tasks of dimensionality
reduction, prediction, or classification.

A prime example of this is the LDA paper that evaluates the model's
usefulness for classification and collaborative filtering in addition
to comparing its performance against its ancestor -- the PLSA model --
whose intention was to use the latent variables for indexing
documents. These tasks were performed without modification to the
generative model because they only required the evaluation of
probability densities such as $p(\mathbf{w})$ for document density,
$p(\mathbf{q} | \mathbf{w}_d)$ for query relevance against a document,
and $p(w | \mathbf{w})$ for recommending an item given a set of items.

## Lack of supervision

It was realized early on that supervision would be required to get the
most out of a generative model. Consider the case of clustering a set
of points where we let a Gaussian mixture model group together points
into $k$ clusters. The entire idea is that we hope for the clusters of
points to have some significance to us. It's quite unlikely in most
non-trivial circumstances for the clusters to make complete sense to
us after a run of the GMM.

So, a logical modification of clustering comes about when the sample
points $X_i$ (the independent variable) come along with accompanying
points $Y_i$ (the dependent variable). While we may just get lucky and
find that the clusters perfectly partitions $X_i$ such that each
cluster contains a disjoint subset of the $Y$s, we can't expect this
to happen unaided. Thus, the way to proceed is to treat $Y_i$ as
constraints and to find clusters that do their *best* to encompass a
disjoint subset of $Y$s.

What I want to look at is one way [1] the LDA model can be augmented to
learn topics with the help of document labels such as categories,
ratings, or other information.

## Regression

The independent variables are taken to be the topic-count vector $z_d$
for each document $d$ and the dependent variable are ordered user
ratings (such as a score from 1 to 10) $y_d \in \mathbf{R}$. The hunch
is that the ratings change linearly with respect to topic counts,
i.e, two different ratings are differentiated by a linear change in
the topic-counts of the corresponding documents with possible
variations entertained by a Gaussian noise.

The independent variables are the normalized topic counts

$$
\bar{z}_d := \frac{1}{N} \sum_{n=1}^{N} z_{dn}
$$

and the ratings $y_d$ are modeled as values drawn from a Gaussian
distribution with mean given by a *linear combination* via $\eta$ of the above topic counts

$$
y_d \sim \text{N}(\eta^T \bar{z}_d, \sigma^2)
$$

To keep this post short, I'll postpone the inference discussion to
another post, which will also give me a chance to walk through
variational inference.

## Generalization

What if we want to entertain other responses such as a binomial or
categorical response? For this, we employ the Generalized Linear Model
(GLM). Note first the salient components of the Gaussian model above:

1. The mean $\mu$ is linearly determined as $\eta^T \bar{z}_d$. This
linear combination is also called the *linear predictor* in the paper.
2. The dispersion (variance) parameter $\sigma^2$ controls how much the
response varies from the linearly determined $\mu$

The GLM defines the probability of the dependent variable as

$$
p(y | \zeta,\delta) = h(y,\delta) \exp \left( \frac{\zeta y - A(\zeta)}{\delta} \right)
$$

where $\zeta$ is the natural parameter, $\delta$ the dispersion
parameter, $h(y,\delta)$ is the base measure and $A(\zeta)$ is the
log-normalizer. Let's see if the normal distribution fits this form

$$
p(y | \eta,\bar{z},\sigma^2) = \frac{1}{\sqrt{2\pi}\sigma} \exp \left( \frac{(y - \eta^T \bar{z})^2}{2\sigma^2} \right) \\
= \frac{1}{\sqrt{2\pi}\sigma} \exp \left( \frac{y^2 - 2y\eta^T \bar{z} + (\eta^T \bar{z})^2}{2\sigma^2} \right) \\
= \frac{1}{\sqrt{2\pi}\sigma} \exp \left( \frac{-y^2}{2\sigma^2} \right)
                              \exp \left( \frac{2y\eta^T \bar{z} - (\eta^T \bar{z})^2}{2\sigma^2} \right) \\
= \frac{1}{\sqrt{2\pi}\sigma} \exp \left( \frac{-y^2}{2\sigma^2} \right)
                              \exp \left( \frac{\eta^T \bar{z}y - (\eta^T \bar{z})^2/y}{\sigma^2} \right)
$$

This fits the bill with $h(y,\sigma^2) = \frac{1}{\sqrt{2\pi}\sigma} \exp \left( \frac{-y^2}{2\sigma^2} \right)$, $\zeta = \eta^T \bar{z}$, $\delta = \sigma^2$,
and $A(\zeta) = \frac{\zeta^2}{2}$. In the next post I'll look at how
other response types fit into the Generalized Linear Model format.

--------

[1] Jon D. Mcauliffe and David M. Blei. 2008. "Supervised Topic
Models." *Advances in Neural Information Processing Systems 20*
