    [BLOpts]
    profile    = nanonaren
    title      = "Modeling atop a document representation"
    tags       = modeling, statistics
    categories = modeling

> -- started : Thu May 28 16:36:07 IST 2015
> -- finished: Fri May 29 22:54:09 IST 2015

The paper "DiscLDA: Discriminative Learning for Dimensionality
Reduction and Classification" [1] describes a model that not only
generates documents but learns them by associating each document with
a label. The discrimination of a document against its label is a
function of the generative representation of the document (i.e. the
topic mixtures). This is a theme of generative models that do more
than provide density estimates.

This paper interests me for two reasons. First, it describes a model
that combines two modes of learning: the generative and the
discriminative. It generates the documents whereas it only
discriminates the labels unlike the supervised LDA paper [2] that
generates both the documents and the responses. Second, as a result of
the combination of both modes of learning we get a slightly different
inference procedure that maximizes the conditional likelihood of
responses rather than the total likelihood of both the responses and
the documents.

The model works in the following manner

1. Each document is associated with a topic mixture $\mathbf{z}_{ij} \sim \theta_i \sim \text{Dirichlet}(\alpha)$. Same as LDA.

2. Given a label $y_i$ for document $i$, the topics are transfomed to
topics $\mathbf{u}_{ij} \sim T^y_{\mathbf{z}_{ij}}$. Thus, $T^y$ can
be viewed as a transformation matrix that linearly transforms the
document-level topic mixture $\mathbf{z}_i$ to a topic mixture
$\mathbf{u}_i$ that is dependent on the document label $y_i$. The key
being that this transformation is constrained by the same set of
shared word distributions $\Phi$.

3. Words are generated as $\mathbf{w}_{ij} \sim \Phi_{\mathbf{u}_{ij}}$

## Inference

The main part I want to look at is the learning of these
transformation matrices $\{ T^y \}$ for each label $y$. The inference
follows an EM procedure so we start by stating the conditional
log-likelihood (after introducing the hidden variables $\gamma$)

$$
p(\mathbf{y} | \mathbf{w},T,\Phi) = \prod_i \pi_{y_i} \prod_{w,z} \left( p(z) \sum_u T_{zu}^{y_i} \Phi_{uw} \right)^{V_{iw} \gamma_{iwz}} \\
\log p(\mathbf{y} | \mathbf{w},T,\Phi) = \sum_i \log \pi_{y_i} + \sum_{w,z} V_{iw} \gamma_{iwz} (\log p(z) + \log (\sum_u T_{zu}^{y_i} \Phi_{uw}) )
$$

where $V_{iw}$ is the number of $w$ words in document $i$ and $\pi$ is
the distribution over labels. Taking the derivative with respect to a
component of $T^y$ after adding lagrange multipliers we get the
$M$-step of the EM algorithm below (the paper solves this using Gradient descent)

$$
\frac{\partial \log p(\mathbf{y} | \mathbf{w},T,\Phi)} {\partial T_{zu}^y} =
  \sum_{i : y_i = y} \sum_{w,z} V_{iw} \gamma_{iwz}
  \frac{\Phi_{uw}}{ \sum_u T_{zu}^y \Phi_{uw} } - \lambda_z^y
$$

The $E$-step is given by

$$
\gamma_{iwz} = \frac{ \theta_{iz} \sum_u T_{zu}^{y_i} \Phi_{uw} }
                    { \sum_k \theta_{ik} \sum_u T_{ku}^{y_i} \Phi_{uw} }
$$

Here's comes the difference from a vanilla EM. To actually compute
this, we'll require samples of $\theta_i$ which can be taken from a
Gibbs sampling procedure that iterates between sampling
$\mathbf{z}_{ij}$ and $\theta_i$.

## Notes

It's interesting that we can make discriminative modifications to a
generative model. It would have been useful to see how this approach
compares with supervised LDA, which also supports non-discrete
document labels.

[1] Lacoste-Julien, Simon and Sha, Fei and Michael
I. Jordan. 2009. "DiscLDA: Discriminative Learning for Dimensionality
Reduction and Classification." *Advances in Neural Information
Processing Systems 21*.

[2] Jon D. Mcauliffe and David M. Blei. 2008. "Supervised Topic
Models." *Advances in Neural Information Processing Systems 20*.
