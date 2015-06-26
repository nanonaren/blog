    [BLOpts]
    profile    = nanonaren
    postid     = 241
    title      = "Topic Coherence"
    tags       = statistics,modeling,topic,knowledge
    categories = statistics,modeling

Evaluating unsupervised topic models is tricky business. If the
resulting model is not employed in retrieval, classification, or
regression there really is no way of convincing someone of the
model's worth. You may, rightly, say that there is no use for an
unsupervised model without one of these objectives and that the
*unsupervised* soubriquet serves only to distinguish it from a model
whose optimization procedure includes supervision in the form of
labels or outputs. The only way a generative model will have a meaning
of its own is if there is a natural or physical interpretation of the
generative process itself picked out by its inference over the given
samples.

Nevertheless, people try to evaluate unsupervised generative models
without labels of some kind. A popular method is to hold-out a portion
of the data for testing and to compute its log-likelihood $\log
p(\mathbf{w})$ (also called predictive log-likelihood) by integrating
out latent variables. While this works for any probabilistic model, a
slightly different metric is employed called predictive-perlexity for
textual models

$$
perplexity(\mathbf{w}^{\text{test}}) =
  \exp \left( - \frac{\sum_{d=1}^M \log p(\mathbf{w}^{\text{text}}_d) }
                   {\sum_{d=1}^M N_d}
       \right)
$$

where $N_d$ is the number of words in document $d$.

## Topic Coherence

The worth of this metric with respect to human interpretations of text
is contested [2]. The authors Mimno et. al [2] suggest an alternative
method of evaluation called *topic coherence*. The idea here is to
prefer topics whose most frequent words appear, in general, together
than apart. Meaning, a topic shouldn't consist of many separate
central words around which a host of supporting words are found;
instead, a topic should consist of one coherent set of words. You can
imagine this as being able to give a single solid name for each topic
rather than having to choose from many ambiguous possibilities. The
evaluation is expressed as follows

$$
C(t,V^{(t)}) = \sum_{m=2}^M \sum_{l=1}^{m-1} \log \frac{D(v^t_m, v^t_l)+1}{D(v^{t}_l)}
$$

where $V^t = (v^t_1,\dots, v^t_M)$ is a list of the $M$ most probable
words in topic $t$. Exponentiating this we find that is is computing
the empirical joint probability of all the pairwise combinations of
the top $M$ words in a topic $t$.

## Example

Recent work has focused on improving topic models in this regard with
the addition of external knowledge about textual behavior such as
knowing which words can go together. The authors Chen et. al [1]
consider improving topics by running topic models over several domains
and then running a frequent item-set miner to find commonalities
between the domains to iteratively enhance the topics. Like [1] they
make use of the *Generalized Polya urn model* which I'll explore in a
another post.

I am not too taken in by the model but the key thing is that the
models are evaluated using average topic coherence over held-out data
and show an improvement over LDA and other knowledge-based models.

-------

[1] Zhiyuan Chen and Bing Lui. 2014. "Topic Modeling using Topics from
Many Domains, Lifelong Learning and Big Data". *International
Conference on Machine Learning*

[2] David M. Mimno and others. 2011. "Optimizing Semantic Coherence in
Topic Models". *Empirical Methods in Natural Language Processing*
