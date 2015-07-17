    [BLOpts]
    profile    = nanonaren
    postid     = 263
    title      = "Starting Probabilistic Document Retrieval"
    tags       = statistics,modeling,documents,topic,retrieval
    categories = statistics,modeling,retrieval

I want to work through some papers on probabilistic document retrieval
mainly to find out the state of things in this area with regards to
the depth of infiltration of generative models in this domain. Note
that literature refers to document retrieval as 'ad-hoc retrieval' as
opposed to say 'passage retrieval' for retrieving parts of a document.

I'm starting with a paper from (2006) by Xing Wei et. al [1] that
introduces LDA to extend the basic language model for retrieval. The
basic model (often referred to as the *query likelihood* model)
evaluates each query term $q$ (multiple query terms are treated
independently) with respect to a document $d$ according to

$$
p_1(q|d) = \frac{N_d}{N_d + \mu} p^*(q|d) + \left( 1 - \frac{N_d}{N_d + \mu} \right) p^*(q)
$$

where $p^*(w|d) = \frac{N_{dw}}{N_d}$ is the empirical probability of
$w$ in document $d$; $p^*(w)$ is the empirical probability of $w$ in
the entire collection; and $\mu$ is a tunable Dirichlet prior to
prefer words in the document or to prefer words in the entire collection.

The main problem with this approach is when the query terms are words
that do not exist within the target documents. For instance, a user
might search for "daily planes from Chicago to New York" where
"planes" is an unusual term to use compared to the usual
"flights". The use of "planes" will cause problems with the query
likelihood model but we know that LDA, for instance, will tend to
group "planes" and "flights" under one topic and will not confuse it
much.

So, the authors augment the query likelihood model with LDA's judgment
of a query word given the topic mixture at document $d$ with
preference $(1-\lambda)$.

$$
p_2(q|d) = \lambda p_1(q|d) + (1-\lambda)\sum_z p(z|d)p(q|z)
$$

Though it introduces yet another tuning parameter, it's not much
effort to automate its inference using hill-climbing. The results show
consistent improvement over the query likelihood model. There's not
much more to say about this since many models have supplanted
this; I'll take a look at further models in the upcoming posts.

-------

[1] Xing Wei and W. Bruce Croft. 2006. *LDA-based document models for
ad-hoc retrieval*. Research and Development in Information Retrieval
(SIGIR).
