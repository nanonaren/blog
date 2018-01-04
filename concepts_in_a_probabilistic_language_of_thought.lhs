    [BLOpts]
    profile    = nanonaren
    postid     = 1137
    title      = "Paper: Concepts in a Probabilistic Language of Thought (64/365)"
    tags       = daily, language, nlp
    categories = [Papers]

In a [previous post](https://nanonaren.wordpress.com/2018/01/03/paper-attention-is-all-you-need-63-365/), I described a paper that wrote
questions as a LISP expression. The authors of that paper take their
input from today's paper by Goodman et. al. I think it's clear to
everyone that concepts, as humans use them, is highly
compositional. We can use them abstractly, concretely,
suppositionally, and in every way imaginable. It's interesting to
ponder whether this is the reason why our languages are so flexible or
whether languague's flexibility has made our thought so flexible.

So, the first claim of this paper is that "concepts have a
language-like compositionality and encode probabilistic
knowledge. These features allow them to be extended productively to
new situations and support flexible reasoning and learning by
probabilistic inference." This is fairly uncontroversial.

What the authors consider then is a formal system capable of
expressing the same. Their suggestion is a probabilistic programming
language called *Church*. The develop various helper constructs in
this language that help with capturing 'randomness', 'conditionality',
and 'queries'. There isn't must to show about these programs, as they
are just regular programs. Compositionality is obvious in that
functions can be used and extended. Uncertainty is encoded by impure
functions which return sampled values. But these functions are
memoized so that when called by the same input the same answer is
returned.

The paper doesn't introduce anything computational but I'll bring this
back when I find papers that develop this concept computationally.
