    [BLOpts]
    profile    = nanonaren
    postid     = 1032
    title      = "DSL for Generative Models (51/365)"
    tags       = daily, probability, Haskell
    categories =

The backlog becomes longer. I've changed jobs two weeks ago and it has
upset my routine. No matter. Here we go again. I want to deviate from
my problem solving mode for a while and use up a few posts to develop
a little library for inference and use of probabilistic models. Why?

I've spent a lot of time coding generative models from scratch and
it's repetitive and painful and error-prone and my current job has put
me back in the thick of machine learning research and I hope I'll get
to use this. The problem with coding models from scratch is keeping
track of the distributions and carefully constructing the conditional
probabilities for each latent variable that needs to be sampled. For
some reason, I wasn't keen on using existing libraries and I wanted to
have a go at making one myself.

After many false starts, I decided that I'd first write up a DSL that
can be used to describe the generative model in the way it's usually
represented by the [plate
notation](https://en.wikipedia.org/wiki/Plate_notation). The key to
the plate notation is that it makes it easy to represent indexed
distributions on top of the underlying bayesian network constructed by
drawing nodes and edges.

I'll keep the Hidden Markov Model as a running example. First, the
user defines his own type that provides names for the various random
variables.

> data HMMLabels = Alpha | Beta | Transition
>                | Initial | Topic | Symbols | Symbol

The library now needs to provide a way to define the generative model
on top of this. As a first step, we need to be able to define the
plates; that is, to tell when a name is indexed by another name. In
the case of the HMM, the symbol distributions are indexed by a topic
and the topic distributions is either initial or is indexed by a
topic.

Suppose the library provides the following

> data Indexed a = Only a | a :@ [a]

Then we can write

> -- Symbols :@ [Topic]
> -- Transition :@ [Initial,Topic]

And we can also define variables that stand on their own

> -- Only Alpha
> -- Only Beta

Next, is to allow the edges to be defined. Suppose we provide

> data Edge a = Indexed a :-> a
> type Network a = [Edge a]

The whole network can now be defined

> hmm :: Network HMMLabels
> hmm =
>   [
>     Only Alpha                      :-> Transition
>   , Only Beta                       :-> Symbols
>   , (Transition :@ [Initial,Topic]) :-> Topic
>   , (Symbols :@ [Topic])            :-> Symbol
>   ]

Next time, I'll try to define a couple more models with this language
to see if I am on the right track and then start writing an
interpreter.
