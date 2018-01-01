    [BLOpts]
    profile    = nanonaren
    postid     = 1045
    title      = "DSL for Generative Models - Examples (52/365)"
    tags       = daily, probability, Haskell
    categories =

In the [previous
post](https://nanonaren.wordpress.com/2016/11/12/dsl-for-generative-models-51365/)
I attempted to introduce a DSL for probabilistic models inspired by
the plate notation. Let's try to see if we can define LDA with it.

> data LDALabels = Alpha | Beta | Topics | Topic
>                | Doc | Symbols | Symbol
> lda :: Network LDALabels
> lda =
>   [
>     Only Alpha :-> Docs
>   , Only Beta :-> Symbols
>   , (Topics :@ Doc) :-> Topic
>   , (Symbols :@ Topic) :-> Symbol
>   ]

Here is a topic model where topics are arranged in nodes of a fixed
binary tree for each document. Let's say the tree has depth $d$, then
the distribution is parameterized by a `TopicPath` distribution (to
select a leaf) and a `TopicDepth` distribution (to select a node along
the path).

> data LDATreeLabels =
>     Alpha1 | Alpha2 | Beta
>   | TopicDepth | TopicPath | Topic | Doc
>   | Symbols | Symbol
>
> ldaTree :: Network LDATreeLabels
> ldaTree =
>   [
>     Only Alpha1         :-> TopicDepth
>   , Only Alpha2         :-> TopicPath
>   , Only Beta           :-> Symbols
>   , (TopicPath :@ Doc)  :-> Topic
>   , (TopicDepth :@ Doc) :-> Topic
>   , (Symbols :@ Topic)  :-> Symbol
>   ]

I think it looks pretty good so far. Let's see how it up once I start
interpreting the DSL.
