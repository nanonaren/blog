    [BLOpts]
    profile    = nanonaren
    postid     = 1047
    title      = "DSL for Generative Models - Interpreter (53/365)"
    tags       = daily, probability, Haskell
    categories =

In this post, I write some functions to interpret the
DSL. Specifically, I present some functions to figure out the children
and parents of a node and discover what the prior, observed, and
latent variables are.

> import Control.Monad (msum)
> import Data.Maybe (mapMaybe)
> import Data.List (nub,(\\))

Recapping the DSL.

> data Indexed a = Only a | a :@ [a]
> data Edge a = Indexed a :-> a
> type Network a = [Edge a]

Enumerating the names.

> names :: Eq a => Network a -> [a]
> names = nub . concatMap f
>   where f (Only a :-> b) = [a,b]
>         f ((p :@ _) :-> a) = [p, a]

Enumerating the children.

> children :: Eq a => Network a -> a -> [a]
> children xs a = concatMap f xs
>   where f (Only p :-> c) | p == a = [c]
>         f ((p :@ is) :-> c) | p == a || elem a is = [c]
>         f _ = []

Enumerating the parents.

> parents :: Eq a => Network a -> a -> [a]
> parents xs a = concatMap f xs
>   where f (Only p :-> c) | c == a = [p]
>         f ((p :@ _) :-> c) | c == a = [p]
>         f _ = []

Enumerating the observed variables.

> observed :: Eq a => Network a -> [a]
> observed n = filter (null . children n) . names $ n

Enumerating the priors.

> prior :: Eq a => Network a -> [a]
> prior n = filter (null . parents n) . names $ n

Enumerating the latent variables.

> latent :: Eq a => Network a -> [a]
> latent xs = names xs \\ (prior xs ++ observed xs)

Index of a random variable

> indexOf :: Eq a => Network a -> a -> Maybe [a]
> indexOf xs a = msum (map f xs)
>   where f ((p :@ is) :-> _) | p == a = Just is
>         f _ = Nothing

Running on the hmm example.

> data HMMLabels = Alpha | Beta | Transition
>                | Initial | Topic | Symbols | Symbol
>     deriving (Show,Eq)
> 
> hmm :: Network HMMLabels
> hmm =
>   [
>     Only Alpha                      :-> Transition
>   , Only Beta                       :-> Symbols
>   , (Transition :@ [Initial,Topic]) :-> Topic
>   , (Symbols :@ [Topic])            :-> Symbol
>   ]

    [ghci]
    observed hmm
    prior hmm
    latent hmm
    indexOf hmm Alpha
    indexOf hmm Transition
    indexOf hmm Symbols
    children hmm Alpha
    parents hmm Alpha
    children hmm Topic
    parents hmm Topic

Next time, I want to look at how to specify the distributions.
