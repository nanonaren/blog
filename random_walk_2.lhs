    [BLOpts]
    profile    = nanonaren
    postid     = 921
    title      = "Random Walk - Reading And Recall (34/365)"
    tags       = daily, probability, haskell
    categories =

I said I wanted some awkward examples of random walks. After trying to
shoehorn situations into a $+1$ or $-1$ movement, I think I have
something.

Consider the reading of a book. It's an activity that proceeds in
sequence as we read one word after another from left to right. Let
$w_1, \dots, w_n$ be the sequence of words in a book. Let's say that
there are two actions we can take when we encounter a word $w_i$.

1. $w_i$ is a new word that we have to **learn**: $\omega_i = +1$
2. $w_i$ is a word we already know that we can just **recall**: $\omega_i = -1$

What remains to figure out is what do we mean by 'know the
word'. Let's get to this slowly. For now, consider the simplest form
of memory. Let's say that memory is a set $S$ to which we add an
unknown word when **learning** and then remove a known word when
**recalling**. I'll end this post with some code and plots.

Let's start with a typeclass for a memory model (for learning and
recalling) that we can use again later. The `learn` method updates the
model with an entry and the `recall` method returns a new model and
also returns `True` if the given `a` was recalled.

> {-# LANGUAGE BangPatterns #-}
>
> import Data.Hashable
> import qualified Data.HashSet as HS
> import Data.Char
>
> class Mem m where
>   recall :: (Eq a, Hashable a) => m a -> a -> (m a, Bool)
>   learn :: (Eq a, Hashable a) => a -> m a -> m a

We create an instance for the simple model I described above.

> newtype SimpleMem a = SimpleMem (HS.HashSet a)
>
> instance Mem SimpleMem where
>   recall (SimpleMem mem) a | HS.member a mem = (SimpleMem (HS.delete a mem), True)
>                            | otherwise = (SimpleMem mem, False)
>   learn a (SimpleMem mem) = SimpleMem (HS.insert a mem)

Given a sequence of words we will now read it left to right and then
label it $+1$ if we need to learn the word and $-1$ if we are
recalling it.

> walk :: (Mem m, Eq a, Hashable a) => m a -> [a] -> [Int]
> walk initial = go initial
>   where go _ [] = []
>         go !mem (a:as) =
>           case recall mem a of
>             (mem', False) -> 1 : go (learn a mem') as
>             (mem', True) -> (-1) : go mem' as

Finally, let's have a simple way to read a text file. We won't bother
with stemming and all that.

> readTextFile :: String -> IO [String]
> readTextFile fp = readFile fp >>= return . words . map clean
>   where clean c | isLetter c = toLower c
>                 | isMark c = c
>                 | otherwise = ' '

    [ghci]
    rs <- readTextFile "frankenstein.txt" >>= return . walk (SimpleMem HS.empty)
    take 50 rs
    take 50 $ scanl1 (+) rs

For now, I leave you with a plot of the walk on *frankenstein.txt*
and *pride_and_prejudice.txt*. Already note something curious

1. Number of words in *frankenstein.txt* is $78329$. Value of sum of
random variables is $4741$.

2. Number of words in *pride_and_prejudice.txt* is $125879$. Value of
sum of random variables is $4151$.

![Example walks on two books](frankenstein_pride_prejudice_plot.png)
