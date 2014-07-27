    [BLOpts]
    profile    = nanonaren
    title      = "(DRAFT) Performance: If you wondered when a difference list helps..."
    tags       = performance, dlist
    categories = haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/dlist.lhs)

In Haskell mythology, laziness is one of the Sirens: you can't but be
lured in by its raciness in the heat of learning Haskell; you are then
inevitably wrecked by it as your imperative sensibilites take hold;
and a year or so later you sit down to describe your adventure in a
Homerian epic. Laziness doesn't deserve this level of aggrandizing but
it's good fun talking about it to a functional programming initiate.

In this post, I'm going to look at one of the canonical lazy
structures in Haskell: the humble List `[a]`. I'm going to consider a
simple pattern one might encounter with lists and then improve it --
not by adding strictness but by employing an even more lazy structure:
the difference list `DList a`.

> module Main
>     (
>       main
>     ) where
>
> import Data.Monoid
> import Control.Monad (foldM)
> import Data.Foldable
> import Data.DList hiding (map,replicate)
> import Criterion.Main

The function of interest is the finite cartesian product of lists

$$ \prod_i^n L_i = \{ (l_1, \dots, l_n) | l_i \in L_i \} $$

For our purposes, rather than return a list of tuples, we'll fold the
tuple using a binary operator as follows, which we evaluate to
weak-head normal form.

> list_cartesian :: (a -> a -> a) -> a -> [[a]] -> [a]
> list_cartesian f s = loop
>     where loop (xs:xss) = xs >>= \x ->
>                           loop xss >>= \y ->
>                           return $! f x y
>           loop [] = return s

And it works as expected

    [ghci]
    list_cartesian (*) (1::Int) [[3,4],[2],[1,2]]

As with anything in Haskell, the temptation to generalize is
immense. So, here is a more general version -- the list is replaced by
a Monad.

> cartesian :: Monad m => (a -> a -> a) -> a -> [m a] -> m a
> cartesian f s = loop
>     where loop (xs:xss) = xs >>= \x ->
>                           loop xss >>= \y ->
>                           return $! f x y
>           loop [] = return s

As an example, consider evaluation under the `Maybe` monad

    [ghci]
    cartesian (*) (1::Int) [Just 20,Nothing,Just 10]
    cartesian (*) (1::Int) [Just 20,Just 3,Just 10]

We can also evaluate the list example using a difference list

    [ghci]
    cartesian (*) (1::Int) (map fromList [[3,4],[2],[1,2]])

Consider the following specialized functions to compute the sum of the
cartesian products using a regular list and a difference list.

> cart_list :: [[Double]] -> Double
> cart_list = foldl' (+) 0 . cartesian (*) 1
>
> cart_dlist :: [DList Double] -> Double
> cart_dlist = foldl' (+) 0 . cartesian (*) 1

Benchmark
---------

Should we expect any difference in performance? Well, first off, here
are the relevant function definitions

```
(>>=)   :: [a] -> (a -> [b]) -> [b]
m >>= k = foldr ((++) . k) [] m
```

On the other hand, a difference list and its concatenation function
`append` is defined as

```
newtype DList a = DL { unDL :: [a] -> [a] }

fromList    :: [a] -> DList a
fromList    = DL . (++)

toList      :: DList a -> [a]
toList      = ($[]) . unDL

append       :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

m >>= k = foldr (append . k) empty m
```

Structurally, the bind functions seem the same in both cases. But, now
for some quick benchmarking code.`

> main = defaultMain
>        [
>          bcompare
>          [
>            bench "list" $ whnf cart_list one
>          , bench "dlist" $ whnf cart_dlist (map fromList one)
>          ]
>        , bcompare
>          [
>            bench "list" $ whnf cart_list two
>          , bench "dlist" $ whnf cart_dlist (map fromList two)
>          ]
>        ]
>     where one = [[1..1000],[1..10],[1..100]]
>           two = replicate 6 [1..10]

The results show that dlist performs better and the performance gap
increases as the we cartesian product a greater number of lists or
with a greater number of elements in each list. Furthermore, the
memory consumption in dlist and list seems to be the same for this
application.

**TODO:** I'm not exactly sure why yet.

```
warming up
estimating clock resolution...
mean is 4.362846 us (160001 iterations)
found 1891 outliers among 159999 samples (1.2%)
  1362 (0.9%) high severe
estimating cost of a clock call...
mean is 1.253718 us (35 iterations)
found 2 outliers among 35 samples (5.7%)
  2 (5.7%) high mild

benchmarking list
collecting 100 samples, 1 iterations each, in estimated 5.593204 s
mean: 55.25133 ms, lb 55.21188 ms, ub 55.30506 ms, ci 0.950
std dev: 233.8865 us, lb 184.6386 us, ub 297.9634 us, ci 0.950

benchmarking dlist
mean: 39.95902 ms, lb 39.93537 ms, ub 39.98984 ms, ci 0.950
std dev: 137.4931 us, lb 110.2501 us, ub 189.0927 us, ci 0.950

benchmarking list
collecting 100 samples, 1 iterations each, in estimated 17.37580 s
mean: 180.6279 ms, lb 180.2802 ms, ub 181.0926 ms, ci 0.950
std dev: 2.041791 ms, lb 1.446918 ms, ub 2.850368 ms, ci 0.950

benchmarking dlist
collecting 100 samples, 1 iterations each, in estimated 13.90359 s
mean: 151.4917 ms, lb 151.0659 ms, ub 152.0473 ms, ci 0.950
std dev: 2.468668 ms, lb 2.020109 ms, ub 3.235238 ms, ci 0.950
found 3 outliers among 100 samples (3.0%)
  2 (2.0%) high mild
  1 (1.0%) high severe
variance introduced by outliers: 9.412%
variance is slightly inflated by outliers
```
