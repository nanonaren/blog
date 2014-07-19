    [BLOpts]
    profile    = nanonaren
    postid     = 1
    title      = "Prototype: Parallel execution of Sinks in Conduit"
    tags       = conduit, concurrency
    categories = haskell

Introduction
============

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/prototype_parallel_sink.lhs)

The Conduit package is a good choice for applications of Bayesian modeling.
In particular, I've used it in the implementation of the inference of
generative models like LDA, HDP, CTM and so forth starting from reading the
data file to performing several sampling passes for each random variable.
And, as I mostly use strict or unboxed data structures, the implementations
all run in constant space.

One may choose from several methods of inference and I often go with
Gibbs sampling, which gives one the option to integrate out (or not)
the parameters - often called collapsing. When certain parameters are not
collapsed, the option to sample a random variable in parallel arises.
So, having done all the work in creating sinks for sampling, the option
to parallelize them becomes an extra burden.

Of course, such parallel capability in useful in other situations
involving data processing. So, it would be nice to have a function
that can take a source and supply input to a list of sinks as they
demand for it in parallel. In this post, I've extracted a clean prototype
from my hacky attempts to do this in the past.

Get the imports out of the way.

> module Main
>     (
>       main
>     ) where
>
> import Prelude hiding (mapM_)
> import Data.Foldable (mapM_)
> import Data.Conduit
> import qualified Data.Conduit.List as L
> import Control.Monad (zipWithM,void)
> import Control.Monad.Trans (lift)
> import Control.Concurrent hiding (yield)
> import Control.Concurrent.MVar
> import Control.Concurrent.Chan
> import qualified Data.IntMap.Strict as I
> import System.IO
>
> main = do
>   hSetBuffering stdout LineBuffering

For now, I'll be concerned with taking a source with IO as the underlying monad

>   let src = L.sourceList [1..50::Int]

and a list of sinks to do some work. In this case, show the sequence of
(first 20) values produced when the Collatz algorithm is run

>       collatz n | n == 1    = 1
>                 | even n    = n `div` 2
>                 | otherwise = 3*n + 1
>       collatzSeq n | n == 1    = [1]
>                    | otherwise = n : collatzSeq (collatz n)

To test early termination of a sink, we'll write two sinks: one that
indefinitely process numbers and one that terminates after processing just three

>       sinkI k = L.mapM_ (putStrLn . (k ++) . (": " ++) .
>                          show . take 20 . collatzSeq)
>       sinkE k = L.isolate 3 =$ sinkI k

We've now written the sinks with no foresight of parallelism but I want to
run them in parallel and let the input be divided among them.

>   src `link` [sinkI "1",sinkI "2",sinkE "3"]

Method
======

Since this is a prototype, I decided to implement this without using
Data.Conduit.Internal. All I do is decouple the semantics of

1. a sink requesting input $\rightarrow$ places request in a common channel \
   a sink receiving input  $\rightarrow$ waits on a MVar after requesting
2. a source satisfying request $\rightarrow$ reads request from channel \
   a source has output         $\rightarrow$ places output in MVar matching request

The Detached Sink
-----------------

We tackle the Sink first: take the given sink, apply a mediating source, 
and run in a new thread.

> detachedSink :: Int              -- sink ID
>              -> Chan Int         -- channel to place request
>              -> MVar (Maybe a)   -- input
>              -> Sink a IO ()     -- the sink
>              -> IO ()
> detachedSink i chan var snk = void . forkIO $ do
>   src $$ snk

kill self after completion

>   myThreadId >>= killThread

create a source to mediate the sink's requests

>     where src = do

place request for input in Chan by writing my ID

>             lift (writeChan chan i)

takeMVar acts just like await

>             maybea <- lift (takeMVar var)
>             case maybea of
>               Nothing -> return ()
>               Just a -> yield a >> src

This completes the sink.

The Detached Source
-------------------

Next, take the source, the request channel, the response channels, and
apply a mediating sink. Runs in the current thread.

> detachedSource :: Chan Int                   -- request channel
>                -> I.IntMap (MVar (Maybe a))  -- sink ID -> mvar
>                -> Source IO a                -- the source
>                -> IO ()
> detachedSource chan dmap src = src $$ snk
>     where snk = do

read request and identify response channel

>             var <- lift . fmap (dmap I.!) . readChan $ chan

pass request upstream

>             maybeVal <- await
>             case maybeVal of

when no more input, signal completion to all response channels

>               Nothing -> lift $ mapM_ (flip putMVar Nothing) dmap

otherwise, provide the output

>               Just a -> lift (putMVar var (Just a)) >>
>                         snk

This completes the source.

The Link
--------

We can now write the function we want.

> link :: Show a => Source IO a -> [Sink a IO ()] -> IO ()
> link src snks = do

create request and response channels and detached sinks

>   chan <- newChan
>   let f i snk = do
>         var <- newEmptyMVar
>         detachedSink i chan var snk
>         return (i,var)
>   destMap <- fmap I.fromList . zipWithM f [0..] $ snks

and the detachedSource

>   detachedSource chan destMap src

Sample Output
-------------

Compile as

ghc --make -o test -O -threaded Test.lhs

and run as

./test +RTS -N3

1: [1] \
3: [3,10,5,16,8,4,2,1] \
2: [2,1] \
1: [4,2,1] \
3: [5,16,8,4,2,1] \
2: [6,3,10,5,16,8,4,2,1] \
1: [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
2: [9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
3: [8,4,2,1] \
1: [10,5,16,8,4,2,1] \
2: [11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
1: [12,6,3,10,5,16,8,4,2,1] \
2: [13,40,20,10,5,16,8,4,2,1] \
1: [14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
1: [16,8,4,2,1] \
2: [15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1] \
2: [18,9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2] \
1: [17,52,26,13,40,20,10,5,16,8,4,2,1] \
2: [19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2] \
1: [20,10,5,16,8,4,2,1] \
2: [21,64,32,16,8,4,2,1] \
1: [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
2: [23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1] \
1: [24,12,6,3,10,5,16,8,4,2,1] \
2: [25,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16] \
1: [26,13,40,20,10,5,16,8,4,2,1] \
2: [27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91] \
1: [28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
2: [29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
1: [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1] \
2: [31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103] \
1: [32,16,8,4,2,1] \
2: [33,100,50,25,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20] \
1: [34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
2: [35,106,53,160,80,40,20,10,5,16,8,4,2,1] \
1: [36,18,9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4] \
2: [37,112,56,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4] \
1: [38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4] \
2: [39,118,59,178,89,268,134,67,202,101,304,152,76,38,19,58,29,88,44,22] \
1: [40,20,10,5,16,8,4,2,1] \
2: [41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137] \
1: [42,21,64,32,16,8,4,2,1] \
2: [43,130,65,196,98,49,148,74,37,112,56,28,14,7,22,11,34,17,52,26] \
1: [44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
2: [45,136,68,34,17,52,26,13,40,20,10,5,16,8,4,2,1] \
1: [46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1] \
2: [47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155] \
1: [48,24,12,6,3,10,5,16,8,4,2,1] \
2: [49,148,74,37,112,56,28,14,7,22,11,34,17,52,26,13,40,20,10,5] \
1: [50,25,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5]
