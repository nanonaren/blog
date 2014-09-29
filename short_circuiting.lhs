    [BLOpts]
    postid     = 116
    profile    = nanonaren
    title      = "Short-circuiting"
    tags       = pattern, monad
    categories = haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/short_circuiting.lhs)

Short-circuiting in imperative languages is a doddle -- just put a
`break;` to get out of a loop. Having said that, things are trickier
when you want to break out into various levels within nested
loops. Gabriel Gonzalez, [here](http://www.haskellforall.com/2012/07/breaking-from-loop.html), explains how short-circuiting is easy in
Haskell and moreover short-circuiting didn't have to be built into
Haskell to achieve it. Have a read through it before continuing.

What do I need to write this post for then? If you've been following
any of the previous posts you'll note a persistent performance-harping
theme. This will be a concern here too but only after I explore a
general solution for jumping out of nested loops with great
ease. After which you can go off and work out solutions in other
languages or even come up with another solution in Haskell.

A quick review of short-circuiting
----------------------------------

> {-# LANGUAGE BangPatterns #-}
> module Main (main) where
>
> import Debug.Trace
> import Criterion.Main
> import Control.Monad
> import Control.Monad.Cont
> import Control.Monad.Identity
> import Control.Monad.Trans.Either
> import Data.List
> import Data.Array.ST
> import Data.Array.Base (unsafeWrite)
> import Control.Monad.ST
> import Data.STRef

Suppose we have a simple loop to sum up values in a list

> sum' :: [Double] -> Double
> sum' = foldl' (+) 0

and we decide to conduct this summation only while the sum remains
below $n$, we can write it idiomatically as Gabriel suggests using `EitherT`

> -- | just adding a strict fold
> foldM'             :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
> foldM' _ !a []      =  return a
> foldM' f !a (x:xs)  =  f a x >>= \fax -> foldM' f fax xs
>
> sumEither :: Double -> [Double] -> Double
> sumEither n = either id id . runIdentity . runEitherT .
>     foldM' (\acc x -> let acc' = acc+x
>                       in if acc' > n then left acc
>                          else return acc') 0

You can see that the change is quite minimal and, more importantly,
retains the `fold` abstraction. As mentioned in Gabriel's post, using the
continuation monad just for this simple shorting is unnecessary --
although, it is just as simple.

> sumCont :: Double -> [Double] -> Double
> sumCont n xs = flip runCont id . callCC $ \exit ->
>     foldM' (\acc x -> let acc' = acc+x
>                       in if acc' > n then exit acc
>                          else return acc') 0 xs

    [ghci]
    sum' [1..100]
    sumEither 100 [1..100]
    sumCont 100 [1..100]

Even with a great face, there is always a "but(t)". The following
benchmark shows why.

> bill = 1000000000
> benchSum = [ bench "prim" $ nf (sumPrim bill) [1..bill]
>            , bench "eitherPrim" $ nf (sumEitherPrim bill) [1..bill]
>            , bench "either" $ nf (sumEither bill) [1..bill]
>            , bench "cont" $ nf (sumCont bill) [1..bill]
>            ]
>
> sumPrim :: Double -> [Double] -> Double
> sumPrim n = loop 0
>     where loop !acc (x:xs) = let acc' = acc+x
>                              in if acc' > n then acc
>                                 else loop acc' xs
>           loop acc [] = acc
>
> -- at the cost of avoiding the fold abstraction
> sumEitherPrim :: Double -> [Double] -> Double
> sumEitherPrim n xs = either id id . runIdentity . runEitherT $
>     let loop !acc (x:xs) = let acc' = acc+x
>                            in if acc' > n then left acc
>                               else loop (acc+x) xs
>         loop acc [] = return acc
>     in loop 0 xs

```
benchmarking prim
mean: 657.3056 us, lb 650.1865 us, ub 670.2494 us, ci 0.950

benchmarking eitherPrim
mean: 692.7036 us, lb 685.9469 us, ub 703.1273 us, ci 0.950

benchmarking either
mean: 1.378055 ms, lb 1.368699 ms, ub 1.392701 ms, ci 0.950

benchmarking cont
mean: 4.097969 ms, lb 4.090660 ms, ub 4.108388 ms, ci 0.950
```

The good news is that using `EitherT` is faster than using
`ContT` and is only acceptably slower than `shortPrim`. The bad news is
the difference between `sumEither` and `sumEitherPrim` due to the
overloading of `foldM'`. I'll save the
discussion and solution of this problem for another post.

Shorting out of dynamic nesting
-------------------------------

What if we had to deal with jumping out of nested loops where new
nesting is created dynamically. This is not as unrealistic as it
sounds because everyone has heard of dynamic nesting in the form of
the nesting of function calls where the caller invokes a function
which goes on top of the stack and then unwinds one step at a
time. Now consider slightly generalizing it where we may directly
unwind to anyone in the stack and not just the immediate ancestor.

Here is a general simulation of it as a game played on a
tree. You start at the first child of the root. Let's call it level 1 and

1. traverse children from left to right of level $k$
2. at each child an oracle $O(k)$ either directs you to the
   first child of the current node or sends you back to the node at
   one of level $j<k$ that you came from where you continue the left-right
   traversal from the child you returned to.
3. you play this game till you reach the last level $n$ and report the
   route taken (if any).

I declare a simple concrete version of this game where

1. Each node is an integer
2. $O(k)(x) = x \text{ mod } k$ where $0$ means goto the first child and
   otherwise go $x \text{ mod } k$ levels back

The tree itself is dynamically defined by a list `[[Int]]` where
children are determined by multiplying itself with the parent. For
example, the list `[[1,2],[1,4]]` and the list `[[1,3],[3,5]]` defines the trees

![Example](./short_circuiting_fig1.png)

where the solution on the left is the route given by second child in level 1 and
the first child in level 2 while the second tree has no solution.

Coding this with an `EitherT` stack will require programming with
dependent types because the stack depends on the depth you are at:
`EitherT b m a` at the first level and `EitherT b1 (EitherT b2 m) a`
at the next. So, we can solve it with continuations instead.

> shortCont :: [[Int]] -> Maybe [Int]
> shortCont xss = runST $ do
>   v <- newArray (0,length xss) 0 :: ST s (STUArray s Int Int)
>   flip runContT return $ loop v 1 xss []
>   xs <- getElems v
>   return $ if head xs == 1 then Just (tail xs) else Nothing
>     where loop _ d [] _ = return ()
>           loop v d (xs:xss) lbls = void . callCC $ \lbl -> forM_ (zip [1..] xs) $ \(idx,i) -> do
>             let rem = i `mod` d
>                 lbls' = lbl:lbls
>             if rem == 0
>               then lift (unsafeWrite v d idx) >>
>                    when (null xss) ( lift (unsafeWrite v 0 1) >>
>                                      (last lbls' $ ())
>                                    )
>               else let goto = lbls' !! (rem-1)
>                    in goto ()
>             loop v (d+1) (map (map (i*)) xss) lbls'

    [ghci]
    shortCont [[1,2],[1,4]]
    shortCont [[1,3],[3,5]]

But keeping track of the `callCC` labels is cumbersome and
error-prone. `EitherT` would be great if the transformer stack didn't
have to change. Well, let's write a new monad to make this
happen. I'll leave you to work out how it works from the code (it's
simple and I can avoid making this post any longer!).

> newtype SC b m a = SC { runSC :: Int -> m (Either (Int,b) a,Int)}
>
> -- works just like EitherT but keeps track of
> -- the depth with an integer state
> instance Monad m => Monad (SC b m) where
>   return a = SC $ \s -> return (Right a,s)
>   {-# INLINE return #-}
>   m >>= f = SC $ \s -> do
>                   (a',s') <- runSC m s
>                   case a' of
>                     Right r -> runSC (f r) s'
>                     Left (i,l)  -> return (Left (i,l),s')
>   {-# INLINE (>>=) #-}
>
> instance MonadTrans (SC e) where
>   lift m = SC $ \s -> m >>= \x -> return (Right x,s)
>   {-# INLINE lift #-}
>
> -- function to exit to a particular level
> exit :: Monad m => Int -> b -> SC b m a
> exit i b = SC $ \s -> return (Left (i,b),s)
> {-# INLINE exit #-}
>
> -- function to initiate a new exit context
> -- the new context is at current level + 1
> lvl :: Monad m => SC b m a -> SC b m (Either b a)
> lvl m = SC $ \s -> do
>   (a,_) <- runSC m (s+1)
>   case a of
>     Right r -> return (Right (Right r),s)
>     Left (i,l)  -> if s >= i
>                    then return (Left (i,l),s)
>                    else return (Right (Left l),s)

Now for the solution in this new monad.

> short :: [[Int]] -> Maybe [Int]
> short xss = runST $ do
>   v <- newArray (0,length xss) 0 :: ST s (STUArray s Int Int)
>   flip runSC 0 $ loop v 1 xss
>   xs <- getElems v
>   return $ if head xs == 1 then Just (tail xs) else Nothing
>     where loop _ d [] = return ()
>           loop v d (xs:xss) = (>> return ()) . lvl . forM_ (zip [1..] xs) $ \(idx,i) -> do
>             let rem = i `mod` d
>             if rem == 0
>               then lift (unsafeWrite v d idx) >>
>                    when (null xss) ( lift (unsafeWrite v 0 1) >>
>                                      exit 0 ()
>                                    )
>               else exit (rem+1) ()
>             loop v (d+1) (map (map (i*)) xss)

    [ghci]
    short [[1,2],[1,4]]
    short [[1,3],[3,5]]
    short (replicate 10 [1..10])

Not bad huh? Here's a benchmark. Sometime later, I'll make a package
of it and upload it to Hackage unless you, the reader, would like to do
it if you find this useful (just give me a shout).

> main = defaultMain
>      [ bench "short" $ nf short (replicate 10 [1..10])
>      , bench "shortCont" $ nf shortCont (replicate 10 [1..10])
>      ]

```
benchmarking short
mean: 10.09475 us, lb 10.08404 us, ub 10.11061 us, ci 0.950

benchmarking shortCont
mean: 22.25052 us, lb 22.23272 us, ub 22.27561 us, ci 0.950
```
