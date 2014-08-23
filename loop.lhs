    [BLOpts]
    profile    = nanonaren
    title      = "Performance: Looping"
    tags       = performance, vector
    categories = Haskell

[View source file on Github](https://github.com/nanonaren/blog/blob/master/loop.lhs)

Before I finish off the Hungarian algorithm, I want to settle (nominally) the
matter of performance with respect to array/vector based
number-crunching in Haskell. I never sat down to consider this matter
systematically and now seems like a good time.

Let's do a bit of benchmarking where a typical operation of interest
might be an operation on the rows/columns of a matrix: compute the sum
of the minimum value of each row/column. I work with a square matrix
of `Double`s stored in row-major order.

> {-# LANGUAGE BangPatterns #-}
> module Main where
> import Data.List (foldl',transpose)
> import qualified Data.Vector.Fusion.Stream as S
> import qualified Data.Vector.Unboxed as U
> import qualified Data.Vector.Unboxed as V
> import qualified Statistics.Matrix as M
> import System.Random
> import System.Random.MWC
> import Criterion.Main
>
> sample :: Int -> IO (U.Vector Double)
> sample n = withSystemRandom . asGenST $ \gen -> uniformVector gen (n*n)
>
> asListR :: [[Double]] -> Double
> asListR = foldl' (+) 0 . map minimum
>
> asListC :: [[Double]] -> Double
> asListC = foldl' (+) 0 . map minimum . transpose

The `vector` package is the logical next step. For those unfamiliar
with boxed and unboxed values, I give you an implementation with
`Data.Vector` and with `Data.Vector.Unboxed` -- the boxed vector
points to each entry whereas the unboxed vector is like a c-array. Of
course, we can write a single function with the generic vector
constraint but I want to avoid type constraints in the signature for
benchmarking purposes. Note that I have avoided Haskell lists by using
`Stream` from the `vector` package.

Vector
------

> asVectorR :: Int -> V.Vector Double -> Double
> asVectorR dim v = S.foldl' (+) 0 . S.generate dim $ \i ->
>     V.minimum (V.slice (i*dim) dim v)
>
> asVectorC :: Int -> V.Vector Double -> Double
> asVectorC dim v = S.foldl' (+) 0 . S.generate dim $ \c ->
>     S.foldl1' min (S.generate dim (\r -> V.unsafeIndex v (dim*r + c)))
>
> asUVectorR :: Int -> U.Vector Double -> Double
> asUVectorR dim v = S.foldl' (+) 0 . S.generate dim $ \i ->
>     U.minimum (U.slice (i*dim) dim v)
>
> asUVectorC :: Int -> V.Vector Double -> Double
> asUVectorC dim v = S.foldl' (+) 0 . S.generate dim $ \c ->
>     S.foldl1' min (S.generate dim (\r -> U.unsafeIndex v (dim*r + c)))

What else can we do? How about a C-like for-loop in the hope of
avoiding the mutliplication (e.g. in `asVectorC`) and the *possible*
overhead of streams. I'll abstract the for-loop using a callback strategy.

> type Loop r = Int -> U.Vector Double -> (r -> r -> r) -> (Double -> r) -> r
>
> {-# INLINE foldFor #-}
> foldFor :: Int -> Int -> Int -> Loop r
> foldFor !start !step !end dim v f cb = loop (cb $ v `U.unsafeIndex` start) (start+step)
>     where loop !acc !i | i == end  = acc
>           loop !acc !i | otherwise = loop (f acc $ cb (U.unsafeIndex v i)) (i+step)
>           {-# INLINE [0] loop #-}
>
> asFoldForR :: Int -> U.Vector Double -> Double
> asFoldForR dim v = loop 0 0
>     where loop !acc !i | i == dim  = acc
>           loop !acc !i | otherwise = loop (acc + foldFor (i*dim) 1 (i*dim+dim) dim v min id) (i+1)
>
> asFoldForC :: Int -> U.Vector Double -> Double
> asFoldForC dim v = loop 0 0
>     where loop !acc !i | i == dim  = acc
>           loop !acc !i | otherwise = loop (acc + foldFor i dim (dim*dim+i) dim v min id) (i+1)

Results
-------

> main = do
>   v <- sample 1000
>   let lstv = chunk 1000 $ U.toList v
>       boxv = U.convert v
>   -- make sure results match
>   print (asListR lstv)
>   print (asVectorR 1000 boxv)
>   print (asUVectorR 1000 v)
>   print (asFoldForR 1000 v)
>   print (asListC lstv)
>   print (asVectorC 1000 boxv)
>   print (asUVectorC 1000 v)
>   print (asFoldForC 1000 v)
>   
>   defaultMain
>     [
>       bgroup "Row compute"
>       [
>         bench "asListR" (nf asListR lstv)
>       , bench "asVectorR" (nf (asVectorR 1000) boxv)
>       , bench "asUVectorR" (nf (asUVectorR 1000) v)
>       , bench "asFoldForR" (nf (asFoldForR 1000) v)
>       ]
>     , bgroup "Column compute"
>       [
>         bench "asListC" (nf asListC lstv)
>       , bench "asVectorC" (nf (asVectorC 1000) boxv)
>       , bench "asUVectorC" (nf (asUVectorC 1000) v)
>       , bench "asFoldForC" (nf (asFoldForC 1000) v)
>       ]
>     ]
>   where chunk _ [] = []
>         chunk n xs = let (cur,rest) = splitAt n xs
>                      in cur : chunk n rest

```{.dia width='400'}
bar nm n = ((text (show n ++ "ms") # fontSize (Local 0.2)) === strutY 0.15 === stk) # alignB
    where stk = ((text nm # fontSize (Local 0.3)) <> rect n 1) # rotateBy (1/4)
rows = hcat' (with & sep .~ 0.1) [ bar "asVectorR" 3.83
                                 , bar "asUVectorR" 3.81
                                 , bar "asFoldForR" 3.81
                                 , bar "C" 2.32]
cols = hcat' (with & sep .~ 0.1) [ bar "asVectorC" 12.91
                                 , bar "asUVectorC" 8.64
                                 , bar "asFoldForC" 7.26
                                 , bar "C" 5.43]
dia = rows ||| strutX 1 ||| cols
```

This was compiled on (GHC 7.6.3 because I'm not using my archlinux
machine where 7.8 would be available in the main repo unlike in
ubuntu; I'll update with 7.8 later, gcc 4.8.2) and executed on a
paltry ASUS laptop with (<2GHz) low voltage processor. The time for
`asListR` is 30.74ms and for `asListC` is 97.03. To view the C
implementation see the
[source](https://github.com/nanonaren/blog/blob/master/loop.lhs) for
this blog post.

As you can see, the performance of vector is excellent indeed with
only a slight advantage for writing your own loop. Depending on the
use-case, the advantage maybe worth it. In the future, I'll (or if
someone else wants to write that post here) investigate the Core (the
intermediate language) for the best implementations. I'm sure more
performance can be squeezed out with `MagicHash` but I'm not that
obsessed with performance.

<!---
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <time.h>

double computeR(double* m,int row)
{
  double total = 0;
  int i,j;

  for (i=0; i<1000; i++)
  {
    int x = INT_MAX;
    for (j=0; j<1000; j++)
    {
      x = x < (*m) ? x : (*m);
      m++;
    }
    total += x;
  }
  return total;
}

double computeC(double* m,int row)
{
  double total = 0;
  int i,j;

  for (i=0; i<1000; i++)
  {
    int x = INT_MAX;
    for (j=i; j<row*1000+i; j=j+row)
    {
      int y = m[j];
      x = x < y ? x : y;
    }
    total += x;
  }
  return total;
}


int main ()
{
  double m[1000*1000];
  int i,j;
  double* p = m;

  for (i=0; i<1000; i++)
  {
    for (j=0; j<1000; j++)
    {
      *p = rand();
      p++;
    }
  }

  clock_t t;
  double res=0;

  t = clock();

  for (i=0; i<1000; i++)
  {
    res += computeR(m,1000);
  }
  printf ("%f\n", res);

  t = clock() - t;

  printf ("%f milliseconds\n",1000 * (((float)t)/CLOCKS_PER_SEC/1000));
}
-->
