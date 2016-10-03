    [BLOpts]
    profile    = nanonaren
    postid     = 851
    title      = "Modular Arithmetic (21/365)"
    tags       = daily, haskell
    categories =

In the paper [Functional Pearl: Implicit
Configurations](http://okmij.org/ftp/Haskell/tr-15-04.pdf), the
authors use modular arithmetic as a running example to demonstrate the
many ways of injecting the modulus into the arithmetic operations. For
example, one approach is to explicitly pass around the modulus

> add1 :: Integral a => a -> a -> a -> a
> add1 m x y = (x+y) `mod` m
>
> mult1 :: Integral a => a -> a -> a -> a
> mult1 m x y = (x*y) `mod` m

But this is error prone and cumbersome because someone using these two
functions to do something like

> example :: Integral a => a -> a -> a -> a
> example m x y = (((x+y) `mod` x) * y) `mod` m

when they actually intended to mod by `m` both times. You could say
add a `newtype` wrapper to `m` to fix this but this still doesn't stop
the user from using two different modulus operations in `example`.

The paper suggests many different ways to solving this issue and one
of them is to use a reader monad and write add as `add :: (Integral a,
MonadReader a m) => a -> a -> a` which would certainly thread the same
modulus through all operations. But it forces us to write monadic code
when it is unnecessary.

What I realized was that we can still the reader structure but without
the monad instance if we roll our own type with the reader state.

> newtype M a = M (a -> a)
>
> withModulus :: Integral a => a -> M a -> a
> withModulus m (M f) = f m
>
> instance Integral a => Num (M a) where
>   (M f) + (M g) = M $ \s -> (f s + g s) `mod` s
>   (M f) - (M g) = M $ \s -> (f s - g s) `mod` s
>   (M f) * (M g) = M $ \s -> (f s * g s) `mod` s
>   negate (M f)  = M $ \s -> (- f s) `mod` s
>   abs _         = error "Modular numbers are not signed"
>   signum _      = error "Modular numbers are not signed"
>   fromInteger n = M $ \s -> fromIntegral n `mod` s

This is very convenient.

    [ghci]
    withModulus 7 $ (10+3)*(3-4) + 8

This solution seems as convenient and safe as what the paper does for
this particular example. Of course, the paper is solving a much more
general problem but this struck me as a good solution for modular
arithmetic.
