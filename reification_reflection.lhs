    [BLOpts]
    profile    = nanonaren
    postid     = 844
    title      = "Reification And Reflection In Haskell (20/365)"
    tags       = daily, haskell
    categories =

I am reading the paper [Functional Pearl: Implicit Configurations](http://okmij.org/ftp/Haskell/tr-15-04.pdf)
and I will go on to see how to use the hackage library [reflection](http://hackage.haskell.org/package/reflection)
based on this paper which allows one to pass around configuration data
in an elegant manner.

Before that though, I want to look at a certain technique using types
that the paper uses to achieve what it does. Specifically, it needs
the ability to

1. encode a value as a type (**reification**)
2. decode a type to a value (**reflection**)

Let's take a look at how we can reify integers and then reflect back
their corresponding type. You'll be aware that we can specify
integers recursively

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE Rank2Types          #-}
>
> data Zero
> data Succ a
> data Pred a

This allows one to write numbers like

> type One = Succ Zero
> type Two = Succ One

Note how the number $n$ will have `Succ` applied $n$ times. We can
write numbers with fewer recursions if we introduce types to mimic
binary encoding

> data Twice a

We can now write numbers using only $O(\log n)$ recursions. This is not
necessary to demonstrate reification but I just wanted to mention it.

> type Four = Twice Two
> type Eight = Twice Four
> type Nine = Succ Eight

Remember that each number is a different type. To *reflect* each type
back to its corresponding integer we need a typeclass so each type can
have an instance that gives its integeral representation.

> class ReflectNum s where
>   reflectNum :: Num a => s -> a

And the following instances.

> instance ReflectNum Zero where
>   reflectNum _ = 0
> instance ReflectNum s => ReflectNum (Succ s) where
>   reflectNum _ = reflectNum (undefined :: s) + 1
> instance ReflectNum s => ReflectNum (Pred s) where
>   reflectNum _ = reflectNum (undefined :: s) - 1
> instance ReflectNum s => ReflectNum (Twice s) where
>   reflectNum _ = reflectNum (undefined :: s) * 2

Note that the local reference to type `s` requires the use of the
language extension `{-# LANGUAGE ScopedTypeVariables #-}`. Let's test it out.

    [ghci]
    reflectNum (undefined :: Zero) :: Int
    reflectNum (undefined :: Nine) :: Int
    reflectNum (undefined :: Twice Nine) :: Int

Reification
-----------

How do we now take an integer and *reify* it back to its corresponding
type? You might think we just need a function like so

> -- reifyIntegral1 :: Int -> ???

But we can't directly return the corresponding type of `Int` because each
integer returns a different type! One way to get around this is to not
return! The programming idiom that doesn't return is the
continuation. Consider the following type signature

> -- reifyIntegral2 :: Int -> (s -> w) -> w

We are providing the function with a continuation `(s -> w)` which
allows us to continue the computation of the type by passing the
result into the continuation.

> -- reifyIntegral2 n f | n == 0 = f (undefined :: Zero)
> --                    | n > 0  = reifyIntegral (n-1) (\s -> f (undefined :: Succ s))

But this will give us the following error

```
reification_reflection.lhs:92:62:
    Couldn't match expected type ‘s’ with actual type ‘Succ s0’
      ‘s’ is a rigid type variable bound by
          the type signature for reifyIntegral :: Int -> (s -> w) -> w
          at reification_reflection.lhs:87:20
    Relevant bindings include
      f :: s -> w (bound at reification_reflection.lhs:91:19)
      reifyIntegral :: Int -> (s -> w) -> w
        (bound at reification_reflection.lhs:91:3)
    In the first argument of ‘f’, namely ‘(undefined :: Succ s)’
    In the expression: f (undefined :: Succ s)
    In the second argument of ‘reifyIntegral’, namely
      ‘(\ s -> f (undefined :: Succ s))’
Failed, modules loaded: none.
```

The problem is that we have fixed `s` in the continuation to inhabit
only one type. We simply need to free it up by saying `s` can be any
type that can be reflected.

> reifyIntegral :: Int -> (forall s. ReflectNum s => s -> w) -> w
> reifyIntegral n f =
>   case n `quotRem` 2 of
>     (0, 0) -> f (undefined :: Zero)
>     (q, 0) -> reifyIntegral q (\(_ :: s) -> f (undefined :: Twice s))
>     (q, 1) -> reifyIntegral q (\(_ :: s) -> f (undefined :: Succ (Twice s)))
>     (q,-1) -> reifyIntegral q (\(_ :: s) -> f (undefined :: Pred (Twice s)))

Compiling the above will fail without the
`{-# LANGUAGE Rank2Types #-}` extension (the use of `forall`). Let's
test it out.

    [ghci]
    reifyIntegral 138291 reflectNum :: Int

There you have it - reification and reflection.
