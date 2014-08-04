    [BLOpts]
    profile    = nanonaren
    title      = "Quickie: Generating the powerset"
    tags       = pattern, monad, list
    categories = haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/quickie_powerset.lhs)

> import Control.Monad

The traditional way to generate the powerset of a list of elements is
the following

> powerset :: [a] -> [[a]]
> powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs
> powerset [] = [[]]

    [ghci]
    powerset [1,2,3]

But, I found this little gem of a way hidden in the [Haskell
wiki](http://www.haskell.org/haskellwiki/Blow_your_mind).

> powerset2 :: [a] -> [[a]]
> powerset2 = filterM (const [True,False])

This version runs a filter using the List as the underlying
monad. Let's see why this works on `[1,2,3]`.

First, here is the definition of filterM

```
filterM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ []     =  return []
filterM p (x:xs) =  do
   flg <- p x
   ys  <- filterM p xs
   return (if flg then x:ys else ys)
```
and it's expansion for `[1,2,3]` in `powerset2`

```
   powerset2 [1,2,3]
== filterM (const [True,False]) [1,2,3]
== [True,False] >>= \flg ->
   filterM (const [True,False]) [2,3] >>= \ys ->
   return (if b then (1:ys) else ys)
```

There we have it. Another use of List for computations binding on
multiple return values (Bool in this case).
