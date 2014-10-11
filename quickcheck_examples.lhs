This is a Literate Haskell file.

> import Test.QuickCheck
> import Test.QuickCheck.Function
> import Data.List

Reverse
-------

> myreverse :: [a] -> [a]
> myreverse = foldl (flip (:)) []

> myreverse2 :: [a] -> [a]
> myreverse2 acc [] = acc
> myreverse2 acc (x:xs) = myreverse2 (x:acc) xs

> prop_reverse :: [Int] -> Bool
> prop_reverse xs = myreverse xs == reverse xs

> prop_reverse2 :: [Int] -> Bool
> prop_reverse2 xs = myreverse2 xs == reverse xs

Inverse functions and inverse as a property
-------------------------------------------

> toEven :: Int -> Int
> toEven = (2*)

> fromEven :: Int -> Int
> fromEven = (`div` 2)

> prop_even_inverse x = (fromEven . toEven) x == id x

Generalize it!

This is particularly useful for encode/decode functions
so you can package it up!

> prop_inverse :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
> prop_inverse f g = \x -> (g . f) x == id x

or for instance

> prop_commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
> prop_commutative op a b = a `op` b == b `op` a

Exercise... associativity

> prop_associativity = error "exercise"

Data structures
---------------

A revised way to write using latest QuickCheck

> prop_insert x = isOrdered . insert x . getOrdered
>     where isOrdered xs = and $ zipWith (<=) xs (tail xs)

Another invarint for insert

> prop_insertFind x = elem x . insert x . getOrdered

Exercise... union

> prop_union = error "exercise"

Think about the number of data structures that support
- insert
- find
- union
- difference
- delete
- and more

All these can be packaged into tests just once and then used for each
data structure.

Inifinite structures
--------------------

> prop_doubleCycle xs' n' = take n (cycle xs) == take n (cycle (xs++xs))
>     where xs = getNonEmpty xs'
>           n = getPositive n'

Coarbitrary
-----------

A whole lot of functions in haskell are higher-order functions so
Coarbitrary is very useful for testing.

> prop_filterLength f' xs =
>     length (filter f xs) <= length xs
>     where f = apply f'

Exercise

> prop_takewhile f xs = error "exercise"


One more exercise
-----------------

> prop_permutations = error "exercise"
