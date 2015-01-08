    [BLOpts]
    profile    = nanonaren
    title      = Cover
    tags       = pattern, monad
    categories = haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/cover_part1.lhs)

Recently, while constructing a domain-specific language (DSL), I had
to solve a problem analagous to the following. You are given a company
with boss $B$ and employees $\{ E_1,\dots,E_n \}$ represented by
their free-time schedules. For instance, $E_1$ is an employee who
might be available to meet on Monday, Wedneday, and Friday between
10am-1pm. Things could have stopped at this point with the problem

1. Provide a way to specify schedules using recurrences and
exceptions;

But, to makes this worse, we are asked to

2. Derive the schedule satisfying constraints over boss and employee
schedules such as "the schedule of when the boss can meet with at
least two employees for 30 minutes or more".

I originally solved the first part of this problem without too much
focus on abstractions; later, after playing around, it turned out that
regular Haskell abstractions provide a simple combinator-based
solution to this mess -- like they do in parsers, streams, and
lenses.

This post's objective is to use this problem as a guide to
understanding how basic abstractions such as functors, applicative
functors, and monads come about. I hope I can show you that these
abstractions arise naturally when *computations* -- rather than plain
data -- are taken as our basic objects of manipulation. I invite you
now to ponder on this problem a while and maybe even sketch-out a
solution before reading on.

The Object
----------

A program computing means and variances works with `Double` objects
and operates upon them with `+` and `*`; a program
manipulating documents might work with `[Char]` objects and operates
upon them with `++` and `take`; likewise, people working with
Java, C, and languages without functions as first-class objects might
be tempted to surmise that the object most appropriate for our
problem is a list of time ranges to represent a schedule: `[(DateTime,DateTime)]`.

They might think that each person should be represented as such a list
of free-time ranges. On top of this, they would construct union and
intersection like operators to work with schedule construction and
constraint manipulation. I won't spend anytime telling you how
short-sighted and inflexible this solution would be.

Instead, examine the problem once more and you will see that the
object in each sentence is *schedule*; moreover, there are verbs
operating on these schedules. We see that a schedule will need to be
annotated with simple information like `True` and `False` to indicate
free-time; or will need to be annotated with complex information like
"$E_1,E_3$ can meet but $E_2$ cannot"; or, more importantly be
annotated by other schedules to express nested schedules. Already, the
object `[(DateTime,DateTime)]` will fail to suffice.

We can attempt to codify this discussion by stating that a schedule is
a function from time to some user-defined range. By function, I really
mean a function -- in particular, asking for the 2005 schedule of
employee $E_1$ I ought to have a value defined for every time point in
this year. Could we start with the following type for a schedule?

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> import SchedulePrimitives --hidden
> import Data.Time.Format (readTime)
> import System.Locale (defaultTimeLocale)
> import Data.Time.Clock (UTCTime)
> import Data.List (elemIndex)
> 
> newtype Schedule1 r a = Schedule1 [(a,(r,r))]

It certainly ticks the property of associating a user-defined value to
a range; it also allows you to split a big range into smaller
sub-ranges; however, it can only give us a concrete pre-computed
schedule. Imagine I had $E_1$ and $E_2$ specified as two of these
objects, I wouldn't even know if they are defined over a common range
and I am stuck if I want the schedule outside the ranges in this
list.

Instead, let's define the schedule as a function that only computes
this list given a range.

> newtype Schedule2_ r a = Schedule2_ {runSchedule2_ :: (r,r) -> [(a,(r,r))]}
> -- Those familiar with Haskell libraries will see that
> -- this is the same as
> -- ScheduleH r a = ScheduleH {runSchedule :: StateT (r,r) [] a}

Now, if we are given a function `christmas :: Schedule2 DateTime Bool`
we can write `runSchedule christmas ("2000-1-1","2014-10-11")` to get
the christmas schedule between thos two dates. We don't have to
provide a list of all possible dates for christmas! Simply compute
them over the requested range.

The type of `r`
---------------

For our purposes, the type of `r` will be `Integer`, that is, a unix
timestamp. Remember that the unix timestamp $0$ is the date "Thursday,
Jan 1 1970".

> newtype DT = DT { getDT :: Int }
>     deriving (Eq,Ord,Num,Enum,Integral,Real,Bounded)
> instance Show DT where
>   show (DT t) = show (readTime defaultTimeLocale "%s" (show t) :: UTCTime)

    [ghci]
    show $ DT 381622
    show $ DT (-2819134)

Primitive constructions
-----------------------

There's one more specification to satify before moving further: a
schedule must be a *function*. This means we cannot have a schedule
that takes `(1,10)` and returns [(True,(1,3)),(False,(6,10))] because
no values are defined over `(4,5)`. Therefore we cannot expose the
`Schedule2` constructor to the user. Instead, we will provide two
primitives to construct valid schedules whose implementation I will
give in the next post as their details are irrelevant to our main
discussion.

-- single :: (Ord r,Enum r,Bounded r) => r -> r -> Schedule r Bool
-- periodic :: () => r -> r -> r -> Schedule r (Bool,Int,(r,r))

To show you how they work here are some examples.


Using the primitives
--------------------

Let's use these primitives to define a schedule that defines a
specified day of the week.

> dayOfWeek :: String -> Schedule2 DT ((Int,Bool),(DT,DT))
> dayOfWeek str = periodic (fromIntegral i*24*3600) (fromIntegral (i+1)*24*3600 -1) (fromIntegral (7 + i)*24*3600)
>   where Just idx = str `elemIndex` ["Mo","Tu","We","Th","Fr","Sa","Su"]
>         i = (idx+4) `mod` 7

    [ghci]
    mapM_ print $ runCover (dayOfWeek "Th") (0,14*24*3600-1)
    mapM_ print $ runCover (dayOfWeek "Fr") (1234,14*24*3600-3012)

It's all swell that dayOfWeek is doing what its supposed to but what
if you wanted a simpler user-defined range? What if just wanted it to
say `Maybe "Mo"` or `Nothing` rather than this complex type
`((Int,Bool),(DT,DT)`? The library doesn't yet allow you to do
this. The abstraction that captures this idea is the same one that
captures converting a list of `[Int]` to a list of `[String]` -- a
functor.

> instance Functor (Schedule2_ r) where
>   fmap f (Schedule2_ m) = Schedule2_ $ \s ->
>     map (\(a,r) -> (f a,r)) (m s)

and now

    [ghci]
    let f ((_,b),_) = if b then Just "Th" else Nothing
    mapM_ print $ runCover (f `fmap` dayOfWeek "Th") (0,14*24*3600-1)

Let's create another pattern using the `periodic` primitive.

> arbitraryRange :: DT -> Int -> Schedule2 DT ((Int,Bool),(DT,DT))
> arbitraryRange dt secs = periodic dt (dt + fromIntegral secs -1) (dt + fromIntegral secs)

    [ghci]
    mapM_ print . map snd $ runCover (arbitraryRange (5*3600) (12*3600)) (4412,2*24*3600-5432)

Combining Schedules 1
---------------------

Now we have a couple of primitive schedules in hand and we need to put
them together.

 instance Applicative () where
   pure = undefined
   (<*>) = undefined

What if we don't have a value to provide immediately? What if we want
to ask the user to confirm if he is free when the schedule says so?
Can we do this without breaking this nice structure? Of course. Change
our schedule so that the `a` is wrapped in a type.

 newtype Schedule r m a = Schedule {runSchedule :: (r,r) -> [m a,(r,r)]}
 -- once again Haskell has a built-in answer
 -- Compose ScheduleH m

and provide the following applicative instance.

 instance Applicative () where
   pure = undefined
   (<*>) = undefined

Example.

Combining Schedules 2
---------------------

There is another way to combine schedules. Take two schedules and
merge values. Say I want the schedule where either $E_1$ is free or
$E_2$ is free. We can't do this using `Applicative`. How about `Alternative`?

 instance Alternative () where
   empty = undefined
   (<|>) = undefined

Introduce alternative.

Next time, I'll provide the implementations and show these interfaces
in action after which we'll endow schedules with a monadic interface
as well and solve the problem.
