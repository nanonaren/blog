    [BLOpts]
    profile    = nanonaren
    postid     = 143
    title      = Haskell Abstractions At Work (Part I)
    tags       = pattern, functor, applicative
    categories = Haskell

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/cover_part1.lhs)

Recently, while constructing a domain-specific language (DSL), I had
to solve a problem analogous to the following. You are given a company
with boss $B$ and employees $\{ E_1,\dots,E_n \}$ represented by
their free-time schedules. For instance, $E_1$ is an employee who
might be available to meet on Monday, Wednesday, and Friday between
10am-1pm. We could have dined with familiarity if the problem was to

1. provide a way to specify schedules using recurrences and
exceptions;

but, spoiling our meal is this added problem:

2. derive the schedule satisfying constraints over given
schedules. For example, the schedule for "when the boss can meet with
at least two employees for 30 minutes or more when $E_1$ will not meet
with $E_2$ and $E_2$ will not meet with $E_3$ on Fridays".

Initially, I solved the first part of this problem without too much
focus on abstractions; later, after playing around, it turned out that
regular Haskell abstractions provide a simple combinator-based
solution (that has served well for parsers, streams, and lenses) to
both parts of the problem.

This post will attempt to guide your through basic Haskell
abstractions such as functors, applicative-functors, and monads as
they arise as solutions to aspects of this problem. I hope I can show
you that these abstractions arise naturally when *computations* --
rather than plain data -- are taken as primitives. I invite you now to
ponder on this problem a while and maybe even sketch-out a solution
before reading on.

Date-Time
---------

For our purposes, we'll let a date-time value be an integer
representing the number of seconds passed (in positive and negative
directions) since "Thursday, 1 Jan 1970 00:00:00" (represented by $0$).

> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> import SchedulePrimitives --hidden for now
> import Control.Applicative
> import Data.Time.Format     (readTime)
> import Data.Time.Clock      (UTCTime)
> import Data.List            (elemIndex)
> import Data.Functor.Compose
> import Data.Traversable
> import System.Locale        (defaultTimeLocale)
> 
> newtype DT = DT { getDT :: Int }
>     deriving (Eq,Ord,Num,Enum,Integral,Real,Bounded)
> instance Show DT where
>   show (DT t) = show (readTime defaultTimeLocale "%s" (show t) :: UTCTime)

    [ghci]
    show $ DT 381622
    show $ DT (-2819134)

The Object
----------

You might be tempted to represent a person's free-time schedule as a
simple list of date-time ranges `[(DT,DT)]` but you'll immediately
find it a nuisance to represent recurring ranges like every Monday
between 10am-11am.

You can try to solve this by replacing `(DT,DT)` by some flexible
date-time matching specification. While this would help with recurring
schedules and so forth you've done nothing but push the complexity of
defining a schedule into this matching specification. Future requests
for more matching capabilities from users will only leave you annoyed
by the constant modifications to this type.

We haven't even talked about labeling arbitrary ranges in a schedule
like saying labeling "every Monday 10am-11am" with "Meeting with
boss" or even label with a runnable command "sh /root/backup.sh".

We can attempt to codify this discussion by stating that a schedule is
a function from time to some user-defined range. By function, I really
mean a function -- in particular, asking for the 2005 schedule of
employee $E_1$ I ought to have a value defined for every time point in
this year. Could we start with the following type for a schedule?

> newtype Schedule1 r a = Schedule1 [(a,(r,r))]

It certainly ticks the property of associating a user-defined value to
a range; it also allows you to split a big range into smaller
sub-ranges; however, it suffers from the same problem as using
`[(DT,DT)]`. Instead, let's define the schedule as a function that
only *computes* this list given a range.

    [haskell]
    newtype Schedule r a = Schedule {runSchedule :: (r,r) -> [(a,(r,r))]}

Now, if we are given a schedule like `christmas :: Schedule DT Bool`
we can write `runSchedule christmas (date1,date2)` to get
the christmas schedule between those two dates. We don't have to
provide a list of all possible dates for christmas! Simply compute
them over the requested range.

Primitive constructions
-----------------------

There's one more specification to satisfy before moving further: a
schedule must be a *function*. This means we cannot have a schedule
that takes `(1,10)` and returns `[(True,(1,3)),(False,(6,10))]` because
no values are defined over `(4,5)`. Therefore we cannot expose the
`Schedule` constructor to the user. Instead, we will provide two
primitives to construct very basic schedules whose implementation I
will give in the next post as their details are irrelevant to our main
discussion.

    [haskell]
    single   :: Integral r => r -> r -> Schedule r Bool
    periodic :: Integral r => r -> r -> r -> Schedule r ((Int,Bool),(r,r))

To show you how they work, here are some examples.

    [ghci]
    pretty $ runSchedule (single 3 7) (-3,10::Int)
    pretty $ runSchedule (periodic 3 7 12) (-3,20::Int)

Note all the extra information periodic returns for each matched
range. It returns the match offset from `(3,7)` and whether it is
matching using `Bool` and also returns the used match `(-6,-2)` though
only `(-3,-2)` was matched. We won't be using all these pieces of
information in this post but will need them in the next one. We'll
also need one primitive to echo the input range.

    [haskell]
    getRange :: Schedule DT (DT,DT)
    getRange = Schedule $ \rr -> [(rr,rr)]

<h3> Using the primitives: Day of week </h3>

Let's use these primitives to define a schedule based on a day of the week.

> dayOfWeek :: String -> Schedule DT ((Int,Bool),(DT,DT))
> dayOfWeek str = periodic (i*24*3600) ((i+1)*24*3600 -1) ((i+7)*24*3600)
>   where Just idx = str `elemIndex` ["Mo","Tu","We","Th","Fr","Sa","Su"]
>         i = fromIntegral $ (idx+4) `mod` 7

    [ghci]
    pretty $ runSchedule (dayOfWeek "Th") (0,14*24*3600-1)
    pretty $ runSchedule (dayOfWeek "Fr") (1234,14*24*3600-3012)

Abstraction one: Functor
--------------------

It's all swell that `dayOfWeek` is doing what its supposed to but what
if you wanted a simpler user-defined range? What if you just wanted it to
say `True` or `False` rather than this complex type
`((Int,Bool),(DT,DT)`? The library doesn't yet allow you to do
this. The abstraction that captures this idea is the same one that
helps you convert a list of `[a]` to a list of `[b]` -- a
functor.

    [haskell]
    instance Functor (Schedule r) where
      fmap f (Schedule m) = Schedule $ \s -> map (\(a,r) -> (f a,r)) (m s)

and now

    [ghci]
    pretty $ runSchedule ((snd.fst) `fmap` dayOfWeek "Th") (0,14*24*3600-1)

<h3> Using the primitives: Arbitrary time range </h3>

Let's create another schedule that starts off at time $t_1$ and stops
the range after a specified duration $d$ to give $(t_1,t_1 + d)$. It
then repeats from $t_1+d+1$.

> arbitraryRange :: DT -> Int -> Schedule DT ((Int,Bool),(DT,DT))
> arbitraryRange dt secs = periodic dt (dt + fromIntegral secs -1) (dt + fromIntegral secs)

The following defines 24hour ranges starting at 5am everyday.

    [ghci]
    pretty $ runSchedule (arbitraryRange (5*3600) (24*3600)) (0,2*24*3600)

Abstraction two: Applicative Functor
--------------------------------

We are able to define schedules that identify the day of the week and
also define schedules identifying particular time ranges. What we
really want is to create a schedule for "every Monday, Wednesday,
Friday from 10am-12noon". Given two schedules we want to take every
range matched by the first schedule and pass it as input to the second
schedule for further refinement. The following applicative interface
gives us exactly that

    [haskell]
    instance Applicative (Schedule r) where
      pure a = Schedule $ \rr -> [(a,rr)]
      (Schedule f) <*> (Schedule g) = Schedule $ \rr -> do
          (func,rr') <- f rr
          (a,rr'') <- g rr'
          return (func a,rr'')

We can go about creating our schedule now. I've given each small
schedule a name to make the applicative interface stand-out when
combining schedules.

> tenToTen       = arbitraryRange (10*3600) (24*3600)
> twelveToTwelve = arbitraryRange (12*3600) (24*3600)
> tenToTwelve    = (\(_,(a,_)) (_,(b,_)) -> a > b) <$> tenToTen <*> twelveToTwelve

    [ghci]
    pretty $ runSchedule tenToTwelve (0,7*24*3600-1)

and the days of the week,

> isTrue ((_,b),_) = b
> mondays          = isTrue <$> dayOfWeek "Mo"
> wednesdays       = isTrue <$> dayOfWeek "We"
> fridays          = isTrue <$> dayOfWeek "Fr"
> mwf              = (\m w f -> m || w || f) <$> mondays <*> wednesdays <*> fridays
> mwf_10_12        = (&&) <$> mwf <*> tenToTwelve

    [ghci]
    pretty $ runSchedule mwf_10_12 (0,7*24*3600-1)

Performing actions rather than carrying values
----------------------------------------------

Suppose, the boss wants to send out a survey to each employee at the
start the week to ask whether an employee can attend the meetings
defined by the above schedule (MWF 10am-12noon). You would hope that
all we would have to do is replace `Bool` carried by the schedule to
an `IO` action that asks the user `[y/n]`. Luckily, that's exactly
what we'll do -- keeping the original schedule structure intact.

First, here is how we can ask a question

> canYouMakeIt :: String -> IO Bool
> canYouMakeIt str = do
>   putStr $ "Can you make it on " ++ str ++ "? [Y/n] "
>   ln <- getLine
>   return $ (ln == "y" || ln == "")

Second, `Schedule DT Bool` will become `Schedule DT (IO Bool)` which,
happily, is a common enough pattern than there is a `newtype` that
takes care of it called `Data.Functor.Compose`
([here](http://hackage.haskell.org/package/transformers-0.4.2.0/docs/Data-Functor-Compose.html)). The
following function takes a normal schedule returning `Bool` and lifts
it to this composed schedule that asks the user if he can attend
whenever the normal schedule returns `True`.

> canYouMakeItOn :: Schedule DT Bool -> Compose (Schedule DT) IO Bool
> canYouMakeItOn sched = Compose (ask <$> sched <*> getRange)
>   where ask True rr = canYouMakeIt (show rr)
>         ask _ _ = return False

We need a simple function to run a composed schedule,

> runComposedSchedule :: Applicative m => (r,r) -> Compose (Schedule r) m a -> m [(a,(r,r))]
> runComposedSchedule rr = sequenceA
>                        . map (\(ma,rr) -> (,rr) <$> ma)
>                        . flip runSchedule rr
>                        . getCompose

Here we go,

<pre><code><span style="color: gray;">ghci&gt; </span>runComposedSchedule (0,7*24*3600) (canYouMakeItOn mwf_10_12) >>= pretty . filter fst
  Can you make it on (1970-01-02 10:00:00 UTC,1970-01-02 11:59:59 UTC)? [Y/n] y
  Can you make it on (1970-01-05 10:00:00 UTC,1970-01-05 11:59:59 UTC)? [Y/n] n
  Can you make it on (1970-01-07 10:00:00 UTC,1970-01-07 11:59:59 UTC)? [Y/n] y
  (1970-01-02 10:00:00 UTC,1970-01-02 11:59:59 UTC) ---> True
  (1970-01-07 10:00:00 UTC,1970-01-07 11:59:59 UTC) ---> True
</code></pre>

Not bad huh? Just to give you an idea, you could just as easily
retrieve the value from a database instead of asking over the
terminal. In the above example, we asked the employee for a
conformation whenever the schedule asserted `True`. But, we can also
apply our abstractions to composed schedules just as we did with
schedule and ask the employee a question only when certain
sub-patterns match. For example, we can ask the employee to confirm
if he is free only on Wednesday in the schedule "Monday,
Wednesday,Friday".

> liftS :: Applicative m => Schedule DT a -> Compose (Schedule DT) m a
> liftS s = Compose $ pure <$> s
> 
> chooseWed :: Compose (Schedule DT) IO Bool
> chooseWed = (\m w f -> m || w || f)
>           <$> liftS mondays
>           <*> canYouMakeItOn wednesdays
>           <*> liftS fridays

Next time
---------

Though we can already structure our computations and effects around
the structure of a schedule, we can't yet write certain
schedules. Try, for example, to write a schedule that annotates the
year -- keeping in mind that you have to take care of leap years. In the next post, we'll
see how to take care of this and also take care of specifying constraints.

**NOTE:** If we want to, we could make our `Schedule` type the same as the
composed schedule because `Schedule r a` can be recovered using
the identity functor.

    [haskell]
    Schedule r a == Compose (Schedule r) Identity a
