    [BLOpts]
    profile    = nanonaren
    postid     = 812
    title      = "More Time Utilities (14/365)"
    tags       = daily, haskell
    categories =

In the [previous
post](https://nanonaren.wordpress.com/2016/09/24/more-time-utilities-13365/)
I introduced a basic date-time type and said that I'll provide a DSL
to do much more with it. Here is a date-time matching DSL I will
support.

    [haskell]
    data Match = DowMatch (UArray Int Bool)
               | MonthMatch (UArray Int Bool)
               | DayMatch (UArray Int Bool)
               | TodMatch (Int,Int)
               | DTMatch (DT,DT)
               | Not Match
               | Or Match Match
               | And Match Match
               | Never
               | Always
               deriving (Show)

The actual implementation and tests are
[here](https://github.com/nanonaren/timeutils). I'll just show what
you can do with it. First off, you can check if a date matches a spec.

    [ghci]
    :l DateTime.hs
    let Right dt = toDT 2016 09 22 10 12 0
    Always `match` dt
    Never `match` dt
    dowMatch [Tuesday,Thursday] `match` dt
    dowMatch [Monday .. Wednesday] `match` dt
    todMatch [((3,10,0),(11,0,0))] `match` dt
    dayMatch [1..10] `match` dt
    monthMatch [7..11] `match` dt

We can also use the logical combinators.

    [ghci]
    dowMatch [Tuesday,Thursday] `And` monthMatch [7..11] `match` dt
    Not (dowMatch [Thursday]) `And` monthMatch [7..11] `match` dt

The module also provides extracting matched ranges within a provided
date range.

    [ghci]
    let Right dt2 = toDT 2016 10 16 12 0 0
    mapM_ print $ split (dowMatch [Monday]) dt dt2
    mapM_ print $ split (dowMatch [Monday] `And` todMatch [((3,10,0),(4,30,0)), ((18,0,0),(19,0,0))]) dt dt2

The test file does a QuickCheck using the `Arbitrary` instance
(although I can now use the [generic-random
package](https://byorgey.wordpress.com/2016/09/20/the-generic-random-library-part-1-simple-generic-arbitrary-instances/)!)
and compares `match` and `split` against a brute-force implementation
using the `tick` function to compute a match at every second.
