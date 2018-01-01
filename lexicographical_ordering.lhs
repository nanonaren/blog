    [BLOpts]
    profile    = nanonaren
    postid     = 1069
    title      = "Lexicographical Ordering (60/365)"
    tags       = daily, Haskell
    categories =

> import Data.Ord (comparing)

This is completely random but I thought it was a neat use of
laziness. You are familiar with lexicographical ordering? Haskell's
compare of lists implements this.

    [ghci]
    [1,2] < [1,3]
    [1,3,4] < [1,3,4,5]
    [2,4] < [2,3]

Note that this favors shorter strings.

    [ghci]
    [1,2] < [1,2,3]
    [1,3] < [1,3,4]

For whatever reason, I wanted to favor longer strings. How can we do
this? First, note that the above is equivalent to doing the comparison
after appending an infinite list of zeros to each operand (assuming we
are using only positive numbers).

    [ghci]
    let aug xs = xs ++ cycle [0]
    comparing aug [1,2] [1,3]
    comparing aug [1,3,4] [1,3,4,5]
    comparing aug [2,4] [2,3]

If I, instead, append an infinite list of a large number I can get
what I want.

    [ghci]
    let aug xs = xs ++ cycle [9999999]
    comparing aug [1,2] [1,2,3]
    comparing aug [1,3] [1,3,4]
