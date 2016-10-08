    [BLOpts]
    profile    = nanonaren
    postid     = 878
    title      = "Ballot Theorem - 2 (25/365)"
    tags       = daily, probability
    categories =

[Last
time](https://nanonaren.wordpress.com/2016/10/06/ballot-theorem-1-24365/)
I left you with a recursive solution to counting the number of vote
sequences where candidate $A$ always has the higher number of
votes. This time, I want to look at the closed-form solution to
counting the same.

There is a reason why I want to do this because when I was thinking of
a solution it didn't strike me at all that the answer had to do with
Catalan numbers even after I realized that counting the number of
**balanced parantheses** is an equivalent problem. Rustiness annoys
me. So, this time I want to come up a proof I'll remember.

Suppose candidate $A$ ends up with only one vote more $n+1$ than
candidate $B$. We know that the first vote has to be for candiate
$A$. That leaves us with $n$ votes for each candidate that we have to
arrange so that $A$ always stays on top. You will note that is
equivalent to arranging $n$ pairs of parantheses so that they remain
balanced.

Below is a procedure for generating all sequences of balanced parantheses.

> import Data.List
> 
> gen_valid :: Int -> [String]
> gen_valid n = loop n n 0
>   where
>     loop 0 0 _ = [""]
>     loop 0 b _ = [')' : s | s <- loop 0 (b-1) 0]
>     loop a b k = ['(' : s | s <- loop (a-1) b (k+1)] ++
>                  if k > 0 then [')' : s | s <- loop a (b-1) (k-1)] else []

    [ghci]
    gen_valid 1
    gen_valid 2
    gen_valid 3
    gen_valid 4

We also know that the total number of possible arrangements (valid and
invalid) is given by $\binom{2n}{n}$. The Catalan number gives the
number of valid arrangements as a fraction of all possible arrangements

$$
C_n = \frac{1}{n+1}\binom{2n}{n} = \frac{(2n)!}{(n+1)!n!}
$$

How does this fraction come about?
---------------------------------

One way to interpret this fraction is to say that for every valid
arrangement there are $n$ corresponding invalid arrangements. Can we
come up with a way to transform a valid arrangement into $n$ unique
invalid arragements?

I suspect that it should be possible given that the $n$ invalid
arrangments might have to do with inverting each of the parantheses in
the sequence. For example, we can transform $()$ to $)($ or $()()$ to
these two $)(()$ and $())($. What happens when we have nested
parantheses? What does $(())$ become? Note that to flip the internal
parantheses alone is bad because we get $()()$! It would seem that we
should flip the parent before its children: that way we get these two
$)()($ and $))(($. So far so good. I now code the general procedure
and check that it generates all arrangments from just the valid ones.

The following function extracts a top level balanced string and
returns the rest.

> split :: String -> Maybe (String,String)
> split [] = Nothing
> split ss = Just $ splitAt (len+1) ss
>   where len = length . takeWhile (>0) . tail . scanl (\x c -> if c=='(' then x+1 else x-1) 0 $ ss

    [ghci]
    split "(())()()"

This function returns all top level balanced strings.

> splits :: String -> [String]
> splits = unfoldr split

    [ghci]
    splits "(())()()"

The following takes a valid sequence and generates $n$ invalid
sequences.

> validToInvalids :: String -> [String]
> validToInvalids str = concat $ map (\i -> modAt i lst) [0..length lst-1]
>   where lst = splits str
>         change (_:xs) = ')' : init xs ++ "("
>         modAt i xs = let (lhs,ss:rhs) = splitAt i xs
>                      in -- this flips the outer and leavs the inner the same
>                         concat (lhs ++ [change ss] ++ rhs) :
>                         -- this recurses into the inner and wraps with a flipped outer
>                         map (\x -> concat $ lhs ++  [")" ++ x ++ "("] ++ rhs) (validToInvalids (init . tail $ ss))
>
> choose :: Int -> Int -> Int
> choose n k = fact n `div` fact k `div` fact (n-k)
>   where fact a = product [2..a]

Let's check that all the invalid sequences generated are unique and
that it sums up to all possible arrangements.

    [ghci]
    validToInvalids "()"
    validToInvalids "()()"
    validToInvalids "(())"
    choose 6 3 == (length . nub . concat . map (\x -> x : validToInvalids x) $ gen_valid 3)
    choose 8 4 == (length . nub . concat . map (\x -> x : validToInvalids x) $ gen_valid 4)
    choose 10 5 == (length . nub . concat . map (\x -> x : validToInvalids x) $ gen_valid 5)
