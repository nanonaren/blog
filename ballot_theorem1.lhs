    [BLOpts]
    profile    = nanonaren
    postid     = 874
    title      = "Ballot Theorem - 1 (24/365)"
    tags       = daily, probability
    categories =

The introductory chapter on martingales in this book leads up to the
Ballot Theorem. I feel it should have motivated the need for the study
of martingales with a problem that couldn't be solved with simple
methods. So, I want to take a few posts to try to do this.

Let's start with this ballot problem. Let $\omega_1, \dots,
\omega_n$ be a sequence of independently and identically distributed
Bernoulli random variables. Let's say each $\omega_i$ represents a
vote either for candidate $A$ ($\omega_i = 1$) or represents a vote
for candidate $B$ ($\omega_i = -1$). Let $S_k = \sum_{i=1}^k
\omega_i$. Suppose $P(\omega_i=1)=P(\omega_i=-1)$ and candidate $A$
receives a total of $a$ votes and candidate $b$ receives a total of
$b$ votes and $a > b$ compute the following probability that candidate
$A$ was always ahead of candidate $B$.

$$
P(S_1 > 0, \dots, S_n > 0 | S_n = a - b) = \frac{a-b}{a+b}
$$

Let's try to attack this combinatorially. The total number of
assignments is given by $\binom{a+b}{b}$ because we can place $b$
votes in one of $a+b$ positions.

> choose :: Int -> Int -> Double
> choose n k = fact n / fact k / fact (n-k)
>   where fact a = product [2..fromIntegral a]

    [ghci]
    choose (10+4) 4

Now, the number of sequences in which candidate $A$ always has a
higher number of votes is given by the following recursion.

> valid :: (Int,Int) -> Double
> valid (_,0) = 1
> valid (a,b) | a-b == 1 = valid (a,b-1)
>             | otherwise = valid (a-1,b) + valid (a,b-1)

    [ghci]
    valid (10,4)
    valid (10,4) / choose (10+4) 4 == (10-4) / (10+4)

We can easily speed up valid using memoization. A closed form solution
should not be hard to come by. But next time let's see how this
chapter approaches this problem and how martingales play a part.
