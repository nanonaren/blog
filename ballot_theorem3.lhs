    [BLOpts]
    profile    = nanonaren
    postid     = 882
    title      = "Ballot Theorem - 3 (26/365)"
    tags       = daily, probability
    categories =

In the [last
post](https://nanonaren.wordpress.com/2016/10/08/ballot-theorem-2-25365/)
I showed how to count the number of valid vote sequences given that
candidate $A$ ends up with only one extra vote over candidate $B$. In
general, candidate $A$ may end up with $n+k$ votes. How do the counts
change?

In terms of balanced parantheses this means we have $k$ extra open
parantheses to make use of. Let's say $n=3$ and $k=3$ then

1. There are $C_3$ ways to arrange $3$ votes for each candidate

2. For each of the $C_3$ valid ways we can insert an extra open
paranthesis in $7$ possible places. After this, we can insert the next
extra open paranthesis in $8$ possible places. So, we now have $8
\times 7 \times C_3$ arrangements.

3. The last extra parantheses we know we have to place it at the
beginning. But there are $k$ choices for the first paranthesis. So now
have $3 \times 8 \times 7 \times C_3$ possible arrangements.

4. Finally, since the extra parantheses are also indistinguishable we
have to divide by $(8!)(7!)$.

Thus the number of ways to arrange $n+k$ votes for candidate $A$ and
$n$ votes for candidate $B$ where candidate $A$ always has the higher
number of votes is

$$
\frac{k \times (n+k-1)\times \dots \times (n+1)}{(n+k)\times \dots \times (n+2)} \times \frac{(2n)!}{(n+1)!n!} = \frac{k(n+k-1)!}{(n+k)!n!}
$$

The probability that candidate $A$ always has the higher number of
votes than candidate $B$ and ends up with $a$ and $b$ votes respectively is

$$
\frac{(a-b)(a+b-1)!}{(a+b)!n!} \div \frac{(a+b)!}{(a+b)!n!} = \frac{a-b}{a+b}
$$

We have in fact arrived at the solution without the use of
martingales. Next time, let's see why this example is used in the
chapter on martingales.
