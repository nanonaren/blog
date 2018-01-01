    [BLOpts]
    profile    = nanonaren
    postid     = 1057
    title      = "Sharing a Birthday (56/365)"
    tags       = daily, probability
    categories =

I think most have heard something like you only need suprisingly few
people in a room before two people in the room end up sharing a
birthday. But I never bothered to work it out. Let me do that.

First, forget leap years. Let a year have $365$ days. Note that if you
have 366 people in the room you are guaranteed that someone will share
a birthday. On the other end of the spectrum, if there are only two
people in the room then the probability that the two of them share a
birthday is given by one minus the number of ways they cannot share a
birthday divided by the number of ways we can assign them a birthday:
$\frac{(365)(364)}{(365)(365)}$.

In general, let's say there are $k$ people in the room. The following
gives the number of ways to assign different birthdays to each of the
$k$ people.

$$
D(k) = (365)(365-1)(365-2)\dots(365-k+1) = (365)_k \\
\text{Note that when } k > 365 \text{ we get } D_k = 0
$$

The number of ways to assign any birthday to each of the $k$ people.

$$
N(k) = 365^k
$$

So, the probability that at least one pair out of $k$ will share a
birthday is given by

$$
\frac{D(k)}{N(k)} = 1 - \frac{(365)_k}{365^k}
$$

Graphing it below. By $k=41$ the probability is already at $0.9$.

![Probability of sharing a birthday](birthday.png)
