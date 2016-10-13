    [BLOpts]
    profile    = nanonaren
    postid     = 904
    title      = "Markov Chains - Problem (31/365)"
    tags       = daily, probability
    categories =

Going to work on a few more problems before I take a look at Random
Walks, Martingales, and Markov chains together as they share some
things in common as presented in the book.

Let $\theta = (\theta_0, \dots, \theta_n)$ be a Markov chain with
values in $X$ and $f = f(x)$ a function. Will the sequence
$(f(\theta_0), \dots, f(\theta_n))$ form a Markov chain? Will the
reversed sequence $(\theta_n, \dots, \theta_0)$ form a Markov chain?

For the first part, the answer is yes because

$$
P(f(\theta_{k+1})=a_{k+1}|f(\theta_{1})=a_{1},\dots,f(\theta_{k})=a_{k}) \\
= \sum_{f(b_{i})=a_{i}}P(\theta_{k+1}=b_{k+1}|\theta_{1}=b_{1},\dots,\theta_{k}=b_{k}) \\
\mbox{ because of finite phase space the sum is well-defined} \\
= \sum_{f(b_{k+1})=a_{k+1},f(b_{k})=a_{k}}P(\theta_{k+1}=b_{k+1}|\theta_{k}=b_{k})\mbox{ by markov property} \\
= P(f(\theta_{k+1})=a_{k+1}|f(\theta_{k})=a_{k})
$$

The reversed sequence $(\theta_n, \dots, \theta_0)$ will also form a Markov chain

$$
P(\theta_{k-1} = a_{k-1}|\theta_{k}=a_{k},\dots,\theta_{n}=a_{n}) \\
= \frac{P(\theta_{k-1}=a_{k-1},\theta_{k}=a_{k},\dots,\theta_{n}=a_{n})}{P(\theta_{k}=a_{k},\dots,\theta_{n}=a_{n})} \\
= \frac{P(\theta_{n}=a_{n}|\theta_{n-1}=a_{n-1})\dots P(\theta_{k}=a_{k}|\theta_{k-1}=a_{k-1})P(\theta_{k-1}=a_{k-1})}{P(\theta_{n}=a_{n}|\theta_{n-1}=a_{n-1})\dots P(\theta_{k+1}=a_{k+1}|\theta_{k}=a_{k})P(\theta_{k}=a_{k})}\mbox{ by markov property} \\
= \frac{P(\theta_{k}=a_{k}|\theta_{k-1}=a_{k-1})P(\theta_{k-1}=a_{k-1})}{P(\theta_{k}=a_{k})} \\
= \frac{P(\theta_{k}=a_{k},\theta_{k-1}=a_{k-1})}{P(\theta_{k}=a_{k})} \\
= P(\theta_{k-1}=a_{k-1}|\theta_{k}=a_{k})
$$
