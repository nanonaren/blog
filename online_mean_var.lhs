    [BLOpts]
    profile    = nanonaren
    postid     = 795
    title      = "Online Mean Variance (9/365)"
    tags       = daily, probability
    categories =

I am uploading a module I had written to compute the mean and the
estimated variance in a streaming fashion as described
[here](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance). I've
wrapped it in a `Monoid` for convinence. The module can be found
[here](https://github.com/nanonaren/stats/blob/master/OMVar.lhs).

    [ghci]
    :l OMVar.lhs
    let m1 = mconcat $ zipWith mkMeanVar (repeat 1) [1..10]
    let m2 = mconcat $ zipWith mkMeanVar (repeat 1) [11..20]
    print (meanVar m1)
    print (meanVar $ m1 <> m2)

