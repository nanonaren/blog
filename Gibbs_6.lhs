    [BLOpts]
    profile    = nanonaren
    postid     = 1065
    title      = "DSL for Generative Models (58/365)"
    tags       = daily, probability, Haskell
    categories =

I've now cleaned up (in `befb0f3cca0c212e368497e86f030aa96355be18`)
the `Reader` and `Writer` interfaces and added it to
`Statistics.GModeling.Gibbs`. I've removed references to `Support` and
simply parameterized using a key type `k` and value type `v`.

> data Reader k v = Reader
>   {
>     -- | Number of available indices
>     size :: Int
>     -- | Read the value at the given key
>   , readn :: Int -> k -> v
>     -- | Create a copy for writing only
>   , copy :: IO (Writer k v)
>   }
> 
> data Writer k v = Writer
>   {
>     -- | Write the value at the given key
>     writen :: Int -> k -> v -> IO ()
>     -- | Create a read-only copy
>   , readOnly :: IO (Reader k v)
>   }

I've also simplified the type of `Indexed` and added an implementation
of `Reader` and `Writer` for HMM in `Statistics.GModeling.Models.HMM`.
