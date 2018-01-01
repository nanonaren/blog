    [BLOpts]
    profile    = nanonaren
    postid     = 1050
    title      = "DSL for Generative Models - Interpreter (54/365)"
    tags       = daily, probability, Haskell
    categories =

The next step is for the library to have access to the latent variable
data. I also don't want the library to decide how to store the data
because the user will have a much better idea of what is the most
efficient way to store it. In terms of the library, I will need both
read and write access to this data.

There are two kinds of data I need access to. In the case of HMM, for
example, the values of `Topic` and `Symbol` form the `Support` to some
distribution while the values of `Topics` and `Symbols` are the index
of their respective distributions that's currently active. So,
consider the following signature

> type Support = Int
> -- Int -> a -> Either (a,Int) Support

For now, I'll restrict `Support` to only integers. The second type
takes some integer index, a label, and returns either the index or the
support depending on what is being asked. I'll make this clear in the
end with the HMM example.

For the library to have read access to the data I will provide this
data type.

> data Reader a = Reader
>   {
>     size :: Int
>   , read :: Int -> a -> Either (a,Int) Support
>   , copy :: IO (Writer a)
>   }

Field `size` tells us how many indices are there `[0..size-1]`; `read`
is the function we just saw, and `copy` creates a writable copy of the
data.

> data Writer a = Writer
>   {
>     write :: Int -> a -> Support -> IO ()
>   , readOnly :: IO (Reader a)
>   }

Here `write` allows us to write at some `Int` index for the label `a`
a new value. Let me take the HMM as an example again. For simplicity,
let's say we store the sequences as a list of lists.

> data HMMLabels = Alpha | Beta | Transition
>                | Initial | Topic | Symbols | Symbol
>
> 
> type Sequences = [[(Int,Int)]]

We can provide a reader.

> reader :: Sequences -> Reader HMMLabels
> reader ss = Reader
>   {
>     size = length (concat ss)
>   , read = \idx -> let (i,j) = indices !! idx
>                        (topic,symbol) = ss !! i !! j
>                        (prev_topic,_) = ss !! i !! (j-1)
>                    in \name -> case name of
>                                  Topic -> topic
>                                  Symbol -> symbol
>                                  Symbols -> (Topic,topic)
>                                  Transition -> if j==0
>                                                then (Initial,0)
>                                                else (Topic,prev_topic)
>   , copy = error "undefined"
>   }
>   where indices = concat $
>           map (\(i,s) -> [(i,j) | j <- [0..length s-1]]) (zip [0..] ss)

Note how the signature of `read` encourages caching; that is, the
library can first supply only the index and then repeatedly query the
resulting partial function for various names. This seems to be alright
so far but I'll only know if this holds up when I look at managing the
distributions in the next post.
