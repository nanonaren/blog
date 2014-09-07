    [BLOpts]
    postid     = 104
    profile    = nanonaren
    title      = "Young Tableau: A Monoid (Part III)"
    tags       = diagrams
    categories = Haskell, Combinatorics

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/schensted.lhs)

> import YT

Last time, we defined an operation that allowed us to grow a young
tableau by a single number. It turns out that we can use this to give
a monoid structure to the space of young tableaux called the *Schensted operation*.

The row-bumping lemma asserted that when inserting $x_1$ followed by
$x_2$ into $T$ (as in $(T \leftarrow x_1) \leftarrow x_2$) when
$x_1 \le x_2$ the box introduced by $x_1$ is strictly to the left of
and weakly below the box introduced by $x_2$. By extension, we arrive
at the following proposition.

**Proposition ** Suppose we constructed $U = ((T \leftarrow x_1)
\leftarrow x_2) \leftarrow \dots \leftarrow x_p$ where $x_1 \le \dots
\le x_p$ and $U$ and $T$ have shapes $\mu$ and $\lambda$ then no
two boxes in $\mu / \lambda$ are in the same column. Conversely, if no
two boxes are in the same column in $\mu / \lambda$, then there is a
unique tableau $T$ of shape $\lambda$ and unique $x_1 \le \dots \le
x_p$ such that $U = ((T \leftarrow x_1) \leftarrow x_2) \leftarrow
\dots \leftarrow x_p$ where $x_1 \le \dots \le x_p$.

Thus, we may define a product tableau $T \cdot U$ from any two tableau
$T,U$. And it turns out that this product has an identity (empty
tableau) and is associative.

> productTableau :: Yt -> Yt -> Yt
> productTableau t =
>     foldl' (\y x -> fst $ rowInsertion x y) t . concat . reverse . yt
>
> instance Semigroup Yt where
>     (<>) = productTableau
>
> instance Monoid Yt where
>     mempty = Yt []
>     mappend = (<>)

Here is an example of a product and a `QuickCheck` to test that the
operation is associative.

    [ghci]
    let yt1 = Yt [[1,2,2],[3]]
    yt1
    let yt2 = Yt [[3,5],[4]]
    yt2
    yt1 <> yt2

![Example](./schensted_fig1.png)

> prop_associative_schensted :: Property
> prop_associative_schensted = do
>   forAll (arbitrary::Gen Yt) $ \y1 ->
>     forAll arbitrary $ \y2 ->
>       forAll arbitrary $ \y3 -> y1 <> (y2 <> y3) == (y1 <> y2) <> y3

Chapter one of the book contains one more section that shows another
construction (*jeu de tarqin* operation) to arrive at this monoid but
I'll skip it for now and move on to the next chapter where it looks at
how this operation behaves on *words* (i.e. encodings of young
tableaux).
