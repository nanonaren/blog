---
bibliography: references.bib
---

    [BLOpts]
    profile    = nanonaren
    title      = "Hungarian Algorithm (Part I): Minimal covering of zeros"
    tags       = algorithm
    categories = Algorithm, Combinatorics

[View source file on Github](https://github.com/nanonaren/blog/blob/master/minimum_row_col_cover.lhs)

> module Main where
> import Control.Lens hiding (assign)
> import Control.Monad
> import Data.List
> import Data.Ord (comparing)
> import Test.QuickCheck hiding (sample)
> import qualified Data.IntSet as I

I will resist posts on random or well-explained topics and though this
may seem like one I hope I can show otherwise. The [Hungarian
algorithm](http://en.wikipedia.org/wiki/Hungarian_algorithm)
(a.k.a. Kuhn-Munkres algorithm) is an efficient algorithm for
determining a minimal-cost assignment of $n$ different tasks to $n$
persons given that each person provides the cost of performing each of
the tasks.

I found it curious enough after coming across a good use for it in a
topic modeling paper [@Cai:2009]. A particular experiment requires
comparing a clustering of documents (with labels $c_1,...,c_k$)
against known human labels ($h_1,\dots,h_k$) assigned to each
document. The problem is we don't know which $c_i$ matches with which
$h_i$ but we do know how many documents labelled $c_i$ are labelled
$h_i$ for any $1 \le i \le k$: the Hungarian algorithm beckons.

The algorithm is simple enough but I'm going to cover it in two parts
because in this post I first explore a required subroutine and then in
the next present the whole. There comes a point in the algorithm where
every person has at least one task that he can perform with zero
cost. If it so happens that we can pick a zero from each row without
repeating tasks then the job is done but if we can't the algorithm
requires we select the smallest set of rows and columns such that the
zero's are covered. Let's start with an example costs table: the rows
are the persons and the columns the tasks.

    [dia-def]
    cell n = (text (show n) # fontSize (Local 0.07)) <> square 0.2

    idxs rs cs m = (strutY 0.2 === vcat (map txt rs)) ||| (hcat (map txt cs) === m)
        where txt n = (text n # fontSize (Local 0.06)) <> (square 0.2 # lw none)

    mat xss marked = vcat $ zipWith (\r -> hcat . zipWith (f r) [1..]) [1..] xss
        where f r c x = if elem (r,c) marked
                        then (text (show x ++ "!") # fontSize (Local 0.06)) <> square 0.2
                        else cell x

```{.dia width='200'}

dia = idxs ["1","2","3","4","5"] ["1","2","3","4","5"] $
      mat [[0,23,4,0,3],[57,76,32,0,94],[38,32,0,19,31],[0,68,24,2,40],[68,23,0,3,12]] []
```

Maximal assignment
------------------

We try to find an assignment with a simple depth-first search
procedure to enumerate all the maximum assignments (will be sped-up in
next post). Figure below shows the corresponding assignment (for the
example above) as zeros with an exclamation mark **0!**.

> sample :: [[Int]]
> sample = [[0,23,4,0,3],[57,76,32,0,94],[38,32,0,19,31]
>          ,[0,68,24,2,40],[68,23,0,3,12]]
>
> assign :: (Eq a,Num a) => [[a]] -> [(Int,Int)]
> assign = snd . maximumBy (comparing fst) . assigns I.empty . zip [1..] . sortMat
>
> assigns :: (Eq a,Num a) => I.IntSet -> [(Int,[a])] -> [(Int,[(Int,Int)])]
> assigns _ [] = [(0::Int,[])]
> assigns visited ((r,xs):xss) = do
>   (i,_) <- filter ((==0).snd) (zip [1..] xs)
>   if I.member i visited
>    then assigns visited xss
>    else let rest = assigns (I.insert i visited) xss
>         in map (\(n,is) -> (n+1,(r,i):is)) rest
>
> sortMat :: (Eq a,Num a) => [[a]] -> [[a]]
> sortMat = map snd . sortBy (comparing fst)
>         . map (\xs -> (length $ filter (==0) xs,xs))

    [ghci]
    mapM_ print $ assigns I.empty (zip [1..] sample)
    assign sample

```{.dia width='200'}

dia = idxs ["1","2","3","4","5"] ["1","2","3","4","5"] $
      mat [[0,23,4,0,3],[57,76,32,0,94],[38,32,0,19,31],[0,68,24,2,40],[68,23,0,3,12]] [(1,4),(3,3),(4,1)]
```

Minimal cover
-------------

Saving the question of why for the next post, the objective now is to
cover every zero with a minimal selection of columns and rows. It
should be clear that if all the zeros were included in the assignment
then it will take either all rows or all columns to cover. Otherwise,
we may consider the following proposition to help create our
algorithm.

**Proposition:** Every other case involves a row (e.g. row 2) not
containing an assigned zero (call them *unmarked* rows) and *unmarked* rows
will never be selected for covering.

**Proof** Every zero in an unmarked row is accompanied by a marked
zero in the corresponding column. E.g. row 2, column 4 is accompanied
by the marked cell at row 4 column 4. In general, let $z$ be the
number of zeros in an unmarked row. Then for each zero there is a
correspnding marked zero in its column. Two solutions emerge: cover these
by choosing $z+1$ rows or by choosing $z$ columns. Hence, unmarked
rows remain unselected (any column and row combination selection performs worse).

We may now deduce an algorithm. Start by making a list of cells with
an unmarked zero and the list of unmarked rows.

> unmarkedZeros marked xss = do
>   (r,xs) <- zip [1..] xss
>   (c,x)  <- zip [1..] xs
>   guard (x==0 && notElem (r,c) marked)
>   return (r,c)
>
> unmarkedRows n as = filter (flip notElem rs) [1..n]
>     where rs = map fst as

We know we have to select the columns intersecting at *unmarked zeros* in
unmarked rows and -- once marked and removed -- leaves us with a
simpler problem to solve, which we solve recursively. The first
iteration of this procedure is illustrated in the figure below.

> selCols cs rs = over (_1.mapped) snd
>               . partition (\(r,c) -> I.member r rs && I.notMember c cs)

    [ghci]
    let marked = assign sample
    let unmarked = unmarkedZeros marked sample
    unmarked
    unmarkedRows 5 marked
    selCols I.empty (I.fromList [2,5]) unmarked

```{.dia width='700'}

dia = markr ||| strutX 0.3 ||| markc ||| strut 0.3 ||| subprob
markr = idxs ["1","(2)","3","4","(5)"] ["1","2","3","4","5"] $
        mat [[0,23,4,0,3],[57,76,32,0,94],[38,32,0,19,31],[0,68,24,2,40],[68,23,0,3,12]] [(1,4),(3,3),(4,1)]
markc = idxs ["1","(2)","3","4","(5)"] ["1","2","(3)","(4)","5"] $
        mat [[0,23,4,0,3],[57,76,32,0,94],[38,32,0,19,31],[0,68,24,2,40],[68,23,0,3,12]] [(1,4),(3,3),(4,1)]
subprob = idxs ["1","(2)","3","4","(5)"] ["1","2","5"] $
          mat [[0,23,3],[57,76,94],[38,32,31],[0,68,40],[68,23,12]] [(1,4)]
```

To select unmarked rows in the smaller matrix we can, given currently
marked columns, return rows intersecting at a *marked zero*. The
recursion stops when there are no more columns to select from
discarded rows -- the final solution consists of the marked columns
and the unselected rows.

> discardRows rs cs = over (_1.mapped) fst
>                   . partition (\(r,c) -> I.member r rs && I.member c cs)
>
> minCover :: (Eq a,Num a) => [[a]] -> ([Int],[Int])
> minCover m = loop (unmarkedZeros marked m) marked urows
>                   (I.fromList $ [1..length m]\\urows) I.empty
>     where marked = assign m
>           urows = unmarkedRows (length m) marked
>           loop unmarkedCells markedCells curRows rows cols =
>               let (cs,unmarkedCells') =
>                       selCols cols (I.fromList curRows) unmarkedCells
>                   (rs,markedCells') =
>                       discardRows rows (I.fromList cs) markedCells
>               in if null cs
>                  then (I.toList rows,I.toList cols)
>                  else loop unmarkedCells' markedCells' rs
>                            (foldl' (flip I.delete) rows rs)
>                            (foldl' (flip I.insert) cols cs)

    [ghci]
    minCover sample
    minCover [[0,1,1,1],[1,1,1,0],[0,1,1,1],[1,0,0,1]]

```{.dia width='400'}

dia = ex1 ||| strut 0.2 ||| ex2
ex1 = idxs ["1","2","3","4","5"] ["(1)","2","(3)","(4)","5"] $
      mat [[0,23,4,0,3],[57,76,32,0,94],[38,32,0,19,31],[0,68,24,2,40],[68,23,0,3,12]] [(1,4),(3,3),(4,1)]
ex2 = idxs ["1","(2)","3","(4)"] ["(1)","2","3","4"] $
      mat [[0,1,1,1],[1,1,1,0],[0,1,1,1],[1,0,0,1]] [(1,1),(2,4),(4,3)]
```

Quickcheck
----------

For good measure, let's write a simple quickcheck to test for
algorithm completion.

> gen_mat :: Gen [[Int]]
> gen_mat = do
>   n <- choose (1,40)
>   xss <- vectorOf n (vectorOf n (choose (0,100)))
>   let f xs = let m = minimum xs in map (subtract m) xs
>   return (sortMat $ map f xss)
>
> test_cover = forAll gen_mat
>     (\xss -> let (rows,cols) = minCover xss
>                  zeros = zip [1..] xss >>= \(r,xs) ->
>                          zip [1..] xs >>= \(c,x) ->
>                          guard (x==0) >> return (r,c)
>              in all (\(r,c) -> elem r rows || elem c cols) zeros
>     )

    [ghci]
    quickCheck test_cover

Next time, we complete the Hungarian algorithm and make sure it
performs on large matrices.

References
----------
