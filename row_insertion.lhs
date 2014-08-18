    [BLOpts]
    profile    = nanonaren
    title      = "Young Tableau: Row-insertion (Part II)"
    tags       = diagrams
    categories = Haskell, Combinatorics

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/row_insertion.lhs)

> import Control.Lens
> import Control.Monad
> import Control.Monad.State
> import Data.List
> import Test.QuickCheck

Having defined a Young Tableau, let's consider one of the ways to
construct it. How can you go about adding a single integer $x$ to a
tableau $T$ (written $T \leftarrow x$)? The answer is the *row-insertion* or *row-bumping* algorithm that

0. starts at first row
1. adds $x$ to end of current row if it is not less than any number in the row
2. otherwise, it stops at the first $y>x$, takes its place, and
   loops to step 1 starting at the next row with $y$ to insert

> type Yt = [[Int]]
> type Route = [(Int,Int)]
>
> rowInsertion :: Int -> Yt -> (Yt,Route)
> rowInsertion x yt = runState (loop 1 x yt) [] & _2 %~ reverse
>     where loop row i [] = modify ((row,1):) >> return [[i]]
>           loop row i (xs:xss)
>               | null rs = modify ((row,length xs+1):) >> return ((xs++[i]) : xss)
>               | otherwise = do
>                   modify ((row,length ls+1):)
>                   ((ls++(i:tail rs)) :) `liftM` loop (row+1) (head rs) xss
>               where (ls,rs) = break (>i) xs

    [ghci]
    let (yt,rt) = rowInsertion 2 [[1,2,2,3],[2,3,5,5],[4,4,6],[5,6]]
    mapM_ print yt
    print rt

This insertion procedure leaves a trace called the *bumping route*
which tracks the blocks knocked out of position and the position of
the new block added at the end. Below is an illustration of an example
run where the shaded blocks form the *bumping route*.

I'll also take this moment to write a simpler way to draw a Young
Tableau. We can use the `named` attribute on each cell so that we can
later modify the basic tableau. See the source for this post for details.

```{.dia width='700'}

colorCell c = withName (c::(Int,Int)) $ beneath . place (square 0.2 # fc green) . location
replaceCell c x = withName (c::(Int,Int)) $ atop . place (cell x c <> (square 0.2 # fc green)) . location

sepr d = strutX 0.1 ||| d ||| strutX 0.1
cell x nm = ((text (show x) # fontSize (Local 0.07)) <> square 0.2) # named nm
yt = vcat . map hcat . zipWith (\r -> zipWith (\c x -> cell x (r,c)) [(1::Int)..]) [(1::Int)..]

begin = yt [[1,2,2,3],[2,3,5,5],[4,4,6],[5,6]]
step1 = begin ||| strutX 0.1 ||| (text "." # fontSize (Local 0.07)) ||| strutX 0.1 ||| cell 2 "adding"
step2 = begin # replaceCell (1,4) 2
step3 = begin # replaceCell (1,4) 2 # replaceCell (2,3) 3
step4 = begin # replaceCell (1,4) 2 # replaceCell (2,3) 3 # replaceCell (3,3) 5
final = yt [[1,2,2,2],[2,3,3,5],[4,4,5],[5,6,6]] # replaceCell (1,4) 2 # replaceCell (2,3) 3 # replaceCell (3,3) 5 # colorCell (4,3)

dia =   (step1 ||| sepr (text "=" # fontSize (Local 0.07)) ||| step2 ||| sepr (arrowV (r2 (0.2,0))) ||| step3)
    === strutY 0.1
    === (sepr (arrowV (r2 (0.2,0))) ||| step4 ||| sepr (arrowV (r2 (0.2,0))) ||| final)

```

Row Bumping Lemma
-----------------

I'll leave you with a QuickCheck proof of the Row Bumping Lemma that
relates the bumping route (and also the last cell in the route in
particular) traced by $T \leftarrow x$ and the route traced by $(T
\leftarrow x) \leftarrow x'$ with respect to when $x <= x'$ or $x >
x'$.

> yt_gen :: Gen [[Int]]
> yt_gen = do
>   nrows <- choose (1,20::Int)
>   let rows 0 _ _ = return []
>       rows n prev_k prev_xs = do
>         k <- choose (1,prev_k::Int)
>         xs <- fmap sort $ mapM (\x -> choose (x+1,79+nrows-n)) (take k prev_xs)
>         fmap (xs:) $ rows (n-1) k xs
>   rows nrows 20 (repeat 0)
>
> row_bumping_lemma = forAll yt_gen
>     (\yt -> do
>        x <- choose (1,99)
>        x' <- choose (1,99)
>        let (yt1,r1) = rowInsertion x yt
>            (_,r2)   = rowInsertion x' yt1
>            (row1,col1) = last r1
>            (row2,col2) = last r2
>        return $ if x <= x'
>                 then cmp r1 r2 == LT && col1 < col2 && row1 >= row2
>                 else cmp r2 r1 /= GT && col2 <= col1 && row2 > row1
>     )
>     where cmp xs ys = let l = min (length xs) (length ys)
>                       in compare (take l xs) (take l ys)

    [ghci]
    quickCheckWith (stdArgs{maxSuccess=1000}) row_bumping_lemma

Next time, we look at how this row-insertion algorithm leads to a
monoid over the Young Tableaux.
