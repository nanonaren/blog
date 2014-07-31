    [BLOpts]
    profile    = nanonaren
    title      = "Young Tableau: Introduction (Part I)"
    tags       = diagrams
    categories = haskell, combinatorics

[View literate file on Github](https://github.com/nanonaren/blog/blob/master/young_tableaux_1.lhs)

I have a book sitting on my shelf: Young Tableaux by William Fulton
(ISBN 0 521 56724 6). I bought it after having the fantastic chance to see these
beautiful and deep combinatorial structures in a couple of math
courses. So, as I work through this book, I will be illustrating Young
tableux and associated algorithms with Haskell. The great
advantage of this subject is that we can explore it in pictures. I intend to do just this.

In this first post, let's look at what a Young Tableux looks like. I
will only list the essential definitions for the sake of brevity and
to avoid replicating too much material from the book.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine

A Young Diagram is a drawing of a partition $\lambda = (\lambda_1,
\dots, \lambda_k)$ such that each $\lambda_i > \lambda_{i+1}$ where
$\lambda_i$ is represented as a row of boxes aligned to the left.

A single box can be rendered as

> box :: Diagram B R2
> box =  square 0.2 # fc white

and, we represent a set of rows of boxes as the following type along
with a method to draw the Young diagram corresponding to a partition.

> type YoungDiagram a = [[a]]
>
> youngDiagram :: [Int] -> YoungDiagram (Diagram B R2)
> youngDiagram = map (flip replicate box)
>
> draw :: YoungDiagram (Diagram B R2) -> Diagram B R2
> draw = vcat . map hcat

Here is how the partition $\lambda = (6,4,4,2)$ looks like as a Young
diagram rendered as `draw (youngDiagram [6,4,4,2])`

![Example of a young diagram](./tableau1.png)

Given a young diagram $\mu$ contained in $\lambda$ one can construct a
*skew diagram* or *skew shape*, $\lambda / \mu$ as the diagram
obatained by removing a smaller Young diagram from the larger one.

> skew :: [Int] -> YoungDiagram (Diagram B R2)
> skew = map (flip replicate shadedCell)
>
> shadedCell :: Diagram B R2
> shadedCell =  square 0.2 # fc green

The following is the skew shape for $\lambda = (6,4,4,2)$ and $\mu =
(3,3,1)$ rendered as `draw (skew [3,3,1]) <> draw (youngDiagram
[6,4,4,2])`

![Example of a skew diagram](./tableau2.png)

Finally, a young tableau is a filling of a young diagram that is

1. weakly increasing across each row
2. strictly increasing down each column

> numbering :: [[Int]] -> YoungDiagram (Diagram B R2)
> numbering = map (map (\x -> text (show x) # fontSizeN 0.1 # fc black))
>
> zipDiagrams :: YoungDiagram (Diagram B R2) -> YoungDiagram (Diagram B R2)
>             -> YoungDiagram (Diagram B R2)
> zipDiagrams = zipWith (zipWith (<>))
>
> youngTableau :: [Int] -> [Int] -> [[Int]] -> Diagram B R2
> youngTableau shape skewShape nums =
>     draw (skew skewShape)
>     <> draw (numbering nums `zipDiagrams` youngDiagram shape)

Here are two examples rendered by
```
    youngTableau [3,3,1] [] [[1,2,2],[2,3,3],[4]]
||| strutX 1
||| youngTableau [2,2,1] [[1],[1]] [[2,2],[3],[3]]
```

![Example of young and skew tableau](./tableau3.png)

Next time, I'll work through the bumping and sliding
algorithms and hopefully explore it with animation support in
`diagrams`.
