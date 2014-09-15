    [BLOpts]
    postid     = 100
    profile    = nanonaren
    title      = "Hungarian Algorithm (Part II)"
    tags       = algorithm
    categories = Algorithm, Combinatorics

[View source file on Github](https://github.com/nanonaren/blog/blob/master/hungarian_algorithm.lhs)

[part 1 is not necessary to follow this] When I first looked up the
Hungarian algorithm on Wikipedia, I was immediately drawn away from
what seemed like a tortured graphical explanation and went straight to
the rather simpler matrix represented solution. I was tempted to
lookup the explanation for the algorithm in the original paper but I
resisted; I wanted to work out the reason for the steps in the
algorithm myself; and -- joy upon joy -- the explanation is
beautifully symmetrical.

Symmetry
--------

Let $S$ be the set of $n \times n$ matrices with non-negative
entries. $S$ comprises of all the problem sets definable for $n$ tasks
and $n$ people. Let $\mathcal{A}(s)$ be the cost of the minimal-cost
assignment for matrix $s$. Let's see if there is a transformation of
$s \in S$ that doesn't affect the solution.

**Proposition** Let $s \in S$ and $(\cdot)^T$ be the transpose. Then
$\mathcal{A}(s) = \mathcal{A}(s^T)$.

*Proof: * If $(i_k,j_k)$ make up the indices of a minimal assignment,
then $(j_k,i_k)$ make up those in $s^T$. Thus, $\sum_k s_{i_k j_k} =
\sum_k s^T_{i_k j_k}$.

**Proposition** Let $s \in S$ and let $R(k,x) : S \rightarrow S$ for
$1 \le k \le n$ and $x \in \mathbb{R}$ be the operation where $R(k,x)$
adds $x$ to row $k$. Then, $\mathcal{A}(s) = \mathcal{A}(R(k,x)(s)) - x$.

*Proof* In a minimal assignment, exactly one cell is selected from
every row. Therefore, adding $x$ to row $k$ means every possible
assignment (minimal or not) increases in total cost by $x$.


Being symmetrical to the problem, these operations $(\cdot)^T$ and
$R(k,x)$ can be composed. Not only that but $(\cdot)^T$ is its own
inverse and $R(k,-x)$ is the inverse for $R(k,x)$. A group is
formed. For our purposes, composition is going to allow us to perform
steps in sequence while inverses ensure that our steps are reversible
-- taking us back to the original problem.

Steps 1 and 2
-------------

Step 1 is the composition of the transformations $R(k_i,-x_i)$ for $1 \le i
\le n$, $k_i = i$, and $x_i = \min \{ s_{i(\cdot)} \}$.

Step 2 is the composition of the above transformations applied to the
transpose of the matrix at the end of step 1.

Step 3
------

Now that we have at least one zero in each row, the task now is to
assign as many tasks as possible such that the total cost is zero. How
can we do this without resorting to brute-force? The explanations I
found were confusing to say the least and I decided to see if I can
work it out myself. Here's what I came up with.

1. Consider that the first $k$ people have been given the best assignment
   such that $a_i = j$ meaning the $i$th person is assigned task $j$
   (obviously $M_{ij}=0$) and $a_i = -1$ meaning no task was assigned to
   the $i$th person.

2. We encounter two possible cases when trying to assign a task for
   the $(k+1)$th person.

3. *CASE 1:* There exists $j$ with $M_{(k+1)j}=0$ such that $\nexists
   i \le k$ where $a_i = j$. In which case, set $a_{k+1} = j$.

4. *CASE 2:* No such $j$ exists. The question is, can we re-adjust the
   assignments for the first $k$ people such that $k+1$ can be given an
   assignment? The solution can be written in terms of the simple
   linear algorithm that determines if a vertex in a graph is
   reachable from another. See if you can spot how it works in the
   following example.

![Using vertex reachability to figure out assignment for row 4.](./hungarian_fig1.png)

*Explaining the example:* Column 4 is not used which can be used by
row 1 (i.e. acting as the starting vertex), 2) the next row can take
the column currently taken by row 1 (i.e. an edge connects rows $x
\rightarrow y$ if $x$ takes column $c$ and $y$ has a $0$ in the same
column), which recursively repeated leads to 3) row 4 that can
finally take the column of row 3.

In general, the assignment for the $(k+1)$th row is determined by
finding a path from a row with a zero in a free column to the
$(k+1)$th row by traversing edges connecting two rows if the second
can take the column currently taken by the first. If no such path
exists, then the $(k+1)th row has no assignment.

Step 4
------

If every person was given a task the algorithm stops. Otherwise, we
create a new zero in the matrix by first minimally covering the
existing zeros (see the Wikipedia link), computing the minimum $m$ from
the uncovered cells, and then applying the row operations to uncovered
rows $r$, $R(r,-m)$, and the column operation, $C(c,m)$, to covered columns $c$.

There you have it. A problem solved purely by reducing it with
symmetrical transforms (the cleverness going into determining which
symmetries to apply!). With the Hungarian algorithm, I decided to make
it my contribution to the newly developing `Statistics.Matrix` module
-- right now I use `hmatrix` for my matrix needs which plugs-in to the C
LAPACK/BLAS libraries -- that's part of the excellent `statistics`
package. The pull request is
[here](https://github.com/bos/statistics/pull/69).
