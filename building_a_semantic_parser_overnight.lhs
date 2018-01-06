    [BLOpts]
    profile    = nanonaren
    postid     = 1140
    title      = "Paper: Building a Semantic Parser Overnight (65/365)"
    tags       = daily, language, nlp
    categories = [Papers]

In a [previous
post](https://nanonaren.wordpress.com/2018/01/02/paper-question-asking-as-program-generation-62-365/)
I talked about a paper that encoded questions as programs which in
turn defined a procedure that executed in the environment of a
battleship game. In other words, each question defines a program that
defines the semantics of the question.

Of course, that paper did not consider the translation of the natural
text into the program. Today's paper by Wang et. al considers building
a *semantic parser*: to translate a natural language text into a
precise internal program that can be executed against an environment
(database) to produce an answer.

This paper encodes the semantics in a langugage called [lambda DCS](https://arxiv.org/pdf/1309.4408.pdf)
based on lambda calculus. It's compositional form is quite
interesting. Every logical form is either a set of entities (unary) or
a set of entity pairs (binary). A unary $u$ and a binary $b$ can be
composed to produce a binary $b.u$ which are pairs in $b$ restricted
to pairs whose second entities are present in $u$. The other operators
are the set operations on the unary terms.

They have also defined a canonical translation of programs into natural language.

    "start date of student alice whose university is brown university"
    R(date).(student.Alice ∩ university.Brown)

Since there are only so many ways to compose these programs, the
canonical phrases can be generated easily. What they then do is use
Amazon's Mechanical Turk to convert these canonical phrases to
something more familiar. The above example is converted to "When did
alice start attending brown university?". These pairs are then used to train a paraphrasing model

$$
p_\theta(z, c | x, w) \propto \exp \left( \phi(c, z, x, w)^T \theta\right)
$$

where $\phi(z,c,x,w)$ is a feature vector and $\theta$ is a parameter
vector; $z$ is the logical form (i.e. a program); $c$ is the canonical
phrase; $x$ is the human phrase for the canonical phrase; and $w$ is
the database (represented as a set of triples) that can be queried by
the logical forms. I'll come back to the details of the implementation
as I find similar papers.
