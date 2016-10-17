    [BLOpts]
    profile    = nanonaren
    postid     = 928
    title      = "Expectation Maximization - Link (35/365)"
    tags       = daily, probability
    categories =

Dan Piponi has [written
up](http://blog.sigfpe.com/2016/10/expectation-maximization-with-less.html)
a simple to follow derivation of the Expectation-Maximization
algorithm. It give a very practical derivation of the algorithm which
also makes it easy to remember.

What it clarifies for me is the step in the EM algorithm where one
introduces auxilliary variables $\beta_z$ -- one for each value hidden
value $z$ that the hidden variable can take on -- which somehow turns out to
be the conditional probability of $z$ given everything else. Why this
turns out to be the case has always been a little fuzzy to me. And
Dan's post clarifies it greatly. The step that determines the
auxilliary variables comes from equating the derivative of the
log-likelihood and the derivative of the simpler function involving
$\beta_z$'s and solving for $\beta_z$. Please have a read.
