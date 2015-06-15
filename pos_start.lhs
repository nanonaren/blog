    [BLOpts]
    profile    = nanonaren
    postid     = 236
    title      = "Starting Part-of-Speech Tagging"
    tags       = statistics,modeling,POS,tagging
    categories = statistics,modeling

This is by no means the latest on the subject of probabilistic
part-of-speech tagging of documents but nevertheless provides a good
starting point to look at the basic model along with training and testing
data.

This paper [1] takes a stab at comparing unsupervised HMM-based POS
taggers. It is quite strange that something as simple as an HMM
can be expected to tag words according to parts of speech without any
supervision. By general standards of classification accuracy, we find
that an unsupervised HMM does in fact perform poorly: [1] shows the
average greedy 1-to-1 accuracy (explained in last section) of $51\%$ at best and the paper we
looked at [last time](https://nanonaren.wordpress.com/2015/06/08/adding-more-relaxed-constraints-during-model-inference/) also gives the same performance on this data.

Clearly, some supervision is required. Though it isn't the main
purpose of the paper from last time, two approaches are taken. The
first is to consider *closed-class* words. These are words which
amount to a handful having a common tag such as *punctuation*,
*pronouns*, and *possessive markers* while open classes are things
like verbs and nouns. The authors modify the models by fixing the
class of these closed-class words by giving them zero probability to
tags they definitely don't belong to. The results improve to about
$58\%$ which is not a huge jump.

The second thing they try is to use the hidden representation from an
unsupervised run of a HMM (in addition to other simple features) and
feed them as features to a classification model. I am not too
interested in this but suffice it to say accuracy jumps up yet again
to about $85\%$.

### Data

Here are some links to high-quality data used in these papers.

* [The Penn Treebank Project](http://www.cis.upenn.edu/~treebank/)
* [Portugese Floresta Sinta(c)tica Treebank](http://corp.hum.sdu.dk/tgrepeye_pt.html)
* [Bulgarian BulTreeBank](http://www.bultreebank.org/)

### Evaluation Details

Luckily, anything falling into the category of classification is by
far the easiest to evaluate in a completely intuitive and concrete
way. It's not as easy to fool someone with shady looking experiments
as it is with unsupervised models without a classification objective.

The method employed in this paper is to split the data into a training
and test set. Then, from the model inferred from the training set a
greedy 1-to-1 map is established between each hidden state with a
part-of-speech tag (another approach would be find a best-matching
instead). This mapping is employed to label the test data and compare
against the treebanks.

Another method is to allow a 1-to-Many mapping where more than one
hidden state could be mapped to a tag by simply sending each hidden
state to the best matching tag. The results with this approach seem to
show higher accuracies.

-------
[1] Jianfeng Gao and Mark Johnson. 2008. "A comparison of Bayesian
estimators for unsupervised Hidden Markov Model POS
taggers". *Empirical Methods in Natural Language Processing*
