    [BLOpts]
    profile    = nanonaren
    postid     = 1127
    title      = "Paper: Question Asking as Program Generation (62/365)"
    tags       = daily, language, nlp
    categories = [Papers]

I am very bored by language models that just pump the training data
through something and then predict the word following a partially
revealed sentence. I mean, I don't learn anything from them apart from
a lot of technical wizardry. So, I am trying to look for papers that
lean towards true comprehension.

This paper "Question Asking as Program Generation" is from NIPS
2017. They have collected 605 questions that humans have asked in the
context of several partially revealed battleship game board with a view
to gain information about the positions of the hidden ships. So each
of these questions comes with a context (the game board). The authors
aim to create a system to predict the question given a context.

Questions as Programs
---------------------

In order to generate questions we need something compositional. The
authors turn to programming. In this case, lisp programs. Every
question is written as a lisp program. For example, `How many tiles
have the same colour as tile 2F?` is written as `(setSize
(colouredTiles (colour 2F)))`. These are converted by hand.

They have a limited set of allowed predicates and constructions as the
domain is limited to this battleship board. As a result, they define a
simple context-free grammar that covers all possible valid programs.

Question Usefulness
-------------------

In the context of the battleship game, the human subjects were asked
to come up with 'useful' questions. They were given plenty of
training. It's great having a grammar to generate valid programs but
how many of them are actually useful? To that end, they define several
features. I'll describe these and skip over the probability model that
is optimized.

The first is the Expected Information Gain value of a question. That
is, how much uncertainty is reduced in knowing the actual board state
after the answers (averaged over multiple contexts) to that
question. So, questions which reveal more (on average) about the
actual state get a higher score.

The second is complexity. Favoring the above feature only can lead to
very long questions. So, this feature is thrown in to favor shorter
questions. This measures the complexity of a question based on the
grammar.

Another feature is the answer type. That is whether it is a yes/no
question or a location or a colour.

Conclusion
----------

What's interesting is that they were able to generate questions
(i.e. programs) given a context. It's interesting to tackle a problem
like this. It is similar to say a robot learning language with respect
to its senses. Learning can be very quick because the language has to
be *computed* unlike unsupervised language models.
