# babbler
Randomly sampling vaguely grammatical nonsense since 2009

This repository contains some experiments in sampling English
text with stylistic structure (but very little semantic sense).
It is built on a fairly complex fragment of English grammar
encoded in a 3 different ways:

    * `grammar.pl` -- A fairly straightforward representation as a DCG
    * `grammar_attr.pl` -- An attribute or feature grammar using graph unification
    * `grammar_gap.pl` -- Using the gap threading version of the sampling meta-interpreter from sampledcg

The latter two are untested since several years ago and probably don't work.
The graph unification algorithm is based on code by Bob Carpenter.

It requires the Prolog lexical databases which are available at
    https://code.soundsoftware.ac.uk/hg/plex

Also requires the following SWI Prolog packs:

    plrand
    genutils
    textutils
    dcgutils
    sampledcg (available at https://github.com/samer--/sampledcg)
    

Some sample outputs can be found in the examples directory.

The top level file poetry.pl can be set to use either biglex or
littlelex (from the plex repository referenced above). The former
uses a big lexicon derived mainly from the Oxford Advanced Learners
Dictionary, and therefore has lots of great words in it, but can
result in a somewhat shotgun approach to vocabulary selection. The
smaller lexicon was handwritten.

Sample session:

    $ swipl -s poetry.pl
    ?- main(100,s).
    >> resample_probs(dirichlet(2)).
    >> render(fmt,rep(5,line(s))).

This will (1) generate and throw away 100 random sentences, (2) enter
a stateful shell that keeps track of probability distributions over
choices in the grammar, initialised with the choices made while sampling
the initial 100 sentences, (3) sample new probability distributions for
all known distributions from a Dirichlet process with concentration
parameter 3 and (4) sample 5 lines from the resulting grammar.
I take no responsibility for any of the nonsense and occasional filth
that might emerge from this.

The 'renderer' `fmt` writes the output to the terminal. If you are using
a Mac, you can get the machine to recite the output as well by using
`fmt && async(say(Voice))` where `Voice` is one of the voice names.
You can log the output to a file with `fmt && append(Filename)`.
See `render/2` in the textutils pack for more information.
