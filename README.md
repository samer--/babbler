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
