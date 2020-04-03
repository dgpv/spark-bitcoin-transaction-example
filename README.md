# Example of Bitcoin transaction parsing with SPARK

## Purpose

This was made to educate myself on the topic of formally proved code, Ada and SPARK.

I succeeded in specifying and proving important properties of not entirely trivial
code, and thus I consider my self-study of the topic successful.

The code and approaches used in it need better description and explanation,
but I cannot dedicate time to do this right now, so I'm just releasing it as it is.

I hope that this might be useful to someone, even in this 'raw' state, without an
accompanying detailed explanation.

## Proved properties

The parsing code can be proven free of runtime errors with
GNAT community 2019 edition (https://blog.adacore.com/gnat-community-2019-is-here).
It needs quite a lot of resources, though -- at least 32GB free memory, and may take
8 hours to complete when run with a single thread (`-j1` option in `bitcoin.gpr`)

There's currently still tiny part of unproven code in `test_transactions.adb`,
but that code deals with printing the results of the parsing, not the parsing itself.
I plan to finish that when I have time.

In addition to absence of runtime errors, a number of functional
properties of the code is also proven, such as the adherence of the number of
bytes read to the size of the resulting structures after parsing.

Equivalence of the data bytes read from the medum with the data stored in the
resulting structures is also proven to certain extent (for sub-components of the transaction,
but not for the whole transaction structure). This can be proven for the whole transaction,
too, but it becomes really heavy for the prover the further up the structure hierarchy you
get with this checks -- because you have to use a 'virtual data tape' to track the data read.
At the same time, the approach to the proof of this property resembles the approach to
the proof of data size equivalence property, so there's not much to be gained from finishing
this experience-wise.

## Misc 

The interesting place to start looking at the code might be the `Deserialize` procedure
in `bitcoin_like-transactions.ads` (and corresponding `.adb`) and the postconditions for this
procedure.

There's also a bit of serialization code present, but it was not developed past some basic structures.

The code does not use dynamic memory, and allocates static structures at the start, and this
is not very practical. For real application you'll likely want to either allocate memory dynamically,
or make the parsing 'streamed', where you parse one sub-structure at a time and collect the
needed data as you go. The goal was not to make totally-practical code from the start, but
some of the code and approaches can be reused to create practical safe and secure code to
parse bitcoin transactions.
