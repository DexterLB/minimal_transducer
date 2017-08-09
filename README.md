# minimal-transducer

This is a tool for creating a minimal subsequential transducer
from a given key-value dictionary. It is based on the paper
`S. Mihov, D. Maurel, Direct Construction of Minimal Acyclic
Subsequential Transducers, 2001` and is written in haskell.

By default, the executable reads the dictionary provided as
a first argument, constructs the transducer and goes into an
interactive prompt mode for dictionary lookup.

If the second argument is `-j`, the tool outputs a JSON
representation of the generated transducer to stdout.

It achieves about 3.5sec performance for an 90k word English
dictionary on a 3.0GHz Core i7.