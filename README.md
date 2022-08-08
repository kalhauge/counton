# Counton

As a continuation of the [count words challenge](https://benhoyt.com/writings/count-words/), let's make Haskell
fast!

This repo is ordered primarily after different ways of counting in the `src/Count` folders
and modules. 

Hopefully, this can serve as an experimentation ground for different ways of counting items.

## Getting started 

1. Download nix with flakes
2. Run `nix develop`
  - This also installs the haskell language server and other nice things for development.
3. Run `cabal bench --enable-benchmarks -O2 --benchmark-options '--output index.html --csv stats/output.csv'`

## Benchmarks

Two kinds of benchmarks:

- `count`: Counting all words in the `data/kjvbible.txt`:
- `numbers`: Counting the same number of numbers as words in kjvbible `data/numbers.txt`

| Name                         |   Mean [ms] |   Stddev [ms] |
|:-----------------------------|------------:|--------------:|
| numbers/lengthBaseline       |        2.67 |          0.07 |
| kjvbible/lengthBaseline      |        3.96 |          0.08 |
| numbers/viaIntCounter        |        5.05 |          0.27 |
| numbers/viaUnboxedVectorHash |       22.03 |          1.19 |
| numbers/viaVectorHashMap     |       28.81 |          1.16 |
| numbers/viaFinite            |       45.27 |          2.03 |
| kjvbible/viaFinite           |       61.59 |          2.84 |
| kjvbible/viaVectorHashMap    |       68.81 |          2.8  |
| numbers/viaDiscrimination    |       85.74 |          4.55 |
| kjvbible/viaStrictHashMap    |       95.94 |          5.23 |
| numbers/viaStrictHashMap     |       97.46 |          6.79 |
| kjvbible/viaStrictMap        |      302.66 |          8.05 |
| numbers/viaIntMap            |      401.75 |         30.22 |
| kjvbible/viaDiscrimination   |      502.48 |         24.61 |
| numbers/viaStrictMap         |      545.15 |         15.31 |


## Contributions

Are welcome and encouraged!

### Mentions

- `data/kjvbible.txt` is borrowed from the https://github.com/benhoyt/countwords repository.
- `src/IntCounter.hs` is borrowed from Jaro Reinders (https://github.com/noughtmare/clutter/blob/main/src/IntCounter.hs)
