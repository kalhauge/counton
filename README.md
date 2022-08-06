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

### Kjvbible

Two kinds of benchmarks:

- `count`: Counting all words in the `data/kjvbible.txt`:
- `numbers`: Counting the same number of numbers as words in kjvbible `data/numbers.txt`


| Name                         |   Mean [ms] |   Stddev [ms] |
|:-----------------------------|------------:|--------------:|
| numbers/lengthBaseline       |        3.36 |          0.02 |
| kjvbible/lengthBaseline      |        4.86 |          0.03 |
| numbers/viaIntCounter        |        7.91 |          0.37 |
| numbers/viaUnboxedVectorHash |       32.02 |          0.38 |
| numbers/viaVectorHashMap     |       44.92 |          2.51 |
| kjvbible/viaVectorHashMap    |      104.74 |          5.04 |
| numbers/viaDiscrimination    |      137.95 |          3.97 |
| numbers/viaStrictHashMap     |      156.28 |          6.84 |
| kjvbible/viaStrictHashMap    |      163.86 |         12.36 |
| kjvbible/viaStrictMap        |      534.48 |         23.67 |
| numbers/viaIntMap            |      628.55 |          6.87 |
| kjvbible/viaDiscrimination   |      819.17 |         47.02 |
| numbers/viaStrictMap         |      836.98 |          7.41 |

## Contributions

Are welcome and encouraged!

## Mentions

- `data/kjvbible.txt` is borrowed from the https://github.com/benhoyt/countwords repository.
- `src/IntCounter.hs` is borrowed from Jaro Reinders (https://github.com/noughtmare/clutter/blob/main/src/IntCounter.hs)
