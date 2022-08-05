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
| numbers/lengthBaseline       |        2.75 |          0.06 |
| kjvbible/lengthBaseline      |        4.23 |          0.25 |
| numbers/viaUnboxedVectorHash |       22.81 |          0.73 |
| numbers/viaVectorHashMap     |       29.83 |          1.50 |
| kjvbible/viaVectorHashMap    |       88.56 |          7.44 |
| numbers/viaDiscrimination    |       98.94 |          8.26 |
| numbers/viaStrictHashMap     |      103.16 |          6.31 |
| kjvbible/viaStrictHashMap    |      105.71 |          3.19 |
| kjvbible/viaStrictMap        |      319.47 |          7.96 |
| numbers/viaIntMap            |      422.99 |         13.11 |
| kjvbible/viaLazyMap          |      435.35 |         25.05 |
| kjvbible/viaDiscrimination   |      522.60 |         21.68 |
| numbers/viaStrictMap         |      563.08 |          5.30 |
| numbers/viaLazyMap           |      719.31 |         15.18 |

## Contributions

Are welcome and encouraged!

## Mentions

- `data/kjvbible.txt` is borrowed from the https://github.com/benhoyt/countwords repository.
