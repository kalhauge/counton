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

Three kinds of benchmarks:

- `numbers`: Counting the same number of numbers as words in kjvbible `data/numbers.txt`
- `kjvbible`: Counting all words in the `data/kjvbible.txt`:
- `10xkjvbible`: Counting all words in the `data/kjvbible.txt`, 10 times:

### numbers

| Name                         |   Mean [ms] |   Stddev [ms] |
|:-----------------------------|------------:|--------------:|
| numbers/lengthBaseline       |        7.74 |          0.18 |
| numbers/viaIntCounter        |       10.73 |          1.04 |
| numbers/viaFinite            |       31.10 |          1.61 |
| numbers/viaUnboxedVectorHash |       31.13 |          2.31 |
| numbers/viaVectorHashMap     |       50.43 |          2.51 |
| numbers/viaDiscrimination    |       80.69 |          4.47 |
| numbers/viaStrictHashMap     |      113.74 |          7.63 |
| numbers/viaIntMap            |      451.08 |         17.36 |
| numbers/viaStrictMap         |      595.45 |         14.78 |

### kjvbible

| Name                          |   Mean [ms] |   Stddev [ms] |
|:------------------------------|------------:|--------------:|
| kjvbible/lengthBaseline       |        6.24 |          0.19 |
| kjvbible/viaFinite            |       51.56 |          3.43 |
| kjvbible/viaVectorHashMap     |       69.18 |          2.60 |
| kjvbible/viaStrictHashMap     |      278.48 |          6.07 |
| kjvbible/viaStrictMap         |      315.82 |          9.40 |
| kjvbible/viaDiscrimination    |      399.38 |         20.89 |

### 10xkjvbible

| Name                          |   Mean [ms] |   Stddev [ms] |
|:------------------------------|------------:|--------------:|
| 10xkjvbible/lengthBaseline    |       22.40 |          0.32 |
| 10xkjvbible/viaFinite         |      452.61 |         41.70 |
| 10xkjvbible/viaVectorHashMap  |      639.77 |         16.51 |
| 10xkjvbible/viaStrictHashMap  |     3159.40 |        250.81 |
| 10xkjvbible/viaDiscrimination |     3398.02 |         50.65 |
| 10xkjvbible/viaStrictMap      |     3563.63 |        100.53 |

### all

| Name                          |   Mean [ms] |   Stddev [ms] |
|:------------------------------|------------:|--------------:|
| kjvbible/lengthBaseline       |        6.24 |          0.19 |
| numbers/lengthBaseline        |        7.74 |          0.18 |
| numbers/viaIntCounter         |       10.73 |          1.04 |
| 10xkjvbible/lengthBaseline    |       22.40 |          0.32 |
| numbers/viaFinite             |       31.10 |          1.61 |
| numbers/viaUnboxedVectorHash  |       31.13 |          2.31 |
| numbers/viaVectorHashMap      |       50.43 |          2.51 |
| kjvbible/viaFinite            |       51.56 |          3.43 |
| kjvbible/viaVectorHashMap     |       69.18 |          2.60 |
| numbers/viaDiscrimination     |       80.69 |          4.47 |
| numbers/viaStrictHashMap      |      113.74 |          7.63 |
| kjvbible/viaStrictHashMap     |      278.48 |          6.07 |
| kjvbible/viaStrictMap         |      315.82 |          9.40 |
| kjvbible/viaDiscrimination    |      399.38 |         20.89 |
| numbers/viaIntMap             |      451.08 |         17.36 |
| 10xkjvbible/viaFinite         |      452.61 |         41.70 |
| numbers/viaStrictMap          |      595.45 |         14.78 |
| 10xkjvbible/viaVectorHashMap  |      639.77 |         16.51 |
| 10xkjvbible/viaStrictHashMap  |     3159.40 |        250.81 |
| 10xkjvbible/viaDiscrimination |     3398.02 |         50.65 |
| 10xkjvbible/viaStrictMap      |     3563.63 |        100.53 |


## Contributions

Are welcome and encouraged!

### Mentions

- `data/kjvbible.txt` is borrowed from the https://github.com/benhoyt/countwords repository.
- `src/IntCounter.hs` is borrowed from Jaro Reinders (https://github.com/noughtmare/clutter/blob/main/src/IntCounter.hs)
