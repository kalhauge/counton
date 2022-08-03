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
3. Run `cabal bench --enable-benchmarks -O2 --benchmark-options '--output index.html'`

## Benchmarks

Counting all words in the `data/kjvbible.txt`:

```
benchmarking count/viaVectorHashMap
time                 99.49 ms   (94.43 ms .. 103.7 ms)
                     0.996 R²   (0.988 R² .. 1.000 R²)
mean                 96.28 ms   (94.18 ms .. 98.78 ms)
std dev              3.717 ms   (2.427 ms .. 4.750 ms)

benchmarking count/viaStrictHashMap
time                 139.9 ms   (135.4 ms .. 143.1 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 138.5 ms   (135.5 ms .. 140.9 ms)
std dev              3.644 ms   (2.192 ms .. 5.413 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking count/viaStrictMap
time                 437.7 ms   (425.6 ms .. 447.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 428.7 ms   (424.3 ms .. 432.9 ms)
std dev              4.887 ms   (4.160 ms .. 5.405 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking count/viaLazyMap
time                 586.7 ms   (567.1 ms .. 608.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 568.5 ms   (555.9 ms .. 575.1 ms)
std dev              12.09 ms   (5.715 ms .. 15.34 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking count/viaDiscrimination
time                 718.2 ms   (694.9 ms .. 740.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 696.2 ms   (669.9 ms .. 705.7 ms)
std dev              17.70 ms   (1.683 ms .. 22.03 ms)
variance introduced by outliers: 19% (moderately inflated)
```

## Contributions

Are welcome and encouraged!

## Mentions

- `data/kjvbible.txt` is borrowed from the https://github.com/benhoyt/countwords repository.
