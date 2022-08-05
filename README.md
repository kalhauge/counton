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

### Kjvbible

Counting all words in the `data/kjvbible.txt`:

```
benchmarking count/lengthBaseline
time                 4.824 ms   (4.806 ms .. 4.840 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 4.843 ms   (4.827 ms .. 4.887 ms)
std dev              77.77 μs   (36.67 μs .. 154.5 μs)

benchmarking count/viaVectorHashMap
time                 94.01 ms   (91.38 ms .. 100.6 ms)
                     0.997 R²   (0.989 R² .. 1.000 R²)
mean                 95.45 ms   (94.44 ms .. 97.05 ms)
std dev              2.044 ms   (1.024 ms .. 3.282 ms)

benchmarking count/viaStrictHashMap
time                 140.8 ms   (135.9 ms .. 144.6 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 140.6 ms   (138.7 ms .. 142.7 ms)
std dev              2.850 ms   (1.944 ms .. 4.569 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking count/viaStrictMap
time                 444.2 ms   (436.9 ms .. 454.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 437.4 ms   (433.0 ms .. 440.7 ms)
std dev              4.392 ms   (2.529 ms .. 5.420 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking count/viaLazyMap
time                 581.1 ms   (573.6 ms .. 588.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 566.0 ms   (555.4 ms .. 571.4 ms)
std dev              10.03 ms   (65.44 μs .. 12.31 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking count/viaDiscrimination
time                 722.2 ms   (667.9 ms .. 759.6 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 705.4 ms   (687.4 ms .. 714.8 ms)
std dev              17.09 ms   (7.782 ms .. 21.63 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Numbers

Same number of numbers as words in kjvbible.

```
benchmarking numbers/lengthBaseline
time                 3.182 ms   (3.119 ms .. 3.240 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.208 ms   (3.180 ms .. 3.239 ms)
std dev              94.37 μs   (75.04 μs .. 137.1 μs)
variance introduced by outliers: 14% (moderately inflated)

benchmarking numbers/viaVectorHashMap
time                 26.39 ms   (24.91 ms .. 27.83 ms)
                     0.989 R²   (0.975 R² .. 0.999 R²)
mean                 26.71 ms   (26.16 ms .. 27.99 ms)
std dev              1.751 ms   (919.4 μs .. 3.001 ms)
variance introduced by outliers: 26% (moderately inflated)

benchmarking numbers/viaStrictHashMap
time                 98.41 ms   (88.29 ms .. 105.4 ms)
                     0.991 R²   (0.977 R² .. 0.999 R²)
mean                 97.32 ms   (92.60 ms .. 100.7 ms)
std dev              6.537 ms   (4.069 ms .. 10.34 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking numbers/viaStrictMap
time                 533.8 ms   (470.9 ms .. 582.3 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 523.8 ms   (506.8 ms .. 534.1 ms)
std dev              17.07 ms   (7.909 ms .. 22.93 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking numbers/viaLazyMap
time                 677.4 ms   (628.2 ms .. 765.0 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 661.5 ms   (651.7 ms .. 670.3 ms)
std dev              12.09 ms   (5.721 ms .. 16.84 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking numbers/viaDiscrimination
time                 87.27 ms   (82.14 ms .. 91.42 ms)
                     0.991 R²   (0.967 R² .. 0.999 R²)
mean                 85.13 ms   (82.77 ms .. 89.25 ms)
std dev              5.268 ms   (2.815 ms .. 8.732 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking numbers/viaIntMap
time                 402.6 ms   (351.2 ms .. 458.3 ms)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 395.1 ms   (386.3 ms .. 403.0 ms)
std dev              9.220 ms   (7.601 ms .. 10.54 ms)
variance introduced by outliers: 19% (moderately inflated)
```

## Contributions

Are welcome and encouraged!

## Mentions

- `data/kjvbible.txt` is borrowed from the https://github.com/benhoyt/countwords repository.
