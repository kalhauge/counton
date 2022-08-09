### numbers

| Name                         |   Mean [ms] |   Stddev [ms] |
|:-----------------------------|------------:|--------------:|
| numbers/lengthBaseline       |        7.74 |          0.18 |
| numbers/viaIntCounter        |       10.73 |          1.04 |
| numbers/viaFinite            |       31.1  |          1.61 |
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
| 10xkjvbible/lengthBaseline    |       22.4  |          0.32 |
| kjvbible/viaFinite            |       51.56 |          3.43 |
| kjvbible/viaVectorHashMap     |       69.18 |          2.6  |
| kjvbible/viaStrictHashMap     |      278.48 |          6.07 |
| kjvbible/viaStrictMap         |      315.82 |          9.4  |
| kjvbible/viaDiscrimination    |      399.38 |         20.89 |
| 10xkjvbible/viaFinite         |      452.61 |         41.7  |
| 10xkjvbible/viaVectorHashMap  |      639.77 |         16.51 |
| 10xkjvbible/viaStrictHashMap  |     3159.4  |        250.81 |
| 10xkjvbible/viaDiscrimination |     3398.02 |         50.65 |
| 10xkjvbible/viaStrictMap      |     3563.63 |        100.53 |

### 10xkjvbible

| Name                          |   Mean [ms] |   Stddev [ms] |
|:------------------------------|------------:|--------------:|
| 10xkjvbible/lengthBaseline    |       22.4  |          0.32 |
| 10xkjvbible/viaFinite         |      452.61 |         41.7  |
| 10xkjvbible/viaVectorHashMap  |      639.77 |         16.51 |
| 10xkjvbible/viaStrictHashMap  |     3159.4  |        250.81 |
| 10xkjvbible/viaDiscrimination |     3398.02 |         50.65 |
| 10xkjvbible/viaStrictMap      |     3563.63 |        100.53 |

### all

| Name                          |   Mean [ms] |   Stddev [ms] |
|:------------------------------|------------:|--------------:|
| kjvbible/lengthBaseline       |        6.24 |          0.19 |
| numbers/lengthBaseline        |        7.74 |          0.18 |
| numbers/viaIntCounter         |       10.73 |          1.04 |
| 10xkjvbible/lengthBaseline    |       22.4  |          0.32 |
| numbers/viaFinite             |       31.1  |          1.61 |
| numbers/viaUnboxedVectorHash  |       31.13 |          2.31 |
| numbers/viaVectorHashMap      |       50.43 |          2.51 |
| kjvbible/viaFinite            |       51.56 |          3.43 |
| kjvbible/viaVectorHashMap     |       69.18 |          2.6  |
| numbers/viaDiscrimination     |       80.69 |          4.47 |
| numbers/viaStrictHashMap      |      113.74 |          7.63 |
| kjvbible/viaStrictHashMap     |      278.48 |          6.07 |
| kjvbible/viaStrictMap         |      315.82 |          9.4  |
| kjvbible/viaDiscrimination    |      399.38 |         20.89 |
| numbers/viaIntMap             |      451.08 |         17.36 |
| 10xkjvbible/viaFinite         |      452.61 |         41.7  |
| numbers/viaStrictMap          |      595.45 |         14.78 |
| 10xkjvbible/viaVectorHashMap  |      639.77 |         16.51 |
| 10xkjvbible/viaStrictHashMap  |     3159.4  |        250.81 |
| 10xkjvbible/viaDiscrimination |     3398.02 |         50.65 |
| 10xkjvbible/viaStrictMap      |     3563.63 |        100.53 |

