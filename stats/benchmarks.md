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