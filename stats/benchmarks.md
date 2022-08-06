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