| Name                         |   Mean [ms] |   Stddev [ms] |
|:-----------------------------|------------:|--------------:|
| numbers/lengthBaseline       |        2.75 |          0.06 |
| kjvbible/lengthBaseline      |        4.23 |          0.25 |
| numbers/viaUnboxedVectorHash |       22.81 |          0.73 |
| numbers/viaVectorHashMap     |       29.83 |          1.5  |
| kjvbible/viaVectorHashMap    |       88.56 |          7.44 |
| numbers/viaDiscrimination    |       98.94 |          8.26 |
| numbers/viaStrictHashMap     |      103.16 |          6.31 |
| kjvbible/viaStrictHashMap    |      105.71 |          3.19 |
| kjvbible/viaStrictMap        |      319.47 |          7.96 |
| numbers/viaIntMap            |      422.99 |         13.11 |
| kjvbible/viaLazyMap          |      435.35 |         25.05 |
| kjvbible/viaDiscrimination   |      522.6  |         21.68 |
| numbers/viaStrictMap         |      563.08 |          5.3  |
| numbers/viaLazyMap           |      719.31 |         15.18 |