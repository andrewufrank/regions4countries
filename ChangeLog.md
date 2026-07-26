# ChangeLog for EconomyDiffEq

0.0.5 changed the organisation of the region table to include the data and summmarize later 

0.0.4 add the country name etc. to the database 
    all tables have (Maybe Double) to indicate where observations are missing
0.0.3 separate folder for each study. (baseTest, pt)
    produce md table (which can be sorted)
    add makedb to create a new db 
    change structure of worldBank: keep zip files in book
        automatically expand in a non-synced dir and load from there the cvs file
        (not split for different topics!)

a table 
| Region | Population 2024 (M) | Surface 2023 (M km²) | Surface per capita (ha/person) |
|---|---|---|---|
| ANZ | 32.5 | 8.01 | 24.7 |
| RUSSIA | 143.7 | 17.13 | 11.9 |
| CENTRAL_ASIA | 82.3 | 4.01 | 4.9 |
| SAMERICA | 658.5 | 20.50 | 3.1 |
| NORTH_AFRICA | 271.5 | 7.63 | 2.8 |
| GULF | 239.1 | 5.30 | 2.2 |
| SUBSAHARA | 1240.7 | 22.49 | 1.8 |
| EUROPE | 593.1 | 7.16 | 1.2 |
| FAREAST | 775.7 | 6.28 | 0.8 |
| CHINA | 1409.0 | 9.56 | 0.7 |
| SOUTH_ASIA | 519.0 | 1.81 | 0.3 |
| JAPAN | 124.0 | 0.38 | 0.3 |
| INDIA | 1450.9 | 3.29 | 0.2 |

0.0.2 separate the R4C library from code specific for *next* book. 


0.0.1 initial copy and changed package  
    reads population and surface area data for testing
    produces aggregates not completely tested


a table 
| Region | Population 2024 (M) | Surface 2023 (M km²) | Surface per capita (ha/person) |
|---|---|---|---|
| USCAN | 0.0 | 25.47 | 23150836.4 |
| ANZ | 32.5 | 8.01 | 24.7 |
| RUSSIA | 143.7 | 17.13 | 11.9 |
| CENTRAL_ASIA | 82.3 | 4.01 | 4.9 |
| SAMERICA | 658.5 | 20.50 | 3.1 |
| NORTH_AFRICA | 271.5 | 7.63 | 2.8 |
| GULF | 239.1 | 5.30 | 2.2 |
| SUBSAHARA | 1240.7 | 22.49 | 1.8 |
| EUROPE | 593.1 | 7.16 | 1.2 |
| FAREAST | 775.7 | 6.28 | 0.8 |
| CHINA | 1409.0 | 9.56 | 0.7 |
| SOUTH_ASIA | 519.0 | 1.81 | 0.3 |
| JAPAN | 124.0 | 0.38 | 0.3 |
| INDIA | 1450.9 | 3.29 | 0.2 |
