# Read me

## run code while developing

crepl to start the cabal repl (i.e. ghci)

```` 
commands
:r reload - after any change 
:l load
:m +Lib.OpenClass  -- zu add the symbols to context
:sh modules



````

The list of ignored hlint is in `Workspace11/.hlint.yaml`

The ignored markdown list is in `settings.json` - use ctrl `,` and go to `markdownlint.config`

# Goal of PostT

I want to construct the same statistics for a dozen regions of the world from public datasets.
I expect to be able to find, extract and format data for the countries and to combine them to the regional aggregates.

# Goal of *regions4countries*

*regions4countries* is a package, which provides the functions to 

- read WorldBank data files (in the CSV *wide format* )
- store the data in a SQLite database
- retrives data and computes statistics for regions

It provides the functionality to produce regional aggregates for arbitrary regions, which are defined as sets of countries. 

The R4C code is separated from its use to produce the statistics for the *next* book. It consists of 

- Model: The data types for Country, Region, Indicator and Observation with their subtypes. Typically keys are formed for Country, Region, Indicator by attaching "Id"; for Country and Indicator, the Worldbank codes are used as Text. 

    Currently Model.hs includes the instances ToField and FromField. TODO Move

- WorldBank: Parser to read the WorldBank CSV wide format files, which can be download and unpacked. The code is flexible to adapt to whatever version of the WorldBank format is provided. 


- Database: Maps the model into the SQL tables, to store and retrieve data into Haskell lists.

- Orchestrator: Connects the functions from WorldBank (reading data files) with Database (storing the data)


- Aggregate: Retrieves data as CountryTable from the database and calculates sum, mean and weighted average (so far.)


For each study, a folder with the specific setup. BaseTest is 2 datasets for testing (will be included in distribution). PT is the study I focus on now.  

- Indicator: Descriptions of the datasets in Haskell format - not stored in the database. This file evolves: for each dataset added an indicator must be added as well. 

- Region: Definitions of the regions as lists of countries. Regions can overlap and are defined for a specific use of the library.




For the *next* book (folder Study) I should have for each tableau a file which states, which dataset are used.

 (how to test if a dataset is alread present - to avoid duplication in storage and stop processing if not available)

then produce each column with the aggregate function and other processing (e.g. scaling) and then combine them into a markdown table for inclusion in book.

# Chat summary -  Regions4Countries (R4C) – Project Summary

## Goal

A Haskell library for importing World Bank indicator data, storing it in SQLite, and computing regional aggregates (sum, weighted average, etc.). The library should be reusable; test code and applications should be kept separate.

## Current module structure

* `R4C.Model`

  * Core data types:

    * `CountryId`
    * `RegionId`
    * `IndicatorId`
    * `Year`
    * `Value`
    * `Observation`
    * `YearValue`
* `R4C.WorldBank`

  * Parse World Bank CSV files.
  * Support both:

    * `Indicator Name` / `Indicator Code`
    * `Series Name` / `Series Code`
  * Skip metadata rows automatically.
* `R4C.Database`

  * SQLite schema creation.
  * Insert observations.
* `R4C.Query`

  * Query functions returning observations and year/value series.
* `R4C.Aggregate`

  * Aggregation functions:

    * `aggregate`
    * `weightedAverage`
* `R4C.Region`

  * Region definitions.
* `R4C.Indicator`

  * Indicator definitions.
* `R4C.Orchestrator`

  * Imports one or more World Bank files into a database.
  * Keep this module for now.

## Region model

Regions are data, not constructors.

Example:

```haskell
RegionId "EU"
RegionId "G7"
RegionId "GULF"
```

Membership is represented as

```haskell
[(RegionId, CountryId)]
```

using a helper

```haskell
mk :: Text -> [Text] -> [(RegionId, CountryId)]
```

Overlapping regions are allowed.

## Indicators

Indicators are identified by World Bank codes, for example

```haskell
IndicatorId "SP.POP.TOTL"
IndicatorId "AG.SRF.TOTL.K2"
```

`R4C.Indicator` provides named constants such as `population`.

## Testing

Using **tasty** + **tasty-hunit**.

Directory:

```text
test/
    Spec.hs
    WorldBankSpec.hs
    AggregateSpec.hs
    OrchestratorSpec.hs
    RegionSpec.hs
    IndicatorSpec.hs
    DatabaseSpec.hs

test/data/
```

`Spec.hs` imports each test module qualified and combines them into one `TestTree`.

Current integration test:

* import population and surface-area World Bank CSV files
* query Austria's surface area
* verify expected values with assertions rather than `print`

Tests should use

```haskell
actual @?= expected
```

or

```haskell
assertBool "message" condition
```

instead of printing output.

## Current development priorities

1. from the three (multiple) region - value lists, produce a markdown table for printing.
    requires scaling to values as integers and produce a table header with G or M units (eg M$) 
    produce markdown layout with header row
4. Later add a command-line application in `app/`.
5. Continue implementing regional aggregation and indicator support.

## Design principles

* Keep the library independent of the application.
* Test pure functions first.
* Use integration tests only where necessary.
* Prefer real World Bank sample files in `test/data` over embedded CSV strings.
* Use an SQLite test database for integration tests.
* Avoid hard-coded paths in library code.
* use indent 4 spaces
* prefer 'where' instead of 'let' constructions 
