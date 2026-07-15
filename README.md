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
# Regions4Countries – Project Summary

## Purpose

Regions4Countries is a Haskell library for working with country-level indicator data, primarily from the World Bank. It provides facilities to import, store, query and aggregate observations while keeping the library independent from any particular application.

---

## Project structure

### `R4C.Model`

Core domain types.

Main entities:

* `Country`
* `Region`
* `Indicator`
* `Observation`

Identifiers are lightweight wrappers around `Text`:

```haskell
CountryId
RegionId
IndicatorId
```

### Indicators

```haskell
Indicator
    { indicatorId         :: IndicatorId
    , indicatorName       :: Text
    , sourceNote          :: Text
    , sourceOrganization  :: Text
    , aggregation         :: Aggregation
    }
```

Current aggregation model:

```haskell
data Aggregation
    = Sum
    | Mean
    | WeightedBy IndicatorId
```

Indicators are imported from the World Bank metadata file with

```haskell
aggregation = Sum
```

The aggregation value represents application semantics rather than World Bank metadata.

---

## `R4C.WorldBank`

Responsible only for reading World Bank data.

Supports parsing of:

### Observation files

```
API_....csv
```

Returns

```haskell
[Observation]
```

### Country metadata

```
Metadata_Country_....csv
```

Returns

```haskell
[Country]
```

### Indicator metadata

```
Metadata_Indicator_....csv
```

Returns

```haskell
Indicator
```

### World Bank archive

A World Bank download ZIP archive contains

* one observation CSV
* one country metadata CSV
* one indicator metadata CSV

The module provides

```haskell
readArchive
    :: FilePath
    -> IO WorldBankArchive
```

where

```haskell
WorldBankArchive
    { archiveIndicator
    , archiveCountries
    , archiveObservations
    }
```

`readArchive` is implemented by extracting the three CSV files and reusing the individual parsers.

The individual parsers remain public so that World Bank CSV files can also be used independently of ZIP archives.

---

## `R4C.Database`

Responsible only for persistence.

Creates the SQLite schema.

Stores

* countries
* indicators
* observations

Provides functions such as

```haskell
insertCountry
insertCountries

insertIndicator

insertObservation
insertObservations

countries4db
indicators4db
observations
```

The database is viewed as the application's data store rather than a direct mirror of the World Bank files.

Country information may later be enriched (for example German names) without changing the World Bank importer.

---

## `R4C.Query`

Provides query functions returning observations and year/value series.

---

## `R4C.Aggregate`

Provides aggregation functions including

```haskell
aggregate
weightedAverage
```

---

## `R4C.Region`

Defines regions as data rather than constructors.

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

using

```haskell
mk
    :: Text
    -> [Text]
    -> [(RegionId, CountryId)]
```

Overlapping regions are allowed.

---

## `R4C.Orchestrator`

Coordinates imports but contains as little logic as possible.

Primary function:

```haskell
importArchive
    :: Connection
    -> FilePath
    -> IO ()
```

Workflow:

1. read a World Bank ZIP archive
2. insert indicator metadata
3. insert country metadata
4. insert observations

The orchestrator delegates parsing to `R4C.WorldBank` and persistence to `R4C.Database`.

Higher-level convenience functions (for importing directories of archives) may be added later.

---

## Testing

Framework:

* tasty
* tasty-hunit

Directory layout:

```
test/
    Spec.hs
    WorldBankSpec.hs
    DatabaseSpec.hs
    AggregateSpec.hs
    RegionSpec.hs
    IndicatorSpec.hs
    OrchestratorSpec.hs

test/testdata/
```

Tests use real World Bank sample files rather than embedded CSV strings.

SQLite integration tests use

```haskell
open ":memory:"
createSchema conn
```

Assertions use

```haskell
actual @?= expected
```

or

```haskell
assertBool
```

rather than printing values.

---

## Design principles

* Keep the library independent of applications.
* Separate parsing, persistence and orchestration.
* Prefer small, composable functions.
* Keep parsers usable independently of archive imports.
* Test pure functions first.
* Use integration tests only where appropriate.
* Use real World Bank sample files.
* Use SQLite for persistence.
* Use `package.yaml`.
* Package name: `Regions4Countries`.
* Repository:

```
git@github.com:andrewufrank/regions4countries.git
```

* Prefer four-space indentation.
* Prefer `where` over `let`.


