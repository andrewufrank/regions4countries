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



The following modules are specific for some use of the library. A minimal test case should be included.

- Indicator: Descriptions of the datasets in Haskell format - not stored in the database. This file evolves: for each dataset added an indicator must be added as well. 

- Region: Definitions of the regions as lists of countries. Regions can overlap and are defined for a specific use of the library.

For testing so far: 

- Query

For the *next* book I should have for each tableau a file which states, which dataset are used.

 (how to test if a dataset is alread present - to avoid duplication in storage and stop processing if not available)

then produce each column with the aggregate function and other processing (e.g. scaling) and then combine them into a markdown table for inclusion in book.