# FathomR : Easy manipulation of Innovasea's Fathom file formats <img src="vignettes/FathomR_hex_logo.png" align="right" width="120" />

Toolkit of functions to help convert Innovasea's receiver logfiles and parsing Fathom interleaved csv format. 

## Overview

`FathomR` is a toolkit of functions designed to ease the conversion of Innovasea's proprietary file formats and wrangling of user readable csv or paraquet files. The package contains functions for converting receiver logfiles in the legacy .vrl format when receivers were downloaded from VUE, and the latest generation .vdat format when receivers were downloaded using Fathom Connect or Fathom Mobile. Once converetd, there are several functions for easy parsing of the interleave Fathom csv, Fathom csv split by record type, and parquet file format.  

To assist with first step of exploring detection data a False Detection Analysis (FDA) has been included to help identify false positive detections and methods for removing them from the detection records prior to further analysis. 

## Main functions

### Convert Innovasea Receiver Logfiles

`convert_log_files()`

Converts Innovasea's acoustic receiver logfile formats (.vrl, .vdat) to csv or parquet file formats. Conversion of receiver logfiles requires either the instillation of Innovasea's Fathom Connect software or access to the standalone 'vdat.exe' executable. Files converted to Fathom's interleaved csv will contain all records type within a single file, files converted to Fathom split csv format will result in a folder for each logfile with separate csv files for each record type. Files converted to parquet format will result in a folder for each logfile with separate paraquet files for each record type.  

### View Fathom csv Schema

`peek_fathom_schema()`

Peek into the Fathom interleaved csv schema and extract summary information of the record types expected from parse_fathom_files(). Summary reports can include samples of the interleaved data for users to inspect variable class. 

### Parse Fathom csv file formats

`parse_fathom_files()`

Parsing of Fathom's interleaved csv file format where each record type if contained within a single file. Produces a list of named tibbles, one for each record type and appends records from each csv file located within the file path.  

`parse_split_fathom_csv_files()`

Parsing of Fathom's export of csv files which are split by record type into separate subfolders. Working with the csv split format allows users to parse specific record types into memory without parsing all possible record types.  

`parse_fathom_parquet_files()`

Parsing of Fathom's exported parquet file format which are split by record type into separate subfolders. Working with parquey file formats has performance advantages over csv for large file size and provides options for parsing specific record types without parsing all available parquet files. 

### False Detection Analysis

`flag_false_detections()`
False Detection Analysis function which identifies false positive detection based on a revised version of the Pincock (2012) method, isolating single detections and comparing the ratio of short to long periods between successive deetctions across one or multiple receivers. Appends a 'false_positive_flag' variable to the input dataframe and provides a summary of flagged false positives. 

# Installation

- **R** `FathomR` requires`R` version ≥ 4.1 which can be installed from **R-project [`R`](https://www.r-project.org)**. If you are unsure of your R version, try `R.version.string` from the `R` console. To update R from within RStudio, install and load the `installr` package and run `updateR()`

- **Install remotes**
``` 
# Install the remotes package
install.packages("remotes")

# Use remotes to install the FathomR package from github repo
remotes::install_github("hughpederson/FathomR", 
                        build_vignettes = TRUE,
                        dependencies = TRUE)
```

