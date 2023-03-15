# Rinvent 1.0.9
- Release 2023-03-15
- write_to_S3 function is added. This function provides a more robust saving mechanism, especially when writing out large files to Amazon S3.

# Rinvent 1.0.8

## Documentation

* Release 2023-03-12
* readparquetR: default value fix for partition parameter. "" to NULL


# Rinvent 1.0.7

## Documentation

* Release 2023-03-11
* readparquetR: partitioned parquet files can be read now
* readparquetR: bug fix in add_part_names parameter
* readparquetR: smart download in parquet format if partition is filled
* write_to_blob: new function
* date_mdy_ymd removed, date_to_yyyymmdd added

# Rinvent 1.0.6

## Documentation

* Release 2023-03-03

* Missing documentation pages are added

# Rinvent 1.0.5

## New Functions

* Release 2023-03-02

* fundamental small functions added

* read csv from azure function is added read_csv_azure

# Rinvent 1.0.4

## Documentation

* Release 2023-02-27

* documentation update for blob connection

# Rinvent 1.0.3

## Bug Fix

- Release 2023-02-25
- Bug fix in readparquetR. It was not reading if we want to read a single file with parquet name.


# Rinvent 1.0.2

## Documentation

* Release 2023-02-25

* New.md added. Example data added

## New Function added

* maR function is added


# Rinvent 1.0.1

## Bug fixes

* Release 2023-02-20

* Bug fix if there is no checkpoint file


# Rinvent 1.0.0

## First Push

* Release 2023-02-17

* first push
