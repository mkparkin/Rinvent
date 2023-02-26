
<!-- README.md is generated from README.Rmd. Please edit README.Rmd file -->

# Rinvent

## Intro Page

<https://mkparkin.github.io/Rinvent/>

# First Time Install

``` r
# install.packages("devtools")
devtools::install_github("mkparkin/Rinvent",upgrade="never")
```

# example functions

#### readparquetR

Objective is to read parquet or delta files with R. Location of the file
can be local, aws s3 or azure blob There are useful parameters to filter
the data while reading it

``` r
# read parquet from local with where condition in the partition
readparquetR(pathtoread="C:/users/...", add_part_names=F, sample=F, where="sku=1 & store=1", partition="2022")

#read local delta files
readparquetR(pathtoread="C:/users/...", format="delta")

your_connection = AzureStor::storage_container(AzureStor::storage_endpoint(your_link, key=your_key), "your_container")

readparquetR(pathtoread="blobpath/subdirectory/", filelocation = "azure", format="delta", containerconnection = your_connection) 
```

## Contributing

------------------------------------------------------------------------

**Checklist before MR**

1.  clone the repo

2.  Increase version number in DESCRIPTION

3.  Add what you did in NEWS.md release notes

4.  For the documentation, you can use “devtools::document()”

5.  Run devtools::build_readme() to update README.md

6.  pkgdown::build_site() for website update

#### this is it. to make sure it is working, you can click on “Build” in RStudio top menu then click “Clean and Rebuild”
