
# Rinvent
## Intro Page
https://mkparkin.github.io/Rinvent/

## Release Notes

### Version: 1.0.1 Date=2023-02-20
-   Bug fix if there is no checkpoint file
### Version: 1.0.0 Date=2023-02-17
-   first push



# First Time Install

``` r
devtools::install_github("mkparkin/Rinvent",upgrade="never")
```
# example functions 
#### readparquetR
Objective is to read parquet or delta files with R.
Location of the file can be local, aws s3 or azure blob
There are useful parameters to filter the data while reading it
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

**Checlist before MR**

1.  clone the repo

2.  For the documentation, you can use "devtools::document()"

3.  Increase version number in DESCRIPTION

4.  Add what you did in README release notes

5.  build_site() for website update

#### this is it. to make sure it is working, you can click on "Build" in RStudio top menu then click "Clean and Rebuild"
