
# Rinvent
## Intro Page
https://mkparkin.github.io/Rinvent/

## Release Notes

### Version: 1.0.0 Date=2023-02-17
-   first push



# First Time Install

``` r
devtools::install_github("mkparkin/Rinvent")
```
# example functions 
#### readparquetR
Objective is to read parquet or delta files with R.
Location of the file can be local, aws s3 or azure blob
There are useful parameters to filter the data while reading it
``` r
# read parquet from local with where condition in the partition
readparquetR(pathtoread="C:/users/...",
add_part_names=F,
sample=F,
where="sku=1 & store=1",
partition="2022")

#read local delta files
readparquetR(pathtoread="C:/users/...",
format="delta")

readparquetR(pathtoread="blobpath/subdirectory/",
filelocation = "azure",
format="delta",
containerconnection = your_connection) 

```



## Contributing

------------------------------------------------------------------------

**How to contribute to this package?**

1.  clone the repo

2.  when you add a new function, there will be three edits.

    i.e. let's say your function name is my_great_function \* add the
    function under "R"" folder. you can check an existing function and
    copy the template. file name will be my_great_function.R

3.  For the documentation, you can use "devtools::document()"

4.  make sure setwd is in library folder- this will be the help page of
    the function. the more detail you write, the better it is.

5.  Increase version number in DESCRIPTION

6.  Add what you did in README release notes

7.  build_site() for website update

#### this is it. to make sure it is working, you can click on "Build" in RStudio top menu then click "Clean and Rebuild"
