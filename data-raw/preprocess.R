
# Load raw data from .csv file
exampleDataR <- read.csv("data-raw/salesdatasample.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(exampleDataR,overwrite = T)



