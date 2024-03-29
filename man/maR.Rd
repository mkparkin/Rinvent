% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ma.R
\name{maR}
\alias{maR}
\title{moving average of a column}
\usage{
maR(value, period_count, align = "right", gap = 1)
}
\arguments{
\item{value}{column name to take average}

\item{period_count}{average period count}

\item{align}{average from past="right" average from future="left". default is right.}

\item{gap}{timelag between avg value vs row. i.e. if you want to avg with one day lag, type gap=1. default is 1.}
}
\description{
moving average of he column. if there are not enough records, it will take average whatever it can. i.e. if period_cound=5 but data only has 3 values
it will take average of three.
don't forget to sort the data frame. most probably based on date
}
\examples{
exampleDataR= data.table::as.data.table(exampleDataR)
exampleDataR = exampleDataR[order(location,item,date)]
exampleDataR[,ma5:= maR(sales,4),list(location,item)]
head(exampleDataR,15)
}
