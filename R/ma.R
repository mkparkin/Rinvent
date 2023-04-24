#' @title moving average of a column
#' @description moving average of he column. if there are not enough records, it will take average whatever it can. i.e. if period_cound=5 but data only has 3 values
#' it will take average of three.
#' don't forget to sort the data frame. most probably based on date
#' @param value column name to take average
#' @param period_count average period count
#' @param gap timelag between avg value vs row. i.e. if you want to avg with one day lag, type gap=1. default is 1.
#' @param align average from past="right" average from future="left". default is right.
#' @examples exampleDataR= data.table::as.data.table(exampleDataR)
#' exampleDataR = exampleDataR[order(location,item,date)]
#' exampleDataR[,ma5:= maR(sales,4),list(location,item)]
#' head(exampleDataR,15)
#' @import data.table
#' @export maR



maR <- function(value, period_count, align="right", gap=1) {

  nk <- c(seq.int(pmin(period_count, length(value))), rep(period_count, pmax(0, length(value) - period_count)))
  lenght_nk = length(nk)
  switch(align,
         left={
           head(c(rep(NA,gap),rev(data.table::frollmean(rev(value), nk, align="right", adaptive=TRUE))),lenght_nk)
         },
         right={
           head(c(rep(NA,gap),data.table::frollmean(value, nk, align="right", adaptive=TRUE)),lenght_nk)
         })
}

