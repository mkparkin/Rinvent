#' @title Convert date string to YYYY-MM-DD format
#' @description This function attempts to detect the format of a date string and convert it to the YYYY-MM-DD format.
#' @param date_str A character string representing a date in an unknown format.
#' @param forceformat  to make this effective, smart_format should be False
#' @param smart_format if it is T, then it will try to detect format. otherwise force format
#' @examples date_to_yyyymmdd('2022-02-28')
#' @examples date_to_yyyymmdd('02/28/2022')
#' @examples date_to_yyyymmdd('28.02.2022')
#' @examples date_to_yyyymmdd('Feb 28, 2022')
#' @export date_to_yyyymmdd

date_to_yyyymmdd <- function(date_str, smart_format=T, forceformat='%Y.%m.%d') {
  output <- NA

  if(smart_format){
  date_formats <- c('%Y-%m-%d', '%m/%d/%Y', '%d/%m/%Y', '%m-%d-%Y', '%d-%m-%Y', '%Y/%m/%d', '%Y.%m.%d', '%d.%m.%Y', '%Y%m%d', '%b %d, %Y')
  format_str=date_formats[7]
  for (format_str in date_formats) {
    try_date <- try(as.Date(date_str, format = format_str))
    if(!is.na(try_date)){
      output=try_date
    }
  }
  }else{
    output = try(as.Date(date_str, format = forceformat))
  }
  output
}

#' @title find latest sunday
#' @description find latest sunday date
#' @param date date value
#' @examples exampleDataR= data.table::as.data.table(exampleDataR)
#' exampleDataR = exampleDataR[order(location,item,date)]
#' exampleDataR = exampleDataR[,date:= date_mdy_ymd(date)]
#' exampleDataR[,week_start_date:= week_start(date)]
#' head(exampleDataR,15)
#' @import data.table
#' @export week_start

week_start = function(date){
  date - data.table::wday(date) + 1
}




#' @title table cross join
#' @description table cross join
#' @param t1 table 1
#' @param t2 table 2
#' @return crossjoin()
#' @examples  exampleDataR = data.table::as.data.table(exampleDataR)
#' location_list = exampleDataR[,.N,list(location)]
#' item_list = exampleDataR[,.N,list(item)]
#' crossjoin(t1=location_list, t2=item_list)
#' @import data.table
#' @export crossjoin
crossjoin <- function(t1, t2)
{
  t1=cbind(t1,Dummy=1)
  t2=cbind(t2,Dummy=1)
  t1=data.table::as.data.table(t1)
  t2=data.table::as.data.table(t2)
  data.table::setkey(t1, Dummy)
  data.table::setkey(t2, Dummy)
  final <- t2[t1,
              allow.cartesian = T]
  t2[, Dummy := NULL]
  t1[, Dummy := NULL]
  final[, Dummy := NULL]
  return(final)
}


#' @title forecast accuracy
#' @description measure critical metrics for forecasting
#' @param actual target column to predict
#' @param forecast prediction column
#' @param outlist export column. available options: n,mean,sd,CV,R2,DB,FBias,MPE,MAPE,RMSE,MAD,MADP,MASE,RAE,WMAPE
#' @return NULL
#' @examples  accu(actual=105,forecast=100,outlist="mean,WMAPE,FBias")
#' @export accu
accu=function(actual,forecast,outlist="mean,FBias,WMAPE"
){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  AD=abs(actual-mean)
  R2=1-sum(error^2)/sum((actual-mean)^2)
  #AdjR2=1-(1-R2)*(n-1)/(n-k-1)
  DB=sum(diff(error)^2)/sum(error^2)
  #FE=sqrt(sum(error^2)/(n-k))
  FBias=sum(error)/sum(actual)
  MPE=sum(error/actual)/n
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  MASE=MAD/mean(abs(diff(actual)))
  RAE= sum(abs(error))/sum(abs(actual-mean))
  WMAPE=MAD/mean
  #l=data.frame(n,mean,sd,CV,AD,R2,DB,FBias,MPE,MAPE,RMSE,MAD,MADP,MASE,RAE,WMAPE,error)
  ctr = paste0("l=data.frame(",outlist,")")
  eval(parse(text=ctr))
  return(l)
}



#' @title fast date conversion
#' @description works faster than as.Date.
#' @param x date column to convert
#' @return NULL
#' @examples  fast.date(as.factor('2021-01-01'))
#' @export fast.date

fast.date=function(x){
  if(class(x)=="factor"){
    as.Date(levels(x))[as.integer(x)]
  }else{
    as.Date(levels(as.factor(x)))[as.integer(as.factor(x))]
  }
}


#' @title faster date conversion
#' @description works faster than fast.date
#' @param dt # data table
#' @param dateCol # date column name
#' @return NULL
#' @examples  exampleDataR = data.table::as.data.table(exampleDataR)
#' exampleDataR = exampleDataR[,dateformat:= date_mdy_ymd(date)]
#' faster_date(exampleDataR,"dateformat")
#' @import data.table
#' @export faster_date


faster_date <- function(dt,
                        dateCol = 'Date')
{
  dateCol_a=dateCol[1]
  for(dateCol_a in dateCol){
    condition=eval(parse(text=paste0("class(dt[1]$",dateCol_a,")=='Date'"
    )
    )
    )
    if (condition){
      return(dt)
    }else{
      init_col_order <- data.table::copy(colnames(dt))
      data.table::setnames(dt, dateCol_a, 'SuperDummyDate')
      dt=data.table::as.data.table(dt)
      dt[SuperDummyDate == '',
         SuperDummyDate := NA]
      dt[, c(dateCol_a) := as.Date(SuperDummyDate[1]),
         SuperDummyDate]
      dt[, SuperDummyDate := NULL]
      data.table::setcolorder(dt, c(init_col_order))
      return(dt)
    }
  }
}





#' @title gc without printed message
#' @description gcQuiet
#' @param quiet  T or F.
#' @return NULL
#' @examples  gcQuiet()
#' @export gcQuiet
#'
gcQuiet <- function(quiet = TRUE) {
  if(quiet) invisible(gc()) else gc()
}


#' @title list variables and size
#' @description list variables and size in r memory
#' @param n top N rows
#' @return NULL
#' @examples  data = data.table::as.data.table(exampleDataR)
#' lsos()
#' @import data.table
#' @export lsos

lsos <- function(..., n=10) {

  .ls.objects <- function (pos = 1, pattern, order.by,
                           decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
      fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
      capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
      as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
      out <- head(out, n)
    out
  }

  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


#' @title count unique values in a table
#' @description count unique values in a table
#' @param data input data table
#' @return NULL
#' @examples  uniquen(exampleDataR)
#' @export uniquen

uniquen<-function(data) sapply(data,function(x)length(unique(x)))

