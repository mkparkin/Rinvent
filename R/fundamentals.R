#' @title find latest sunday
#' @description find latest sunday date
#' @param date date value
#' @examples exampleDataR= data.table::as.data.table(exampleDataR)
#' exampleDataR = exampleDataR[order(location,item,date)]
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
#' @examples  crossjoin()
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


#' @title accu
#'
#' @description accu
#' @param actual target column to predict
#' @param forecast prediction column
#' @param outlist export column. available options: n,mean,sd,CV,R2,DB,FBias,MPE,MAPE,RMSE,MAD,MADP,MASE,RAE,WMAPE
#' @return NULL
#' @examples  accu(actual=5,forecast=10,outlist="mean,WMAPE,FBias")
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
#' @examples  fast.date('as.factor('2021-01-01')')
#' @export fast.date

fast.date=function(x){
  if(class(x)=="factor"){
    as.Date(levels(x))[as.integer(x)]
  }else{
    as.Date(levels(as.factor(x)))[as.integer(as.factor(x))]
  }
}


#' @title faster date conversion
#'
#' @description works faster than fast.date
#'
#' @param dt,dateCol
#'
#' @return NULL
#'
#' @examples  faster_date(dt,"Date")
#'
#' @export faster_date
#' @import data.table


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





#' @title gcQuiet
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
#' @description list variables and size
#' @param n top N rows
#' @return NULL
#' @examples  lsos()
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


#' @title uniquen
#' @description uniquen
#' @param data input data table
#' @return NULL
#' @examples  uniquen(df)
#' @export uniquen

uniquen<-function(data) sapply(data,function(x)length(unique(x)))




