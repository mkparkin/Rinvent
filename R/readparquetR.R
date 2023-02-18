#' @title reading parquet or delta files from local directory or aws s3 or azure blob
#'
#' @description reads parquet,delta files from local or cloud
#' @param pathtoread reading path, local or azure cloud
#' @param where it will read datatable and filter with this condition. i.e. you can write where="column='A'"
#' @param partition if you want to read a partition files with a pattern. i.e. partition=c('2017','2018')
#' @param collist specific columns to read
#' @param sample sample=T just to see sample rows. dont read whole table
#' @param samplesizecount default=3 rows. you can change it
#' @param add_part_names when it is partitioned, you need to make this T to add partition names as column
#' @param filelocation "local" or "azure" or "s3"
#' @param containerconnection if filelocation="azure" then we need connection. something like this
#' @param bucket if filelocation="s3" we need to put bucket name
#' bl <- AzureStor::storage_endpoint(az.congif$production$link,
#'   key=az.congif$production$key)
#'   container_datastore <- AzureStor::storage_container(bl, "datastore")
#'
#' @return data table
#'
#' @examples  sample_data <- readparquetR("localfolder/",
#' collist = c("column1"
#'            ,"column2"
#'            ,"column3"
#' ),
#' format="delta",
#' where="SKU==19058901 & STORE!='1905'")
#' @examples readparquetR(pathtoread="C:/users/...",add_part_names=F,collist="",sample=F,where="sku=1 & store=1",partition="2022")
#' @export readparquetR
#' @import data.table


readparquetR = function(pathtoread,
                         where="",
                         partition="",
                         collist="",
                         sample=F,
                         samplesizecount=3,
                         add_part_names=F,
                         format="parquet",
                         filelocation="local",
                         containerconnection=NULL,
                         bucket=NULL
){

  starttime=Sys.time()

  if(filelocation=="azure" | filelocation=="s3"){
    if(filelocation == "azure" & is.null(containerconnection)){
      print("no valid connection. please use 'containerconnection'")
      break
    }

    if(filelocation == "s3" & is.null(bucket)){
      print("no valid bucket please use 'bucket' for aws connections")
      break
    }

    dest_path = file.path(tempdir(),
                          as.numeric(Sys.time()))

    if(filelocation=="azure"){
      filelistincloud = AzureStor::list_blobs(containerconnection,
                                              dir=pathtoread,
                                              prefix="delta",
                                              recursive = T)$name
    }

    if(filelocation=="s3"){

      filelistincloud = data.table::as.data.table(aws.s3::get_bucket_df(bucket = bucket,
                                                                        prefix = file.path(pathtoread),
                                                                        max = Inf))$Key


    }

    filelistincloud = filelistincloud[filelistincloud%like% 'delta']
    filelistincloud = filelistincloud[filelistincloud%like% '.json' |
                                        filelistincloud%like% '.checkpoint.parquet'
    ]

    filelistincloud=filelistincloud[order(filelistincloud)]

    #
    if(length(filelistincloud[filelistincloud %like% '.checkpoint.parquet'])>0){
      lastcheckpoint = tail(filelistincloud[filelistincloud %like% '.checkpoint.parquet'],1)
      filelistincloud=as.data.table(filelistincloud)
      filelistincloud[,cut_point:=0]
      filelistincloud[filelistincloud==lastcheckpoint,cut_point:=1]
      filelistincloud[,cut_point_cs:=cumsum(cut_point)]
      filelistincloud=filelistincloud[cut_point_cs==1]
      filelistincloud=filelistincloud$filelistincloud

    }







    if(length(filelistincloud)>0 & format!="delta"){
      print("enter format parameter")
      break
    }

    if(length(filelistincloud)>0 & format=="delta"){
      ff=filelistincloud[1]
      for(ff in filelistincloud){

        if(filelocation=="azure"){

          AzureStor::multidownload_blob(containerconnection,
                                        src = ff,
                                        dest = file.path(dest_path,
                                                         ff
                                        )
          )
        }

        if(filelocation=="s3"){
          system(paste0("aws s3 sync ",
                        file.path("s3:/", bucket, file.path(pathtoread,
                                                            ff)),
                        " ",
                        file.path(dest_path,
                                  ff)),
                 show.output.on.console = F)
        }


      }



      if(format=="delta"){
        jsonlist = list.files(dest_path,
                              pattern = ".json",
                              full.names = T,
                              recursive = T)

        jsonlist=jsonlist[order(jsonlist)]
        js = jsonlist[1]
        data_part_add_master = data.table()
        data_part_remove_master = data.table()

        for(js in jsonlist){
          data_lines = readr::read_lines(js)
          line_length = length(data_lines)
          ll=1
          for(ll in 1:line_length){
            data_json  =RJSONIO::fromJSON(data_lines[ll])
            data_path_add = data_json$add$path
            data_path_rmv = data_json$remove$path
            data_part_add_master=rbind(data_part_add_master,
                                       data_path_add
            )
            data_part_remove_master=rbind(data_part_remove_master,
                                          data_path_rmv
            )
          }
        }

        data_part_final = data_part_add_master[!x %in%
                                                 data_part_remove_master$x ]

        checkpointlist = list.files(dest_path,
                                    pattern = ".checkpoint.parquet",
                                    full.names = T,
                                    recursive = T)

        checkpointlist=checkpointlist[order(checkpointlist)]
        checkpointlist=tail(checkpointlist,1)
        allchecks=lapply(checkpointlist, function(x){
          as.data.table(arrow::read_parquet(x)
          )}
        )
        allchecks=data.table::as.data.table(rbindlist(allchecks))
        allchecks=allchecks[,list(x=`add.path`,
                                  remove=`remove.path`,
                                  txn.version
        )]
        allchecks=allchecks[!duplicated(allchecks)]
        allchecks_add = allchecks[!is.na(x),list(x)]
        allchecks_add=allchecks_add[!x %in% allchecks[!is.na(remove)]$remove]

        allchecks_add=allchecks_add[!x %in% data_part_remove_master$x]



        data_part_final = rbind(data_part_final,
                                allchecks_add
        )

        data_part_final=data_part_final[!duplicated(data_part_final)]






        downloadtolocallist = data_part_final$x
      }

      dd = downloadtolocallist[1]
      for(dd in downloadtolocallist){

        if(filelocation=="azure"){
          AzureStor::multidownload_blob(containerconnection,
                                        src = file.path(file.path(pathtoread,
                                                                  dd
                                        )),
                                        dest = file.path(dest_path,
                                                         dd
                                        ),
                                        overwrite = T)
        }

        if(filelocation=="s3"){
          system(paste0("aws s3 sync ",
                        file.path("s3:/", bucket, file.path(pathtoread,
                                                            dd)),
                        " ",
                        file.path(dest_path,
                                  dd)),
                 show.output.on.console = F)

        }



      }

    }


    if(format=="parquet"){

      if(filelocation=="azure"){

        if(stringr::str_sub(pathtoread,-8,-1)==".parquet"){
          AzureStor::multidownload_blob(containerconnection,
                                        src = file.path(file.path(pathtoread
                                        )),
                                        dest = file.path(dest_path,"singledownload.parquet"))

        }else{

          print("downloading..")

          AzureStor::multidownload_blob(containerconnection,
                                        src = file.path(file.path(pathtoread
                                        ),
                                        paste0("*.*",
                                               "parquet")),
                                        dest = dest_path)

          print("download done")
          print(dest_path)
        }

      }

      if(filelocation=="s3"){

        if(stringr::str_sub(pathtoread,-8,-1)==".parquet"){

          system(paste0("aws s3 sync ",
                        file.path("s3:/", bucket, file.path(pathtoread)),
                        " ",
                        file.path(dest_path,
                                  "singledownload.parquet")),
                 show.output.on.console = F)

        }else{

          system(paste0("aws s3 sync ",
                        file.path("s3:/", bucket, file.path(pathtoread)),
                        " ",
                        file.path(dest_path)),
                 show.output.on.console = F)
        }

      }


    }




    removetempdirectory = dest_path

    pathtoread = dest_path



  }









  checkd_delta_format = list.dirs(pathtoread)
  checkd_delta_format = checkd_delta_format[checkd_delta_format %like% '_delta_log']
  if(length(checkd_delta_format)>0 & format!="delta"){
    print("ERROR!!! format looks like DELTA, parameter needed= format='delta'")
    break
  }

  # if condition is written to handle if we give an exact parquet name as a path
  # else if will handle multiple partitions under the main folder
  if(stringr::str_sub(pathtoread,-8,-1)==".parquet"){
    x=pathtoread

    if(length(collist)>1){
      df = data.table::as.data.table(arrow::read_parquet(x,
                                                         col_select = all_of(collist)))
    }else{
      df = data.table::as.data.table(arrow::read_parquet(x))
    }
    if(nchar(where)>0){
      eval(parse(text=paste0("df=df[",where,"]")))
    }



    if(sample & nrow(df)>0){
      df=df[1]
    }

    colnames_all = (colnames(df))

    friendlycheck = colnames_all[colnames_all %ilike% "fiscal" |
                                   colnames_all %ilike% "iso" |
                                   colnames_all=="week" |
                                   colnames_all=="year" |
                                   colnames_all=="yearweek" |
                                   colnames_all=="yearmonth" |
                                   colnames_all=="month"|

                                   colnames_all=="WEEK" |
                                   colnames_all=="YEAR" |
                                   colnames_all=="YEARWEEK" |
                                   colnames_all=="YEARMONTH" |
                                   colnames_all=="MONTH"|

                                   colnames_all=="Week" |
                                   colnames_all=="Year" |
                                   colnames_all=="YearWeek" |
                                   colnames_all=="YearMonth" |
                                   colnames_all=="Month"

    ]

    ff = friendlycheck[1]
    for(ff in friendlycheck){

      code = paste0("class(df$",ff,")")
      if(eval(parse(text=code))=="character"){
        print(paste0("be careful,",ff," is character. might be trouble"))
      }


    }

    df


  }else{

    allfiles = list.files(pathtoread,
                          pattern=".parquet",
                          full.names = T,
                          recursive = TRUE
    )
    print(pathtoread)
    if(length(allfiles)==0){print("there is no row, make sure path is correct!")}

    #handle delta format

    if(format=="delta"){
      jsonlist = list.files(pathtoread,
                            pattern = ".json",
                            full.names = T,
                            recursive = T)

      jsonlist=jsonlist[order(jsonlist)]
      js = jsonlist[1]
      data_part_add_master = data.table()
      data_part_remove_master = data.table()

      for(js in jsonlist){
        data_lines = readr::read_lines(js)
        line_length = length(data_lines)
        ll=1
        for(ll in 1:line_length){
          data_json  =RJSONIO::fromJSON(data_lines[ll])
          data_path_add = data_json$add$path
          data_path_rmv = data_json$remove$path
          data_part_add_master=rbind(data_part_add_master,
                                     data_path_add
          )
          data_part_remove_master=rbind(data_part_remove_master,
                                        data_path_rmv
          )
        }
      }

      data_part_final = data_part_add_master[!x %in%
                                               data_part_remove_master$x ]

      #last checkpoint
      checkpointlist = list.files(pathtoread,
                                  pattern = ".checkpoint.parquet",
                                  full.names = T,
                                  recursive = T)

      checkpointlist=checkpointlist[order(checkpointlist)]
      checkpointlist=tail(checkpointlist,1)
      allchecks=lapply(checkpointlist, function(x){
        as.data.table(arrow::read_parquet(x)
        )}
      )
      allchecks=data.table::as.data.table(rbindlist(allchecks))
      allchecks=allchecks[,list(x=`add.path`,
                                remove=`remove.path`,
                                txn.version
      )]
      allchecks=allchecks[!duplicated(allchecks)]
      allchecks_add = allchecks[!is.na(x),list(x)]
      allchecks_add=allchecks_add[!x %in% allchecks[!is.na(remove)]$remove]

      allchecks_add=allchecks_add[!x %in% data_part_remove_master$x]



      data_part_final = rbind(data_part_final,
                              allchecks_add
      )

      data_part_final=data_part_final[!duplicated(data_part_final)]






      allfiles_sub = c()
      for(rr in 1:nrow(data_part_final)){
        allfiles_subs = allfiles[allfiles %like% data_part_final[rr]$x]
        allfiles_sub = c(allfiles_sub,allfiles_subs)
      }

      allfiles = copy(allfiles_sub)


    }


    if(length(partition)>0){
      pp=partition[1]
      partpartlist= c()
      for(pp in partition){
        partpartlist_sub = allfiles[allfiles%like% pp]
        partpartlist=c(partpartlist,partpartlist_sub)
      }

      allfiles = copy(partpartlist)
      #allfiles=allfiles[allfiles%like% partition]
    }

    if(sample){

      allfiles=head(allfiles,min(samplesizecount,
                                 length(allfiles)
      ))
    }

    x=allfiles[1]

    if(length(allfiles)==1 & add_part_names==T){
      print("are you sure with add_part_names=T since there is only one file")
    }

    output = data.table::as.data.table(data.table::rbindlist(lapply(allfiles,
                                                                    function(x){
                                                                      if(length(collist)>1){
                                                                        df = data.table::as.data.table(arrow::read_parquet(x,
                                                                                                                           col_select = all_of(collist)))
                                                                      }else{
                                                                        df = data.table::as.data.table(arrow::read_parquet(x))
                                                                      }
                                                                      if(add_part_names){
                                                                        dirnames = stringr::str_replace(x,
                                                                                                        pattern = pathtoread,
                                                                                                        "")
                                                                        dirnames= (strsplit(dirnames,'/'))
                                                                        i=1
                                                                        for(i in 1:(length(dirnames[[1]])-2)){
                                                                          name_at = dirnames[[1]][i+1]
                                                                          name_at= (strsplit(name_at,'='))
                                                                          name_at_1 = name_at[[1]][1]
                                                                          name_at_2 = name_at[[1]][2]
                                                                          df=as.data.table(df)
                                                                          eval(parse(text=paste0("df[,",name_at_1,
                                                                                                 ":='",name_at_2,"']")))


                                                                        }



                                                                      }

                                                                      if(nchar(where)>0){
                                                                        eval(parse(text=paste0("df=df[",where,"]")))
                                                                      }

                                                                      if(sample & nrow(df)>0){
                                                                        df=df[1]
                                                                      }




                                                                      df

                                                                    })))

    colnames_all = (colnames(output))

    friendlycheck = colnames_all[colnames_all %ilike% "fiscal" |
                                   colnames_all %ilike% "iso" |
                                   colnames_all=="week" |
                                   colnames_all=="year" |
                                   colnames_all=="yearweek" |
                                   colnames_all=="yearmonth" |
                                   colnames_all=="month"|

                                   colnames_all=="WEEK" |
                                   colnames_all=="YEAR" |
                                   colnames_all=="YEARWEEK" |
                                   colnames_all=="YEARMONTH" |
                                   colnames_all=="MONTH"|

                                   colnames_all=="Week" |
                                   colnames_all=="Year" |
                                   colnames_all=="YearWeek" |
                                   colnames_all=="YearMonth" |
                                   colnames_all=="Month"

    ]

    ff = friendlycheck[1]
    for(ff in friendlycheck){

      code = paste0("class(output$",ff,")")
      if(eval(parse(text=code))=="character"){
        print(paste0("be careful,",ff," is character. might be trouble"))
      }


    }

    endtime=Sys.time()
    difference <- difftime(endtime, starttime, units='mins')

    if(as.numeric(difference) > 5 ){
      print("long wait.. you can consider to use sample=T just to see the columns or 'where' or 'collist' to make it smaller. ")
    }

    if(filelocation=="azure" | filelocation=="s3"){
      unlink(removetempdirectory, recursive = T)

    }


    output

  }


}


