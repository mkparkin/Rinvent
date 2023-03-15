#' @title save your files to Amazon S3 with "tryCatch" capability
#'
#' @description save your files with tryCatch() to implement retry capability
#'
#' @param df, format, object, bucket
#'
#' @return NULL
#'
#' @examples s3save_with_tryCatch(df = results_df, format = "csv", object = file.path("target_s3_path.csv"), bucket = "bucket_name")
#'
#' @export s3save_with_tryCatch
#' @import aws.s3
#' @import data.table
#' @import arrow

s3save_with_tryCatch <- function(df, format, object, bucket){
  
  # this function provides a more robust saving mechanism
  # when writing large files to Amazon S3
  
  ## input control
  
  # 1 - file format control
  allowed_formats <- c("csv",
                       "rds",
                       "parquet")
  
  if (!format %in% allowed_formats){
    stop(paste0("You can only write .csv, .rds and .parquet files with ",
                "this function."))
    
  }else{
    
    if(format == "rds"){
      
      tryCatch(
        expr = {
          aws.s3::s3saveRDS(df,
                            object = object,
                            bucket = bucket)
        },
        error = function(e){
          
          print(paste0("Couldn't write in the first try."))
          print(paste0("Now trying again..."))
          
          Sys.sleep(5)
          
          aws.s3::s3saveRDS(df,
                            object = object,
                            bucket = bucket)
          
          print(paste0("Successfully completed in the second try."))
          
        }
      )
      
    }
    if(format == "csv"){
      function_to_write = fwrite
    }
    else{
      function_to_write = arrow::write_parquet
    }
    
    if(format %in% c("csv", "parquet")){
      
      tryCatch(
        expr = {
          aws.s3::s3write_using(df,
                                FUN = function_to_write,
                                object = object,
                                bucket = bucket)
        },
        error = function(e){
          
          print(paste0("Couldn't write in the first try."))
          print(paste0("Now trying again..."))
          
          Sys.sleep(5)
          
          aws.s3::s3write_using(df,
                                FUN = function_to_write,
                                object = object,
                                bucket = bucket)
          
          print(paste0("Successfully completed in the second try."))
          
        }
      )
    }
  }
  
  print(paste0("File has been successfully written to the following path on S3: ",
               object))
  
}