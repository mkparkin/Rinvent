#' @title read_csv_azure read csv files from azure blob
#' @description read_csv_azure read csv files from azure blob
#' @param pathtoread blob path to read
#' @param containerconnection blob connection
#' @return NULL
#' @examples  read_csv_azure()
#' @export read_csv_azure
#' @import AzureStor

read_csv_azure <- function(
    file_path,
    containerconnection
){


  path_sep = unlist(strsplit(file_path,"/"))
  file_name <- path_sep[length(path_sep)]


  AzureStor::download_blob(containerconnection,
                           src= file.path(file_path),
                           dest=file.path(tempdir(),
                                          file_name),
                           overwrite = T)

  df <- fread(file.path(file.path(tempdir(),
                                  file_name)))

  unlink(file.path(file.path(tempdir(),
                             file_name)),
         recursive = T)

  return(df)

}


