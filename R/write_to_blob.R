#' Write data to an Azure Blob storage container
#'
#' @title write_to_blob
#' @description Writes data to an Azure Blob storage container using the specified format
#'
#' @param data A data object to write to Azure Blob storage
#' @param templocalpath The path to a temporary local directory to use for intermediate files (default is a new directory in the system temp directory)
#' @param savename The name to use for the file in Azure Blob storage
#' @param containerconnection The connection to the Azure Blob storage container
#' @param blobfilepath The path within the container to write the file (default is the root of the container)
#' @param format The format to use when writing the data (one of "parquet", "rds", or "csv")
#' @param overwrite overwrite existing file (one of "parquet", "rds", or "csv")
#' @examples
#' write_to_blob(datatable,
#'               savename = "data.parquet",
#'               containerconnection = mycontainer,
#'               blobfilepath = file.path("mainfolder", "subfolder"),
#'               overwrite=FALSE
#'               )
#'
#' @import AzureStor
#' @import arrow
#' @export write_to_blob

write_to_blob = function(data,
                         templocalpath="default",
                         savename="my_df.parquet",
                         containerconnection=NULL,
                         blobfilepath="trial",
                         format="parquet",
                         overwrite=FALSE) {

  # If the file already exists in the specified path and overwrite is FALSE, then do not overwrite the file.
  # If overwrite is set to TRUE, then overwrite the existing file.
  if (overwrite == FALSE) {
    filelistincloud = AzureStor::list_blobs(containerconnection, 
                                            dir = blobfilepath, 
                                            prefix = savename, 
                                            recursive = T)$name
    if (paste0(blobfilepath, "/", savename) %in% filelistincloud) {
      print("File exists in the path and you have selected to not overwrite. Change overwrite = T if you want.")
      break
    }		
  }
  
  
  # Create a temporary local directory if one wasn't provided.
  if (templocalpath == "default") {
    templocalpath = file.path(tempdir(), as.numeric(Sys.time()))
    dir.create(templocalpath)
  }

  # Write the data to a file in the specified format.
  if (format == "parquet") {
    arrow::write_parquet(data, file.path(templocalpath, savename))
  }
  if (format == "rds") {
    saveRDS(data, file.path(templocalpath, savename))
  }
  if (format == "csv") {
    data.table::fwrite(data, file.path(templocalpath, savename))
  }

  # Upload the file to the Azure Blob storage container.
  AzureStor::storage_upload(containerconnection,
                            file.path(templocalpath, savename),
                            file.path(blobfilepath, savename))

  # Remove the temporary local directory and its contents if one was created.
  if (templocalpath == "default") {
    unlink(templocalpath, recursive = TRUE)
  } else {
    file.remove(file.path(templocalpath, savename))
  }
}
