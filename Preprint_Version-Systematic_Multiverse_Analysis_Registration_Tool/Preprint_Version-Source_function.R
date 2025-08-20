library(shiny)
library(visNetwork)
library(shinyjs)
library(shinythemes)
library(DT)
library(bslib)
library(bsplus)
library(htmltools)
library(sortable)
library(shinyBS)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(webshot2)
library(pagedown)

Sys.setlocale('LC_ALL', 'C')

#####################
##### Upload File ####
#####################

upload_to_cloud <- function(local_file_path, cloud_filename) {
  require(httr)
  
  dav <- "https://INSERT_PERSONAL_OR_INSTITUTIONAL_CLOUD_DIRECTORY/remote.php/dav/files/"
  username <- "INSERT USERNAME"
  password <- "INSERT PASSWORD"
  
  # Construct full remote path
  remote_url <- paste0(dav, username, "/INSERT_THE_FOLDER_NAME_WITHIN_CLOUD/", cloud_filename)
  
  # Upload file to WebDAV using PUT request
  response <- httr::PUT(
    url = remote_url,
    authenticate(username, password),
    body = upload_file(local_file_path),
    verbose()
  )
  
  if (response$status_code == 201 || response$status_code == 204) {
    message("✅ Successfully uploaded to cloud: ", cloud_filename)
  } else {
    message("❌ Upload failed! HTTP Status: ", response$status_code)
    message("Response: ", content(response, as = "text"))
  }
}