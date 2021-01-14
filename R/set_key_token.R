#' Set Key-Token
#'
#' This function allows you to set the user key and token for API calls.  You
#' can either link to text files which contain the key and token, or you
#' can leave the arguments blank and manually input the key and token when prompted
#' @param key_file The name of the file which contains the Trello key
#' @param token_file The name of the file which contains the user token
#' @param path The path to the location of the user token file
#' @importFrom utils read.delim
#' @keywords repello
#' @export

set_key_token <- function(key_file=NULL, token_file=NULL, path=getwd()){
  if (is.null(key_file)){
    key <- readline("Input the Trello key: ")
    key <- as.character(unlist(strsplit(key, ",")))
    if (length(key)==0){
      return(warning("Please input a valid key value"))
    }
    globals$trello_api_key_01142021 <- key
  } else {
    if (key_file %in% list.files(path=path)){
      globals$trello_api_key_01142021 <- suppressWarnings(read.delim(paste0(path, "/", key_file), header=FALSE)[1,1])
    } else {
      return(warning("File not found: please input path to key file"))
    }
  }
  
  if (is.null(token_file)){
    token <- readline("Input your user token: ")
    token <- as.character(unlist(strsplit(token, ",")))
    if (length(token)==0){
      return(warning("Please input a valid token value"))
    }
    globals$trello_api_token_01142021 <- token
  } else {
    if (token_file %in% list.files(path=path)){
      globals$trello_api_token_01142021 <- suppressWarnings(read.delim(paste0(path, "/", token_file), header=FALSE)[1,1])
    } else {
      return(warning("File not found: please input path to token file"))
    }
  }

  globals$key_token_exists <- TRUE
}
globals <- new.env()
