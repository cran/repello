#' Set Token
#'
#' This function allows you to set the path to the user token file.
#' @param filename The name of the file which contains the user token.
#' @param path The path to the location of the user token file
#' @importFrom utils read.delim
#' @keywords repello
#' @export

set_token <- function(filename, path=getwd()){
  if (filename %in% list.files(path=path)){
    globals$trello_api_token_08192020 <- suppressWarnings(read.delim(paste0(path, "/", filename), header=FALSE)[1,1])
  } else {
    return(warning("File not found: please input path to token file"))
  }
}
globals <- new.env()
