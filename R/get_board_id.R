#' Get Board ID
#'
#' This function allows you to obtain the ID of the Trello board you want to view.
#' @param board_name The name of the board you want to browse.
#' @importFrom httr GET
#' @importFrom httr content
#' @keywords repello
#' @export

get_board_id <- function(board_name){
  if (!exists("key_token_exists", envir = globals)){
    return(warning("Need to set the key/token using 'set_key_token()'"))
  } else {
    key <- globals$trello_api_key_01142021
    token <- globals$trello_api_token_01142021
  }
  boards_info <- GET(paste0("https://api.trello.com/1/members/me/boards?fields=name,url&key=", key, "&token=", token))
  boards_content <- content(boards_info)
  boardnames <- data.frame()
  for (i in 1:length(boards_content)){
    temp <- data.frame(boards_content[[i]])
    boardnames <- rbind(boardnames, temp)
  }
  target_board <- boardnames[which(boardnames$name==board_name),]$id
  target_board
}
