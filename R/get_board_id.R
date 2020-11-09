#' Get Board ID
#'
#' This function allows you to obtain the ID of the Trello board you want to view.
#' @param board_name The name of the board you want to browse.
#' @param user_token The user token for an individual's Trello account.
#' @importFrom httr GET
#' @importFrom httr content
#' @keywords repello
#' @export

get_board_id <- function(board_name, user_token=NULL){
  if (is.null(user_token) & !exists("trello_api_token_08192020", envir = globals)){
    return(warning("Need to input a user token or set the token using 'set_token()'"))
  }
  if (is.null(user_token) & exists("trello_api_token_08192020", envir = globals)){
    user_token <- globals$trello_api_token_08192020
  }
  token <- user_token
  boards_info <- GET(paste0("https://api.trello.com/1/members/me/boards?fields=name,url&key=5b771b8595d9fa76ac8724387d9642b4&token=", token))
  boards_content <- content(boards_info)
  boardnames <- data.frame()
  for (i in 1:length(boards_content)){
    temp <- data.frame(boards_content[[i]])
    boardnames <- rbind(boardnames, temp)
  }
  target_board <- boardnames[which(boardnames$name==board_name),]$id
  target_board
}
