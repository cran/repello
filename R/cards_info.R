#' Get Cards Information
#'
#' This function allows you to obtain the cards and the date of most recent modification for the cards on a specified board.
#' @param board_id The ID of the board you want to browse.
#' @importFrom httr GET
#' @importFrom httr content
#' @keywords repello
#' @export

cards_info <- function(board_id){
  if (!exists("key_token_exists", envir = globals)){
    return(warning("Need to set the key/token using 'set_key_token()'"))
  } else {
    key <- globals$trello_api_key_01142021
    token <- globals$trello_api_token_01142021
  }
  cards_info <- GET(paste0("https://api.trello.com/1/boards/", board_id, "/cards?key=", key, "&token=",token))
  cards_content <- content(cards_info)
  num_cards <- length(cards_content)

  cards <- c()
  card_id <- c()
  last_modified <- c()
  list <- c()
  list_name <- c()

  lists <- content(GET(paste0("https://api.trello.com/1/boards/", board_id, "/lists?key=", key, "&token=",token)))

  for (i in 1:num_cards){
    cards[i] <- cards_content[[i]]$name
    card_id[i] <- cards_content[[i]]$id
    last_modified[i] <- unlist(strsplit(cards_content[[i]]$dateLastActivity, "T"))[1]
    list[i] <- cards_content[[i]]$idList
    for (j in 1:length(lists)){
      if (list[i] == lists[[j]]$id){
        list_name[i] <- lists[[j]]$name
      }
    }
  }
  trello_activity <- data.frame(cards[1:num_cards], card_id[1:num_cards], last_modified[1:num_cards], list_name[1:num_cards])
  colnames(trello_activity) <- c("Card", "ID", "Date last modified", "Trello List")
  trello_activity
}
