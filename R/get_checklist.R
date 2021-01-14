#' Get Checklist
#'
#' This function allows you to obtain a checklist for a card of interest.
#' @param board_id The ID of the board you want to browse.
#' @param card_name The name of the card you want to view
#' @importFrom httr GET
#' @importFrom httr content
#' @keywords repello
#' @export

get_checklist <- function(board_id, card_name){
  if (!exists("key_token_exists", envir = globals)){
    return(warning("Need to set the key/token using 'set_key_token()'"))
  } else {
    key <- globals$trello_api_key_01142021
    token <- globals$trello_api_token_01142021
  }
  activity <- cards_info(board_id)
  ID <- activity[which(activity$Card==card_name),]$ID
  checklist_url <- paste0("https://api.trello.com/1/cards/", ID, "/checklists?key=", key, "&token=", token)
  checklist_info <- GET(checklist_url)
  checklist_content <- content(checklist_info)
  if (length(checklist_content)==0){
    item <- "No checklist for this project"
    status <- "No checklist for this project"
    checklist <- data.frame(item, status)
    checklist
  } else {
    list_length <- length(checklist_content[[1]]$checkItems)
    item <- c()
    status <- c()
    for (i in 1:list_length){
      item[i] <- (checklist_content[[1]]$checkItems)[[i]]$name
      status[i] <- (checklist_content[[1]]$checkItems)[[i]]$state
    }
    checklist <- data.frame(item, status)
    checklist
  }
}

