#' All Checklists
#'
#' This function allows you to obtain checklists and card info for all cards on a chosen board.
#' @param board_name The name of the board you want to browse.
#' @param save Set to TRUE to save a copy of the card information list.
#' @import dplyr
#' @importFrom stringr str_remove_all
#' @keywords repello
#' @export

all_checklists <- function(board_name, save=FALSE){
  if (!exists("key_token_exists", envir = globals)){
    return(warning("Need to set the key/token using 'set_key_token()'"))
  } else {
    key <- globals$trello_api_key_01142021
    token <- globals$trello_api_token_01142021
  }
  board_id <- get_board_id(board_name)
  activity <- cards_info(board_id)
  my_checklist <- list()
  for (i in 1:nrow(activity)){
    my_checklist[[activity$ID[i]]] <- c(name=activity$Card[i], ID=activity$ID[i], date=activity$`Date last modified`[i], list=activity$`Trello List`[i], suppressWarnings(get_checklist(board_id, activity$Card[i])))
  }

  if (save==TRUE){
    filename <- gsub("date", paste0(Sys.Date(), "_", str_remove_all(unlist(strsplit(as.character(Sys.time()), " "))[2], ":")),"checklist_date.rds")
    saveRDS(my_checklist, filename)
  }
  my_checklist
}

