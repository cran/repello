#' All Checklists
#'
#' This function allows you to obtain checklists and card info for all cards on a chosen board.
#' @param board_name The name of the board you want to browse.
#' @param save Set to TRUE to save a copy of the card information list.
#' @param user_token The user token for an individual's Trello account.
#' @import dplyr
#' @importFrom stringr str_remove_all
#' @keywords repello
#' @export

all_checklists <- function(board_name, save=FALSE, user_token=NULL){
  if (is.null(user_token) & !exists("trello_api_token_08192020", envir = globals)){
    return(warning("Need to input a user token or set the token using 'set_token()'"))
  }
  if (is.null(user_token) & exists("trello_api_token_08192020", envir = globals)){
    user_token <- globals$trello_api_token_08192020
  }
  board_id <- get_board_id(board_name, user_token)
  activity <- cards_info(board_id, user_token)
  my_checklist <- list()
  for (i in 1:nrow(activity)){
    my_checklist[[activity$ID[i]]] <- c(name=activity$Card[i], ID=activity$ID[i], date=activity$`Date last modified`[i], list=activity$`Trello List`[i], suppressWarnings(get_checklist(board_id, activity$Card[i], user_token)))
  }

  if (save==TRUE){
    filename <- gsub("date", paste0(Sys.Date(), "_", str_remove_all(unlist(strsplit(as.character(Sys.time()), " "))[2], ":")),"checklist_date.rds")
    saveRDS(my_checklist, filename)
  }
  my_checklist
}

